-- | Directory index entries from @$INDEX_ROOT@ and @$INDEX_ALLOCATION@.
module NTFS.Index
  ( IndexEntry (..)
  , attributeTypeIndexRoot
  , attributeTypeIndexAllocation
  , parseIndexRoot
  , parseIndexRecord
  , readDirectoryEntries
  , findDirectoryEntry
  ) where

import           Data.Bits               (shiftL, (.&.), (.|.))
import qualified Data.ByteString         as BS
import           Data.Word               (Word16, Word32, Word64)

import           NTFS.Attribute.Data     (FileData (..))
import           NTFS.Attribute.FileName (FileName (..), parseFileNameValue)
import           NTFS.BootSector
import           NTFS.MFT.Attribute
import           NTFS.MFT.Record         (MFTRecord (..), RawAttribute (..),
                                          fixupUpdateSequence)
import           NTFS.Runlist            (DataRun (..), parseRunlist)
import           NTFS.Types
import           NTFS.Volume

-- | Type code of the @$INDEX_ROOT@ attribute.
attributeTypeIndexRoot :: Word32
attributeTypeIndexRoot = 0x90

-- | Type code of the @$INDEX_ALLOCATION@ attribute.
attributeTypeIndexAllocation :: Word32
attributeTypeIndexAllocation = 0xa0

-- | One entry in a directory index. Entries pointing only to a child node are
-- omitted because they carry no file name.
data IndexEntry = IndexEntry
  { indexFileReference :: MFTRef
  , indexFileName      :: FileName
  , indexChildVCN      :: Maybe VCN
  } deriving (Eq, Show)

-- | Parse the resident @$INDEX_ROOT@ of a directory record.
parseIndexRoot :: Attribute -> Either ParseError [IndexEntry]
parseIndexRoot attr
  | attributeTypeCode attr /= attributeTypeIndexRoot =
      Left (InvalidValue "attribute is not $INDEX_ROOT")
  | otherwise =
      case attributeValue attr of
        NonResidentValue{} -> Left (InvalidValue "$INDEX_ROOT is not resident")
        ResidentValue bytes
          | BS.length bytes < 0x20 ->
              Left (UnexpectedEndOfData "$INDEX_ROOT is truncated")
          | indexedType /= attributeTypeFileName ->
              Left (InvalidValue "$INDEX_ROOT does not index $FILE_NAME")
          | entryEnd > BS.length bytes ->
              Left (UnexpectedEndOfData "$INDEX_ROOT entries are truncated")
          | otherwise -> parseEntries entryStart entryEnd bytes
          where
            indexedType = word32At bytes 0x00
            entryStart = 0x10 + fromIntegral (word32At bytes 0x10)
            entryEnd = 0x10 + fromIntegral (word32At bytes 0x14)

-- | Parse one @INDX@ block from @$INDEX_ALLOCATION@, applying its fix-up.
parseIndexRecord :: Word16 -> BS.ByteString -> Either ParseError [IndexEntry]
parseIndexRecord bytesPerSector bytes
  | BS.length bytes < 0x28 =
      Left (UnexpectedEndOfData "index record is truncated")
  | BS.take 4 bytes /= indexSignature =
      Left (InvalidValue "index record signature is not INDX")
  | otherwise = do
      fixed <- fixupUpdateSequence bytesPerSector bytes
      let entryStart = 0x18 + fromIntegral (word32At fixed 0x18)
          entryEnd = 0x18 + fromIntegral (word32At fixed 0x1c)
      if entryEnd > BS.length fixed
        then Left (UnexpectedEndOfData "index record entries are truncated")
        else parseEntries entryStart entryEnd fixed

-- | Read all named entries of a directory record, including @$INDEX_ALLOCATION@.
readDirectoryEntries ::
  Volume -> MFTRecord -> IO (Either ParseError [IndexEntry])
readDirectoryEntries volume record =
  case traverse parseAttribute (filter isIndex (recordAttributes record)) of
    Left err -> pure (Left err)
    Right attrs -> do
      let roots = filter ((== attributeTypeIndexRoot) . attributeTypeCode) attrs
          allocations = filter ((== attributeTypeIndexAllocation) . attributeTypeCode) attrs
      case traverse parseIndexRoot roots of
        Left err -> pure (Left err)
        Right rootEntries -> do
          blocks <- traverse (readIndexAllocation volume) allocations
          pure $ fmap (\groups -> concat rootEntries ++ concat groups) (sequence blocks)
  where
    isIndex raw =
      rawAttributeType raw == attributeTypeIndexRoot
        || rawAttributeType raw == attributeTypeIndexAllocation

-- | Find an entry by exact file name. NTFS name collation is not applied.
findDirectoryEntry ::
  Volume -> MFTRecord -> String -> IO (Either ParseError (Maybe IndexEntry))
findDirectoryEntry volume record name = do
  entries <- readDirectoryEntries volume record
  pure $ fmap (findNamed name) entries

findNamed :: String -> [IndexEntry] -> Maybe IndexEntry
findNamed name entries =
  case filter ((== name) . fileNameName . indexFileName) entries of
    entry:_ -> Just entry
    []      -> Nothing

readIndexAllocation :: Volume -> Attribute -> IO (Either ParseError [IndexEntry])
readIndexAllocation volume attr
  | attributeTypeCode attr /= attributeTypeIndexAllocation =
      pure (Left (InvalidValue "attribute is not $INDEX_ALLOCATION"))
  | otherwise =
      case attributeValue attr of
        ResidentValue _ -> pure (Left (InvalidValue "$INDEX_ALLOCATION is not non-resident"))
        NonResidentValue _ lastVCN runlistBytes allocated real initialized ->
          case parseRunlist runlistBytes of
            Left err -> pure (Left err)
            Right runs
              | null runs -> pure (Left (InvalidValue "$INDEX_ALLOCATION runlist is empty"))
              | sum (map runLength runs) <= fromIntegral (vcnNumber lastVCN) ->
                  pure (Left (InvalidValue "$INDEX_ALLOCATION runlist does not cover its VCN range"))
              | otherwise -> do
                  bytes <- readData volume (NonResidentData runs allocated real initialized)
                  pure $ do
                    content <- bytes
                    ClusterSize blockSize <- indexRecordSize (volumeBootSector volume)
                    let blocks = chunks (fromIntegral blockSize) content
                        sectorBytes = bsBytesPerSector (volumeBootSector volume)
                    concat <$> traverse (parseIndexRecord sectorBytes) blocks

parseEntries :: Int -> Int -> BS.ByteString -> Either ParseError [IndexEntry]
parseEntries start end bytes = go start []
  where
    go offset acc
      | offset >= end = Right (reverse acc)
      | offset + 0x10 > end =
          Left (UnexpectedEndOfData "index entry header is truncated")
      | entryLength < 0x10 = Left (InvalidValue "index entry length is too small")
      | offset + entryLength > end =
          Left (UnexpectedEndOfData "index entry extends past index area")
      | keyLength == 0 = go (offset + entryLength) acc
      | keyEnd > offset + entryLength =
          Left (UnexpectedEndOfData "index entry key is truncated")
      | otherwise = do
          name <- parseFileNameValue (BS.take keyLength (BS.drop keyStart bytes))
          child <- parseChild flags keyEnd (offset + entryLength)
          let next = IndexEntry (MFTRef fileRefValue) name child : acc
          if flags .&. 0x01 /= 0
            then Right (reverse next)
            else go (offset + entryLength) next
      where
        fileRefValue = word64At bytes offset
        entryLength = fromIntegral (word16At bytes (offset + 0x08))
        keyLength = fromIntegral (word16At bytes (offset + 0x0a))
        flags = word16At bytes (offset + 0x0c)
        keyStart = offset + 0x10
        keyEnd = keyStart + keyLength

    parseChild flags position limit
      | flags .&. 0x02 == 0 = Right Nothing
      | position + 8 > limit =
          Left (UnexpectedEndOfData "index entry child VCN is truncated")
      | otherwise = Right (Just (VCN (fromIntegral (word64At bytes position))))

indexSignature :: BS.ByteString
indexSignature = BS.pack [0x49, 0x4e, 0x44, 0x58]

vcnNumber :: VCN -> Integer
vcnNumber (VCN value) = toInteger value

chunks :: Int -> BS.ByteString -> [BS.ByteString]
chunks size bytes
  | BS.null bytes = []
  | otherwise = BS.take size bytes : chunks size (BS.drop size bytes)

word16At :: BS.ByteString -> Int -> Word16
word16At bytes offset =
  fromIntegral (BS.index bytes offset)
    .|. (fromIntegral (BS.index bytes (offset + 1)) `shiftL` 8)

word32At :: BS.ByteString -> Int -> Word32
word32At bytes offset =
  fromIntegral (word16At bytes offset)
    .|. (fromIntegral (word16At bytes (offset + 2)) `shiftL` 16)

word64At :: BS.ByteString -> Int -> Word64
word64At bytes offset =
  fromIntegral (word32At bytes offset)
    .|. (fromIntegral (word32At bytes (offset + 4)) `shiftL` 32)
