-- | Pure parser for MFT record headers and attribute slices.
module NTFS.MFT.Record
  ( MFTRecord (..)
  , RawAttribute (..)
  , parseMFTRecord
  , isRecordInUse
  , isDirectoryRecord
  , fixupUpdateSequence
  ) where

import           Data.Bits       (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import           Data.Word       (Word16, Word32, Word64)

import           NTFS.Types      (MFTRef (..), ParseError (..))

-- | Header and attributes of one MFT record.
data MFTRecord = MFTRecord
  { recordNumber               :: Word32
  , recordSequenceNumber       :: Word16
  , recordHardLinkCount        :: Word16
  , recordFirstAttributeOffset :: Word16
  , recordFlags                :: Word16
  , recordUsedSize             :: Word32
  , recordAllocatedSize        :: Word32
  , recordBaseRecord           :: MFTRef
  , recordNextAttributeId      :: Word16
  , recordBytes                :: BS.ByteString
  , recordAttributes           :: [RawAttribute]
  } deriving (Eq, Show)

-- | An attribute kept as an unparsed byte slice, including its header.
data RawAttribute = RawAttribute
  { rawAttributeType  :: Word32
  , rawAttributeBytes :: BS.ByteString
  } deriving (Eq, Show)

-- | Parse an MFT record. The sector size comes from the boot sector.
parseMFTRecord :: Word16 -> BS.ByteString -> Either ParseError MFTRecord
parseMFTRecord bytesPerSector bytes = do
  fixed <- applyFixup bytesPerSector bytes
  header <- parseHeader fixed
  attributes <- parseAttributes header
  pure header {recordAttributes = attributes}

-- | Whether the record is marked as in use.
isRecordInUse :: MFTRecord -> Bool
isRecordInUse record = recordFlags record .&. 0x0001 /= 0

-- | Whether the record describes a directory.
isDirectoryRecord :: MFTRecord -> Bool
isDirectoryRecord record = recordFlags record .&. 0x0002 /= 0

fileSignature :: BS.ByteString
fileSignature = BS.pack [0x46, 0x49, 0x4c, 0x45]

applyFixup :: Word16 -> BS.ByteString -> Either ParseError BS.ByteString
applyFixup bytesPerSector bytes
  | BS.length bytes < 0x30 =
      Left (UnexpectedEndOfData "MFT record header is truncated")
  | BS.take 4 bytes /= fileSignature =
      Left (InvalidValue "MFT record signature is not FILE")
  | bytesPerSector < 2 = Left (InvalidValue "bytes per sector is too small")
  | otherwise = fixupUpdateSequence bytesPerSector bytes

-- | Restore the sector trailers listed in a record's update sequence array.
-- The caller is responsible for checking the record signature.
fixupUpdateSequence :: Word16 -> BS.ByteString -> Either ParseError BS.ByteString
fixupUpdateSequence bytesPerSector bytes
  | BS.length bytes < 8 = Left (UnexpectedEndOfData "record header is truncated")
  | bytesPerSector < 2 = Left (InvalidValue "bytes per sector is too small")
  | usaCount == 0 = Left (InvalidValue "update sequence array is empty")
  | usaOffset < 0x08 = Left (InvalidValue "update sequence array offset is too small")
  | usaOffset + 2 * usaCount > BS.length bytes =
      Left (UnexpectedEndOfData "update sequence array is truncated")
  | any outOfRange positions =
      Left (UnexpectedEndOfData "record sector trailer is truncated")
  | any trailerMismatch positions =
      Left (InvalidValue "record update sequence number mismatch")
  | otherwise = Right (foldl replace bytes (zip positions replacements))
  where
    usaOffset = fromIntegral (word16At bytes 0x04)
    usaCount = fromIntegral (word16At bytes 0x06)
    usn = word16At bytes usaOffset
    sectorBytes = fromIntegral bytesPerSector
    sectorCount = usaCount - 1
    positions = [(i + 1) * sectorBytes - 2 | i <- [0 .. sectorCount - 1]]
    replacements = [word16At bytes (usaOffset + 2 * (i + 1)) | i <- [0 .. sectorCount - 1]]
    outOfRange position = position + 2 > BS.length bytes
    trailerMismatch position = word16At bytes position /= usn
    replace value (position, replacement) = replaceWord16 position replacement value

parseHeader :: BS.ByteString -> Either ParseError MFTRecord
parseHeader bytes
  | BS.length bytes < 0x30 =
      Left (UnexpectedEndOfData "MFT record header is truncated")
  | BS.take 4 bytes /= fileSignature =
      Left (InvalidValue "MFT record signature is not FILE")
  | firstAttribute < 0x30 || firstAttribute > BS.length bytes =
      Left (InvalidValue "first attribute offset is out of range")
  | fromIntegral allocated > BS.length bytes =
      Left (UnexpectedEndOfData "MFT record is truncated")
  | used > allocated =
      Left (InvalidValue "used size exceeds allocated size")
  | otherwise =
      Right
        MFTRecord
          { recordNumber = word32At bytes 0x2c
          , recordSequenceNumber = word16At bytes 0x10
          , recordHardLinkCount = word16At bytes 0x12
          , recordFirstAttributeOffset = word16At bytes 0x14
          , recordFlags = word16At bytes 0x16
          , recordUsedSize = word32At bytes 0x18
          , recordAllocatedSize = word32At bytes 0x1c
          , recordBaseRecord = MFTRef (word64At bytes 0x20)
          , recordNextAttributeId = word16At bytes 0x28
          , recordBytes = bytes
          , recordAttributes = []
          }
  where
    firstAttribute = fromIntegral (word16At bytes 0x14)
    used = word32At bytes 0x18
    allocated = word32At bytes 0x1c

parseAttributes :: MFTRecord -> Either ParseError [RawAttribute]
parseAttributes header = go (fromIntegral (recordFirstAttributeOffset header))
  where
    bytes = recordBytes header
    limit = fromIntegral (recordUsedSize header)
    go offset
      | offset > limit = Left (InvalidValue "first attribute offset exceeds used size")
      | offset == limit = Right []
      | offset + 8 > limit = Left (UnexpectedEndOfData "attribute header is truncated")
      | attributeType == 0xffffffff = Right []
      | attributeLength < 8 = Left (InvalidValue "attribute length is too small")
      | offset + fromIntegral attributeLength > limit =
          Left (InvalidValue "attribute length exceeds used size")
      | otherwise = do
          rest <- go (offset + fromIntegral attributeLength)
          pure (RawAttribute attributeType (slice offset attributeLength) : rest)
      where
        attributeType = word32At bytes offset
        attributeLength = word32At bytes (offset + 4)
    slice offset attributeLength =
      BS.take (fromIntegral attributeLength) (BS.drop offset bytes)

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

word16Bytes :: Word16 -> BS.ByteString
word16Bytes value =
  BS.pack
    [ fromIntegral value
    , fromIntegral (value `shiftR` 8)
    ]

replaceWord16 :: Int -> Word16 -> BS.ByteString -> BS.ByteString
replaceWord16 offset value bytes =
  BS.take offset bytes <> word16Bytes value <> BS.drop (offset + 2) bytes
