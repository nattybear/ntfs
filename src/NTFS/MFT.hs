-- | Enumeration of MFT records in a volume.
module NTFS.MFT
  ( mftRecordCount
  , readAllMFTRecords
  ) where

import qualified Data.ByteString     as BS

import           NTFS.Attribute.Data (FileData (..), parseDataAttribute)
import           NTFS.BootSector
import           NTFS.MFT.Attribute
import           NTFS.MFT.Record
import           NTFS.Types
import           NTFS.Volume

-- | Number of records in the MFT, from the size of record 0's @$DATA@.
mftRecordCount :: Volume -> IO (Either ParseError Int)
mftRecordCount volume = do
  bytes <- readMFTRecordBytes volume (MFTRef 0)
  pure $ do
    raw <- bytes
    record <- parseMFTRecord sectorBytes raw
    dataAttributes <- mapM parseAttribute (filter isData (recordAttributes record))
    let unnamed = filter (maybe True BS.null . attributeName) dataAttributes
    case unnamed of
      [] -> Left (InvalidValue "$MFT has no unnamed $DATA attribute")
      attribute:_ -> do
        value <- parseDataAttribute attribute
        ClusterSize recordSize <- mftRecordSize (volumeBootSector volume)
        let total =
              case value of
                ResidentData valueBytes -> toInteger (BS.length valueBytes)
                NonResidentData _ _ realSize _ -> toInteger realSize
        Right (fromIntegral (total `div` toInteger recordSize))
  where
    sectorBytes = bsBytesPerSector (volumeBootSector volume)
    isData raw = rawAttributeType raw == attributeTypeData

-- | Read and parse every MFT record in order.
-- A malformed record stops the enumeration with an error.
readAllMFTRecords :: Volume -> IO (Either ParseError [MFTRecord])
readAllMFTRecords volume = do
  count <- mftRecordCount volume
  case count of
    Left err    -> pure (Left err)
    Right limit -> go 0 limit []
  where
    sectorBytes = bsBytesPerSector (volumeBootSector volume)
    go index limit acc
      | index >= limit = pure (Right (reverse acc))
      | otherwise = do
          bytes <- readMFTRecordBytes volume (MFTRef (fromIntegral index))
          case bytes >>= parseMFTRecord sectorBytes of
            Left err     -> pure (Left err)
            Right record -> go (index + 1) limit (record : acc)
