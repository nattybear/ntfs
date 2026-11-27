-- | Volume-level access to an NTFS image.
module NTFS.Volume
  ( Volume (..)
  , openVolume
  , clusterOffset
  , mftOffset
  , readMFTRecordBytes
  , readData
  ) where

import qualified Data.ByteString     as BS
import           System.IO           (Handle, SeekMode (AbsoluteSeek), hSeek)

import           NTFS.Attribute.Data (FileData (..))
import           NTFS.BootSector
import           NTFS.Runlist        (DataRun (..))
import           NTFS.Types

-- | An open NTFS volume backed by a seekable handle.
data Volume = Volume
  { volumeHandle     :: Handle
  , volumeBootSector :: BootSector
  }

-- | Read the boot sector and open the volume.
openVolume :: Handle -> IO (Either ParseError Volume)
openVolume handle = do
  hSeek handle AbsoluteSeek 0
  bytes <- BS.hGet handle bootSectorSize
  pure (fmap (Volume handle) (parseBootSector bytes))

-- | Byte offset of a logical cluster.
clusterOffset :: Volume -> LCN -> Integer
clusterOffset volume (LCN lcn) =
  fromIntegral lcn * fromIntegral clusterBytes
  where
    ClusterSize clusterBytes = bytesPerCluster (volumeBootSector volume)

-- | Byte offset of the first MFT record.
mftOffset :: Volume -> Integer
mftOffset volume = clusterOffset volume (bsMFTStart (volumeBootSector volume))

-- | Read the bytes of one MFT record.
readMFTRecordBytes :: Volume -> MFTRef -> IO (Either ParseError BS.ByteString)
readMFTRecordBytes volume (MFTRef ref) =
  case mftRecordSize (volumeBootSector volume) of
    Left err -> pure (Left err)
    Right (ClusterSize size) -> do
      let offset = mftOffset volume + fromIntegral ref * fromIntegral size
      hSeek (volumeHandle volume) AbsoluteSeek offset
      bytes <- BS.hGet (volumeHandle volume) (fromIntegral size)
      if fromIntegral (BS.length bytes) == size
        then pure (Right bytes)
        else pure (Left (UnexpectedEndOfData "MFT record is truncated"))

-- | Read the bytes described by a @$DATA@ attribute.
-- Sparse runs are returned as zero bytes; the result is trimmed to the real size.
readData :: Volume -> FileData -> IO (Either ParseError BS.ByteString)
readData _ (ResidentData bytes) = pure (Right bytes)
readData volume (NonResidentData runs realSize _ _) = do
  results <- sequence (map (readRun volume) runs)
  pure $ do
    chunks <- sequence results
    let bytes = BS.concat chunks
    let total = fromIntegral (BS.length bytes) :: Integer
    if fromIntegral realSize > total
      then Left (UnexpectedEndOfData "$DATA is shorter than its real size")
      else Right (BS.take (fromIntegral realSize) bytes)

readRun :: Volume -> DataRun -> IO (Either ParseError BS.ByteString)
readRun volume run =
  case runLCN run of
    Nothing -> pure (Right (BS.replicate (fromIntegral size) 0))
    Just lcn -> do
      hSeek (volumeHandle volume) AbsoluteSeek (clusterOffset volume lcn)
      bytes <- BS.hGet (volumeHandle volume) (fromIntegral size)
      if fromIntegral (BS.length bytes) == size
        then pure (Right bytes)
        else pure (Left (UnexpectedEndOfData "data run is truncated"))
  where
    ClusterSize clusterBytes = bytesPerCluster (volumeBootSector volume)
    size = toInteger (runLength run) * toInteger clusterBytes
