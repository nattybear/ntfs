-- | Volume-level access to an NTFS image.
module NTFS.Volume
  ( Volume (..)
  , openVolume
  , clusterOffset
  , mftOffset
  , readMFTRecordBytes
  ) where

import qualified Data.ByteString as BS
import           System.IO       (Handle, SeekMode (AbsoluteSeek), hSeek)

import           NTFS.BootSector
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
