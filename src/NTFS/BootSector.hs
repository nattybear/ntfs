-- | Pure parser for the NTFS volume boot record (VBR).
module NTFS.BootSector
  ( BootSector (..)
  , bootSectorSize
  , parseBootSector
  , bytesPerCluster
  , mftRecordSize
  , indexRecordSize
  ) where

import           Data.Bits       (shiftL, (.|.))
import qualified Data.ByteString as BS
import           Data.Int        (Int8)
import           Data.Word       (Word16, Word32, Word64, Word8)

import           NTFS.Types      (ClusterSize (..), LCN (..), ParseError (..))

-- | Size of a boot sector in bytes.
bootSectorSize :: Int
bootSectorSize = 512

-- | Geometry and offsets read from the volume boot record.
data BootSector = BootSector
  { bsBytesPerSector       :: Word16
  , bsSectorsPerCluster    :: Word8
  , bsReservedSectors      :: Word16
  , bsHiddenSectors        :: Word32
  , bsTotalSectors         :: Word64
  , bsMFTStart             :: LCN
  , bsMFTMirrorStart       :: LCN
  , bsClustersPerMFTRecord :: Int8
  , bsClustersPerIndexRec  :: Int8
  , bsVolumeSerial         :: Word64
  } deriving (Eq, Show)

-- | Parse a 512-byte boot sector.
parseBootSector :: BS.ByteString -> Either ParseError BootSector
parseBootSector bytes
  | BS.length bytes < bootSectorSize =
      Left (UnexpectedEndOfData "boot sector is shorter than 512 bytes")
  | word16At bytes 0x1fe /= 0xaa55 =
      Left (InvalidValue "boot sector signature is not 0x55AA")
  | word16At bytes 0x0b == 0 =
      Left (InvalidValue "bytes per sector is zero")
  | byteAt bytes 0x0d == 0 =
      Left (InvalidValue "sectors per cluster is zero")
  | otherwise =
      Right
        BootSector
          { bsBytesPerSector = word16At bytes 0x0b
          , bsSectorsPerCluster = byteAt bytes 0x0d
          , bsReservedSectors = word16At bytes 0x0e
          , bsHiddenSectors = word32At bytes 0x1c
          , bsTotalSectors = word64At bytes 0x28
          , bsMFTStart = LCN (fromIntegral (word64At bytes 0x30))
          , bsMFTMirrorStart = LCN (fromIntegral (word64At bytes 0x38))
          , bsClustersPerMFTRecord = int8At bytes 0x40
          , bsClustersPerIndexRec = int8At bytes 0x44
          , bsVolumeSerial = word64At bytes 0x48
          }

-- | Bytes in one cluster.
bytesPerCluster :: BootSector -> ClusterSize
bytesPerCluster bs =
  ClusterSize
    ( fromIntegral (bsBytesPerSector bs)
        * fromIntegral (bsSectorsPerCluster bs)
    )

-- | Size in bytes of one MFT record.
mftRecordSize :: BootSector -> Either ParseError ClusterSize
mftRecordSize bs = recordSize "MFT record" (bsClustersPerMFTRecord bs) bs

-- | Size in bytes of one index record.
indexRecordSize :: BootSector -> Either ParseError ClusterSize
indexRecordSize bs = recordSize "index record" (bsClustersPerIndexRec bs) bs

recordSize :: String -> Int8 -> BootSector -> Either ParseError ClusterSize
recordSize label count bs
  | count > 0 = Right (ClusterSize (fromIntegral count * clusterBytes))
  | count < 0 =
      let shift = negate (fromIntegral count :: Int)
       in if shift > 31
            then Left (InvalidValue (label ++ " size shift is too large"))
            else Right (ClusterSize (1 `shiftL` shift))
  | otherwise = Left (InvalidValue (label ++ " cluster count is zero"))
  where
    ClusterSize clusterBytes = bytesPerCluster bs

byteAt :: BS.ByteString -> Int -> Word8
byteAt = BS.index

word16At :: BS.ByteString -> Int -> Word16
word16At bytes offset =
  fromIntegral (byteAt bytes offset)
    .|. (fromIntegral (byteAt bytes (offset + 1)) `shiftL` 8)

word32At :: BS.ByteString -> Int -> Word32
word32At bytes offset =
  fromIntegral (byteAt bytes offset)
    .|. (fromIntegral (byteAt bytes (offset + 1)) `shiftL` 8)
    .|. (fromIntegral (byteAt bytes (offset + 2)) `shiftL` 16)
    .|. (fromIntegral (byteAt bytes (offset + 3)) `shiftL` 24)

word64At :: BS.ByteString -> Int -> Word64
word64At bytes offset =
  fromIntegral (word32At bytes offset)
    .|. (fromIntegral (word32At bytes (offset + 4)) `shiftL` 32)

int8At :: BS.ByteString -> Int -> Int8
int8At bytes offset = fromIntegral (byteAt bytes offset)
