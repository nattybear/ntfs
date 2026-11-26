module Main (main) where

import           Control.Exception (finally)
import           Control.Monad     (unless)
import           Data.Bits         (shiftR)
import qualified Data.ByteString   as BS
import           Data.Word         (Word16, Word32, Word64, Word8)
import           System.Directory  (getTemporaryDirectory, removeFile)
import           System.Exit       (exitFailure)
import           System.IO         (SeekMode (AbsoluteSeek), hClose, hFlush,
                                    hSeek, openBinaryTempFile)

import           NTFS.BootSector
import           NTFS.MFT.Record
import           NTFS.Types        (ClusterSize (..), LCN (..), MFTRef (..))
import           NTFS.Volume

main :: IO ()
main = do
  results <-
    mapM
      runCheck
      [ ("valid boot sector", pure testValidBootSector)
      , ("short boot sector", pure testShortBootSector)
      , ("bad signature", pure testBadSignature)
      , ("small record sizes", pure testSmallRecordSizes)
      , ("volume opens and reads MFT record", testVolume)
      , ("MFT record header and attributes", pure testMFTRecord)
      , ("MFT record bad fixup", pure testBadFixup)
      ]
  unless (and results) exitFailure

runCheck :: (String, IO Bool) -> IO Bool
runCheck (name, check) = do
  ok <- check
  putStrLn ((if ok then "PASS " else "FAIL ") ++ name)
  pure ok

testValidBootSector :: Bool
testValidBootSector =
  case parseBootSector bootSector of
    Right bs ->
      bsBytesPerSector bs == 512
        && bsSectorsPerCluster bs == 8
        && bsMFTStart bs == LCN 4
        && bsVolumeSerial bs == 0x1234
        && bytesPerCluster bs == ClusterSize 4096
    Left _ -> False

testShortBootSector :: Bool
testShortBootSector =
  case parseBootSector (BS.replicate 511 0) of
    Left _  -> True
    Right _ -> False

testBadSignature :: Bool
testBadSignature =
  case parseBootSector (BS.replicate bootSectorSize 0) of
    Left _  -> True
    Right _ -> False

testSmallRecordSizes :: Bool
testSmallRecordSizes =
  case parseBootSector bootSector of
    Right bs ->
      mftRecordSize bs == Right (ClusterSize 1024)
        && indexRecordSize bs == Right (ClusterSize 512)
    Left _ -> False

testVolume :: IO Bool
testVolume = do
  tmpDir <- getTemporaryDirectory
  (path, handle) <- openBinaryTempFile tmpDir "ntfs-volume-test.img"
  let record = BS.replicate 1024 0xaa
      check = do
        BS.hPut handle bootSector
        hSeek handle AbsoluteSeek 16384
        BS.hPut handle record
        hFlush handle
        opened <- openVolume handle
        case opened of
          Left _ -> pure False
          Right volume -> do
            bytes <- readMFTRecordBytes volume (MFTRef 0)
            pure (mftOffset volume == 16384 && bytes == Right record)
  check `finally` (hClose handle >> removeFile path)

testMFTRecord :: Bool
testMFTRecord =
  case parseMFTRecord 512 mftRecordFixture of
    Right record ->
      recordNumber record == 9
        && recordSequenceNumber record == 5
        && recordHardLinkCount record == 2
        && recordFirstAttributeOffset record == 0x38
        && recordFlags record == 3
        && recordUsedSize record == 0x70
        && recordAllocatedSize record == 0x400
        && recordBaseRecord record == MFTRef 0x1234
        && recordNextAttributeId record == 7
        && isRecordInUse record
        && isDirectoryRecord record
        && map rawAttributeType (recordAttributes record) == [0x10, 0x30]
        && map (BS.length . rawAttributeBytes) (recordAttributes record) == [0x18, 0x18]
        && BS.index (recordBytes record) 510 == 0x11
        && BS.index (recordBytes record) 511 == 0x11
        && BS.index (recordBytes record) 1022 == 0x22
        && BS.index (recordBytes record) 1023 == 0x22
    Left _ -> False

testBadFixup :: Bool
testBadFixup =
  case parseMFTRecord 512 (putAt 510 (word16Bytes 0xffff) mftRecordFixture) of
    Left _  -> True
    Right _ -> False

bootSector :: BS.ByteString
bootSector =
  build
    (BS.replicate bootSectorSize 0)
    [ (0x00, [0x00, 0x00, 0x00])
    , (0x03, [0x4e, 0x54, 0x46, 0x53])
    , (0x0b, word16Bytes 512)
    , (0x0d, [0x08])
    , (0x30, word64Bytes 4)
    , (0x40, [0xf6])
    , (0x44, [0xf7])
    , (0x48, word64Bytes 0x1234)
    , (0x1fe, [0x55, 0xaa])
    ]

mftRecordFixture :: BS.ByteString
mftRecordFixture =
  build
    (BS.replicate 1024 0)
    [ (0x00, [0x46, 0x49, 0x4c, 0x45])
    , (0x04, word16Bytes 0x30)
    , (0x06, word16Bytes 3)
    , (0x10, word16Bytes 5)
    , (0x12, word16Bytes 2)
    , (0x14, word16Bytes 0x38)
    , (0x16, word16Bytes 3)
    , (0x18, word32Bytes 0x70)
    , (0x1c, word32Bytes 0x400)
    , (0x20, word64Bytes 0x1234)
    , (0x28, word16Bytes 7)
    , (0x2c, word32Bytes 9)
    , (0x30, word16Bytes 0xaa55)
    , (0x32, word16Bytes 0x1111)
    , (0x34, word16Bytes 0x2222)
    , (0x38, word32Bytes 0x10 ++ word32Bytes 0x18)
    , (0x50, word32Bytes 0x30 ++ word32Bytes 0x18)
    , (0x68, word32Bytes 0xffffffff)
    , (0x1fe, word16Bytes 0xaa55)
    , (0x3fe, word16Bytes 0xaa55)
    ]

build :: BS.ByteString -> [(Int, [Word8])] -> BS.ByteString
build = foldl (\bytes (offset, xs) -> putAt offset xs bytes)

putAt :: Int -> [Word8] -> BS.ByteString -> BS.ByteString
putAt offset xs bytes =
  BS.take offset bytes <> BS.pack xs <> BS.drop (offset + length xs) bytes

word16Bytes :: Word16 -> [Word8]
word16Bytes value =
  [fromIntegral value, fromIntegral (value `shiftR` 8)]

word32Bytes :: Word32 -> [Word8]
word32Bytes value =
  [fromIntegral (value `shiftR` (8 * i)) | i <- [0 .. 3]]

word64Bytes :: Word64 -> [Word8]
word64Bytes value =
  [fromIntegral (value `shiftR` (8 * i)) | i <- [0 .. 7]]
