module Main (main) where

import           Control.Exception  (finally)
import           Control.Monad      (unless)
import           Data.Bits          (shiftR)
import qualified Data.ByteString    as BS
import           Data.Word          (Word16, Word32, Word64, Word8)
import           System.Directory   (getTemporaryDirectory, removeFile)
import           System.Exit        (exitFailure)
import           System.IO          (SeekMode (AbsoluteSeek), hClose, hFlush,
                                     hSeek, openBinaryTempFile)

import           NTFS.BootSector
import           NTFS.MFT.Attribute
import           NTFS.MFT.Record
import           NTFS.Runlist
import           NTFS.Types         (ClusterSize (..), LCN (..), MFTRef (..),
                                     VCN (..))
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
      , ("resident attribute header", pure testResidentAttribute)
      , ("non-resident attribute header", pure testNonResidentAttribute)
      , ("single data run", pure testSingleRun)
      , ("multiple data runs", pure testMultipleRuns)
      , ("sparse data run", pure testSparseRun)
      , ("truncated runlist", pure testTruncatedRunlist)
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

testResidentAttribute :: Bool
testResidentAttribute =
  case parseAttribute residentAttribute of
    Right attr ->
      attributeTypeCode attr == 0x10
        && attributeLength attr == 0x60
        && not (attributeNonResident attr)
        && attributeName attr == Nothing
        && attributeFlags attr == 1
        && attributeId attr == 2
        && attributeValue attr == ResidentValue (BS.replicate 0x30 0xab)
    Left _ -> False

testNonResidentAttribute :: Bool
testNonResidentAttribute =
  case parseAttribute nonResidentAttribute of
    Right attr ->
      attributeTypeCode attr == 0x80
        && attributeLength attr == 0x80
        && attributeNonResident attr
        && attributeName attr == Just (BS.pack [0x44, 0x00, 0x61, 0x00, 0x74, 0x00, 0x61, 0x00])
        && attributeFlags attr == 0
        && attributeId attr == 1
        && attributeValue attr
          == NonResidentValue
            (VCN 3)
            (VCN 9)
            (BS.pack [0x11, 0x02, 0x03, 0x00])
            0x1000
            0x800
            0x800
    Left _ -> False

testSingleRun :: Bool
testSingleRun =
  parseRunlist (BS.pack [0x11, 0x07, 0x04, 0x00])
    == Right [DataRun (VCN 0) (Just (LCN 4)) 7]

testMultipleRuns :: Bool
testMultipleRuns =
  parseRunlist (BS.pack [0x11, 0x02, 0x04, 0x11, 0x03, 0xfe, 0x00])
    == Right
      [ DataRun (VCN 0) (Just (LCN 4)) 2
      , DataRun (VCN 2) (Just (LCN 2)) 3
      ]

testSparseRun :: Bool
testSparseRun =
  parseRunlist (BS.pack [0x01, 0x05, 0x11, 0x02, 0x03, 0x00])
    == Right
      [ DataRun (VCN 0) Nothing 5
      , DataRun (VCN 5) (Just (LCN 3)) 2
      ]

testTruncatedRunlist :: Bool
testTruncatedRunlist =
  case parseRunlist (BS.pack [0x11, 0x02]) of
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

residentAttribute :: RawAttribute
residentAttribute =
  RawAttribute
    0x10
    ( build
        (BS.replicate 0x60 0)
        [ (0x00, word32Bytes 0x10)
        , (0x04, word32Bytes 0x60)
        , (0x08, [0x00])
        , (0x09, [0x00])
        , (0x0a, word16Bytes 0x00)
        , (0x0c, word16Bytes 1)
        , (0x0e, word16Bytes 2)
        , (0x10, word32Bytes 0x30)
        , (0x14, word16Bytes 0x18)
        , (0x18, replicate 0x30 0xab)
        ]
    )

nonResidentAttribute :: RawAttribute
nonResidentAttribute =
  RawAttribute
    0x80
    ( build
        (BS.replicate 0x80 0)
        [ (0x00, word32Bytes 0x80)
        , (0x04, word32Bytes 0x80)
        , (0x08, [0x01])
        , (0x09, [0x04])
        , (0x0a, word16Bytes 0x40)
        , (0x0c, word16Bytes 0)
        , (0x0e, word16Bytes 1)
        , (0x10, word64Bytes 3)
        , (0x18, word64Bytes 9)
        , (0x20, word16Bytes 0x7c)
        , (0x22, [0x00])
        , (0x28, word64Bytes 0x1000)
        , (0x30, word64Bytes 0x800)
        , (0x38, word64Bytes 0x800)
        , (0x40, [0x44, 0x00, 0x61, 0x00, 0x74, 0x00, 0x61, 0x00])
        , (0x7c, [0x11, 0x02, 0x03, 0x00])
        ]
    )

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
