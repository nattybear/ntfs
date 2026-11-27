module Main (main) where

import           Control.Exception                  (finally)
import           Control.Monad                      (unless)
import           Data.Bits                          (shiftR)
import qualified Data.ByteString                    as BS
import           Data.Word                          (Word16, Word32, Word64,
                                                     Word8)
import           System.Directory                   (getTemporaryDirectory,
                                                     removeFile)
import           System.Exit                        (exitFailure)
import           System.IO                          (SeekMode (AbsoluteSeek),
                                                     hClose, hFlush, hSeek,
                                                     openBinaryTempFile)

import           NTFS.Attribute.Data
import           NTFS.Attribute.FileName
import           NTFS.Attribute.StandardInformation
import           NTFS.BootSector
import           NTFS.File
import           NTFS.MFT
import           NTFS.MFT.Attribute
import           NTFS.MFT.Record
import           NTFS.Runlist
import           NTFS.Types                         (ClusterSize (..), LCN (..),
                                                     MFTRef (..), VCN (..),
                                                     mftRefNumber,
                                                     mftRefSequence)
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
      , ("resident data attribute", pure testResidentData)
      , ("non-resident data attribute", pure testNonResidentData)
      , ("standard information attribute", pure testStandardInformation)
      , ("short standard information attribute", pure testShortStandardInformation)
      , ("file name attribute", pure testFileName)
      , ("short file name attribute", pure testShortFileName)
      , ("file entry from MFT record", pure testFileEntry)
      , ("read resident and non-resident data", testReadData)
      , ("enumerate MFT records", testReadAllMFTRecords)
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

testResidentData :: Bool
testResidentData =
  case parseAttribute residentDataAttribute >>= parseDataAttribute of
    Right value -> value == ResidentData (BS.replicate 0x30 0xab)
    Left _      -> False

testNonResidentData :: Bool
testNonResidentData =
  case parseAttribute nonResidentDataAttribute >>= parseDataAttribute of
    Right value ->
      value
        == NonResidentData
          [DataRun (VCN 0) (Just (LCN 3)) 2]
          0x1000
          0x800
          0x800
    Left _ -> False

testStandardInformation :: Bool
testStandardInformation =
  case parseAttribute
    (residentAttributeOf attributeTypeStandardInformation standardInformationFixture)
    >>= parseStandardInformation of
    Right info ->
      standardCreationTime info == 0x11
        && standardFileAlterTime info == 0x12
        && standardMFTChangeTime info == 0x13
        && standardFileReadTime info == 0x14
        && standardFileAttributes info == 0x20
        && standardMaxVersion info == 3
        && standardVersion info == 2
        && standardClassId info == 0x10
        && standardOwnerId info == 5
        && standardSecurityId info == 6
        && standardQuotaCharged info == 0x1000
        && standardUpdateSequence info == 0x1234
    Left _ -> False

testShortStandardInformation :: Bool
testShortStandardInformation =
  case parseAttribute
    ( residentAttributeOf
        attributeTypeStandardInformation
        (BS.take 0x30 standardInformationFixture)
    )
    >>= parseStandardInformation of
    Right info ->
      standardCreationTime info == 0x11
        && standardFileAttributes info == 0x20
        && standardClassId info == 0x10
        && standardOwnerId info == 0
        && standardQuotaCharged info == 0
        && standardUpdateSequence info == 0
    Left _ -> False

testFileName :: Bool
testFileName =
  case parseAttribute (residentAttributeOf attributeTypeFileName fileNameFixture) >>= parseFileName of
    Right name ->
      mftRefNumber (fileNameParent name) == 9
        && mftRefSequence (fileNameParent name) == 2
        && fileNameCreationTime name == 0x21
        && fileNameAllocatedSize name == 0x2000
        && fileNameRealSize name == 0x1000
        && fileNameFlags name == 0x10
        && fileNameNameLength name == 2
        && fileNameNamespace name == fileNameNamespaceWin32
        && fileNameName name == "AB"
    Left _ -> False

testShortFileName :: Bool
testShortFileName =
  case parseAttribute
    (residentAttributeOf attributeTypeFileName (BS.take 0x45 fileNameFixture))
    >>= parseFileName of
    Left _  -> True
    Right _ -> False

testFileEntry :: Bool
testFileEntry =
  case parseFileEntry record of
    Right entry ->
      entryRecordNumber entry == 9
        && isEntryInUse entry
        && isEntryDirectory entry
        && map fileNameName (entryFileNames entry) == ["AB"]
        && maybe False ((== 0x11) . standardCreationTime) (entryStandardInformation entry)
        && map fst (entryData entry) == [Nothing]
        && map snd (entryData entry) == [ResidentData (BS.replicate 0x30 0xab)]
    Left _ -> False
  where
    record =
      ( case parseMFTRecord 512 mftRecordFixture of
          Right value -> value
          Left _      -> error "invalid MFT fixture"
      )
        { recordAttributes =
            [ residentAttributeOf attributeTypeStandardInformation standardInformationFixture
            , residentAttributeOf attributeTypeFileName fileNameFixture
            , residentDataAttribute
            , RawAttribute 0x90 (BS.replicate 8 0)
            ]
        }

testReadData :: IO Bool
testReadData = do
  tmpDir <- getTemporaryDirectory
  (path, handle) <- openBinaryTempFile tmpDir "ntfs-read-data-test.img"
  let runs =
        [ DataRun (VCN 0) (Just (LCN 4)) 1
        , DataRun (VCN 1) Nothing 1
        , DataRun (VCN 2) (Just (LCN 6)) 1
        ]
      expected =
        BS.concat
          [ BS.replicate 4096 0x11
          , BS.replicate 4096 0
          , BS.replicate 4096 0x22
          ]
      check = do
        BS.hPut handle bootSector
        opened <- openVolume handle
        case opened of
          Left _ -> pure False
          Right volume -> do
            hSeek handle AbsoluteSeek (clusterOffset volume (LCN 4))
            BS.hPut handle (BS.replicate 4096 0x11)
            hSeek handle AbsoluteSeek (clusterOffset volume (LCN 6))
            BS.hPut handle (BS.replicate 4096 0x22)
            hFlush handle
            resident <- readData volume (ResidentData (BS.pack [1, 2, 3]))
            nonResident <- readData volume (NonResidentData runs 0x3000 0x3000 0x3000)
            pure
              ( resident == Right (BS.pack [1, 2, 3])
                  && nonResident == Right expected
              )
  check `finally` (hClose handle >> removeFile path)

testReadAllMFTRecords :: IO Bool
testReadAllMFTRecords = do
  tmpDir <- getTemporaryDirectory
  (path, handle) <- openBinaryTempFile tmpDir "ntfs-mft-enum-test.img"
  let check = do
        BS.hPut handle bootSector
        hSeek handle AbsoluteSeek 16384
        BS.hPut handle mftDataRecordFixture
        BS.hPut handle mftRecordFixture
        hFlush handle
        opened <- openVolume handle
        case opened of
          Left _ -> pure False
          Right volume -> do
            count <- mftRecordCount volume
            records <- readAllMFTRecords volume
            pure
              ( count == Right 2
                  && fmap (map recordNumber) records == Right [0, 9]
              )
  check `finally` (hClose handle >> removeFile path)

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

mftDataRecordFixture :: BS.ByteString
mftDataRecordFixture =
  build
    (BS.replicate 1024 0)
    [ (0x00, [0x46, 0x49, 0x4c, 0x45])
    , (0x04, word16Bytes 0x30)
    , (0x06, word16Bytes 3)
    , (0x10, word16Bytes 1)
    , (0x12, word16Bytes 1)
    , (0x14, word16Bytes 0x38)
    , (0x16, word16Bytes 1)
    , (0x18, word32Bytes 0x7c)
    , (0x1c, word32Bytes 0x400)
    , (0x20, word64Bytes 0)
    , (0x28, word16Bytes 1)
    , (0x2c, word32Bytes 0)
    , (0x30, word16Bytes 0xaa55)
    , (0x32, word16Bytes 0x1111)
    , (0x34, word16Bytes 0x2222)
    , (0x38, word32Bytes 0x80 ++ word32Bytes 0x44)
    , (0x40, [0x01])
    , (0x41, [0x00])
    , (0x42, word16Bytes 0)
    , (0x44, word16Bytes 0)
    , (0x46, word16Bytes 0)
    , (0x48, word64Bytes 0)
    , (0x50, word64Bytes 1)
    , (0x58, word16Bytes 0x40)
    , (0x60, word64Bytes 0x800)
    , (0x68, word64Bytes 0x800)
    , (0x70, word64Bytes 0x800)
    , (0x78, [0x11, 0x02, 0x04, 0x00])
    , (0x1fe, word16Bytes 0xaa55)
    , (0x3fe, word16Bytes 0xaa55)
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

residentDataAttribute :: RawAttribute
residentDataAttribute =
  RawAttribute
    0x80
    (putAt 0x00 (word32Bytes 0x80) (rawAttributeBytes residentAttribute))

nonResidentDataAttribute :: RawAttribute
nonResidentDataAttribute =
  RawAttribute
    0x80
    ( putAt 0x18 (word64Bytes 1)
        (putAt 0x10 (word64Bytes 0) (rawAttributeBytes nonResidentAttribute))
    )

standardInformationFixture :: BS.ByteString
standardInformationFixture =
  build
    (BS.replicate 0x48 0)
    [ (0x00, word64Bytes 0x11)
    , (0x08, word64Bytes 0x12)
    , (0x10, word64Bytes 0x13)
    , (0x18, word64Bytes 0x14)
    , (0x20, word32Bytes 0x20)
    , (0x24, word32Bytes 3)
    , (0x28, word32Bytes 2)
    , (0x2c, word32Bytes 0x10)
    , (0x30, word32Bytes 5)
    , (0x34, word32Bytes 6)
    , (0x38, word64Bytes 0x1000)
    , (0x40, word64Bytes 0x1234)
    ]

fileNameFixture :: BS.ByteString
fileNameFixture =
  build
    (BS.replicate 0x46 0)
    [ (0x00, word64Bytes 0x0002000000000009)
    , (0x08, word64Bytes 0x21)
    , (0x10, word64Bytes 0x22)
    , (0x18, word64Bytes 0x23)
    , (0x20, word64Bytes 0x24)
    , (0x28, word64Bytes 0x2000)
    , (0x30, word64Bytes 0x1000)
    , (0x38, word32Bytes 0x10)
    , (0x3c, word32Bytes 0)
    , (0x40, [0x02])
    , (0x41, [0x01])
    , (0x42, [0x41, 0x00, 0x42, 0x00])
    ]

residentAttributeOf :: Word32 -> BS.ByteString -> RawAttribute
residentAttributeOf typeCode value =
  RawAttribute
    typeCode
    ( build
        (BS.replicate 0x18 0)
        [ (0x00, word32Bytes typeCode)
        , (0x04, word32Bytes (fromIntegral (0x18 + BS.length value)))
        , (0x08, [0x00])
        , (0x09, [0x00])
        , (0x0a, word16Bytes 0)
        , (0x0c, word16Bytes 0)
        , (0x0e, word16Bytes 0)
        , (0x10, word32Bytes (fromIntegral (BS.length value)))
        , (0x14, word16Bytes 0x18)
        ]
        <> value
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
