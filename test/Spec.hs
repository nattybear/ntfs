module Main (main) where

import           Control.Monad   (unless)
import           Data.Bits       (shiftR)
import qualified Data.ByteString as BS
import           Data.Word       (Word64, Word8)
import           System.Exit     (exitFailure)

import           NTFS.BootSector
import           NTFS.Types      (ClusterSize (..), LCN (..))

main :: IO ()
main = do
  results <-
    mapM
      runCheck
      [ ("valid boot sector", testValidBootSector)
      , ("short boot sector", testShortBootSector)
      , ("bad signature", testBadSignature)
      , ("small record sizes", testSmallRecordSizes)
      ]
  unless (and results) exitFailure

runCheck :: (String, Bool) -> IO Bool
runCheck (name, ok) = do
  putStrLn ((if ok then "PASS " else "FAIL ") ++ name)
  pure ok

testValidBootSector :: Bool
testValidBootSector =
  case parseBootSector bootSector of
    Right bs ->
      bsBytesPerSector bs == 512
        && bsSectorsPerCluster bs == 8
        && bsMFTStart bs == LCN 4096
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

bootSector :: BS.ByteString
bootSector =
  let blank = BS.replicate bootSectorSize 0
      put offset xs bytes =
        BS.take offset bytes <> BS.pack xs <> BS.drop (offset + length xs) bytes
   in put 0x48 (word64Bytes 0x1234)
        ( put 0x44 [0xf7]
            ( put 0x40 [0xf6]
                ( put 0x30 (word64Bytes 0x1000)
                    ( put 0x0d [0x08]
                        ( put 0x0b [0x00, 0x02]
                            (put 0x1fe [0x55, 0xaa] blank)
                        )
                    )
                )
            )
        )

word64Bytes :: Word64 -> [Word8]
word64Bytes value =
  [fromIntegral (value `shiftR` (8 * i)) | i <- [0 .. 7]]
