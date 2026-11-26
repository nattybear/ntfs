-- | Decoder for NTFS mapping pairs (runlists).
module NTFS.Runlist
  ( DataRun (..)
  , parseRunlist
  ) where

import           Data.Bits       (complement, shiftL, shiftR, testBit, (.&.),
                                  (.|.))
import qualified Data.ByteString as BS
import           Data.Int        (Int64)
import           Data.Word       (Word64)

import           NTFS.Types      (LCN (..), ParseError (..), VCN (..))

-- | One decoded data run. A missing LCN denotes a sparse run.
data DataRun = DataRun
  { runVCN    :: VCN
  , runLCN    :: Maybe LCN
  , runLength :: Word64
  } deriving (Eq, Show)

-- | Decode a runlist into consecutive data runs.
parseRunlist :: BS.ByteString -> Either ParseError [DataRun]
parseRunlist bytes = go 0 0 0 []
  where
    end = BS.length bytes
    go offset vcn lcn acc
      | offset >= end = Right (reverse acc)
      | header == 0 = Right (reverse acc)
      | lengthSize == 0 = Left (InvalidValue "run length field is empty")
      | offset + 1 + lengthSize + offsetSize > end =
          Left (UnexpectedEndOfData "runlist entry is truncated")
      | runLen == 0 = Left (InvalidValue "run length is zero")
      | otherwise =
          let nextLCN =
                if offsetSize == 0
                  then lcn
                  else lcn + signedValue offsetSize (wordLE offsetSize bytes deltaOffset)
              run =
                DataRun
                  { runVCN = VCN vcn
                  , runLCN =
                      if offsetSize == 0
                        then Nothing
                        else Just (LCN nextLCN)
                  , runLength = runLen
                  }
           in go nextOffset (vcn + fromIntegral runLen) nextLCN (run : acc)
      where
        header = BS.index bytes offset
        lengthSize = fromIntegral (header .&. 0x0f)
        offsetSize = fromIntegral ((header `shiftR` 4) .&. 0x0f)
        runOffset = offset + 1
        deltaOffset = runOffset + lengthSize
        runLen = wordLE lengthSize bytes runOffset
        nextOffset = runOffset + lengthSize + offsetSize

wordLE :: Int -> BS.ByteString -> Int -> Word64
wordLE size bytes offset =
  foldl step 0 [0 .. size - 1]
  where
    step acc i =
      acc .|. (fromIntegral (BS.index bytes (offset + i)) `shiftL` (8 * i))

signedValue :: Int -> Word64 -> Int64
signedValue size raw
  | size == 0 = 0
  | size >= 8 = fromIntegral raw
  | testBit raw (size * 8 - 1) = fromIntegral (raw .|. mask)
  | otherwise = fromIntegral raw
  where
    mask = complement ((1 `shiftL` (size * 8)) - 1)
