-- | High-level view of the @$STANDARD_INFORMATION@ attribute.
module NTFS.Attribute.StandardInformation
  ( StandardInformation (..)
  , parseStandardInformation
  ) where

import           Data.Bits          (shiftL, (.|.))
import qualified Data.ByteString    as BS
import           Data.Word          (Word32, Word64)

import           NTFS.MFT.Attribute
import           NTFS.Types         (ParseError (..))

-- | Metadata shared by all file names of a record.
-- Timestamps are Windows @FILETIME@ values (100 ns since 1601-01-01).
-- Fields absent from short (48-byte) values are reported as zero.
data StandardInformation = StandardInformation
  { standardCreationTime   :: Word64
  , standardFileAlterTime  :: Word64
  , standardMFTChangeTime  :: Word64
  , standardFileReadTime   :: Word64
  , standardFileAttributes :: Word32
  , standardMaxVersion     :: Word32
  , standardVersion        :: Word32
  , standardClassId        :: Word32
  , standardOwnerId        :: Word32
  , standardSecurityId     :: Word32
  , standardQuotaCharged   :: Word64
  , standardUpdateSequence :: Word64
  } deriving (Eq, Show)

-- | Interpret an attribute as @$STANDARD_INFORMATION@.
parseStandardInformation :: Attribute -> Either ParseError StandardInformation
parseStandardInformation attr
  | attributeTypeCode attr /= attributeTypeStandardInformation =
      Left (InvalidValue "attribute is not $STANDARD_INFORMATION")
  | otherwise =
      case attributeValue attr of
        NonResidentValue{} ->
          Left (InvalidValue "$STANDARD_INFORMATION is not resident")
        ResidentValue bytes
          | BS.length bytes < 0x30 ->
              Left (UnexpectedEndOfData "$STANDARD_INFORMATION value is truncated")
          | otherwise ->
              Right
                StandardInformation
                  { standardCreationTime = word64At bytes 0x00
                  , standardFileAlterTime = word64At bytes 0x08
                  , standardMFTChangeTime = word64At bytes 0x10
                  , standardFileReadTime = word64At bytes 0x18
                  , standardFileAttributes = word32At bytes 0x20
                  , standardMaxVersion = word32At bytes 0x24
                  , standardVersion = word32At bytes 0x28
                  , standardClassId = word32At bytes 0x2c
                  , standardOwnerId = optionalWord32 bytes 0x30
                  , standardSecurityId = optionalWord32 bytes 0x34
                  , standardQuotaCharged = optionalWord64 bytes 0x38
                  , standardUpdateSequence = optionalWord64 bytes 0x40
                  }

optionalWord32 :: BS.ByteString -> Int -> Word32
optionalWord32 bytes offset
  | offset + 4 <= BS.length bytes = word32At bytes offset
  | otherwise = 0

optionalWord64 :: BS.ByteString -> Int -> Word64
optionalWord64 bytes offset
  | offset + 8 <= BS.length bytes = word64At bytes offset
  | otherwise = 0

word32At :: BS.ByteString -> Int -> Word32
word32At bytes offset =
  fromIntegral (word16At bytes offset)
    .|. (fromIntegral (word16At bytes (offset + 2)) `shiftL` 16)

word64At :: BS.ByteString -> Int -> Word64
word64At bytes offset =
  fromIntegral (word32At bytes offset)
    .|. (fromIntegral (word32At bytes (offset + 4)) `shiftL` 32)

word16At :: BS.ByteString -> Int -> Word32
word16At bytes offset =
  fromIntegral (BS.index bytes offset)
    .|. (fromIntegral (BS.index bytes (offset + 1)) `shiftL` 8)
