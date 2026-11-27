-- | High-level view of the @$FILE_NAME@ attribute.
module NTFS.Attribute.FileName
  ( FileName (..)
  , fileNameNamespacePosix
  , fileNameNamespaceWin32
  , fileNameNamespaceDOS
  , fileNameNamespaceWin32DOS
  , decodeUTF16LE
  , parseFileName
  , parseFileNameValue
  ) where

import           Data.Bits          (shiftL, (.|.))
import qualified Data.ByteString    as BS
import           Data.Word          (Word32, Word64, Word8)

import           NTFS.MFT.Attribute
import           NTFS.Types         (MFTRef (..), ParseError (..))

-- | Namespace of a @$FILE_NAME@ attribute.
fileNameNamespacePosix, fileNameNamespaceWin32, fileNameNamespaceDOS, fileNameNamespaceWin32DOS :: Word8
fileNameNamespacePosix = 0

fileNameNamespaceWin32 = 1

fileNameNamespaceDOS = 2

fileNameNamespaceWin32DOS = 3

-- | Metadata attached to one name of a file or directory.
-- Timestamps are Windows @FILETIME@ values (100 ns since 1601-01-01).
data FileName = FileName
  { fileNameParent        :: MFTRef
  , fileNameCreationTime  :: Word64
  , fileNameFileAlterTime :: Word64
  , fileNameMFTChangeTime :: Word64
  , fileNameFileReadTime  :: Word64
  , fileNameAllocatedSize :: Word64
  , fileNameRealSize      :: Word64
  , fileNameFlags         :: Word32
  , fileNameReparse       :: Word32
  , fileNameNameLength    :: Word8
  , fileNameNamespace     :: Word8
  , fileNameName          :: String
  } deriving (Eq, Show)

-- | Decode UTF-16LE bytes into a string of code units.
-- Surrogate pairs are not combined.
decodeUTF16LE :: BS.ByteString -> String
decodeUTF16LE bytes = go 0
  where
    end = BS.length bytes
    go offset
      | offset + 1 >= end = []
      | otherwise = toEnum (fromIntegral (word16At bytes offset)) : go (offset + 2)

-- | Interpret an attribute as @$FILE_NAME@.
parseFileName :: Attribute -> Either ParseError FileName
parseFileName attr
  | attributeTypeCode attr /= attributeTypeFileName =
      Left (InvalidValue "attribute is not $FILE_NAME")
  | otherwise =
      case attributeValue attr of
        NonResidentValue{} ->
          Left (InvalidValue "$FILE_NAME is not resident")
        ResidentValue bytes -> parseFileNameValue bytes

-- | Interpret the value bytes of a @$FILE_NAME@ attribute. Index entries hold
-- this value directly, without an attribute header.
parseFileNameValue :: BS.ByteString -> Either ParseError FileName
parseFileNameValue bytes
  | BS.length bytes < 0x42 =
      Left (UnexpectedEndOfData "$FILE_NAME value is truncated")
  | 0x42 + 2 * fromIntegral nameLength > BS.length bytes =
      Left (UnexpectedEndOfData "$FILE_NAME name is truncated")
  | otherwise =
      Right
        FileName
          { fileNameParent = MFTRef (word64At bytes 0x00)
          , fileNameCreationTime = word64At bytes 0x08
          , fileNameFileAlterTime = word64At bytes 0x10
          , fileNameMFTChangeTime = word64At bytes 0x18
          , fileNameFileReadTime = word64At bytes 0x20
          , fileNameAllocatedSize = word64At bytes 0x28
          , fileNameRealSize = word64At bytes 0x30
          , fileNameFlags = word32At bytes 0x38
          , fileNameReparse = word32At bytes 0x3c
          , fileNameNameLength = nameLength
          , fileNameNamespace = BS.index bytes 0x41
          , fileNameName = decodeUTF16LE (BS.take (2 * fromIntegral nameLength) (BS.drop 0x42 bytes))
          }
  where
    nameLength = BS.index bytes 0x40

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
