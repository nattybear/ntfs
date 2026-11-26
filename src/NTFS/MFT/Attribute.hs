-- | Parsers for generic NTFS attribute headers.
module NTFS.MFT.Attribute
  ( Attribute (..)
  , AttributeValue (..)
  , parseAttribute
  , attributeTypeStandardInformation
  , attributeTypeFileName
  , attributeTypeData
  ) where

import           Data.Bits       (shiftL, (.|.))
import qualified Data.ByteString as BS
import           Data.Word       (Word16, Word32, Word64)

import           NTFS.MFT.Record (RawAttribute (..))
import           NTFS.Types      (ParseError (..), VCN (..))

-- | Type code of the @$STANDARD_INFORMATION@ attribute.
attributeTypeStandardInformation :: Word32
attributeTypeStandardInformation = 0x10

-- | Type code of the @$FILE_NAME@ attribute.
attributeTypeFileName :: Word32
attributeTypeFileName = 0x30

-- | Type code of the @$DATA@ attribute.
attributeTypeData :: Word32
attributeTypeData = 0x80

-- | A parsed attribute header and its value.
data Attribute = Attribute
  { attributeTypeCode    :: Word32
  , attributeLength      :: Word32
  , attributeNonResident :: Bool
  , attributeName        :: Maybe BS.ByteString
  , attributeFlags       :: Word16
  , attributeId          :: Word16
  , attributeValue       :: AttributeValue
  } deriving (Eq, Show)

-- | Resident data, or the cluster mapping of a non-resident attribute.
data AttributeValue
  = ResidentValue BS.ByteString
  | NonResidentValue
      { valueFirstVCN        :: VCN
      , valueLastVCN         :: VCN
      , valueRunlist         :: BS.ByteString
      , valueAllocatedSize   :: Word64
      , valueRealSize        :: Word64
      , valueInitializedSize :: Word64
      }
  deriving (Eq, Show)

-- | Parse one raw attribute from an MFT record.
parseAttribute :: RawAttribute -> Either ParseError Attribute
parseAttribute raw = do
  checkCommon raw
  name <- parseName bytes nameLength nameOffset limit
  value <-
    if nonResident
      then parseNonResident bytes limit
      else parseResident bytes limit
  pure
    Attribute
      { attributeTypeCode = word32At bytes 0x00
      , attributeLength = attributeLengthValue
      , attributeNonResident = nonResident
      , attributeName = name
      , attributeFlags = word16At bytes 0x0c
      , attributeId = word16At bytes 0x0e
      , attributeValue = value
      }
  where
    bytes = rawAttributeBytes raw
    attributeLengthValue = word32At bytes 0x04
    limit = fromIntegral attributeLengthValue
    nonResident = BS.index bytes 0x08 /= 0
    nameLength = fromIntegral (BS.index bytes 0x09)
    nameOffset = fromIntegral (word16At bytes 0x0a)

checkCommon :: RawAttribute -> Either ParseError ()
checkCommon raw
  | BS.length bytes < 0x18 = Left (InvalidValue "attribute header is truncated")
  | rawAttributeType raw /= word32At bytes 0x00 =
      Left (InvalidValue "attribute type does not match raw attribute")
  | attributeLengthValue < 0x18 = Left (InvalidValue "attribute length is too small")
  | fromIntegral attributeLengthValue > BS.length bytes =
      Left (UnexpectedEndOfData "attribute extends past record data")
  | otherwise = Right ()
  where
    bytes = rawAttributeBytes raw
    attributeLengthValue = word32At bytes 0x04

parseName :: BS.ByteString -> Int -> Int -> Int -> Either ParseError (Maybe BS.ByteString)
parseName bytes nameLength nameOffset limit
  | nameLength == 0 = Right Nothing
  | nameOffset < 0x18 = Left (InvalidValue "attribute name offset is too small")
  | nameOffset + 2 * nameLength > limit = Left (InvalidValue "attribute name is out of range")
  | otherwise = Right (Just (BS.take (2 * nameLength) (BS.drop nameOffset bytes)))

parseResident :: BS.ByteString -> Int -> Either ParseError AttributeValue
parseResident bytes limit
  | valueOffset < 0x18 = Left (InvalidValue "value offset is too small")
  | valueOffset + valueLength > limit = Left (InvalidValue "value is out of range")
  | otherwise = Right (ResidentValue (BS.take valueLength (BS.drop valueOffset bytes)))
  where
    valueLength = fromIntegral (word32At bytes 0x10)
    valueOffset = fromIntegral (word16At bytes 0x14)

parseNonResident :: BS.ByteString -> Int -> Either ParseError AttributeValue
parseNonResident bytes limit
  | limit < 0x40 = Left (InvalidValue "non-resident header is truncated")
  | mappingPairsOffset < 0x40 = Left (InvalidValue "mapping pairs offset is too small")
  | mappingPairsOffset > limit = Left (InvalidValue "mapping pairs offset is out of range")
  | otherwise =
      Right
        NonResidentValue
          { valueFirstVCN = VCN (fromIntegral (word64At bytes 0x10))
          , valueLastVCN = VCN (fromIntegral (word64At bytes 0x18))
          , valueRunlist = BS.take (limit - mappingPairsOffset) (BS.drop mappingPairsOffset bytes)
          , valueAllocatedSize = word64At bytes 0x28
          , valueRealSize = word64At bytes 0x30
          , valueInitializedSize = word64At bytes 0x38
          }
  where
    mappingPairsOffset = fromIntegral (word16At bytes 0x20)

word16At :: BS.ByteString -> Int -> Word16
word16At bytes offset =
  fromIntegral (BS.index bytes offset)
    .|. (fromIntegral (BS.index bytes (offset + 1)) `shiftL` 8)

word32At :: BS.ByteString -> Int -> Word32
word32At bytes offset =
  fromIntegral (word16At bytes offset)
    .|. (fromIntegral (word16At bytes (offset + 2)) `shiftL` 16)

word64At :: BS.ByteString -> Int -> Word64
word64At bytes offset =
  fromIntegral (word32At bytes offset)
    .|. (fromIntegral (word32At bytes (offset + 4)) `shiftL` 32)
