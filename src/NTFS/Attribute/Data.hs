-- | High-level view of the @$DATA@ attribute.
module NTFS.Attribute.Data
  ( FileData (..)
  , parseDataAttribute
  ) where

import qualified Data.ByteString    as BS
import           Data.Word          (Word64)

import           NTFS.MFT.Attribute
import           NTFS.Runlist       (DataRun (..), parseRunlist)
import           NTFS.Types         (ParseError (..), VCN (..))

-- | Data stored inside a record, or the runs holding it on the volume.
data FileData
  = ResidentData BS.ByteString
  | NonResidentData
      { dataRuns            :: [DataRun]
      , dataAllocatedSize   :: Word64
      , dataRealSize        :: Word64
      , dataInitializedSize :: Word64
      }
  deriving (Eq, Show)

-- | Interpret an attribute as @$DATA@.
parseDataAttribute :: Attribute -> Either ParseError FileData
parseDataAttribute attr
  | attributeTypeCode attr /= attributeTypeData =
      Left (InvalidValue "attribute is not $DATA")
  | otherwise =
      case attributeValue attr of
        ResidentValue value -> Right (ResidentData value)
        NonResidentValue _firstVCN lastVCN runlistBytes allocated real initialized -> do
          runs <- parseRunlist runlistBytes
          let VCN lastIndex = lastVCN
          if null runs
            then Left (InvalidValue "$DATA runlist is empty")
            else
              if sum (map runLength runs) <= fromIntegral lastIndex
                then Left (InvalidValue "$DATA runlist does not cover its VCN range")
                else Right (NonResidentData runs allocated real initialized)
