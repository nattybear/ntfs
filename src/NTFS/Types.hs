-- | Domain types shared by all NTFS parsers.
module NTFS.Types
  ( MFTRef (..)
  , mftRefNumber
  , mftRefSequence
  , VCN (..)
  , LCN (..)
  , ClusterSize (..)
  , ParseError (..)
  ) where

import           Data.Bits (shiftR, (.&.))
import           Data.Int  (Int64)
import           Data.Word (Word16, Word64)

-- | Record number and sequence number in the Master File Table.
newtype MFTRef = MFTRef Word64
  deriving (Eq, Ord, Show)

-- | Record number stored in an MFT reference.
mftRefNumber :: MFTRef -> Word64
mftRefNumber (MFTRef ref) = ref .&. 0x0000ffffffffffff

-- | Sequence number stored in an MFT reference.
mftRefSequence :: MFTRef -> Word16
mftRefSequence (MFTRef ref) = fromIntegral (ref `shiftR` 48)

-- | Virtual Cluster Number, relative to the start of a file's data runs.
newtype VCN = VCN Int64
  deriving (Eq, Ord, Show)

-- | Logical Cluster Number, relative to the start of the volume.
newtype LCN = LCN Int64
  deriving (Eq, Ord, Show)

-- | Number of bytes in one cluster.
newtype ClusterSize = ClusterSize Word64
  deriving (Eq, Ord, Show)

-- | Reasons why a low-level parser can fail.
data ParseError
  = UnexpectedEndOfData String
  | InvalidValue String
  deriving (Eq, Show)
