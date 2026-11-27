-- | High-level combination of the attributes of one MFT record.
module NTFS.File
  ( FileEntry (..)
  , isEntryInUse
  , isEntryDirectory
  , parseFileEntry
  ) where

import           Data.Bits                          ((.&.))
import qualified Data.ByteString                    as BS
import           Data.Word                          (Word16, Word32)

import           NTFS.Attribute.Data
import           NTFS.Attribute.FileName
import           NTFS.Attribute.StandardInformation
import           NTFS.MFT.Attribute
import           NTFS.MFT.Record
import           NTFS.Types                         (ParseError (..))

-- | Metadata and data found in one MFT record.
data FileEntry = FileEntry
  { entryRecordNumber        :: Word32
  , entryRecordFlags         :: Word16
  , entryFileNames           :: [FileName]
  , entryStandardInformation :: Maybe StandardInformation
  , entryData                :: [(Maybe BS.ByteString, FileData)]
  } deriving (Eq, Show)

-- | Whether the underlying record is marked as in use.
isEntryInUse :: FileEntry -> Bool
isEntryInUse entry = entryRecordFlags entry .&. 0x0001 /= 0

-- | Whether the underlying record describes a directory.
isEntryDirectory :: FileEntry -> Bool
isEntryDirectory entry = entryRecordFlags entry .&. 0x0002 /= 0

-- | Combine the known attributes of a record. Unknown attributes are ignored.
parseFileEntry :: MFTRecord -> Either ParseError FileEntry
parseFileEntry record = do
  attributes <- mapM parseAttribute relevant
  names <- mapM parseFileName (matching attributeTypeFileName attributes)
  information <- mapM parseStandardInformation (matching attributeTypeStandardInformation attributes)
  dataAttributes <- mapM parseDataAttribute (matching attributeTypeData attributes)
  pure
    FileEntry
      { entryRecordNumber = recordNumber record
      , entryRecordFlags = recordFlags record
      , entryFileNames = names
      , entryStandardInformation = case information of
          value:_ -> Just value
          []      -> Nothing
      , entryData = zip (map attributeName (matching attributeTypeData attributes)) dataAttributes
      }
  where
    relevant = filter (\raw -> rawAttributeType raw `elem` knownTypes) (recordAttributes record)
    knownTypes =
      [ attributeTypeStandardInformation
      , attributeTypeFileName
      , attributeTypeData
      ]
    matching typeCode = filter ((== typeCode) . attributeTypeCode)
