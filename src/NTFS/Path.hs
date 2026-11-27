-- | Path components resolved by following parent MFT references.
module NTFS.Path
  ( rootDirectoryRef
  , readRecordPath
  , readEntryPath
  ) where

import           NTFS.Attribute.FileName (FileName (..), fileNameNamespaceDOS,
                                          fileNameNamespacePosix,
                                          fileNameNamespaceWin32,
                                          fileNameNamespaceWin32DOS)
import           NTFS.BootSector         (bsBytesPerSector)
import           NTFS.File               (FileEntry (..), isEntryInUse,
                                          parseFileEntry)
import           NTFS.MFT.Record         (parseMFTRecord)
import           NTFS.Types
import           NTFS.Volume             (Volume (..), readMFTRecordBytes)

-- | MFT reference of the root directory.
rootDirectoryRef :: MFTRef
rootDirectoryRef = MFTRef 5

-- | Resolve a record to its path components, starting at the root.
-- The root directory itself contributes no component.
readRecordPath :: Volume -> MFTRef -> IO (Either ParseError [String])
readRecordPath volume start = go start [] []
  where
    sectorBytes = bsBytesPerSector (volumeBootSector volume)

    go ref acc visited
      | mftRefNumber ref `elem` visited =
          pure (Left (InvalidValue "cycle in MFT parent chain"))
      | otherwise = do
          bytes <- readMFTRecordBytes volume ref
          case bytes >>= parseMFTRecord sectorBytes >>= parseFileEntry of
            Left err -> pure (Left err)
            Right entry
              | not (isEntryInUse entry) ->
                  pure (Left (InvalidValue "MFT record is not in use"))
              | otherwise ->
                  case preferredName (entryFileNames entry) of
                    Nothing -> pure (Left (InvalidValue "MFT record has no $FILE_NAME"))
                    Just name -> stepCurrent name
      where
        stepCurrent name
          | component == "." = pure (Right acc)
          | mftRefNumber parent == mftRefNumber ref =
              pure (Right (component : acc))
          | otherwise = go parent (component : acc) (mftRefNumber ref : visited)
          where
            parent = fileNameParent name
            component = fileNameName name

-- | Resolve a combined file entry to its path components.
readEntryPath :: Volume -> FileEntry -> IO (Either ParseError [String])
readEntryPath volume entry =
  readRecordPath volume (MFTRef (fromIntegral (entryRecordNumber entry)))

preferredName :: [FileName] -> Maybe FileName
preferredName names =
  case
    [ name
    | namespace <-
        [ fileNameNamespaceWin32
        , fileNameNamespaceWin32DOS
        , fileNameNamespaceDOS
        , fileNameNamespacePosix
        ]
    , name <- names
    , fileNameNamespace name == namespace
    ] of
    name:_ -> Just name
    []     -> Nothing
