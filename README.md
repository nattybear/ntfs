# ntfs

A Haskell library for reading NTFS disk images. The goal is to build up from
the volume structure: boot sector, MFT records, attributes and runlists first,
then a high-level file system API.

## Status

The volume, MFT and attribute layers are implemented, along with a combined
file view:

- `NTFS.BootSector` and `NTFS.Volume` for geometry, MFT reads and data reads.
- `NTFS.MFT.Record`, `NTFS.MFT.Attribute` and `NTFS.MFT` for records,
  attribute headers and MFT enumeration.
- `NTFS.Runlist` for mapping pairs and sparse runs.
- `NTFS.Attribute.Data`, `NTFS.Attribute.StandardInformation` and
  `NTFS.Attribute.FileName` for common attribute values.
- `NTFS.File` for the metadata and data of one record.
- `NTFS.Index` for directory entries and exact-name lookup.
- `NTFS.Path` for path components resolved through parent MFT references.

NTFS name collation is not available yet.
The current Hackage release is `0.1.0.2`.

## Building

    cabal build
    cabal test
