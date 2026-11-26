# ntfs

A Haskell library for reading NTFS disk images. The goal is to build up from
the volume structure: boot sector, MFT records, attributes and runlists first,
then a high-level file system API.

## Status

The volume, MFT and attribute layers are implemented:

- `NTFS.BootSector` and `NTFS.Volume` for geometry and raw MFT record reads.
- `NTFS.MFT.Record` and `NTFS.MFT.Attribute` for records and attribute headers.
- `NTFS.Runlist` for mapping pairs and sparse runs.
- `NTFS.Attribute.Data`, `NTFS.Attribute.StandardInformation` and
  `NTFS.Attribute.FileName` for common attribute values.

A high-level file system API and directory index parsing are not available yet.
The current Hackage release is `0.1.0.1`.

## Building

    cabal build
    cabal test
