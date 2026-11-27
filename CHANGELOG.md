# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Added

- `NTFS.File` with a combined view of the attributes of one MFT record.
- `NTFS.Volume.readData` for resident and non-resident attribute bytes, including sparse runs.
- `NTFS.MFT` with MFT record counts and ordered record enumeration.

### Fixed

- Accept short `$STANDARD_INFORMATION` values used by system files.

## [0.1.0.1] - 2026-11-27

### Added

- `NTFS.MFT.Attribute` with resident and non-resident attribute headers.
- `NTFS.Runlist` with mapping pairs decoding, including sparse runs.
- `NTFS.Attribute.Data` with resident and non-resident @$DATA@ views.
- `NTFS.Attribute.StandardInformation` with timestamps and file flags.
- `NTFS.Attribute.FileName` with parent reference, timestamps, sizes and name.
- `mftRefNumber` and `mftRefSequence` helpers for MFT references.

## [0.1.0.0] - 2026-11-27

### Added

- Initial library skeleton with common NTFS domain types.
- Pure parser for the NTFS boot sector and volume geometry helpers.
- `NTFS.Volume` with boot-sector loading, cluster offsets and MFT record reads.
- `NTFS.MFT.Record` with header parsing, update sequence fix-up and raw attribute slices.
