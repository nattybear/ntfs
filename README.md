# ntfs

A Haskell library for reading NTFS disk images. The goal is to build up from
the volume structure: boot sector, MFT records, attributes and runlists first,
then a high-level file system API.

## Status

Early development. The boot sector parser and volume-level MFT record reads
are available so far.

## Building

    cabal build
    cabal test
