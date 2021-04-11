FAZip is a standalone compression utility like gzip and bzip2.
It doesn't support any cmdline options but features the same great
compression power as FreeArc itself.

I don't recommend to use it for doing real compression due to it's
lack of CRC checking, file identification and other features common
for Unix compression tools. Consider it as technology demonstration
that some day may grow into really useful tool. Compression methods
supported and their parameters are exactly the same as in FreeArc
(so see FreeArc.htm for details). In paricular, on Windows it supports
CLS external compressors placed in cls-*.dll and accelerated compression
functions from facompress*.dll

Usage examples:

Fast binary data compression and decompression:
  fazip rep:512m:64:c64+exe+4x4:tor:3 example.tar example.fz
  fazip d example.fz example.tar

Fast text data compression:
  fazip grzip:m4 example.tar example.fz

Maximum binary data compression:
  fazip rep:512m+exe+delta+tempfile+lzma:max:128m example.tar example.fz

Maximum text data compression:
  fazip dict:p:128m+lzp:64m:105:d1m:s32:h22+ppmd:12:192m example.tar example.fz



FAZip also can be used as replacement of FreeArc internal compression methods
using this syntax:

  fazip compress:method ...
  fazip decompress:method ...

This way, FAZip works only with the compressed data stream and should be provided
with method names both on compression and decompression. File arc-fazip.ini
provides example of using FAZip to replace REP and LZMA internal compression methods,
it should be copied into the directory with FreeArc executable in order to work
