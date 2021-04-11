@echo off
@call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" amd64
nmake XVER=-64 clean
nmake XVER=-64 "extra_cflags=%1"

