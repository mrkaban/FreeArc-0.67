@echo off
@call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" x86
nmake clean
nmake "extra_cflags=%1"
