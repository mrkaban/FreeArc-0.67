@echo off
call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\bin\vcvars32.bat"
t cl -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -D_UNICODE -DUNICODE -Ox -GL -Gy %* lzp2.cpp user32.lib /link  /LARGEADDRESSAWARE
del *.obj
