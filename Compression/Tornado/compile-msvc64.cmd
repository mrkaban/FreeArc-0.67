@echo off
set libraries=user32.lib ole32.lib oleaut32.lib shell32.lib advapi32.lib
call "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" x86_amd64
::call "C:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x86_amd64
::call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" x86_amd64
t cl -DFULL_COMPILE -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -D_UNICODE -DUNICODE -Ox -GL -Gy %* main.cpp %libraries% /Fetor-msvc64.exe /link /SUBSYSTEM:CONSOLE,5.02
:: -DSTAT -DDEBUG -DFULL_COMPILE -DFREEARC_NO_TIMING
