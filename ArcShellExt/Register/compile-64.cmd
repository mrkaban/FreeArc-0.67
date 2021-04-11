@echo off
@call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" amd64
cl -DFREEARC_64BIT -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -D_UNICODE -DUNICODE -Os -Gy %1 %2 %3 Register.cpp user32.lib shell32.lib advapi32.lib ole32.lib /FeRegister64.exe  /link /SUBSYSTEM:WINDOWS,5.02
mt.exe -manifest Register64.exe.manifest -outputresource:Register64.exe
copy Register64.exe "Manager of FreeArc integration settings (64-bit).exe"
del Register64.exe
