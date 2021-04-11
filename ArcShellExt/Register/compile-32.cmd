@echo off
@call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" amd64
cl -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -D_UNICODE -DUNICODE -Os -Gy %1 %2 %3 Register.cpp user32.lib shell32.lib advapi32.lib ole32.lib /FeRegister.exe  /link /LARGEADDRESSAWARE  /SUBSYSTEM:WINDOWS,5.01
mt.exe -manifest Register.exe.manifest -outputresource:Register.exe
copy Register.exe "Manager of FreeArc integration settings.exe"
del Register.exe
