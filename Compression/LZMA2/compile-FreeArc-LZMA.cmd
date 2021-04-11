@echo off
set options=-DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -DUNICODE -D_UNICODE FreeArc-LZMA.cpp ../Common.cpp ../CompressionLibrary.cpp
set options_ms=-arch:SSE2 -MP -Gy -GL -GR- %options% user32.lib ole32.lib oleaut32.lib shell32.lib advapi32.lib -link -LARGEADDRESSAWARE
set options_ms_cl=-Ox -EHsc %options_ms%
set options_ms_icl=-w -O3 -Qipo %options_ms%

call "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" x86
cl  -FeFreeArc-LZMA-x86.exe %options_ms_cl%

call "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" x86_amd64
cl  -FeFreeArc-LZMA-x64.exe %options_ms_cl%
goto :exit

g++ %options% -O3 -oFreeArc-LZMA-x86.exe -lole32 -luuid -s -static -Xlinker --large-address-aware -m32 -msse2
g++ %options% -O3 -oFreeArc-LZMA-x64.exe -lole32 -luuid -s -static

C:\Base\Compiler\MinGW\bin\g++.exe %options% -O3 -oFreeArc-LZMA-x86.exe -lole32 -s -Xlinker --large-address-aware

call "C:\Program Files (x86)\Intel\Parallel Studio 2011\ips-vars.cmd" ia32
icl -FeFreeArc-LZMA-x86.exe %options_ms_icl%
iccpatch.exe FreeArc-LZMA-x86.exe

call "C:\Program Files (x86)\Intel\Parallel Studio 2011\ips-vars.cmd" ia32_intel64
icl -FeFreeArc-LZMA-x64.exe %options_ms_icl%
iccpatch.exe FreeArc-LZMA-x64.exe

:exit
del *.exe.bak *.obj 2>nul