@echo off
set sources=Delta.cpp
set options=-DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -DUNICODE -D_UNICODE -I../_Encryption -I../_Encryption/headers -I../_Encryption/hashes %sources%
set options_gcc4=%options% -O3 -funroll-all-loops -mtune=generic -s -Wno-write-strings -Wno-unused-result -Wno-attributes -Wformat -lole32 -luuid -static
:: -g -Wa,-adhlns=srep.lst  libacof64o.lib -fno-builtin
set options_ms=-DFREEARC_64BIT -MP -Gy -GL -GR- -nologo %options% user32.lib shell32.lib ole32.lib advapi32.lib %* -link -LARGEADDRESSAWARE srep.res
set options_ms_cl=-Ox -GL -Gy -EHsc %options_ms%
:: -Fa
set options_ms_icl=-w -O3 -Qipo %options_ms%
:: /QxHOST /Qopt-prefetch:4 /Qunroll32 /Qinline-factor:10000 /Qipo
:: -QaxCORE-AVX2,AVX,SSE4.2,SSE2  /Qprofile-functions /Qprofile-loops  /Oa /Ow /Qalias-args[-]  -fno-exceptions  /Qopt-report-file:name
set options_ms_x86=-MACHINE:x86
set options_ms_x64=-MACHINE:x64

echo gcc4 x64
"C:\Base\Compiler\MinGW64\bin\g++.exe"  %options_gcc4% -osrep64g.exe -DMY_MEMCPY %*
if errorlevel 1   goto :exit
::goto :exit

echo gcc4 x86
"C:\Base\Compiler\MinGW64\bin\g++.exe"  %options_gcc4% -osrep32g.exe -m32 -msse2  -Xlinker --large-address-aware %*

echo gcc3
g++ %options% -O2 -fno-exceptions -osrep32s.exe -lole32 -s -Xlinker --large-address-aware %*

call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\bin\vcvars32.bat"
rc srep.rc >nul
cl  -Fesrep32m.exe -arch:SSE2 %options_ms_cl% %options_ms_x86%

call "C:\Program Files (x86)\Intel\Parallel Studio 2011\ips-vars.cmd" ia32
icl -Fesrep32i.exe -arch:SSE2 %options_ms_icl% %options_ms_x86%
iccpatch.exe srep32i.exe >nul

call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\bin\amd64\vcvars64.bat"
rc -D_64BIT srep.rc >nul
cl  -Fesrep64m.exe %options_ms_cl% %options_ms_x64%

call "C:\Program Files (x86)\Intel\Parallel Studio 2011\ips-vars.cmd" ia32_intel64
icl -Fesrep64i.exe %options_ms_icl% %options_ms_x64%
iccpatch.exe srep64i.exe >nul

:exit
del *.exe.bak *.obj *.res >nul 2>nul
