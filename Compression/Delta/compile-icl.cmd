@echo off
call "C:\Program Files (x86)\Intel\Parallel Studio 2011\ips-vars.cmd" ia32
t icl -W0 -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -D_UNICODE -DUNICODE -EHsc -MT -O3 -Gy -GR- -Qip -arch:IA32 %1 %2 %3 delta.cpp user32.lib ole32.lib oleaut32.lib shell32.lib /Fedelta-icl.exe /link  /LARGEADDRESSAWARE
:: -DSTAT "-DPROFILING=1" "-DPROFILING=2" "-DPROFILING=3" "-DPROFILING=4"
iccpatch.exe delta-icl.exe
del *.exe.bak *.obj *.res 2>nul
