@echo off
call "C:\Program Files (x86)\Intel\Parallel Studio 2011\ips-vars.cmd" ia32
t icl -W0 -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -D_UNICODE -DUNICODE -EHsc -MT -O3 -Gy -GR- -Qip -arch:IA32 %1 %2 %3 dict.cpp user32.lib ole32.lib oleaut32.lib shell32.lib /Fedict-icl.exe /link  /LARGEADDRESSAWARE
:: -DSTAT -DDEBUG -DFULL_COMPILE -DFREEARC_NO_TIMING
iccpatch.exe dict-icl.exe
del *.exe.bak *.obj *.res 2>nul
