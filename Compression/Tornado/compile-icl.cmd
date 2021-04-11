@echo off
call "C:\Program Files (x86)\Intel\Parallel Studio 2011\ips-vars.cmd" ia32
::call "C:\Program Files (x86)\Intel\Composer XE 2013 SP1\bin\ipsxe-comp-vars.bat" ia32
t icl -W0 -DFULL_COMPILE -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -D_UNICODE -DUNICODE -O3 -Gy -Gr -GL -Qipo -Qinline-factor9999 %1 %2 %3 main.cpp user32.lib ole32.lib oleaut32.lib shell32.lib /Fetor-icl.exe /link  /LARGEADDRESSAWARE
:: -DSTAT -DDEBUG -DFULL_COMPILE -DFREEARC_NO_TIMING
