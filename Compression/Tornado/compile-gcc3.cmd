@echo off
t C:\Base\Compiler\MinGW\bin\g++.exe -DFULL_COMPILE -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -D_UNICODE -DUNICODE -O3 --param inline-unit-growth=999 -funroll-loops -fno-exceptions -fno-rtti -march=i486 -mtune=pentiumpro -fomit-frame-pointer -fstrict-aliasing -ffast-math -fforce-addr %1 %2 %3 main.cpp -otor-full.exe  -lshell32 -lole32 -loleaut32 -luuid -s -Xlinker --large-address-aware
:: -DSTAT -DDEBUG -DFULL_COMPILE -DFREEARC_NO_TIMING
