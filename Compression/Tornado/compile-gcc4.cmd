@echo off
set defines=-DFULL_COMPILE -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -D_UNICODE -DUNICODE
set optimizations=-O3 --param inline-unit-growth=999 -funroll-loops -fno-exceptions
::Comment out the next line in order to build 64-bit executable
set _32bit=-m32 -Xlinker --large-address-aware
t g++.exe %defines% %optimizations% %* main.cpp -otor-full4.exe  -lshell32 -lole32 -loleaut32 -luuid -s -static %_32bit%
:: -DSTAT -DDEBUG -DFULL_COMPILE -DFREEARC_NO_TIMING
