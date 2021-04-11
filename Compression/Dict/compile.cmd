@echo off
set include=C:\Base\Compiler\WATCOM11\h
set lib=C:\Base\Compiler\WATCOM11\lib
set watcom=C:\Base\Compiler\WATCOM11
set path=C:\Base\Compiler\WATCOM11\BINNT;%path%
::wcl386 -mf -5r -oneatxh   -oi+  -DUSE_INDEX1 DICT4.CPP

C:\Base\Compiler\MinGW\bin\g++.exe -DUNICODE -D_UNICODE -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -O3 -lstdc++ -lshell32 -lole32 -loleaut32 -luuid -s -Xlinker --large-address-aware %1 %2 %3 dict.cpp -odict.exe
:: -DPPMD_VERSION -DDEBUG
