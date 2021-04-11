@echo off
gcc.exe -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -DUNICODE -D_UNICODE -O3 -funroll-loops -march=i486 -mtune=pentiumpro -fomit-frame-pointer -fstrict-aliasing -ffast-math -fforce-addr mm.cpp -omm.exe


