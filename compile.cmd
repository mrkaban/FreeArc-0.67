@set exe=Tests\Arc.exe
@set ctempdir=C:\temp\out\FreeArc
@set tempdir=%ctempdir%%1%2%3
@md  %tempdir%  >nul 2>nul
@md  %ctempdir% >nul 2>nul
@md  Tests      >nul 2>nul
@set defines=-DFREEARC_PACKED_STRINGS -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -optc-DFREEARC_WIN -optc-DFREEARC_INTEL_BYTE_ORDER -optc-DWIN32 -optc-D_WINDOWS -optc-D_UNICODE -optc-DUNICODE
@rem ******** -DFREEARC_UNIX -DFREEARC_MOTOROLA_BYTE_ORDER -DFREEARC_ONLY_ALIGNED_ACCESS -DFREEARC_NO_LUA *******
@copy win32-common.mak common.mak
@cd Compression
@call compile
@cd ..
@make
@if exist C:\Base\Compiler\ghc\mingw\lib\CRT_noglob.o   set noglob=C:\Base\Compiler\ghc\mingw\lib\CRT_noglob.o
@if exist C:\Base\Compiler\ghc\gcc-lib\CRT_noglob.o     set noglob=C:\Base\Compiler\ghc\gcc-lib\CRT_noglob.o
@set c_modules=%ctempdir%/Environment.o %ctempdir%/URL.o %ctempdir%/Common.o %ctempdir%/CompressionLibrary.o %ctempdir%/C_PPMD.o %ctempdir%/C_LZP.o %ctempdir%/C_LZ4.o %ctempdir%/C_LZMA.o %ctempdir%/C_BCJ.o %ctempdir%/C_GRZip.o %ctempdir%/C_Dict.o %ctempdir%/C_REP.o %ctempdir%/C_MM.o %ctempdir%/C_TTA.o %ctempdir%/C_Tornado.o %ctempdir%/C_Delta.o %ctempdir%/C_DisPack.o %ctempdir%/C_4x4.o %ctempdir%/C_External.o %ctempdir%/C_CLS.o %ctempdir%/C_Encryption.o %ctempdir%/Client7z.o -lstdc++ -lshell32 -lole32 -loleaut32 -luuid %noglob% -optl-s -optl-Xlinker -optl--large-address-aware
@::%ctempdir%/CELS.o %ctempdir%/cels-rep.o
@if .%1 == .-DFREEARC_GUI  set exe=Tests\FreeArc.exe && set c_modules=%c_modules% %ctempdir%/GuiEnvironment.o -optl-mwindows
@if .%2 == .-DFREEARC_GUI  set exe=Tests\FreeArc.exe && set c_modules=%c_modules% %ctempdir%/GuiEnvironment.o -optl-mwindows
@if .%3 == .-DFREEARC_GUI  set exe=Tests\FreeArc.exe && set c_modules=%c_modules% %ctempdir%/GuiEnvironment.o -optl-mwindows
@if .%1 == .-DFREEARC_DLL  set exe=Tests\FreeArc.dll && set options=-shared Tests\FreeArc.def
@if .%2 == .-DFREEARC_DLL  set exe=Tests\FreeArc.dll && set options=-shared Tests\FreeArc.def
@if .%3 == .-DFREEARC_DLL  set exe=Tests\FreeArc.dll && set options=-shared Tests\FreeArc.def
@set options=%options% -iCompression -iCompression/_TABI -threaded -fglasgow-exts -XUndecidableInstances -XOverlappingInstances -XNoMonomorphismRestriction -XBangPatterns
@set ghc_rts_options=+RTS -A2m -H50m -M300m
@del %exe% >nul 2>nul
t ghc.exe --make %1 %2 %3 Arc.hs %options% %defines% %c_modules% Unarc/gui/icon.o -odir %tempdir% -hidir %tempdir% -o %exe% %ghc_rts_options%
@del Compression\CompressionLib_stub.? Compression\_TABI\TABI_stub.? >nul 2>nul
