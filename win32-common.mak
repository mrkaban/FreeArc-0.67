DEFINES  = -DFREEARC_WIN -DFREEARC_INTEL_BYTE_ORDER -DWIN32 -D_WINDOWS -D_UNICODE -DUNICODE
TEMPDIR  = c:/temp/out/FreeArc
GHCDIR   = C:\Base\Compiler\ghc

#For ghc 6.10.3
LIBDIR   = $(GHCDIR)\gcc-lib
INCDIR   = $(GHCDIR)\include
GCC_EXE  = $(GHCDIR)\gcc.exe
GCC      = $(GCC_EXE) -B$(LIBDIR) -I$(INCDIR) -I$(INCDIR)\mingw -I$(INCDIR)\mingw\c++\3.4.5 -I$(INCDIR)\mingw\c++\3.4.5\mingw32 -march=i486 -mtune=pentiumpro
DLLWRAP  = $(GHCDIR)\gcc-lib\dllwrap.exe -B$(LIBDIR)
WINDRES  = $(GHCDIR)\windres.exe

#For ghc 6.12.3
#LIBDIR   = $(GHCDIR)\gcc-lib
#GCC      = $(GHCDIR)\mingw\bin\gcc.exe -B$(LIBDIR) -I$(GHCDIR)\include\mingw -I$(GHCDIR)\mingw\include -I$(GHCDIR)\lib\include
#DLLWRAP  = $(GHCDIR)\mingw\bin\dllwrap.exe -B$(LIBDIR)
#WINDRES  = $(GHCDIR)\mingw\bin\windres.exe
