TARGETOS = BOTH
!include <win32.mak>

cflags =  $(cflags) -O2 -Gr -Gy -GR-
lflags  = /INCREMENTAL:NO /RELEASE /NOLOGO
dlllflags = $(lflags) -entry:_DllMainCRTStartup$(DLLENTRY) -dll

all: 7z-unarc$(XVER).dll


O = .

OBJS = \



7z-unarc$(XVER).dll: 7z-unarc.obj $(OBJS)
	$(implib) -machine:$(CPU) -def:7z-unarc.def $** -out:7z-unarc$(XVER).lib
	$(link) $(dlllflags) -base:0x00400000  -out:$*.dll $** $(olelibsdll) 7z-unarc$(XVER).exp

.cpp.obj:
	$(cc) $(cflags) /EHsc -I.. -I../.. -I../../7zip/CPP -I../../7zip/CPP/7zip/Archive -I../../7zip/CPP/include_windows -I../../7zip/CPP/myWindows $*.cpp

clean:
	-1 del 7z-unarc$(XVER).dll 7z-unarc$(XVER).exp 7z-unarc$(XVER).lib 7z-unarc.dll.manifest 7z-unarc.obj
