include ../common.mak

windows: mkdir unarc.exe unarc.dll FreeArc.fmt arc.sfx arc-mini.sfx arc-tiny.sfx freearc.sfx freearc-mini.sfx freearc-tiny.sfx freearc-installer.sfx freearc-installer-nodelete.sfx
unix: mkdir unarc arc.linux.sfx arc-mini.linux.sfx arc-tiny.linux.sfx

Extractor_DEFINES = -DFREEARC_DECOMPRESS_ONLY -D_NO_EXCEPTIONS -DUNARC
OBJDIR  = $(TEMPDIR)-unarc
LINKOBJ_FAR_PLUGIN = $(OBJDIR)/Environment.o $(OBJDIR)/Common.o $(OBJDIR)/CompressionLibrary.o \
                     $(OBJDIR)/C_LZMA.o
LINKOBJ_TINY = $(LINKOBJ_FAR_PLUGIN) $(OBJDIR)/C_BCJ.o $(OBJDIR)/C_Dict.o $(OBJDIR)/C_Delta.o
LINKOBJ_MINI = $(LINKOBJ_TINY) $(OBJDIR)/C_REP.o $(OBJDIR)/C_LZP.o $(OBJDIR)/C_PPMD.o $(OBJDIR)/C_External.o
LINKOBJ      = $(LINKOBJ_MINI) $(OBJDIR)/C_MM.o $(OBJDIR)/C_TTA.o $(OBJDIR)/C_Tornado.o $(OBJDIR)/C_GRZip.o $(OBJDIR)/C_4x4.o \
                               $(OBJDIR)/C_DisPack.o $(OBJDIR)/C_LZ4.o $(OBJDIR)/C_Encryption.o $(OBJDIR)/C_CLS.o

FAR_PLUGIN = FarPlugin.cpp ArcStructure.h
UNARC = ArcStructure.h ArcCommand.h ArcProcess.h
CUI = CUI.h
GUI = gui\gui.h gui\gui.cpp
HEADERS =  ../Compression/Compression.h ../Compression/Common.h

LINK_FLAGS = -L$(LIBDIR) -Xlinker --large-address-aware -lstdc++ -s
UNIX_LINK_FLAGS = -L$(LIBDIR) -lstdc++ -lrt -lpthread -s

CONSOLE_LINK_FLAGS = $(LINK_FLAGS) -lole32
GUI_LINK_FLAGS     = $(LINK_FLAGS) -lole32 -lcomctl32 -mwindows


CODE_FLAGS  = -fno-exceptions -fno-rtti -Wall \
              -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -march=i486 -mtune=pentiumpro -ffast-math \
              -fomit-frame-pointer -fstrict-aliasing \
              -fforce-addr
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) -O2 $(DEBUG_FLAGS) $(DEFINES) $(Extractor_DEFINES)
CFLAGS1= $(CODE_FLAGS) $(OPT_FLAGS) -O1 $(DEBUG_FLAGS) $(DEFINES) $(Extractor_DEFINES)

#No more use UPX due to problems with antivirus software
UPX = cmd /c rem
#UPX = upx.exe -9 --lzma --compress-icons=2

mkdir:
	-mkdir $(OBJDIR)



# Windows executables
unarc.exe: $(OBJDIR)/unarc.o $(LINKOBJ)
	$(GCC) $^ $(CONSOLE_LINK_FLAGS) -o $@

unarc.dll: unarcdll.cpp unarcdll.h $(UNARC) $(HEADERS) $(LINKOBJ)
	$(GCC) -c $(CFLAGS) -DFREEARC_LIBRARY -DUNARC_DECRYPTION -o $(OBJDIR)/unarcdll.o $<
	$(DLLWRAP) --driver-name $(GCC_EXE) $(OBJDIR)/unarcdll.o $(LINKOBJ) -lstdc++ -lole32 -def unarc.def -s -o $@

FreeArc.fmt: $(OBJDIR)/FarPlugin.o FarPlugin.def $(LINKOBJ_FAR_PLUGIN)
	$(DLLWRAP) --driver-name $(GCC_EXE) $< $(LINKOBJ_FAR_PLUGIN) -lstdc++ -lole32 -def FarPlugin.def -s -o $@

arc.sfx: $(OBJDIR)/ArcSFX.o $(LINKOBJ)
	$(GCC) $^ $(CONSOLE_LINK_FLAGS) -o $@
	$(UPX) $@

arc-mini.sfx: $(OBJDIR)/ArcMiniSFX.o $(LINKOBJ_MINI)
	$(GCC) $^ $(CONSOLE_LINK_FLAGS) -o $@
	$(UPX) $@

arc-tiny.sfx: $(OBJDIR)/ArcTinySFX.o $(LINKOBJ_TINY)
	$(GCC) $^ $(CONSOLE_LINK_FLAGS) -o $@
	$(UPX) $@

freearc.sfx: $(OBJDIR)/FreeArcSFX.o $(OBJDIR)/res.o $(LINKOBJ)
	$(GCC) $^ $(GUI_LINK_FLAGS) -o $@
	$(UPX) $@

freearc-mini.sfx: $(OBJDIR)/FreeArcMiniSFX.o $(OBJDIR)/res-small.o $(LINKOBJ_MINI)
	$(GCC) $^ $(GUI_LINK_FLAGS) -o $@
	$(UPX) $@

freearc-tiny.sfx: $(OBJDIR)/FreeArcTinySFX.o $(OBJDIR)/res-small.o $(LINKOBJ_TINY)
	$(GCC) $^ $(GUI_LINK_FLAGS) -o $@
	$(UPX) $@

freearc-installer.sfx: $(OBJDIR)/FreeArcInstaller.o $(OBJDIR)/res.o $(LINKOBJ)
	$(GCC) $^ $(GUI_LINK_FLAGS) -o $@
	$(UPX) $@

freearc-installer-nodelete.sfx: $(OBJDIR)/FreeArcInstallerNoDelete.o $(OBJDIR)/res.o $(LINKOBJ)
	$(GCC) $^ $(GUI_LINK_FLAGS) -o $@
	$(UPX) $@



# Linux executables
unarc: $(OBJDIR)/unarc.o $(LINKOBJ)
	$(GCC) $< $(LINKOBJ) $(UNIX_LINK_FLAGS) -o $@

arc.linux.sfx: $(OBJDIR)/ArcSFX.o $(LINKOBJ)
	$(GCC) $< $(LINKOBJ)      $(UNIX_LINK_FLAGS) -o $@
	chmod -x $@

arc-mini.linux.sfx: $(OBJDIR)/ArcSFX.o $(LINKOBJ_MINI)
	$(GCC) $< $(LINKOBJ_MINI) $(UNIX_LINK_FLAGS) -o $@
	chmod -x $@

arc-tiny.linux.sfx: $(OBJDIR)/ArcTinySFX.o $(LINKOBJ_TINY)
	$(GCC) $< $(LINKOBJ_TINY) $(UNIX_LINK_FLAGS) -o $@
#	upx -9 --lzma arc*.linux.sfx
	chmod -x $@



# Object files
$(OBJDIR)/res.o: gui/main.rc gui/common.rc
	$(WINDRES) -Igui $< $@
	$(WINDRES) -Igui gui\icon.rc gui\icon.o

$(OBJDIR)/res-small.o: gui/main-small.rc gui/common.rc
	$(WINDRES) -Igui $< $@

$(OBJDIR)/FarPlugin.o: $(FAR_PLUGIN) $(HEADERS)
	$(GCC) -c $(CFLAGS) -IHeaders.c -o $*.o $<

$(OBJDIR)/unarc.o: unarc.cpp $(UNARC) $(CUI) $(HEADERS)
	$(GCC) -c $(CFLAGS) -DUNARC_DECRYPTION -o $*.o $<

$(OBJDIR)/ArcSFX.o: unarc.cpp $(UNARC) $(CUI) $(HEADERS)
	$(GCC) -c $(CFLAGS1) -DFREEARC_SFX -DUNARC_DECRYPTION -o $*.o $<

$(OBJDIR)/ArcMiniSFX.o: unarc.cpp $(UNARC) $(CUI) $(HEADERS)
	$(GCC) -c $(CFLAGS1) -DFREEARC_SFX -o $*.o $<

$(OBJDIR)/ArcTinySFX.o: unarc.cpp $(UNARC) $(CUI) $(HEADERS)
	$(GCC) -c $(CFLAGS1) -DFREEARC_SFX -DFREEARC_TINY -o $*.o $<

$(OBJDIR)/FreeArcSFX.o: unarc.cpp $(UNARC) $(GUI) $(HEADERS)
	$(GCC) -c $(CFLAGS1) -DFREEARC_SFX -DFREEARC_GUI -DUNARC_DECRYPTION -Igui -o $*.o $<

$(OBJDIR)/FreeArcMiniSFX.o: unarc.cpp $(UNARC) $(GUI) $(HEADERS)
	$(GCC) -c $(CFLAGS1) -DFREEARC_SFX -DFREEARC_GUI -Igui -o $*.o $<

$(OBJDIR)/FreeArcTinySFX.o: unarc.cpp $(UNARC) $(GUI) $(HEADERS)
	$(GCC) -c $(CFLAGS1) -DFREEARC_SFX -DFREEARC_TINY -DFREEARC_GUI -Igui -o $*.o $<

$(OBJDIR)/FreeArcInstaller.o: unarc.cpp $(UNARC) $(GUI) $(HEADERS)
	$(GCC) -c $(CFLAGS1) -DFREEARC_SFX -DFREEARC_INSTALLER -DFREEARC_GUI -DUNARC_DECRYPTION -Igui -o $*.o $<

$(OBJDIR)/FreeArcInstallerNoDelete.o: unarc.cpp $(UNARC) $(GUI) $(HEADERS)
	$(GCC) -c $(CFLAGS1) -DFREEARC_SFX -DFREEARC_INSTALLER -DFREEARC_INSTALLER_NODELETE -DFREEARC_GUI -DUNARC_DECRYPTION -Igui -o $*.o $<


$(OBJDIR)/Environment.o: ../Environment.cpp ../Environment.h $(HEADERS)
	$(GCC) -c $(CFLAGS1) -o $*.o $<

$(OBJDIR)/CompressionLibrary.o: ../Compression/CompressionLibrary.cpp $(HEADERS)
	$(GCC) -c $(CFLAGS1) -o $*.o $<

$(OBJDIR)/Common.o: ../Compression/Common.cpp $(HEADERS)
	$(GCC) -c $(CFLAGS1) -o $*.o $<

$(OBJDIR)/C_External.o: ../Compression/External/C_External.cpp ../Compression/External/C_External.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<


$(OBJDIR)/C_4x4.o: ../Compression/4x4/C_4x4.cpp ../Compression/4x4/C_4x4.h ../Compression/MultiThreading.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_DisPack.o: ../Compression/DisPack/C_DisPack.cpp ../Compression/DisPack/C_DisPack.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_BCJ.o: ../Compression/LZMA2/C_BCJ.cpp ../Compression/LZMA2/C_BCJ.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_LZMA.o: ../Compression/LZMA2/C_LZMA.cpp ../Compression/LZMA2/C_LZMA.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_Dict.o: ../Compression/Dict/C_Dict.cpp ../Compression/Dict/C_Dict.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_Delta.o: ../Compression/Delta/C_Delta.cpp ../Compression/Delta/C_Delta.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_REP.o: ../Compression/REP/C_REP.cpp ../Compression/REP/C_REP.h ../Compression/REP/rep.cpp $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_LZP.o: ../Compression/LZP/C_LZP.cpp ../Compression/LZP/C_LZP.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_PPMD.o: ../Compression/PPMD/C_PPMD.cpp ../Compression/PPMD/C_PPMD.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -O1 -o $*.o $<

$(OBJDIR)/C_MM.o: ../Compression/MM/C_MM.cpp ../Compression/MM/C_MM.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_TTA.o: ../Compression/MM/C_TTA.cpp ../Compression/MM/C_TTA.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_Tornado.o: ../Compression/Tornado/C_Tornado.cpp ../Compression/Tornado/C_Tornado.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_GRZip.o: ../Compression/GRZip/C_GRZip.cpp ../Compression/GRZip/C_GRZip.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_LZ4.o: ../Compression/LZ4/C_LZ4.cpp ../Compression/LZ4/C_LZ4.h ../Compression/LZ4/lz4.c $(HEADERS)
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(OBJDIR)/C_Encryption.o: ../Compression/_Encryption/C_Encryption.cpp ../Compression/_Encryption/C_Encryption.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -I../Compression/_Encryption/headers -o $*.o $<

$(OBJDIR)/C_CLS.o: ../Compression/CLS/C_CLS.cpp ../Compression/CLS/C_CLS.h $(HEADERS)
	$(GCC) -c $(CFLAGS) -fexceptions -o $*.o $<
