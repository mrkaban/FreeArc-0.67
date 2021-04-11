include ../common.mak

ALL: $(TEMPDIR)/Common.o $(TEMPDIR)/CompressionLibrary.o $(TEMPDIR)/CELS.o _TABI/TABI.hs

CODE_FLAGS = -fno-exceptions -fno-rtti \
                -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS = -Os -fomit-frame-pointer \
                 -ffast-math -fforce-addr -fstrict-aliasing
DEBUG_FLAGS = -g0
CFLAGS = -c $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/Common.o: Common.cpp Common.h Compression.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(TEMPDIR)/CompressionLibrary.o: CompressionLibrary.cpp Common.h Compression.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<

$(TEMPDIR)/CELS.o: CELS.cpp CELS.h _TABI/tabi.h Common.h makefile
	$(GCC) -c $(CFLAGS) -fexceptions -o $*.o $<

_TABI/TABI.hs: _TABI/TABI.hsc
	hsc2hs --cc=ghc $<
