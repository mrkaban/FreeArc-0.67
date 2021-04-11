include ../../common.mak

ALL: $(TEMPDIR)/C_LZ4.o

CODE_FLAGS  = -fno-exceptions -fno-rtti -Wall \
              -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -O3 \
              -fomit-frame-pointer -fstrict-aliasing \
              -fforce-addr -funroll-loops
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/C_LZ4.o: C_LZ4.cpp C_LZ4.h makefile
	$(GCC) -c $(CFLAGS) -o $*.o $<
