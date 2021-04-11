include ../../common.mak

ALL: $(TEMPDIR)/C_4x4.o

CODE_FLAGS = -fno-rtti \
                -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion
OPT_FLAGS   = -Os
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

$(TEMPDIR)/C_4x4.o: C_4x4.cpp C_4x4.h makefile ../MultiThreading.h
	$(GCC) -c $(CFLAGS) -o $*.o $<
