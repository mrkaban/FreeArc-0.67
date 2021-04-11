include ../../common.mak

ALL: Example-C.exe fazip.exe mse.exe

CODE_FLAGS  = -fno-exceptions -fno-rtti -Wall \
              -Wno-unknown-pragmas -Wno-sign-compare -Wno-conversion \
               -I../_Encryption -I../_Encryption/headers -I../_Encryption/hashes
OPT_FLAGS   = -O3 \
              -fomit-frame-pointer -fstrict-aliasing \
              -fforce-addr -funroll-loops
DEBUG_FLAGS = -g0
CFLAGS = $(CODE_FLAGS) $(OPT_FLAGS) $(DEBUG_FLAGS) $(DEFINES)

BUILD_FLAGS =  -lstdc++ -lole32 -s -Xlinker --large-address-aware

COMPRESSION_OBJS = $(TEMPDIR)/C_LZMA.o $(TEMPDIR)/C_BCJ.o $(TEMPDIR)/C_Dict.o $(TEMPDIR)/C_Delta.o $(TEMPDIR)/C_LZ4.o \
                   $(TEMPDIR)/C_REP.o $(TEMPDIR)/C_LZP.o $(TEMPDIR)/C_PPMD.o $(TEMPDIR)/C_External.o $(TEMPDIR)/C_CLS.o \
                   $(TEMPDIR)/C_MM.o $(TEMPDIR)/C_TTA.o $(TEMPDIR)/C_Tornado.o $(TEMPDIR)/C_GRZip.o $(TEMPDIR)/C_4x4.o $(TEMPDIR)/C_DisPack.o \
                   $(TEMPDIR)/Common.o $(TEMPDIR)/CompressionLibrary.o

Example-C.exe: Example-C.cpp makefile $(COMPRESSION_OBJS)
	$(GCC) $(CFLAGS) Example-C.cpp -o Example-C.exe $(COMPRESSION_OBJS) $(BUILD_FLAGS)

fazip.exe: fazip.cpp makefile $(COMPRESSION_OBJS)
	$(GCC) $(CFLAGS) fazip.cpp -o fazip.exe $(COMPRESSION_OBJS) $(BUILD_FLAGS)

mse.exe: mse.cpp makefile $(COMPRESSION_OBJS)
	$(GCC) $(CFLAGS) mse.cpp -o mse.exe $(COMPRESSION_OBJS) $(BUILD_FLAGS)
