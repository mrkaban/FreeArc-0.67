// C_LZ4.cpp - ��������� FreeArc � ���������� ������ LZ4 � LZ4HC

extern "C" {
#include "C_LZ4.h"
#include "lz4.c"

#ifndef FREEARC_DECOMPRESS_ONLY
namespace LZ4HC {
#undef   MAX_DISTANCE
#undef   HASH_LOG
#undef   matchlimit
#undef   ML_MASK
#undef   INITBASE
#undef   LZ4_BLINDCOPY
#include "lz4hc.c"
}
#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

}

// Version of LZ4 compressed data format
#define LZ4_VERSION 1

// ��������/���������� � ������
int LZ4_METHOD::DeCompressMem (COMPRESSION direction, void *input, int inputSize, void *output, int *outputSize, CALLBACK_FUNC *callback, void *auxdata, void **CodecState)
{
  if (direction==COMPRESS)
  {
#ifndef FREEARC_DECOMPRESS_ONLY
    *(char*)output=LZ4_VERSION;
    // LZ4_compress*() return compressed size, or 0 if the compression failed for any reason
    *outputSize  =  Compressor?  LZ4HC::LZ4_compressHC      ((const char*)input, (char*)output+1, inputSize)
                              :  LZ4_compress_limitedOutput ((const char*)input, (char*)output+1, inputSize, *outputSize-1);
    return (*outputSize)++ <= 0  ||  MinCompression>0 && *outputSize>=(double(inputSize)*MinCompression)/100?  FREEARC_ERRCODE_OUTBLOCK_TOO_SMALL : FREEARC_OK;
#else
    return FREEARC_ERRCODE_ONLY_DECOMPRESS;
#endif  // !defined (FREEARC_DECOMPRESS_ONLY)
  }
  else
  {
    if (inputSize<=0 || *(char*)input!=LZ4_VERSION)  return FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
    // LZ4_uncompress_unknownOutputSize() returns output size, or negative value if decompression failed
    int result  =  LZ4_uncompress_unknownOutputSize ((const char*)input+1, (char*)output, inputSize-1, *outputSize);
    return (result==*outputSize?  FREEARC_OK : FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
  }
}

// ������� ����������
int LZ4_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
    int errcode = FREEARC_OK;   // Error code returned by last operation or FREEARC_OK
    BYTE* In = NULL;            // ��������� �� ������� ������
    BYTE* Out= NULL;            // ��������� �� �������� ������
    BIGALLOC (BYTE, In,  BlockSize);
    BIGALLOC (BYTE, Out, BlockSize);
    int len; READ_LEN_OR_EOF (len, In, 1);
    if (len!=1 || *In!=LZ4_VERSION)  ReturnErrorCode(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
    for(;;) {
        int InSize, OutSize;     // ���������� ���� �� ������� � �������� ������, ��������������
        READ4_OR_EOF (InSize);
        if (InSize<0) {
            // ����������� ������������� ������
            InSize = -InSize;
            READ  (In, InSize);
            WRITE (In, InSize);
        } else {
            // ���������� ������������� � �������� ������ �������� ������
            READ  (In, InSize);
            // LZ4_uncompress_unknownOutputSize() returns output size, or negative value if decompression failed
            OutSize  =  LZ4_uncompress_unknownOutputSize ((const char*)In, (char*)Out, InSize, BlockSize);
            if (OutSize<0)  ReturnErrorCode(FREEARC_ERRCODE_BAD_COMPRESSED_DATA);
            WRITE (Out, OutSize);
        }
    }
finished:
    BigFreeAndNil(In); BigFreeAndNil(Out);
    return errcode;
}

#ifndef FREEARC_DECOMPRESS_ONLY

// ������� ��������
int LZ4_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
    int errcode = FREEARC_OK;   // Error code returned by last operation or FREEARC_OK
    BYTE* In = NULL;            // ��������� �� ������� ������
    BYTE* Out= NULL;            // ��������� �� �������� ������
    BIGALLOC (BYTE, In,  BlockSize);
    BIGALLOC (BYTE, Out, LZ4_compressBound(BlockSize));
    for (bool FirstTime=true;;FirstTime=false)
    {
        int InSize, OutSize;     // ���������� ���� �� ������� � �������� ������, ��������������
    	READ_LEN_OR_EOF (InSize, In, BlockSize);
        if (FirstTime) {*Out = LZ4_VERSION;  WRITE (Out, 1);}
        // LZ4_compress*() return compressed size, or 0 if the compression failed for any reason
        OutSize  =  Compressor?  LZ4HC::LZ4_compressHC      ((const char*)In, (char*)Out, InSize)
                              :  LZ4_compress_limitedOutput ((const char*)In, (char*)Out, InSize, InSize);
        if (OutSize<=0  ||  MinCompression>0 && OutSize>=(double(InSize)*MinCompression)/100) {
            // ��������� ������ [���������� ������] �� �������, ������� ������ ��� �������� ������
            WRITE4 (-InSize);      // ������������� ����� � �������� ����� ����� - ������� Stored �����
            WRITE  (In, InSize);
        } else {
            // ������ ������� ���������
            WRITE4 (OutSize);
            WRITE  (Out, OutSize);
        }
    }
finished:
    BigFreeAndNil(In); BigFreeAndNil(Out);
    return errcode;
}

// �������� ����� ������, ������������ ��� ��������
MemSize LZ4_METHOD::GetCompressionMem()
{
  return (compress_all_at_once? 0 : BlockSize*2)
       + (Compressor? sizeof(LZ4HC::LZ4HC_Data_Structure) : 0);
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)


// �����������, ������������� ���������� ������ ������ �������� �� ���������
LZ4_METHOD::LZ4_METHOD()
{
  Compressor     = 0;
  BlockSize      = 1*mb;
  HashSize       = 0;
  MinCompression = 100;
}

// �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � parse_LZ4)
void LZ4_METHOD::ShowCompressionMethod (char *buf, bool purify)
{
  LZ4_METHOD defaults; char BlockSizeStr[100], HashSizeStr[100], CompressorStr[100], MinCompressionStr[100];
  showMem (BlockSize, BlockSizeStr);
  showMem (HashSize,  HashSizeStr);
  sprintf (CompressorStr,     Compressor    !=defaults.Compressor?     ":c%d"  : "", Compressor);
  sprintf (MinCompressionStr, MinCompression!=defaults.MinCompression? ":%d%%" : "", MinCompression);
  sprintf (buf, "lz4%s%s%s%s%s%s",
                    CompressorStr,
                    BlockSize!=defaults.BlockSize? ":b"         : "",
                    BlockSize!=defaults.BlockSize? BlockSizeStr : "",
                    HashSize !=defaults.HashSize?  ":h"         : "",
                    HashSize !=defaults.HashSize?  HashSizeStr  : "",
                    MinCompressionStr);
}

// ������������ ������ ���� LZ4_METHOD � ��������� ����������� ��������
// ��� ���������� NULL, ���� ��� ������ ����� ������ ��� �������� ������ � ����������
COMPRESSION_METHOD* parse_LZ4 (char** parameters)
{
  if (strcmp (parameters[0], "lz4") == 0) {
    // ���� �������� ������ (������� ��������) - "lz4", �� ������� ��������� ���������

    LZ4_METHOD *p = new LZ4_METHOD;
    int error = 0;  // ������� ����, ��� ��� ������� ���������� ��������� ������

    // �������� ��� ��������� ������ (��� ������ ������ ��� ������������� ������ ��� ������� ���������� ���������)
    while (*++parameters && !error)
    {
      char* param = *parameters;
      if (strequ(param,"hc"))  {p->Compressor = 2; continue;}
      else switch (*param) {                    // ���������, ���������� ��������
        case 'c':  p->Compressor= parseInt (param+1, &error); continue;
        case 'b':  p->BlockSize = parseMem (param+1, &error); continue;
        case 'h':  p->HashSize  = parseMem (param+1, &error); continue;
      }
      // ���� �������� ������������� ������ ��������. �� ��������� ���������� ��� ��� "N%"
      if (last_char(param) == '%') {
        char str[100]; strcpy(str,param); last_char(str) = '\0';
        int n = parseInt (str, &error);
        if (!error) { p->MinCompression = n; continue; }
        error=0;
      }
      // ����������� �����
      error=1;
    }
    if (error)  {delete p; return NULL;}  // ������ ��� �������� ���������� ������
    return p;
  } else
    return NULL;   // ��� �� ����� lz4a
}

static int LZ4_x = AddCompressionMethod (parse_LZ4);   // �������������� ������ ������ LZ4
