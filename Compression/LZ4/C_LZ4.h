#include "../Compression.h"

#ifdef __cplusplus

// ���������� ������������ ���������� ������� ������ COMPRESSION_METHOD
class LZ4_METHOD : public COMPRESSION_METHOD
{
public:
  // ��������� ����� ������ ������
  int     Compressor;       // >0: use LZ4_compressHC()
  MemSize BlockSize;        // ������ �����, ��������������� �� ���. ���������� ������ ������ ������ ����� �����.
  MemSize HashSize;         // ������ ���-�������.
  int     MinCompression;   // ����������� ������� ������. ���� �������� ������ ������, �� ������ ��� ����� �������� ������������ (��������) ������.

  // �����������, ������������� ���������� ������ ������ �������� �� ���������
  LZ4_METHOD();

  // ��������/���������� � ������
  virtual int DeCompressMem (COMPRESSION direction, void *input, int inputSize, void *output, int *outputSize, CALLBACK_FUNC *callback=0, void *auxdata=0, void **CodecState=0);
  // ������� ���������� � ��������
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress (CALLBACK_FUNC *callback, void *auxdata);

  // ��������/���������� ����� ������, ������������ ��� ��������/����������, ������ ������� ��� ������ �����
  virtual MemSize GetCompressionMem        (void);
  virtual MemSize GetDictionary            (void)               {return BlockSize;}
  virtual void    SetCompressionMem        (MemSize mem)        {}
  virtual void    SetMinDecompressionMem   (MemSize mem)        {}
#endif
  virtual MemSize GetDecompressionMem      (void)               {return compress_all_at_once? 0 : BlockSize*2;}

  // �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � parse_LZMA)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// ��������� ������ ������ ������ LZMA
COMPRESSION_METHOD* parse_LZMA (char** parameters);

#endif  // __cplusplus
