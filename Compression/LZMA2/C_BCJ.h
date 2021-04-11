#include "../Compression.h"
int bcj_x86_de_compress (int encoding, CALLBACK_FUNC *callback, void *auxdata);

#ifdef __cplusplus

// ���������� ������������ ���������� ������� ������ COMPRESSION_METHOD
class BCJ_X86_METHOD : public COMPRESSION_METHOD
{
public:
  // ������� ���������� � ��������
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // ��������/���������� ����� ������, ������������ ��� ��������/����������, ������ ������� ��� ������ �����
  virtual MemSize GetCompressionMem        (void)               {return LARGE_BUFFER_SIZE;}
  virtual void    SetCompressionMem        (MemSize mem)        {}
  virtual void    SetMinDecompressionMem   (MemSize mem)        {}
#endif
  virtual MemSize GetDecompressionMem      (void)               {return LARGE_BUFFER_SIZE;}

  // �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ (�������, �������� � parse_BCJ_X86)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// ��������� ������ ������ ������ BCJ_X86
COMPRESSION_METHOD* parse_BCJ_X86 (char** parameters);

#endif  // __cplusplus
