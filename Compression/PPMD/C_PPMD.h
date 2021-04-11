#include "../Compression.h"

int ppmd_compress2   (int ENCODE, int order, MemSize mem, int MRMethod, MemSize chunk, CALLBACK_FUNC *callback, void *auxdata);
int ppmd_decompress2 (int ENCODE, int order, MemSize mem, int MRMethod, MemSize chunk, CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// ���������� ������������ ���������� ������� ������ COMPRESSION_METHOD
class PPMD_METHOD : public COMPRESSION_METHOD
{
public:
  // ��������� ����� ������ ������
  int     order;     // ������� ������ (�� �������� ��������� �������� ��������������� ���������)
  MemSize mem;       // ����� ������, ������������ ��� �������� ������
  int     MRMethod;  // ��� ������, ����� ������, ���������� ��� �������� ������, ���������
  MemSize chunk;     // ������ ���������� ����� ��� compress_all_at_once

  // �����������, ������������� ���������� ������ ������ �������� �� ���������
  PPMD_METHOD();

  // ������� ���������� � ��������
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // ��������/���������� ����� ������, ������������ ��� ��������/����������, ������ ������� ��� ������ �����
  virtual MemSize GetCompressionMem        (void)               {return mem+1*mb;}
  virtual void    SetCompressionMem        (MemSize _mem);
  virtual void    SetMinDecompressionMem   (MemSize _mem)       {SetCompressionMem(_mem);}
#endif
  virtual MemSize GetAlgoMem               (void)               {return mem;}
  virtual MemSize GetDecompressionMem      (void)               {return mem+1*mb;}

  // �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � parse_PPMD)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// ��������� ������ ������ ������ PPMD
COMPRESSION_METHOD* parse_PPMD (char** parameters);

#endif  // __cplusplus
