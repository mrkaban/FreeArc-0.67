#include "../Compression.h"

int lzp_compress   (MemSize BlockSize, int MinCompression, int MinMatchLen, int HashSizeLog, int Barrier, int SmallestLen, CALLBACK_FUNC *callback, void *auxdata);
int lzp_decompress (MemSize BlockSize, int MinCompression, int MinMatchLen, int HashSizeLog, int Barrier, int SmallestLen, CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// ���������� ������������ ���������� ������� ������ COMPRESSION_METHOD
class LZP_METHOD : public COMPRESSION_METHOD
{
public:
  // ��������� ����� ������ ������
  MemSize BlockSize;        // ������ �����, ��������������� �� ���. ���������� ������ ������ ������ ����� �����
  int     MinCompression;   // ����������� ������� ������. ���� �������� ������ ������, �� ������ ��� ����� �������� ������������ (��������) ������
  int     MinMatchLen;      // ����������� ������ ����������� ������, ������� ����� ���������
  int     HashSizeLog;      // �������� ������� ���� (� 4-�������� ������). ������� �������� ����������� ������, �� ����������� ��������� ���
  int     Barrier;          // �������, ����� ������� ����������� ������������ ���������� �������� ������� (��������� lzma/ppmd �� ����� ��������� ��)
  int     SmallestLen;      // ������ ������ ������� ����������� ���� ��������� > Barrier

  // �����������, ������������� ���������� ������ ������ �������� �� ���������
  LZP_METHOD();
  // ������������� �����: ��� ������������� ����� �� ������� "VeryFast?" ��� ����<=128��
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
  {
      if (strequ (what,"VeryFast?"))  return HashSizeLog<=15;
      else return COMPRESSION_METHOD::doit (what, param, data, callback);
  }

  // ������� ���������� � ��������
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // ��������/���������� ����� ������, ������������ ��� ��������/����������, ������ ������� ��� ������ �����
  virtual MemSize GetCompressionMem        (void)               {return BlockSize*2 + (1<<HashSizeLog)*sizeof(BYTE*);}
  virtual void    SetCompressionMem        (MemSize mem);
  virtual void    SetMinDecompressionMem   (MemSize mem)        {SetCompressionMem(mem);}
  virtual void    SetDictionary            (MemSize dict)       {SetBlockSize (dict);}
  virtual void    SetBlockSize             (MemSize bs);
#endif
  virtual MemSize GetDictionary            (void)               {return BlockSize;}
  virtual MemSize GetBlockSize             (void)               {return BlockSize;}
  virtual MemSize GetDecompressionMem      (void)               {return BlockSize*2 + (1<<HashSizeLog)*sizeof(BYTE*);}

  // �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � parse_LZP)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// ��������� ������ ������ ������ LZP
COMPRESSION_METHOD* parse_LZP (char** parameters);

#endif  // __cplusplus
