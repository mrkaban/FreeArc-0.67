#include "../Compression.h"

int rep_compress   (unsigned BlockSize, int MinCompression, int ChunkSize, int MinMatchLen, int Barrier, int SmallestLen, int HashBits, int Amplifier, CALLBACK_FUNC *callback, void *auxdata);
int rep_decompress (unsigned BlockSize, int MinCompression, int ChunkSize, int MinMatchLen, int Barrier, int SmallestLen, int HashBits, int Amplifier, CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// ���������� ������������ ���������� ������� ������ COMPRESSION_METHOD
class REP_METHOD : public COMPRESSION_METHOD
{
public:
  // ��������� ����� ������ ������
  MemSize BlockSize;        // ������ ������. ���������� ������ ������ � �������� ���� ���������. ������ ������ - BlockSize+BlockSize/4
  int     MinCompression;   // ����������� ������� ������. ���� �������� ������ ������, �� ������ ��� ����� �������� ������������ (��������) ������
  int     ChunkSize;        // ������ ������, �� ������� ��������� � ���
  int     MinMatchLen;      // ����������� ����� ������, ��� ������� ��� ����� ���������� ������� �� ���������� ���������
  int     Barrier;          // �������, ����� ������� ����������� ������������ ���������� �������� ������� (��������� lzma/ppmd �� ����� ��������� ��)
  int     SmallestLen;      // ���� ������� ������
  int     HashSizeLog;      // �������� ������� ���� (� 4-�������� ������). ������� �������� ����������� ������, �� ��������� ���. ��� ������� �������� ����������� ������ ����������� �������������
  int     Amplifier;        // ����������� "��������" ������

  // �����������, ������������� ���������� ������ ������ �������� �� ���������
  REP_METHOD();

  // ������������� �����: ���������� ��������� ������� �������������� ������ ������
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
  {
      if      (strequ (what,"SparseDecompression?"))  return 1;
      else return COMPRESSION_METHOD::doit (what, param, data, callback);
  }

  // ������� ���������� � ��������
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // ��������/���������� ����� ������, ������������ ��� ��������/����������, ������ ������� ��� ������ �����
  virtual MemSize GetCompressionMem        (void);
  virtual void    SetCompressionMem        (MemSize mem);
  virtual void    SetMinDecompressionMem   (MemSize mem)        {if (mem>0)   BlockSize = mem;}
  virtual void    SetDictionary            (MemSize dict)       {if (dict>0)  BlockSize = dict;}
#endif
  virtual MemSize GetDictionary            (void)               {return BlockSize;}
  virtual MemSize GetDecompressionMem      (void)               {return BlockSize;}

  // �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � parse_REP)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// ��������� ������ ������ ������ REP
COMPRESSION_METHOD* parse_REP (char** parameters);

#endif  // __cplusplus
