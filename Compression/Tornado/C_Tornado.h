#include "../Compression.h"

int tor_compress   (PackMethod m, CALLBACK_FUNC *callback, void *auxdata);
int tor_decompress (CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// ���������� ������������ ���������� ������� ������ COMPRESSION_METHOD
class TORNADO_METHOD : public COMPRESSION_METHOD
{
public:
  struct PackMethod m;      // ��������� ����� ������ ������

  // �����������, ������������� ���������� ������ ������ �������� �� ���������
  TORNADO_METHOD();
  // ������������� �����: ��� ������������� ����� �� ������� "VeryFast?" ��� ������� ������ 1-4, � ���� �� ������ "has_progress?"
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
  {
         if (strequ (what, "VeryFast?"))          return m.hash_row_width<=2;
    else if (strequ (what, "has_progress?"))      return 1;                        // ��, ���� �������� ������������ ����� � ��������� ��������
    else                                          return COMPRESSION_METHOD::doit (what, param, data, callback);  // �������� ��������� ������ ������������ ���������
  }

  // ��������/���������� � ������
  virtual int DeCompressMem (COMPRESSION direction, void *input, int inputSize, void *output, int *outputSize, CALLBACK_FUNC *callback=0, void *auxdata=0, void **CodecState=0);

  // ������� ���������� � ��������
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // ��������/���������� ����� ������, ������������ ��� ��������/����������, ������ ������� ��� ������ �����
  virtual MemSize GetCompressionMem        (void);
  virtual void    SetCompressionMem        (MemSize mem);
  virtual void    SetMinDecompressionMem   (MemSize mem)        {SetDictionary (mem);}
  virtual void    SetDictionary            (MemSize dict);
#endif
  virtual MemSize GetDictionary            (void)               {return m.buffer;}
  virtual MemSize GetDecompressionMem      (void);
  virtual LongMemSize GetMaxCompressedSize (LongMemSize insize);

  // �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � parse_TORNADO)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// ��������� ������ ������ ������ TORNADO
COMPRESSION_METHOD* parse_TORNADO (char** parameters);

#endif  // __cplusplus
