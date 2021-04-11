// Wave sound comression algorithm
#include "../Compression.h"
#include "ttaenc.h"


#ifdef __cplusplus

// ���������� ������������ ���������� ������� ������ COMPRESSION_METHOD
class TTA_METHOD : public COMPRESSION_METHOD
{
public:
  // ��������� ����� ������ ������
  int level;        // Compression level (1..3, higher means tighter and slower compression)
  int skip_header;  // Skip WAV header detection
  int is_float;     // Floating-point data format
  int num_chan;     // Channels count
  int word_size;    // Size of each encoded value, in bits
  int offset;       // File offset where MM data start (header is copied intact)
  int raw_data ;    // Write raw predictor's output without using entropy encoder

  // �����������, ������������� ���������� ������ ������ �������� �� ���������
  TTA_METHOD();
  // ������������� �����: ���������� ��������� ������� �������������� ������ ������
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
  {
      // ��������� �����-������, ���� ���������� 2+ �������� �������� ��� ������������ ����� � ������ ������� �����
      if (strequ (what,"nosolid?"))   return word_size!=8 || offset!=0;
      else return COMPRESSION_METHOD::doit (what, param, data, callback);
  }

  // ������� ���������� � ��������
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // ��������/���������� ����� ������, ������������ ��� ��������/����������, ������ ������� ��� ������ �����
  virtual MemSize GetCompressionMem        (void)               {return 2*mb;}
  virtual void    SetCompressionMem        (MemSize mem)        {}
  virtual void    SetMinDecompressionMem   (MemSize mem)        {}
#endif
  virtual MemSize GetDecompressionMem      (void)               {return 1*mb;}

  // �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � parse_TTA)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// ��������� ������ ������ ������ TTA
COMPRESSION_METHOD* parse_TTA (char** parameters);

#endif  // __cplusplus
