// Multimedia preprocessing filter
#include "../Compression.h"
#include "mmdet.h"

int mm_compress   (int skip_header, int is_float, int num_chan, int byte_size, int offset, int reorder, CALLBACK_FUNC *callback, void *auxdata);
int mm_decompress (CALLBACK_FUNC *callback, void *auxdata);


#ifdef __cplusplus

// ���������� ������������ ���������� ������� ������ COMPRESSION_METHOD
class MM_METHOD : public COMPRESSION_METHOD
{
public:
  // ��������� ����� ������ ������
  int mode;         // Detection speed mode (1 - fastest, 9 - most accurate)
  int skip_header;  // Skip file header detection
  int is_float ;    // Floating-point data format
  int num_chan ;    // Channels count
  int word_size;    // Size of each encoded value, in bits
  int offset;       // File offset where MM data start (header is copied intact)
  int reorder;      // Reorder buffer contents so that each channel data are placed continuosly
                    //   (1 - reorder words, 2 - reorder bytes)

  // �����������, ������������� ���������� ������ ������ �������� �� ���������
  MM_METHOD();
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

  // �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � parse_MM)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// ��������� ������ ������ ������ MM
COMPRESSION_METHOD* parse_MM (char** parameters);

#endif  // __cplusplus
