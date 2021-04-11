#include "../Compression.h"

// �������� � ������� ������� ������ ��������� ������������� � arc.ini ������� ���������.
// params �������� �������� ���������� �� arc.ini. ���������� 1, ���� �������� ���������.
int AddExternalCompressor (char *params);

// ������������� ������� � �������
void SynchronizeConio_Enter (void);
void SynchronizeConio_Leave (void);


#ifdef __cplusplus

// ���������� ������������ ���������� ������� ������ COMPRESSION_METHOD
class EXTERNAL_METHOD : public COMPRESSION_METHOD
{
public:
  // ��������� ����� ������ ������
  char    *name;            // ��� ������ (pmm, ccm...)
  bool     can_set_mem;     // �������� ��������� ���������� � ������?
  MemSize  cmem;            // ����� ������, ������������ ��� ������
  MemSize  dmem;            // ����� ������, ������������ ��� ����������
  char    *datafile;        // ������������ ����� � �������������� �������
  char    *packedfile;      // ������������ ����� � ������������ �������
  char    *packcmd;         // ������� �������� ������ (datafile -> packedfile)
  char    *unpackcmd;       // ������� ���������� ������ (packedfile -> datafile)
  char    *options[MAX_PARAMETERS];             // ���. ��������� ������
  char     option_strings[MAX_METHOD_STRLEN];   // ��������� ����� ��� �������� ������ ����������
  char    *defaultopt;      // �������� ���������� �� ���������
  int      solid;           // ��������� ������ �����-�����?
  int      useHeader;       // TRUE, ���� � ������ ������� ������ ������������ 0/1 - ������ �������/�����; FALSE - ��� ������� ���������� �������

  // ���������, ����������� ��� PPMonstr
  int     order;            // ������� ������ (�� �������� ��������� �������� ��������������� ���������)
  int     MRMethod;         // ��� ������, ����� ������, ���������� ��� �������� ������, ���������
  int     MinCompression;   // ����������� ������� ������. ���� �������� ������ ������, �� ������ ��� ����� �������� ������������ (��������) ������

  EXTERNAL_METHOD() {};

  // ������������� �����: ���������� ��������� ������� �������������� ������ ������
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback);

  // ��������/����������
  int DeCompress (COMPRESSION direction, CALLBACK_FUNC *callback, void *auxdata);

  // ������� ���������� � ��������
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata)     {return DeCompress (DECOMPRESS, callback, auxdata);}
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata)     {return DeCompress (COMPRESS, callback, auxdata);}

  // ��������/���������� ����� ������, ������������ ��� ��������/����������, ������ ������� ��� ������ �����
  virtual MemSize GetCompressionMem        (void)               {return cmem;}
  virtual void    SetCompressionMem        (MemSize _mem);
  virtual void    SetMinDecompressionMem   (MemSize _mem)       {SetCompressionMem(_mem);}
#endif
  virtual MemSize GetDecompressionMem      (void)               {return dmem;}

  // �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � parse_EXTERNAL)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// ��������� ������ ������������� EXTERNAL
COMPRESSION_METHOD* parse_EXTERNAL (char** parameters);

#endif  // __cplusplus
