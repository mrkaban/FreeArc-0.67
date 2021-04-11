#include "../Compression.h"
#include "libGRZip.h"

sint32 __cdecl GRZip_CompressBlock        (uint8* Input, sint32 Size, uint8* Output, sint32 Mode);
sint32 __cdecl GRZip_DecompressBlock      (uint8* Input, sint32 Size, uint8* Output);
sint32 __cdecl GRZip_GetAdaptiveBlockSize (uint8* Input, sint32 Size);


#ifdef __cplusplus

// ���������� ������������ ���������� ������� ������ COMPRESSION_METHOD
class GRZIP_METHOD : public COMPRESSION_METHOD
{
public:
  // ��������� ����� ������ ������
  MemSize BlockSize;                        // ������ ����� ������, �������������� ���������
  int     Method;                           // ��������� ��������� GRZip
  int     EnableLZP;
  int     MinMatchLen;
  int     HashSizeLog;
  int     AlternativeBWTSort;
  int     AdaptiveBlockSize;
  int     DeltaFilter;
  int     NumThreads;                       // Number of compression threads
  int     NumExtraBuffers;                  // Number of additional compression buffers used to optimize I/O

  // �����������, ������������� ���������� ������ ������ �������� �� ���������
  GRZIP_METHOD();
  int GetNumThreads()       {return NumThreads>0? NumThreads : GetCompressionThreads();}   // Number of (de)compression threads
  int GetNumExtraBuffers()  {return NumExtraBuffers>=0? NumExtraBuffers : 2;}              // Number of additional compression buffer pairs used for background I/O

  // ������������� �����, �������� �� ������ "has_progress?"
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback);
  // ������� ���������� � ��������
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // ��������/���������� (�����������) ����� ������, ������������ ��� ��������/����������, ������ ������� ��� ������ �����
  virtual MemSize GetCompressionMem        (void)               {return GetSetDeCompressionMem (COMPRESS, 0);}
  virtual MemSize GetMinCompressionMem     (void)               {return GetSetDeCompressionMem (COMPRESS, 0, true);}
  virtual MemSize GetMinDecompressionMem   (void)               {return GetSetDeCompressionMem (DECOMPRESS, 0, true);}
  virtual void    SetCompressionMem        (MemSize mem)        {GetSetDeCompressionMem (COMPRESS, mem);}
  virtual void    SetMinDecompressionMem   (MemSize mem)        {GetSetDeCompressionMem (DECOMPRESS, mem, true);}
  virtual void    SetDictionary            (MemSize dict)       {SetBlockSize (dict);}
  virtual void    SetBlockSize             (MemSize bs);
#endif
  virtual MemSize GetDictionary            (void)               {return BlockSize;}
  virtual MemSize GetBlockSize             (void)               {return BlockSize;}
  virtual MemSize GetDecompressionMem      (void)               {return GetSetDeCompressionMem (DECOMPRESS, 0);}
  virtual void    SetDecompressionMem      (MemSize mem)        {GetSetDeCompressionMem (DECOMPRESS, mem);}
          MemSize GetSetDeCompressionMem(COMPRESSION direction, MemSize mem, bool MINMEM = false);  // Get/Set amount of memory used for compression/decompression

  // �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � parse_GRZIP)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// ��������� ������ ������ ������ GRZIP
COMPRESSION_METHOD* parse_GRZIP (char** parameters);

#endif  // __cplusplus
