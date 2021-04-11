extern "C" {
#include "C_TTA.h"
#include "mmdet.h"
}

#define TTA_LIBRARY
#include "entropy.cpp"
#include "filters.cpp"
#include "tta.cpp"


/*-------------------------------------------------*/
/* ���������� ������ TTA_METHOD                    */
/*-------------------------------------------------*/

// �����������, ������������� ���������� ������ ������ �������� �� ���������
TTA_METHOD::TTA_METHOD()
{
  level       = 3;
  skip_header = 0;
  is_float    = 0;
  num_chan    = 0;
  word_size   = 0;
  offset      = 0;
  raw_data    = 0;
}

// ������� ����������
int TTA_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("tta_decompress");
  if (!f) f = (FARPROC) tta_decompress;

  return ((int (__cdecl *)(CALLBACK_FUNC*, void*)) f) (callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// ������� ��������
int TTA_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  // Use faster function from DLL if possible
  static FARPROC f = LoadFromDLL ("tta_compress");
  if (!f) f = (FARPROC) tta_compress;

  return ((int (__cdecl *)(int, int, int, int, int, int, int, CALLBACK_FUNC*, void*)) f)
                          (level, skip_header, is_float, num_chan, word_size, offset, raw_data, callback, auxdata);
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)


// �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � parse_TTA)
void TTA_METHOD::ShowCompressionMethod (char *buf, bool purify)
{
    TTA_METHOD defaults;  char eStr[100], cStr[100], rStr[100];
    if (num_chan || word_size) {
        sprintf (cStr, ":%d*%d%s", num_chan, word_size, is_float? "f":"");
        if (offset)  sprintf (str_end(cStr), ":o%d", offset);
    } else {
        sprintf (cStr, skip_header? ":s" : "");
    }
    sprintf (eStr, level      !=defaults.level?       ":m%d" : "", level);
    sprintf (rStr, raw_data   !=defaults.raw_data?    ":r%d" : "", raw_data);
    sprintf (buf, "tta%s%s%s", eStr, cStr, rStr);
}

// ������������ ������ ���� TTA_METHOD � ��������� ����������� ��������
// ��� ���������� NULL, ���� ��� ������ ����� ������ ��� �������� ������ � ����������
COMPRESSION_METHOD* parse_TTA (char** parameters)
{
  if (strcmp (parameters[0], "tta") == 0) {
    // ���� �������� ������ (������� ��������) - "tta", �� ������� ��������� ���������

    TTA_METHOD *p = new TTA_METHOD;
    int error = 0;  // ������� ����, ��� ��� ������� ���������� ��������� ������

    // �������� ��� ��������� ������ (��� ������ ������ ��� ������������� ������ ��� ������� ���������� ���������)
    while (*++parameters && !error)
    {
      char* param = *parameters;
      switch (*param) {                    // ���������, ���������� ��������
        case 'm':  p->level       = parseInt (param+1, &error); continue;
        case 's':  p->skip_header = 1;                          continue;
        case 'f':  p->is_float    = 1;                          continue;
        case 'c':  p->num_chan    = parseInt (param+1, &error); continue;
        case 'w':  p->word_size   = parseInt (param+1, &error); continue;
        case 'o':  p->offset      = parseInt (param+1, &error); continue;
        case 'r':  p->raw_data    = parseInt (param+1, &error); continue;
      }
      // ���� �� ��������, ���� � ��������� �� ������� ��� ��������
      // ���� ���� �������� ������� ��������� ��� c*w,
      // �� ���������� ��� �������� ��� ����� num_chan � word_size.
      // �������������� ������ 'f' ��������, ��� ��� ������ � FP-�������
      int a, b;  char s[MAX_METHOD_STRLEN];
      if (sscanf (param, "%d*%d%s", &a, &b, s)==3  &&  strequ(s,"f"))
          p->is_float = 1, p->num_chan=a, p->word_size=b;
      else if (sscanf (param, "%d*%d", &a, &b)==2)
          p->is_float = 0, p->num_chan=a, p->word_size=b;
      else error=1;
    }
    if (error)  {delete p; return NULL;}  // ������ ��� �������� ���������� ������
    return p;
  } else
    return NULL;   // ��� �� ����� TTA
}

static int TTA_x = AddCompressionMethod (parse_TTA);   // �������������� ������ ������ TTA
