extern "C" {
#include "C_MM.h"
}
#define MM_LIBRARY
#include "mm.cpp"


/*-------------------------------------------------*/
/* ���������� ������ MM_METHOD                     */
/*-------------------------------------------------*/

// �����������, ������������� ���������� ������ ������ �������� �� ���������
MM_METHOD::MM_METHOD()
{
    mode        = 9;
    skip_header = 0;
    is_float    = 0;
    num_chan    = 0;
    word_size   = 0;
    offset      = 0;
    reorder     = 0;
}

// ������� ����������
int MM_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
    return mm_decompress (callback, auxdata);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// ������� ��������
int MM_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
    return mm_compress (mode, skip_header, is_float, num_chan, word_size, offset, reorder, callback, auxdata);
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)


// �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ � ��� ��������� (�������, �������� � parse_MM)
void MM_METHOD::ShowCompressionMethod (char *buf, bool purify)
{
    MM_METHOD defaults;
    char dStr[100], cStr[100], rStr[100];
    if (num_chan || word_size) {
        sprintf (cStr, ":%d*%d%s", num_chan, word_size, is_float? "f":"");
        if (offset)  sprintf (str_end(cStr), ":o%d", offset);
    } else {
        sprintf (cStr, skip_header? ":s" : "");
    }
    sprintf (rStr, reorder? ":r%d" : "", reorder);
    sprintf (dStr, mode!=defaults.mode? ":d%d" : "", mode);
    sprintf (buf, "mm%s%s%s", dStr, cStr, rStr);
}

// ������������ ������ ���� MM_METHOD � ��������� ����������� ��������
// ��� ���������� NULL, ���� ��� ������ ����� ������ ��� �������� ������ � ����������
COMPRESSION_METHOD* parse_MM (char** parameters)
{
  if (strcmp (parameters[0], "mm") == 0) {
    // ���� �������� ������ (������� ��������) - "mm", �� ������� ��������� ���������

    MM_METHOD *p = new MM_METHOD;
    int error = 0;  // ������� ����, ��� ��� ������� ���������� ��������� ������

    // �������� ��� ��������� ������ (��� ������ ������ ��� ������������� ������ ��� ������� ���������� ���������)
    while (*++parameters && !error)
    {
      char* param = *parameters;
      switch (*param) {                    // ���������, ���������� ��������
        case 's':  p->skip_header = 1;                          continue;
        case 'f':  p->is_float    = 1;                          continue;
        case 'd':  p->mode        = parseInt (param+1, &error); continue;
        case 'c':  p->num_chan    = parseInt (param+1, &error); continue;
        case 'w':  p->word_size   = parseInt (param+1, &error); continue;
        case 'o':  p->offset      = parseInt (param+1, &error); continue;
        case 'r':  p->reorder     = parseInt (param+1, &error); continue;
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
    return NULL;   // ��� �� ����� MM
}

static int MM_x = AddCompressionMethod (parse_MM);   // �������������� ������ ������ MM

