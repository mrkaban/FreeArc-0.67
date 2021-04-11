/*
 *  Multiple I/O streams (BCJ2-like) experiment
 */

#include <memory>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../Compression.h"
#include "../MultiThreading.h"



// ****************************************************************************************************************************
// ����� "������" MSE *****************************************************************************************************
// ****************************************************************************************************************************

// ���������� ������ "������" MSE
class MSE_METHOD : public COMPRESSION_METHOD
{
public:
  int method;
  virtual int doit (char *what, int param, void *data, CALLBACK_FUNC *callback);
  // ������� ���������� � ��������
  virtual int decompress (CALLBACK_FUNC *callback, void *auxdata);
#ifndef FREEARC_DECOMPRESS_ONLY
  virtual int compress   (CALLBACK_FUNC *callback, void *auxdata);

  // ��������/���������� ����� ������, ������������ ��� ��������/����������, ������ ������� ��� ������ �����
  virtual MemSize GetCompressionMem        (void)               {return BUFFER_SIZE;}
  virtual void    SetCompressionMem        (MemSize)            {}
  virtual void    SetMinDecompressionMem   (MemSize)            {}
#endif
  virtual MemSize GetDecompressionMem      (void)               {return BUFFER_SIZE;}

  // �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ (�������, �������� � parse_MSE)
  virtual void ShowCompressionMethod (char *buf, bool purify);
};

// ��������� ������ ������ ������ MSE
COMPRESSION_METHOD* parse_MSE (char** parameters);


// ***********************************************************************************************************************
// ���������� ������ MSE_METHOD                                                                                          *
// ***********************************************************************************************************************

// ������������� �����, �������� �� ������ "NumOutStreams"
int MSE_METHOD::doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
{
       if (strequ (what, "NumOutStreams"))            return 13;                                                      // ���-�� �������� ������� ����������
  else if (strequ (what, "GetOutStreamCompressor"))   return (strcpy ((char*)data, "tor:3"), 0);                      // ����� �������� ��� param-���� ��������� ������ ������ MSE
  else                                                return COMPRESSION_METHOD::doit (what, param, data, callback);  // �������� ��������� ������ ������������ ���������
}

// ������� ����������
int MSE_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  char buf[100*kb];  int len, errcode;  int x=9;  char readx[99];
  if (method==1) {
    while (sprintf(readx,"read%d",x), ((len = errcode = callback (readx, buf, 4*kb, auxdata)) > 0)) {
      if ((errcode = callback ("write", buf, len, auxdata)) < 0)  break;
      x = (x+1)%13;
    }
  } else {
    while (1) {
      if (((errcode = callback ("read1", buf,     112, auxdata)) <= 0))  break;
      if (((errcode = callback ("read2", buf+512, 200, auxdata)) <= 0))  break;
      if (((errcode = callback ("read1", buf+112, 400, auxdata)) <= 0))  break;
      if (((errcode = callback ("read2", buf+712, 312, auxdata)) <= 0))  break;
      if ((errcode = callback ("write", buf, 1*kb, auxdata)) < 0)  break;
    }
  }
  return errcode;
}

#ifndef FREEARC_DECOMPRESS_ONLY

// ������� ��������
int MSE_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  char buf[100*kb];  int len, errcode;  int x=9;  char writex[99];
  if (method==1) {
    while ((len = errcode = callback ("read", buf, 4*kb, auxdata)) > 0) {
      if (sprintf(writex,"write%d",x), ((errcode = callback (writex, buf, len, auxdata)) < 0))  break;
      x = (x+1)%13;
    }
  } else {
    while ((len = errcode = callback ("read", buf, 1*kb, auxdata)) > 0) {
      if (((errcode = callback ("write1", buf,      12, auxdata)) < 0))  break;
      if (((errcode = callback ("write2", buf+512, 100, auxdata)) < 0))  break;
      if (((errcode = callback ("write1", buf+12,  500, auxdata)) < 0))  break;
      if (((errcode = callback ("write2", buf+612, 412, auxdata)) < 0))  break;
    }
  }
  return errcode;
}

// �������� � buf[MAX_METHOD_STRLEN] ������, ����������� ����� ������ (�������, �������� � parse_MSE)
void MSE_METHOD::ShowCompressionMethod (char *buf, bool purify)
{
  sprintf (buf, "mse%s", method==2?":1kb":"");
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// ������������ ������ ���� MSE_METHOD ��� ���������� NULL, ���� ��� ������ ����� ������
COMPRESSION_METHOD* parse_MSE (char** parameters)
{
  if (strcmp (parameters[0], "mse") == 0) {
    // ���� �������� ������ - "mse"
     MSE_METHOD *p = new MSE_METHOD;
     p->method = (parameters[1]==NULL? 1:2);
     return p;}
  else
    return NULL;   // ��� �� ����� mse
}

static int MSE_x = AddCompressionMethod (parse_MSE);   // �������������� ������ ������ MSE_METHOD



// ****************************************************************************************************************************
// ������� ********************************************************************************************************************
// ****************************************************************************************************************************

// Callback for compression/decompression functions
static int callback (const char *what, void *buf, int size, void *)
{
  fprintf(stderr,"%s %d\n", what, size);
  static bool first_time=TRUE;  static long basepos_output, basepos_input;
  if (first_time) {first_time = FALSE;  basepos_output = ftell(stdout);  basepos_input = ftell(stdin);}

  if      (strequ(what,"read"))          return fread(buf,1,size,stdin);
  else if (strequ(what,"write"))         return fwrite(buf,1,size,stdout)==size? size : FREEARC_ERRCODE_WRITE;
  else if (strequ(what,"seek_output"))   {return fseek (stdout, basepos_output+size, SEEK_SET)==0?  FREEARC_OK : FREEARC_ERRCODE_WRITE;}
  else if (strequ(what,"seek_input"))    {return fseek (stdin,  basepos_input +size, SEEK_SET)==0?  FREEARC_OK : FREEARC_ERRCODE_READ;}
  else                                   return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

int main(int argc, char *argv[])
{
  SetCompressionThreads(8);
  if (argc==2)
  {
    set_binary_mode(stdin);
    set_binary_mode(stdout);
    int result = (argv[1][0]!='-')? Compress   (argv[1],   callback, NULL)
                                  : Decompress (argv[1]+1, callback, NULL);
    if (result) {fprintf (stderr, "(De)compression error %d!\n", result); return EXIT_FAILURE;}
    return EXIT_SUCCESS;
  }
  else
  {
    printf ("MSE v0.1 (2012-01-15) - Multiple I/O streams (BCJ2-like) experiment\n"
            "\n"
            "Usage, compression: mse method <infile >outfile\n"
            "       decompression: mse d <infile >outfile\n"
            "\n"
            "Note: \"mse\" is the only multi-stream method implemented\n"
           );
    return argc==1? EXIT_SUCCESS : EXIT_FAILURE;
  }
}
