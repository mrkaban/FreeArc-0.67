/*
 *  LZMA header-less compressor
 *
 *  This program implements simple file-to-file compressor and decompressor
 *  Run program without parameters to see it's syntax
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../Compression.h"
#include "C_LZMA.cpp"

// Input/output files and callback for (de)compression functions
FILE *infile, *outfile;
static int callback (const char *what, void *buf, int size, void *)
{
  if      (strequ(what,"write"))   return fwrite(buf,1,size,outfile)==size? size : FREEARC_ERRCODE_WRITE;
  else if (strequ(what,"read"))    return fread(buf,1,size,infile);
  else                             return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

int process (char *direction, char *method)
{
  int result;
  if (nameequ (direction, "c")  ||  nameequ (direction, "e"))
    result = Compress (method, callback, NULL);
  else if (nameequ (direction, "d"))
    result = Decompress (method, callback, NULL);
  else
    result = FREEARC_ERRCODE_NOT_IMPLEMENTED;
  if (result) {fprintf (stderr, "(De)compression error %d!\n", result); return EXIT_FAILURE;}
  return EXIT_SUCCESS;
}

int main(int argc, char *argv[])
{
  SetCompressionThreads(2);
  if (argc==3 || argc==5)
  {
    const char *infile_name   =  (argc==3? "-" : argv[3]);
    const char *outfile_name  =  (argc==3? "-" : argv[4]);
    infile  = nameequ(infile_name, "-")? stdin  : fopen (infile_name,  "rb");   if (infile==NULL)  {fprintf (stderr, "Can't open input file %s!\n",    infile_name);  return EXIT_FAILURE;}
    outfile = nameequ(outfile_name,"-")? stdout : fopen (outfile_name, "wb");   if (outfile==NULL) {fprintf (stderr, "Can't create output file %s!\n", outfile_name); return EXIT_FAILURE;}
    set_binary_mode(infile); set_binary_mode(outfile);
    return process (argv[1], argv[2]);
  }
  else
  {
    printf ("FreeArc-LZMA v2.1 (2012-03-15) - external LZMA compressor for FreeArc\n"
            "\n"
            "Usage, compression: FreeArc-LZMA e method <infile >outfile\n"
            "                 or FreeArc-LZMA e method infile outfile\n"
            "       decompression: FreeArc-LZMA d method <infile >outfile\n"
            "                   or FreeArc-LZMA d method infile outfile\n"
            "\n"
            "Notes: method may include any LZMA parameters supported by FreeArc, for example lzma:ht4:fastest:1g\n"
            "       infile/outfile may be specified as \"-\" what means stdin/stdout\n"
           );
    return argc==1? EXIT_SUCCESS : EXIT_FAILURE;
  }
}
