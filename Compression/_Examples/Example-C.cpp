/*
 *  Example program of using FreeArc Unified Compression Library with C/C++
 *
 *  This program implements simplest compressor/decompressor
 *  Run program without parameters to see it's syntax
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../Compression.h"

// Callback for compression/decompression functions
static int callback (const char *what, void *buf, int size, void *)
{
  if      (strequ(what,"read"))    return fread(buf,1,size,stdin);
  else if (strequ(what,"write"))   return fwrite(buf,1,size,stdout)==size? size : FREEARC_ERRCODE_WRITE;
  else                             return FREEARC_ERRCODE_NOT_IMPLEMENTED;
}

int main(int argc, char *argv[])
{
  SetCompressionThreads(8);
  if (argc==2)
  {
    set_binary_mode(stdin);
    set_binary_mode(stdout);
    int result = strcasecmp (argv[1], "d")? CompressWithHeader (argv[1], callback, NULL)
                                          : DecompressWithHeader (callback, NULL);
    if (result) {fprintf (stderr, "(De)compression error %d!\n", result); return EXIT_FAILURE;}
    return EXIT_SUCCESS;
  }
  else
  {
    printf ("Compressor v2.2 (2012-01-15) - demonstration of FreeArc Unified Compression Library\n"
            "\n"
            "Usage, compression: Compressor method <infile >outfile\n"
            "       decompression: Compressor d <infile >outfile\n"
            "\n"
            "Note: all FreeArc compression methods are supported, for example rep:512m+4x4:lzma:ht4:8m\n"
           );
    return argc==1? EXIT_SUCCESS : EXIT_FAILURE;
  }
}
