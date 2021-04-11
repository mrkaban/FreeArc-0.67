/*
 *  Example program of using FreeArc Unified Compression Library with C/C++
 *
 *  This program implements simple file-to-file compressor and decompressor
 *  Run program without parameters to see its syntax
 *

To-do list:
    fazip 0.4:
        ht4 - prefetch entire hash line (disable tagging?)
        save crc/vmac at fileend (read 8mb until random secret number in the first N bytes)
        algorithm descriptions
        check icl11/x64 version (lzma doesn't work)
    fazip 0.5:
        require 64-bit fix: ppmd grzip tta
        rep:3g doesn't work
    fazip 0.x:
        4x4:external - doesn't work, probably due to problem with synchronization when simultaneously starting multiple programs
        ht4/bt4 - full prefetch, check cpu?
        64-bit MemSize (lzma:h4g)
        steal cmdline processing from bzip2 or tornado
        external: pause arc's progress indicator when running non-stdin-to-stdout external compressor
        external - не создавать tempdir при stdin+stdout?
        linux:external - support waiting for b/g processes (RunCommand "...&")
        WRITE4 in C_DisPack -> FWRITE4/FWRITE/FFLUSH
        PROGRESS/FREAD in rep/delta/C_DisPack/...
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../Compression.h"
#include "../MultiThreading.h"
#define STDINOUT_NAME "-"
static enum {INDICATOR_NONE, INDICATOR_ONLY_ERROR, INDICATOR_ONLY_LAST, INDICATOR_FULL}
             indicator = INDICATOR_FULL;             // Print nothing, only error messages, errors+final stats, and errors+live stats, appropriately

static bool  ENCODE;                                 // Is it compression operation?
static Mutex IO;                                     // Protects I/O operations: fwrite(outfile), remove(outname), fprintf(stderr) and Taskbar_*()

static const  char *infile_name, *outfile_name;      // Input/output file names or STDINOUT_NAME for stdin/stdout
static FILE  *infile, *outfile;                      // Input/output files
static int64  filesize;                              // Size of input file
static int64  insize, outsize, last_outsize;         // Input/output bytes processed so far
static int64  progress_insize, progress_outsize;     // Bytes reported so far via the "progress" callback
static bool   indicator_started = false;             // Progress indicator line was printed at least once
static bool   has_progress;                          // Prints stats according to "progress" calls
static FILE  *display = stderr;                      // File handle for printing all program messages
const  double TimeInterval = 0.2;                    // Interval in seconds between statistics updates
static double GlobalTime0, LastGlobalTime;           // Moment when (de)compression operation was started
static bool   delete_input_file = false;             // Delete input file on success
static uint   checksum_type = 0;                     // Type of checksum stored in the compressed file
static bool   nocrc = false;                         // Don't store/check checksum (primarily for benchmarking)
static Buffer buffer;                                // Holds input data not processed yet
static void (*checksum_update)(const void *, size_t);


// ****************************************************************************************************************************
// Progress indicator *********************************************************************************************************
// ****************************************************************************************************************************

static void init_stats(FILE *fin)
{
  has_progress = false;
  insize=outsize=progress_insize=progress_outsize=last_outsize=0;
  filesize = get_flen(fin);
  GlobalTime0 = GetGlobalTime();
  LastGlobalTime = 0;
}

static void print_stats (bool final=false)
{
  int64 _insize  = has_progress && !final? progress_insize  : insize,
        _outsize = has_progress && !final? progress_outsize : outsize;
  if (final? (indicator < INDICATOR_ONLY_LAST)
           : (indicator < INDICATOR_FULL)  ||  (_outsize - last_outsize < 64*kb))  return;   // Prints stats no more often than once per 64 kb
  last_outsize = _outsize;

  double GlobalTime = GetGlobalTime() - GlobalTime0;
  if (final || GlobalTime-LastGlobalTime>TimeInterval)
  {
    Lock _(IO);
    LastGlobalTime = GlobalTime;  double CPUTime = GetCPUTime();
    const char *newline = final? "     \n" : "     \b\b\b\b\b"; char temp1[100], temp2[100];
    int64 origsize = ENCODE? _insize : _outsize;         // Size of original (uncompressed) data
    int64 compsize = ENCODE? _outsize : _insize;         // Size of compressed data
    char progress[10];
    sprintf (progress, (filesize<=0 || filesize<_insize? "" : "%d%%: "), int(double(_insize)*100/filesize));
    fprintf (display, "\r%s%s -> %s: %.2lf%%  Cpu %.3lf mb/s (%.3lf sec), real %.3lf mb/s (%.3lf sec) = %.0lf%%%s",
             progress, show3(_insize,temp1), show3(_outsize,temp2), double(compsize)*100/origsize,
             origsize/CPUTime/mb, CPUTime, origsize/GlobalTime/mb, GlobalTime, CPUTime/GlobalTime*100, newline);
    indicator_started = true;
    if (*progress)
      Taskbar_SetProgressValue (_insize, filesize);
  }
}


// ****************************************************************************************************************************
// I/O ************************************************************************************************************************
// ****************************************************************************************************************************

// Callback for (de)compression functions
static int callback (const char *what, void *buf, int size, void *)
{
  if (strequ(what,"read"))
  {
    int len = buffer.get(buf,size);
    if (ENCODE)  checksum_update(buf,len);
    buf = (char*)buf+len;  insize += len;  size -= len;

    int bytes = fread(buf,1,size,infile);
    if (bytes>0)
    {
      insize += bytes;
      if (ENCODE)  checksum_update(buf,bytes);
    }

    return bytes>=0? len+bytes : bytes;
  }
  else if (strequ(what,"write"))
  {
    int bytes;
    {
      Lock _(IO);
      bytes = fwrite(buf,1,size,outfile);
      if (bytes>0)
      {
        outsize += bytes;
        if (!ENCODE)  checksum_update(buf,bytes);
      }
    }
    if (!has_progress)  print_stats();
    return bytes==size? size : FREEARC_ERRCODE_WRITE;
  }
  else if (strequ(what,"progress"))
  {
    has_progress = true;
    progress_insize  += ((int64*)buf)[0];
    progress_outsize += ((int64*)buf)[1];
    print_stats();
    return FREEARC_OK;
  }
  else
  {
    return FREEARC_ERRCODE_NOT_IMPLEMENTED;
  }
}


// ****************************************************************************************************************************
// VHASH: keyed cryptographic hash ********************************************************************************************
// ****************************************************************************************************************************

#define VMAC_TAG_LEN     128  /* Requesting VMAC-128 algorithm (instead of VMAC-64) */
#define VMAC_KEY_LEN     128  /* Must be 128, 192 or 256 (AES key size)        */
#define VMAC_NHBYTES     4096 /* Must 2^i for any 3 < i < 13. Standard = 128   */
#define VMAC_USE_LIB_TOM_CRYPT 1
#include "vmac/vmac.c"
#define VMAC_ALIGNMENT   16   /* SSE-compatible memory alignment */
#define VMAC_TAG_LEN_BYTES (VMAC_TAG_LEN/CHAR_BIT)
#define VMAC_KEY_LEN_BYTES (VMAC_KEY_LEN/CHAR_BIT)

ALIGN(VMAC_ALIGNMENT) vmac_ctx_t vmac_state;
uint64_t vmac_result[2] = {0,0};

// Initialize VMAC state
void vmac_init (void *seed)
{
  ALIGN(4) unsigned char key[VMAC_KEY_LEN_BYTES];
  memcpy (key, seed, VMAC_KEY_LEN_BYTES);
  vmac_set_key (key, &vmac_state);
}

// Update VMAC value with the buffer contents
void vmac_update (const void *buf, size_t size)
{
  if (size)  vmac_result[0] = vhash((unsigned char*)buf, size, &vmac_result[1], &vmac_state);
}

void none_init (void*) {}
void none_update (const void *, size_t) {}


// ****************************************************************************************************************************
// Compressed format handling *************************************************************************************************
// ****************************************************************************************************************************

#define FAZIP_SIGNATURE 0x0150695A
enum {FAZIP_FORMAT_EOF = 0,  FAZIP_FORMAT_VERSION1 = 1};
enum {TAG_EOF = 0,  TAG_COMPRESSOR = 1,  TAG_COMPRESSED_DATA = 2};
enum {CHECKSUM_TYPE_NONEE = 0,  CHECKSUM_TYPE_VMAC128 = 1};
struct {int size;  void (*init)(void*);  void (*update)(const void *, size_t);}
         checksum[] = { {16, none_init, none_update}
                      , {VMAC_KEY_LEN_BYTES, vmac_init, vmac_update}
                      };
unsigned char rnd[16+VMAC_KEY_LEN_BYTES] = {0x5B, 0x2E, 0xCB, 0xD8, 0x2B, 0xFE, 0x71, 0x76, 0x29, 0x89, 0x07, 0x3C, 0x6C, 0x37, 0x21, 0x3D};

static int write_header (char *method)
{
  int errcode = FREEARC_OK;  void *auxdata = NULL;
  Buffer buf;

  buf.put32 (BULAT_ZIGANSHIN_SIGNATURE);
  buf.put32 (FAZIP_SIGNATURE);
  buf.put8  (FAZIP_FORMAT_VERSION1);

  buf.put8  (TAG_COMPRESSOR);
  buf.putsz (method);

  buf.put8  (TAG_COMPRESSED_DATA);
  buf.put8  (checksum_type);
  systemRandomData (rnd, checksum[checksum_type].size);
  buf.put   (rnd, checksum[checksum_type].size);
  checksum[checksum_type].init(rnd);
  checksum_update = checksum[checksum_type].update;

  WRITE(buf.buf, buf.len());
finished:
  return errcode;
}

static int write_footer()
{
  int errcode = FREEARC_OK;  void *auxdata = NULL;
  Buffer buf;

  buf.put   (rnd, checksum[checksum_type].size);
  if (checksum_type == CHECKSUM_TYPE_VMAC128)
    buf.put (vmac_result, VMAC_TAG_LEN_BYTES);

  buf.put8  (TAG_EOF);
  buf.put32 (BULAT_ZIGANSHIN_SIGNATURE);
  buf.put32 (FAZIP_SIGNATURE);
  buf.put8  (FAZIP_FORMAT_EOF);

  WRITE(buf.buf, buf.len());
finished:
  return errcode;
}

static int read_header (char **method)
{
  int errcode = FREEARC_ERRCODE_BAD_HEADERS;  void *auxdata = NULL;  int len;

  buffer.empty();
  READ_LEN(len, buffer.buf, buffer.remainingSpace());
  buffer.end = buffer.buf + len;

  if (buffer.get32() != BULAT_ZIGANSHIN_SIGNATURE)  goto finished;
  if (buffer.get32() != FAZIP_SIGNATURE)            goto finished;
  if (buffer.get8 () != FAZIP_FORMAT_VERSION1)      goto finished;

  if (buffer.get8 () != TAG_COMPRESSOR)             goto finished;
  *method = strdup((char*)buffer.p);  buffer.p += strlen ((char*)buffer.p)+1;  // read ASCIIZ string

  if (buffer.get8 () != TAG_COMPRESSED_DATA)        goto finished;
  checksum_type = buffer.get8();
  if (checksum_type!=CHECKSUM_TYPE_NONEE && checksum_type!=CHECKSUM_TYPE_VMAC128)  goto finished;
  buffer.get (rnd, checksum[checksum_type].size);
  checksum[checksum_type].init(rnd);
  checksum_update = nocrc? none_update : checksum[checksum_type].update;

  errcode = FREEARC_OK;
finished:
  return errcode;
}


// ****************************************************************************************************************************
// MAIN ***********************************************************************************************************************
// ****************************************************************************************************************************

static char *error_msg (int errcode)
{
  switch (errcode)
  {
    case FREEARC_ERRCODE_INVALID_COMPRESSOR   :  return "invalid compression method or parameters";
    case FREEARC_ERRCODE_ONLY_DECOMPRESS      :  return "program was compiled with FREEARC_DECOMPRESS_ONLY, so don't try to use compress()";
    case FREEARC_ERRCODE_OUTBLOCK_TOO_SMALL   :  return "output block size in (de)compressMem is not enough for all output data";
    case FREEARC_ERRCODE_NOT_ENOUGH_MEMORY    :  return "can't allocate memory required for (de)compression";
    case FREEARC_ERRCODE_READ                 :  return "error when reading data";
    case FREEARC_ERRCODE_BAD_COMPRESSED_DATA  :  return "data can't be decompressed";
    case FREEARC_ERRCODE_NOT_IMPLEMENTED      :  return "requested feature isn't supported";
    case FREEARC_ERRCODE_NO_MORE_DATA_REQUIRED:  return "required part of data was already decompressed";
    case FREEARC_ERRCODE_OPERATION_TERMINATED :  return "operation terminated by user";
    case FREEARC_ERRCODE_WRITE                :  return "error when writing data";
    case FREEARC_ERRCODE_BAD_CRC              :  return "file failed CRC check";
    case FREEARC_ERRCODE_BAD_PASSWORD         :  return "password/keyfile failed checkcode test";
    case FREEARC_ERRCODE_BAD_HEADERS          :  return "archive headers are corrupted";
    case FREEARC_ERRCODE_INTERNAL             :  return "it should never happen: implementation error. Please report this bug to developers!";
    default                                   :  return "some error when (de)compressing";
  }
}

static void happens()
{
  Taskbar_Done();
  if (!nameequ(outfile_name,STDINOUT_NAME)) {
    fclose(outfile);
    remove(outfile_name);
  }
  exit(EXIT_FAILURE);
}

static void signal_handler(int)
{
  Lock _(IO);
  fprintf (display, "%sERROR: ^Break pressed\n", indicator_started?"\n":"");
  happens();
}

static int process (char *method)
{
  int result;  bool use_header;
       if (start_with(method,"compress:"))     ENCODE = true,  use_header = false, method = start_with(method,"compress:");
  else if (start_with(method,"decompress:"))   ENCODE = false, use_header = false, method = start_with(method,"decompress:");
  else if (strcasecmp (method, "d") != EQUAL)  ENCODE = true,  use_header = true;
  else                                         ENCODE = false, use_header = true;

  if (ENCODE)
  {
    if (use_header)  write_header(method);
    result = Compress (method, callback, NULL);
    if (use_header)  write_footer();
  }
  else
  {
    if (use_header)  read_header(&method);
    result = Decompress (method, callback, NULL);
  }

  if (result)
  {
    fprintf (display,  "%sERROR: %s\n", indicator_started?"\n":"",  error_msg(result));
    happens();
  }
  return EXIT_SUCCESS;
}

int main(int argc, char *argv[])
{
  checksum_type = CHECKSUM_TYPE_VMAC128;

  // Parse program options
  while (argv[1])
  {
         if (strequ(argv[1], "-i0"))   {indicator = INDICATOR_NONE;  display = fopen( WINDOWS_ONLY("nul") UNIX_ONLY("/dev/null"), "wb");}
    else if (strequ(argv[1], "-i1"))    indicator = INDICATOR_ONLY_ERROR;
    else if (strequ(argv[1], "-i2"))    indicator = INDICATOR_ONLY_LAST;
    else if (strequ(argv[1], "-i3"))    indicator = INDICATOR_FULL;
#ifdef FREEARC_WIN
    else if (strequ(argv[1], "-slp-"))  DefaultLargePageMode = DISABLE;
    else if (strequ(argv[1], "-slp" ))  DefaultLargePageMode = TRY;
    else if (strequ(argv[1], "-slp+"))  DefaultLargePageMode = FORCE;
#endif
    else if (strequ(argv[1], "-delete"))   delete_input_file = true;
    else if (strequ(argv[1], "-nocrc"))    checksum_type = CHECKSUM_TYPE_NONEE,  nocrc = true;
    else if (start_with(argv[1], "-rem"))  /* ignore option */;
    else break;
    argv++, argc--;
  }

  SetCompressionThreads(GetProcessorsCount());
  if (argc==2 || argc==4)
  {
    infile_name   =  (argc==2? STDINOUT_NAME : argv[2]);
    outfile_name  =  (argc==2? STDINOUT_NAME : argv[3]);
    infile  = nameequ(infile_name, STDINOUT_NAME)? stdin  : file_open_read_binary (infile_name );   if (infile==NULL)  {fprintf (display, "ERROR: can't open input file %s\n",    infile_name);  return EXIT_FAILURE;}
    outfile = nameequ(outfile_name,STDINOUT_NAME)? stdout : file_open_write_binary(outfile_name);   if (outfile==NULL) {fprintf (display, "ERROR: can't create output file %s\n", outfile_name); return EXIT_FAILURE;}
    set_binary_mode(infile); set_binary_mode(outfile);
    Install_signal_handler(signal_handler);
    init_stats(infile);
    int result = process (argv[1]);
    if (result==EXIT_SUCCESS)
    {
      print_stats(true);
      if (delete_input_file && !nameequ(outfile_name,STDINOUT_NAME)) {
        fclose(infile);
        remove(infile_name);
      }
    }
    return result;
  }
  else
  {
    printf ("FAZip v0.31 (2013-12-15) - demonstration of FreeArc Unified Compression Library (http://freearc.org)\n"
            "\n"
            "Usage, compression: FAZip method <infile >outfile\n"
            "                 or FAZip method infile outfile\n"
            "                 or FAZip compress:method ... - for raw compressed data\n"
            "       decompression: FAZip d <infile >outfile\n"
            "                   or FAZip d infile outfile\n"
            "                   or FAZip decompress:method ... - for raw compressed data\n"
            "Options:\n"
            "  -delete: delete input file on success\n"
            "  -nocrc: don't store/check checksum (primarily for benchmarking)\n"
            "  -i0: print nothing\n"
            "  -i1: print only error messages\n"
            "  -i2: print only errors and final stats\n"
#ifdef FREEARC_WIN
            "  -slp-: don't alloc large pages (2mb/4mb)\n"
            "  -slp+: alloc only large pages (2mb/4mb)\n"
#endif
            "  -rem...: command-line remark\n"
            "\n"
            "Notes: all FreeArc compression methods are supported, for example rep:512m+delta+tempfile+4x4:lzma:8\n"
            "         as well as cls-*.dll and facompress*.dll\n"
            "       infile/outfile may be specified as \"-\" what means stdin/stdout\n"
           );
    return argc==1? EXIT_SUCCESS : EXIT_FAILURE;
  }
}
