/****************************************************************************
 *  This file is part of PPMd project                                       *
 *  Written and distributed to public domain by Dmitry Shkarin 1997,        *
 *  1999-2001                                                               *
 *  Contents: compilation parameters and miscelaneous definitions           *
 *  Comments: system & compiler dependent file                              *
 ****************************************************************************/
#if !defined(_PPMDTYPE_H_)
#define _PPMDTYPE_H_

#include <stdio.h>

#ifdef FREEARC_WIN
#include <windows.h>
#else
typedef int   BOOL;
#define FALSE 0
#define TRUE  1
typedef unsigned char  BYTE;
typedef unsigned short WORD;
typedef unsigned int   DWORD;
typedef unsigned int   UINT;
#endif

const DWORD PPMdSignature=0x84ACAF8F, Variant='I';
const int MIN_O=2;                           /* minimum allowed model order  */
const int MAX_O=128;                         /* maximum allowed model order  */
const int MIN_MEM=2048;                      /* minimum allowed memory usage */

#define _USE_PREFETCHING                    /* for puzzling mainly          */

#if !defined(_UNKNOWN_ENVIRONMENT_) && !defined(__GNUC__)
#define _FASTCALL __fastcall
#define _STDCALL  __stdcall
#else
#define _FASTCALL
#define _STDCALL
#endif /* !defined(_UNKNOWN_ENVIRONMENT_) && !defined(__GNUC__) */

#if defined(__GNUC__)
#define _PACK_ATTR __attribute__ ((packed))
#else /* "#pragma pack" is used for other compilers */
#define _PACK_ATTR
#endif /* defined(__GNUC__) */

/* PPMd module works with file streams via ...GETC/...PUTC macros only      */
#include "../Compression.h"
class PRIME_STREAM {
public:
    PRIME_STREAM (CALLBACK_FUNC *_callback, void *_auxdata, int _bufsize): Error(0), StrPos(0), Count(0) {
      callback = _callback;  auxdata = _auxdata;  BufSize = _bufsize;
      p = Buf = (BYTE*) BigAlloc (BufSize);
      if (!p)  Error = FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
    }
    void free()     { BigFree(Buf); Buf=NULL; }
    ~PRIME_STREAM() { free(); }

    int  get(     ) { return (--Count >= 0)?(*p++    ):(fill(),  (--Count >= 0)?(*p++):EOF); }
    int  put(int c) { return (--Count >= 0)?(*p++ = c):(flush(), (--Count >= 0)?(*p++ = c):EOF); }
    int  getErr() const { return Error; }
    int    tell() const { return StrPos+(p-Buf); }
    BOOL  atEOS() const { return (Count < 0); }
    void flush() {                            // it must remove (p-Buf) bytes
      if(Error>=0 && p>Buf)  { int i = callback ("write", Buf, p-Buf, auxdata); if(i<0) Error=i; }
      p = Buf;
      Count = BufSize;
    }
    void  fill() {                             // it must fill Buf[]
      if (Error < 0)                      { Count=0; return; }
      Count = callback ("read", Buf, BufSize, auxdata);
      p = Buf;
      if (Count < 0) { Error=Count; Count=0; }
    }
protected:
    CALLBACK_FUNC *callback;  void *auxdata;
    BYTE *p, *Buf;
    int BufSize, Error, StrPos, Count;
};
typedef PRIME_STREAM _PPMD_FILE;
#define _PPMD_E_GETC(pps)   (pps)->get()
#define _PPMD_E_PUTC(c,pps) (pps)->put(c)
#define _PPMD_D_GETC(pps)   (pps)->get()
#define _PPMD_D_PUTC(c,pps) (pps)->put(c)
#define _PPMD_ERROR_CODE(pps) (pps)->getErr()

#endif /* !defined(_PPMDTYPE_H_) */
