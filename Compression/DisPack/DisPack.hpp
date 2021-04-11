#ifndef __DIS_HPP__
#define __DIS_HPP__

#include "../Common.h"

#pragma warning (disable : 4244)    // 'double' to 'float'
//#pragma warning (disable : 4018)  // signed/unsigned mismatch

typedef uint8             sU8;
typedef int8              sS8;
typedef uint16            sU16;
typedef int16             sS16;
typedef uint32            sU32;
typedef int32             sS32;
typedef uint64            sU64;
typedef int64             sS64;
typedef int               sInt;
typedef char              sChar;
typedef bool              sBool;
typedef float             sF32;
typedef double            sF64;

#define sTRUE             true
#define sFALSE            false


/****************************************************************************/

// Takes "code" as input ("size" bytes), assuming that the first byte will
// be loaded to address "origin" (preferred load address). Returns the
// transformed instruction stream as return value and its size as
// outputSize.
sU8 *DisFilter(sU8 *code,sInt size,sU32 origin,sU32 &outputSize);

// Undo the transform.
sBool DisUnFilter(sU8 *source,sU32 sourceSize,sU8 *dest,sU32 destSize,sU32 memory);

/****************************************************************************/

#endif
