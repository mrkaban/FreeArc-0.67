/*-------------------------------------------------*/
/* GRZipII/libGRZip compressor          libGRZip.c */
/* libGRZip Compression(Decompression) Functions   */
/*-------------------------------------------------*/

/*--
  This file is a part of GRZipII and/or libGRZip, a program
  and library for lossless, block-sorting data compression.

  Copyright (C) 2002-2004 Grebnov Ilya. All rights reserved.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  Lesser General Public License for more details.

  Grebnov Ilya, Ivanovo, Russian Federation.
  Ilya.Grebnov@magicssoft.ru, http://magicssoft.ru/

  This program is based on (at least) the work of:
  Juergen Abel, Jon L. Bentley, Edgar Binder,
  Charles Bloom, Mike Burrows, Andrey Cadach,
  Damien Debin, Sebastian Deorowicz, Peter Fenwick,
  George Plechanov, Michael Schindler, Robert Sedgewick,
  Julian Seward, David Wheeler, Vadim Yoockin.

  BWT compression mode:
    Compression     memory use : [6-9]*BlockLen  + 1Mb   (6 for fast bwt, 9 for strong bwt)
    Decompression   memory use : 5*BlockLen      + 1Mb
  ST4 compression mode:
    Compression     memory use : 5*BlockLen      + 1Mb
    Decompression   memory use : 5.125*BlockLen  + 1Mb

  For more information on these sources, see the manual.
--*/

#include <math.h>
#include <stdlib.h>
#include <string.h>

extern "C" {
#include "C_GRZip.h"
}
#include "../MultiThreading.h"
#include "libGRZip.h"
#include "LZP.c"
#include "BWT.c"
#include "ST4.c"
#include "MTF_Ari.c"
#include "WFC_Ari.c"
#include "Rec_Flt.c"

const sint32 RESERVED = 0;  // неиспользуемые байты в заголовке заполняются этим значением

#ifndef FREEARC_DECOMPRESS_ONLY

sint32 GRZip_StoreBlock(uint8 * Input ,sint32 Size,
                        uint8 * Output,sint32 Mode)
{
  *(sint32 *)(Output+4)=-1;
  *(sint32 *)(Output+8)=DisableAllButLZP(Mode);
  *(sint32 *)(Output+12)=0;
  *(sint32 *)(Output+16)=Size;
  memcpy(Output+28,Input,Size);
  *(sint32 *)(Output+20)=RESERVED;
  *(sint32 *)(Output+24)=RESERVED;
  return (Size+28);
}

sint32 __cdecl GRZip_CompressBlock (uint8* Input, sint32 Size, uint8* Output, sint32 Mode)
{
  sint32 SSize=Size;

  *(sint32 *)Output=Size;

  if ((Size<32)||(Size>GRZ_MaxBlockSize))
    return(GRZip_StoreBlock(Input,Size,Output,0));

  if (Size<1024) Mode|=GRZ_Compression_ST4;

  if ((Size>1024)&&((Mode&GRZ_Disable_DeltaFlt)==0))
  {
    sint32 RecMode=GRZip_Rec_Test(Input,Size);
    if (RecMode)
    {
      sint32 NewSize;
      uint8 * Buffer=(uint8 *)BigAlloc(Size+LZP_MaxMatchLen);
      if (Buffer==NULL) return(GRZip_StoreBlock(Input,Size,Output,0));
      GRZip_Rec_Encode(Input,Size,Buffer,RecMode); Mode+=GRZ_Disable_DeltaFlt;
      if ((RecMode&1)==1)
      {
        sint32 PartSize=(Size>>1);
        sint32 Result=GRZip_CompressBlock(Buffer,PartSize,Output+28,Mode);
        if (Result<0) {BigFree(Buffer);return(GRZip_StoreBlock(Input,Size,Output,0));}
        NewSize=Result;
        Result=GRZip_CompressBlock(Buffer+PartSize,Size-PartSize,Output+28+NewSize,Mode);
        if (Result<0) {BigFree(Buffer);return(GRZip_StoreBlock(Input,Size,Output,0));}
        NewSize+=Result;
      }
      if ((RecMode&1)==0)
      {
        sint32 PartSize=(Size>>2);
        sint32 Result=GRZip_CompressBlock(Buffer,PartSize,Output+28,Mode);
        if (Result<0) {BigFree(Buffer);return(GRZip_StoreBlock(Input,Size,Output,0));}
        NewSize=Result;
        Result=GRZip_CompressBlock(Buffer+PartSize,PartSize,Output+28+NewSize,Mode);
        if (Result<0) {BigFree(Buffer);return(GRZip_StoreBlock(Input,Size,Output,0));}
        NewSize+=Result;
        Result=GRZip_CompressBlock(Buffer+2*PartSize,PartSize,Output+28+NewSize,Mode);
        if (Result<0) {BigFree(Buffer);return(GRZip_StoreBlock(Input,Size,Output,0));}
        NewSize+=Result;
        Result=GRZip_CompressBlock(Buffer+3*PartSize,Size-3*PartSize,Output+28+NewSize,Mode);
        if (Result<0) {BigFree(Buffer);return(GRZip_StoreBlock(Input,Size,Output,0));}
        NewSize+=Result;
      }
      BigFree(Buffer);

      if (NewSize>=Size) return(GRZip_StoreBlock(Input,Size,Output,0));

      *(sint32 *)(Output+4)=-2;
      *(sint32 *)(Output+8)=RecMode;
      *(sint32 *)(Output+16)=NewSize;
      *(sint32 *)(Output+20)=RESERVED;
      *(sint32 *)(Output+24)=RESERVED;

      return (NewSize+28);
    }
  }

  uint8 * LZPBuffer=(uint8 *)BigAlloc(Size+LZP_MaxMatchLen);
  if (LZPBuffer==NULL) return(GRZip_StoreBlock(Input,Size,Output,0));

  if (LZP_Enabled(Mode))
  {
    sint32 Result=GRZip_LZP_Encode(Input,Size,LZPBuffer,Get_LZP_MinMatchLen(Mode),Get_LZP_HT_Size(Mode));
    if (Result==GRZ_NOT_ENOUGH_MEMORY)
    {
      BigFree(LZPBuffer);
      return(GRZip_StoreBlock(Input,Size,Output,0));
    };
    if (Result==GRZ_NOT_COMPRESSIBLE)
    {
      Mode=Disable_LZP(Mode);
      memcpy(LZPBuffer,Input,Size);
      *(sint32 *)(Output+8)=Size;
    }
    else
     { *(sint32 *)(Output+8)=Result,Size=Result;}
  }
  else
  {
    memcpy(LZPBuffer,Input,Size);
    *(sint32 *)(Output+8)=Size;
  }
  sint32 Result;

  for (Result=0;Result<8;Result++) LZPBuffer[Result+Size]=0;
  Size=(Size+7)&(~7);

  if (Mode&GRZ_Compression_ST4)
    Result=GRZip_ST4_Encode(LZPBuffer,Size,LZPBuffer);
  else
    Result=GRZip_BWT_Encode(LZPBuffer,Size,LZPBuffer,Mode&GRZ_BWTSorting_Fast);

  if (Result==GRZ_NOT_ENOUGH_MEMORY)
  {
    if (LZP_Enabled(Mode))
    {
      sint32 Result=GRZip_LZP_Encode(Input,SSize,LZPBuffer,Get_LZP_MinMatchLen(Mode),Get_LZP_HT_Size(Mode));
      if (Result==GRZ_NOT_ENOUGH_MEMORY)
      {
        BigFree(LZPBuffer);
        return(GRZip_StoreBlock(Input,SSize,Output,0));
      };
      Result=GRZip_StoreBlock(LZPBuffer,Result,Output,Mode);
      BigFree(LZPBuffer);
      return (Result);
    }
    BigFree(LZPBuffer);
    return(GRZip_StoreBlock(Input,SSize,Output,0));
  };

  *(sint32 *)(Output+12)=Result;

  if (Mode&GRZ_Compression_MTF)
    Result=GRZip_MTF_Ari_Encode(LZPBuffer,Size,Output+28);
  else
    Result=GRZip_WFC_Ari_Encode(LZPBuffer,Size,Output+28);

  if ((Result==GRZ_NOT_ENOUGH_MEMORY)||(Result==GRZ_NOT_COMPRESSIBLE))
  {
    if (LZP_Enabled(Mode))
    {
      sint32 Result=GRZip_LZP_Encode(Input,SSize,LZPBuffer,Get_LZP_MinMatchLen(Mode),Get_LZP_HT_Size(Mode));
      if (Result==GRZ_NOT_ENOUGH_MEMORY)
      {
        BigFree(LZPBuffer);
        return(GRZip_StoreBlock(Input,SSize,Output,0));
      };
      Result=GRZip_StoreBlock(LZPBuffer,Result,Output,Mode);
      BigFree(LZPBuffer);
      return (Result);
    }
    BigFree(LZPBuffer);
    return(GRZip_StoreBlock(Input,SSize,Output,0));
  };

  *(sint32 *)(Output+4)=Mode;
  *(sint32 *)(Output+16)=Result;
  *(sint32 *)(Output+20)=RESERVED;
  *(sint32 *)(Output+24)=RESERVED;

  BigFree(LZPBuffer);
  return (Result+28);
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

sint32 GRZip_CheckBlockSign(uint8 * Input,sint32 Size)
{
  if (Size<28) return (GRZ_UNEXPECTED_EOF);
  if ((*(sint32 *)(Input+24))!=RESERVED)
    return (GRZ_CRC_ERROR);
  return (GRZ_NO_ERROR);
}

sint32 __cdecl GRZip_DecompressBlock (uint8* Input, sint32 Size, uint8* Output)
{
  if (Size<28) return (GRZ_UNEXPECTED_EOF);
  if ((*(sint32 *)(Input+24))!=RESERVED)
    return (GRZ_CRC_ERROR);
  if ((*(sint32 *)(Input+16))+28>Size) return (GRZ_UNEXPECTED_EOF);
  if ((*(sint32 *)(Input+20))!=RESERVED)
    return (GRZ_CRC_ERROR);
  sint32 Mode=*(sint32 *)(Input+4);
  sint32 Result=*(sint32 *)(Input+16);
  if (Mode==-1)
  {
    Mode=*(sint32 *)(Input+8);
    if (Mode==0)
    {
      memcpy(Output,Input+28,Result);
      return (Result);
    }
    Result=GRZip_LZP_Decode(Input+28,Result,Output,Get_LZP_MinMatchLen(Mode),Get_LZP_HT_Size(Mode));
    return (Result);
  }

  if (Mode==-2)
  {
    sint32 RecMode=*(sint32 *)(Input+8);
              Size=*(sint32 *)(Input);

    uint8 * Buffer=(uint8 *)BigAlloc(Size+LZP_MaxMatchLen);
    if (Buffer==NULL) return(GRZ_NOT_ENOUGH_MEMORY);

    uint8 * Tmp=(Input+28);
    sint32  OutputPos=0;

    if ((RecMode&1)==1)
    {
      Result=GRZip_DecompressBlock(Tmp,(*(sint32 *)(Tmp+16))+28,Buffer+OutputPos);
      if (Result<0) {BigFree(Buffer);return Result;};
      OutputPos+=Result;
      Tmp=Tmp+(*(sint32 *)(Tmp+16))+28;
      Result=GRZip_DecompressBlock(Tmp,(*(sint32 *)(Tmp+16))+28,Buffer+OutputPos);
      if (Result<0) {BigFree(Buffer);return Result;};
    }
    if ((RecMode&1)==0)
    {
      Result=GRZip_DecompressBlock(Tmp,(*(sint32 *)(Tmp+16))+28,Buffer+OutputPos);
      if (Result<0) {BigFree(Buffer);return Result;};
      OutputPos+=Result;
      Tmp=Tmp+(*(sint32 *)(Tmp+16))+28;
      Result=GRZip_DecompressBlock(Tmp,(*(sint32 *)(Tmp+16))+28,Buffer+OutputPos);
      if (Result<0) {BigFree(Buffer);return Result;};
      OutputPos+=Result;
      Tmp=Tmp+(*(sint32 *)(Tmp+16))+28;
      Result=GRZip_DecompressBlock(Tmp,(*(sint32 *)(Tmp+16))+28,Buffer+OutputPos);
      if (Result<0) {BigFree(Buffer);return Result;};
      OutputPos+=Result;
      Tmp=Tmp+(*(sint32 *)(Tmp+16))+28;
      Result=GRZip_DecompressBlock(Tmp,(*(sint32 *)(Tmp+16))+28,Buffer+OutputPos);
      if (Result<0) {BigFree(Buffer);return Result;};
    }
    GRZip_Rec_Decode(Buffer,Size,Output,RecMode);
    BigFree(Buffer);
    return (Size);
  }

  uint8 * LZPBuffer=(uint8 *)BigAlloc(*(sint32 *)(Input+8)+LZP_MaxMatchLen);
  if (LZPBuffer==NULL) return(GRZ_NOT_ENOUGH_MEMORY);

  sint32 TSize;

  if (Mode&GRZ_Compression_MTF)
    TSize=GRZip_MTF_Ari_Decode(Input+28,LZPBuffer);
  else
    TSize=GRZip_WFC_Ari_Decode(Input+28,(*(sint32 *)(Input+8)),LZPBuffer);

  if (Result==GRZ_NOT_ENOUGH_MEMORY)
  {
    BigFree(LZPBuffer);
    return(GRZ_NOT_ENOUGH_MEMORY);
  };

  Result=*(sint32 *)(Input+12);

  if (Mode&GRZ_Compression_ST4)
    Result=GRZip_ST4_Decode(LZPBuffer,TSize,Result);
  else
    Result=GRZip_BWT_Decode(LZPBuffer,TSize,Result);

  if (Result==GRZ_NOT_ENOUGH_MEMORY)
  {
    BigFree(LZPBuffer);
    return(GRZ_NOT_ENOUGH_MEMORY);
  };

  TSize=*(sint32 *)(Input+8);

  if (LZP_Enabled(Mode))
  {
    sint32 Result=GRZip_LZP_Decode(LZPBuffer,TSize,Output,Get_LZP_MinMatchLen(Mode),Get_LZP_HT_Size(Mode));
    if (Result==GRZ_NOT_ENOUGH_MEMORY)
    {
      BigFree(LZPBuffer);
      return(GRZ_NOT_ENOUGH_MEMORY);
    };
  }
  else
    memcpy(Output,LZPBuffer,TSize);

  BigFree(LZPBuffer);
  return (*(sint32 *)Input);
}

#ifndef FREEARC_DECOMPRESS_ONLY

#define ABS_MaxByte      256
#define ABS_MinBlockSize 24*1024

sint32 __cdecl GRZip_GetAdaptiveBlockSize (uint8* Input, sint32 Size)
{
  sint32  TotFreq[ABS_MaxByte];
  sint32     Freq[ABS_MaxByte];

  if (Size<=ABS_MinBlockSize) return Size;

  memset(TotFreq,0,ABS_MaxByte*sizeof(sint32));

  uint8 * SInput=Input;
  uint8 * InputEnd=Input+ABS_MinBlockSize;
  while  (Input<InputEnd) TotFreq[*Input++]++;

  sint32 Pos=ABS_MinBlockSize,BlockSize=ABS_MinBlockSize/2;

  while (Pos+BlockSize<Size)
  {
    memset(Freq,0,ABS_MaxByte*sizeof(sint32));

    sint32 i=0,Sum=BlockSize+(Pos>>1);

    uint8 * Ptr=SInput+Pos;
    uint8 * PtrEnd=Ptr+BlockSize;
    while (Ptr<PtrEnd) Freq[*Ptr++]++;

    double AvgSize=0,RealSize=0;
    for (i=0;i<ABS_MaxByte;i++)
      if (Freq[i])
      {
        sint32 Fr=Freq[i];
        RealSize-=Fr*log10((double)Fr/BlockSize);
        AvgSize-=Fr*log10((double)(Fr+(TotFreq[i]>>1))/Sum);
      }

    if (AvgSize>1.25*RealSize)
       if (BlockSize<256)
         return Pos;
       else
         {BlockSize>>=1;continue;}

    for (i=0;i<ABS_MaxByte;i++) TotFreq[i]+=Freq[i];
    Pos+=BlockSize;
  }
  return Size;
}

#undef ABS_MaxByte
#undef ABS_MinBlockSize


/*-------------------------------------------------*/
/* Multi-threaded GRZip compressor                 */
/*-------------------------------------------------*/
struct GRZipMTCompressor : MTCompressor<>
{
    CALLBACK_FUNC*      callback;                // I/O callback
    void*               auxdata;                 // and its additional parameter

    sint32              Mode;
    int                 BlockSize;               // Size of chunks input split to
    int                 AdaptiveBlockSize;       // использовать переменный размер блока


    // Copy compression parameters into class fields
    GRZipMTCompressor (GRZIP_METHOD *method, CALLBACK_FUNC *_callback, void *_auxdata)
        : MTCompressor<> (method->GetNumThreads(), method->GetNumExtraBuffers())
    {
        callback = _callback;  auxdata = _auxdata;
        switch (method->Method)
        {
            case 1:  Mode = GRZ_Compression_BWT + GRZ_Compression_WFC; break;
            case 2:  Mode = GRZ_Compression_BWT + GRZ_Compression_MTF; break;
            case 3:  Mode = GRZ_Compression_ST4 + GRZ_Compression_WFC; break;
            case 4:  Mode = GRZ_Compression_ST4 + GRZ_Compression_MTF; break;
            default: SetErrCode (FREEARC_ERRCODE_INVALID_COMPRESSOR);        ////
        }
        Mode += method->EnableLZP? Encode_LZP_HT_Size(method->HashSizeLog)+Encode_LZP_MinMatchLen(method->MinMatchLen) : GRZ_Disable_LZP;
        Mode += method->AlternativeBWTSort? GRZ_BWTSorting_Strong : GRZ_BWTSorting_Fast;
        Mode += method->DeltaFilter? GRZ_Enable_DeltaFlt : GRZ_Disable_DeltaFlt;
        BlockSize = mymin (method->BlockSize, GRZ_MaxBlockSize);
    }

    int ReaderThread()
    {
        int errcode = FREEARC_OK;
    	if ((errcode = AllocateBuffers(BlockSize+LZP_MaxMatchLen))  <  0)   return errcode;

        char* RemainderPos; int RemainderSize=0;            // остаток данных с предыдущего раза - адрес и количество
        for(;;)
        {
            CompressionJob *job = FreeJobs.Get();           // Acquire free job record
            if (job == NULL  ||  ErrCode)  break;           // Quit on error in other thread

            job->InBuf = InputBuffers.Get();                // Acquire read buffer
            if (job->InBuf == NULL  ||  ErrCode)  break;    // Quit on error in other thread

            // Перенесём необработанный остаток данных в начало буфера (может даже - того же самого)
            if (RemainderSize>0)  memmove(job->InBuf, RemainderPos, RemainderSize);

            if ( (job->InSize = callback ("read", job->InBuf + RemainderSize, BlockSize - RemainderSize, auxdata)) < 0 )
            	return job->InSize;

            if ((job->InSize+=RemainderSize)==0)  break;    // Данных больше нет
            if (ErrCode)                          break;    // Quit on error in other thread

            RemainderSize=0;
            if (AdaptiveBlockSize)
            {
                // Пошукаем статистику прочитанных данных - может, нет смысла сжимать их общим блоком
                sint32 NewSize = GRZip_GetAdaptiveBlockSize ((uint8*) job->InBuf, job->InSize);
                // Принято решение сжать только первые NewSize байт. Остальное оставим на следующий раз
                RemainderPos  = job->InBuf+NewSize;
                RemainderSize = job->InSize-NewSize;
                job->InSize = NewSize;
            }

            if (ErrCode)                          break;    // Quit on error in other thread
            WorkerJobs.Put(job);
            WriterJobs.Put(job);
        }

        return errcode;
    }


    // Perform one compression operation
    void Process (CompressionJob &job, void* &)
    {
        int res = GRZip_CompressBlock ((uint8*)job.InBuf, job.InSize, (uint8*)job.OutBuf, Mode);
        job.OutSize = (res>=0? res : 0);
        job.result  = (res == GRZ_NOT_ENOUGH_MEMORY? FREEARC_ERRCODE_NOT_ENOUGH_MEMORY :
                       res <  0?                     FREEARC_ERRCODE_GENERAL :
                                                     0);
    }

    // Write output buffer and perform post-processing if required
    void Write (CompressionJob &job)
    {
        int res = callback("write", job.OutBuf, job.OutSize, auxdata);
        if (job.result >= 0)
        {
            job.result = res;
            PROGRESS (job.InSize, job.OutSize);
            //// check result
        }
    }


    sint32 GRZip_GetAdaptiveBlockSize (uint8* Input, sint32 Size)
    {
        // Use faster function from DLL if possible
        static FARPROC f = LoadFromDLL ("GRZip_GetAdaptiveBlockSize");
        if (!f) f = (FARPROC) ::GRZip_GetAdaptiveBlockSize;

        return ((int (__cdecl *)(uint8*, sint32)) f) (Input, Size);
    }

    sint32 GRZip_CompressBlock (uint8* Input, sint32 Size, uint8* Output, sint32 Mode)
    {
        // Use faster function from DLL if possible
        static FARPROC f = LoadFromDLL ("GRZip_CompressBlock");
        if (!f) f = (FARPROC) ::GRZip_CompressBlock;

        return ((int (__cdecl *)(uint8*, sint32, uint8*, sint32)) f) (Input, Size, Output, Mode);
    }
};


#endif  // !defined (FREEARC_DECOMPRESS_ONLY)


/*-------------------------------------------------*/
/* Multi-threaded GRZip decompressor               */
/*-------------------------------------------------*/
struct GRZipMTDecompressor : MTCompressor<>
{
    CALLBACK_FUNC*  callback;    // I/O callback
    void*           auxdata;     // and its additional parameter
    int             BlockSize;   // Size of chunks input split to

    // Copy compression parameters into class fields
    GRZipMTDecompressor (GRZIP_METHOD *method, CALLBACK_FUNC *_callback, void *_auxdata)
        : MTCompressor<> (method->GetNumThreads(), method->GetNumExtraBuffers())
    {
        callback = _callback;  auxdata = _auxdata;
        BlockSize = mymin (method->BlockSize, GRZ_MaxBlockSize);
    }

    int ReaderThread()
    {
        int errcode = FREEARC_OK;
    	if ((errcode = AllocateBuffers(BlockSize+LZP_MaxMatchLen))  <  0)   return errcode;

        uint8 BlockSign[28];
        for(;;)
        {
            sint32 NumRead=callback("read",BlockSign,28,auxdata);
            if (NumRead==0)                                          break;    // Конец данных
            if (NumRead!=28)                                         return NumRead<0? NumRead:FREEARC_ERRCODE_BAD_COMPRESSED_DATA;
            if (GRZip_CheckBlockSign(BlockSign,28)!=GRZ_NO_ERROR)    return FREEARC_ERRCODE_BAD_COMPRESSED_DATA;

            CompressionJob *job = FreeJobs.Get();                              // Acquire free job record
            if (job == NULL  ||  ErrCode)                            break;    // Quit on error in other thread

            job->InBuf = InputBuffers.Get();                                   // Acquire read buffer
            if (job->InBuf == NULL  ||  ErrCode)                     break;    // Quit on error in other thread

            memcpy(job->InBuf,BlockSign,28);

            job->InSize = callback ("read", job->InBuf+28, *(sint32 *)(job->InBuf+16), auxdata);
            if (job->InSize != *(sint32 *)(job->InBuf+16))           return job->InSize<0? job->InSize : FREEARC_ERRCODE_BAD_COMPRESSED_DATA;

            if (ErrCode)                                             break;    // Quit on error in other thread
            WorkerJobs.Put(job);
            WriterJobs.Put(job);
        }

        return errcode;
    }


    // Perform one decompression operation
    void Process (CompressionJob &job, void* &)
    {
        int res = GRZip_DecompressBlock ((uint8*)job.InBuf, job.InSize+28, (uint8*)job.OutBuf);
        job.OutSize = (res>=0? res : 0);
        job.result  = (res == GRZ_NOT_ENOUGH_MEMORY? FREEARC_ERRCODE_NOT_ENOUGH_MEMORY :
                       res <  0?                     FREEARC_ERRCODE_GENERAL :
                                                     0);
    }

    // Write output buffer and perform post-processing if required
    void Write (CompressionJob &job)
    {
        int res = callback("write", job.OutBuf, job.OutSize, auxdata);
        if (job.result >= 0)
        {
            job.result = res;
            PROGRESS (job.InSize+28, job.OutSize);
            //// check result
        }
    }


    sint32 GRZip_DecompressBlock (uint8* Input, sint32 Size, uint8* Output)
    {
        // Use faster function from DLL if possible
        static FARPROC f = LoadFromDLL ("GRZip_DecompressBlock");
        if (!f) f = (FARPROC) ::GRZip_DecompressBlock;

        return ((int (__cdecl *)(uint8*, sint32, uint8*)) f) (Input, Size, Output);
    }
};



/*-------------------------------------------------*/
/* Реализация класса GRZIP_METHOD                  */
/*-------------------------------------------------*/
// Конструктор, присваивающий параметрам метода сжатия значения по умолчанию
GRZIP_METHOD::GRZIP_METHOD()
{
  Method              =  1;
  BlockSize           =  8*mb;
  EnableLZP           =  1;
  MinMatchLen         =  32;
  HashSizeLog         =  15;
  AlternativeBWTSort  =  0;
  AdaptiveBlockSize   =  0;
  DeltaFilter         =  0;
  NumThreads          =  0;
  NumExtraBuffers     = -1;
}

// Универсальный метод, отвечает на запрос "has_progress?"
int GRZIP_METHOD::doit (char *what, int param, void *data, CALLBACK_FUNC *callback)
{
  if (strequ (what, "has_progress?"))  return 1;                                                       // Да, этот алгоритм поддерживает отчёт о прогрессе упаковки
  else                                 return COMPRESSION_METHOD::doit (what, param, data, callback);  // Передать остальные вызовы родительской процедуре
}

// Функция распаковки
int GRZIP_METHOD::decompress (CALLBACK_FUNC *callback, void *auxdata)
{
  GRZipMTDecompressor Decompressor (this, callback, auxdata);
  return Decompressor.Run();
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Функция упаковки
int GRZIP_METHOD::compress (CALLBACK_FUNC *callback, void *auxdata)
{
  GRZipMTCompressor Compressor (this, callback, auxdata);
  return Compressor.Run();
}

// Установить размер блока и уменьшить размер хэша, если он слишком велик для такого маленького блока
void GRZIP_METHOD::SetBlockSize (MemSize bs)
{
  if (bs>0) {
    BlockSize   = mymin (bs, GRZ_MaxBlockSize);
    HashSizeLog = mymin (HashSizeLog, 1+lb(BlockSize-1));
  }
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)


// Get/Set amount of memory used for compression/decompression
MemSize GRZIP_METHOD::GetSetDeCompressionMem (COMPRESSION direction, MemSize mem, bool MINMEM)
{
  int         t      =  MINMEM? 1 : GetNumThreads();
  int         i      =  MINMEM? 0 : GetNumExtraBuffers();
  bool        bwt    =  Method<3;                                                   // TRUE for BWT compression, FALSE for ST4 compression
  double      koeff  =  direction==COMPRESS? (bwt?11:7):(bwt?7:7.125);              // MemoryUsage/BlockSize ratio, including 2 for InBuf+OutBuf
  LongMemSize lzpmem =  direction==COMPRESS? (1<<HashSizeLog)*sizeof(uint32) : 0;   // Memory occupied by LZP hash
  LongMemSize tmem   =  LongMemSize(koeff*BlockSize) + lzpmem + 1*mb;               // Сколько памяти требуется для одного треда упаковки/распаковки
  LongMemSize imem   =  2*BlockSize;                                                // Сколько памяти требуется для одного набора доп. буферов I/O

  if (mem==0)  return mymin(t*tmem+i*imem, MEMSIZE_MAX);   // Either Get(De)compressionMem() or Set(De)compressionMem(0) was called


  // Если дошли досюда - значит выполняем одну из операций SetXxxMem. В зависимости от конкретной операции мы можем менять следующие параметры метода:
  // SetCompressionMem        :t:i и BlockSize
  // SetMinCompressionMem     BlockSize
  // SetDecompressionMem      :t:i
  // SetMinDecompressionMem   BlockSize

  // Посчитаем количество тредов и буферов, которое мы можем себе позволить
  if (mem >= t*tmem+i*imem) {
    // Памяти достаточно - "расширим" алгоритм сжатия
    //::SetDeCompressionMem (Method, mem/GetNumBuffers(), Method);

  } else if (mem >= t*tmem && !MINMEM) {
    // Памяти достаточно для всех тредов сжатия, уменьшаем число дополнительных буферов
    NumExtraBuffers = (mem-t*tmem)/imem;

  } else if (mem >= tmem && !MINMEM) {
    // Памяти достаточно хотя бы для одного треда сжатия - уменьшаем число тредов сжатия и при необходимости число дополнительных буферов
    NumThreads = t = mem/tmem;
    int new_i = (mem-t*tmem)/imem;
    if (new_i<i)  NumExtraBuffers = new_i;

  } else {
    // Памяти не хватило даже для одного треда сжатия - оставим только один и ещё подожмём его

    // Урезаем число тредов в Set(De)CompressionMem
    if (!MINMEM) {
      NumThreads      = 1;
      NumExtraBuffers = 0;
    }
    // Урезаем BlockSize в любой операции, кроме SetDecompressionMem
    if (direction==COMPRESS || MINMEM) {
      BlockSize       = MemSize (mymin (mymax(double(mem)-1*mb,1*mb)/koeff, GRZ_MaxBlockSize));
      HashSizeLog     = 15;
    }
  }
  return 0;
}


// Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_GRZIP)
void GRZIP_METHOD::ShowCompressionMethod (char *buf, bool purify)
{
  char LZP_Str[100], BlockSizeStr[100], ThreadsStr[100], ExtraBuffersStr[100];
  sprintf (LZP_Str, "l%d:h%d", MinMatchLen, HashSizeLog);
  showMem (BlockSize, BlockSizeStr);
  sprintf (ThreadsStr,      ":t%d", NumThreads);
  sprintf (ExtraBuffersStr, ":i%d", NumExtraBuffers);
  sprintf (buf, "grzip:%s:m%d:%s%s%s%s%s%s", BlockSizeStr,
                                             Method,
                                             EnableLZP?          LZP_Str : "l",
                                             AlternativeBWTSort? ":s" : "",
                                             AdaptiveBlockSize?  ":a" : "",
                                             DeltaFilter?        ":d" : "",
                                             !purify && NumThreads>0?        ThreadsStr     :"",
                                             !purify && NumExtraBuffers>=0?  ExtraBuffersStr:"");
}

// Конструирует объект типа GRZIP_METHOD с заданными параметрами упаковки
// или возвращает NULL, если это другой метод сжатия или допущена ошибка при задании параметров
COMPRESSION_METHOD* parse_GRZIP (char** parameters)
{
  if (strcmp (parameters[0], "grzip") == 0) {
    // Если название метода (нулевой параметр) - "grzip", то разберём остальные параметры

    GRZIP_METHOD *p = new GRZIP_METHOD;
    int error = 0;  // Признак того, что при разборе параметров произошла ошибка

    while (!error && *++parameters)  // Переберём все параметры метода
    {
      char *param = *parameters;
      if (strlen(param)==1) switch (*param) {    // Однобуквенные параметры
        case 's':  p->AlternativeBWTSort  = 1; continue;
        case 'a':  p->AdaptiveBlockSize   = 1; continue;
        case 'l':  p->EnableLZP           = 0; continue;
        case 'd':  p->DeltaFilter         = 1; continue;
        case 'p':  p->AdaptiveBlockSize=0; p->EnableLZP=0; p->DeltaFilter=1; continue;
      }
      else switch (*param) {                    // Параметры, содержащие значения
        case 'm':  p->Method           =  parseInt (param+1, &error); continue;
        case 'b':  p->BlockSize        =  parseMem (param+1, &error); continue;
        case 'l':  p->MinMatchLen      =  parseInt (param+1, &error); continue;
        case 'h':  p->HashSizeLog      =  parseInt (param+1, &error); continue;
        case 't':  p->NumThreads       =  parseInt (param+1, &error); continue;
        case 'i':  p->NumExtraBuffers  =  parseInt (param+1, &error); continue;
      }
      // Сюда мы попадаем, если в параметре не указано его название
      // Если этот параметр удастся разобрать как целое число (т.е. в нём - только цифры),
      // то присвоим его значение полю MinMatchLen, иначе попробуем разобрать его как BlockSize
      int n = parseInt (param, &error);
      if (!error) p->MinMatchLen = n;
      else        error=0, p->BlockSize = parseMem (param, &error);
    }
    if (error)  {delete p; return NULL;}  // Ошибка при парсинге параметров метода
    return p;
  } else
    return NULL;   // Это не метод grzip
}

static int GRZIP_x = AddCompressionMethod (parse_GRZIP);   // Зарегистрируем парсер метода GRZIP

/*-------------------------------------------------*/
/* End                                  libGRZip.c */
/*-------------------------------------------------*/
