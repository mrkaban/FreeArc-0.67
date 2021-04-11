/*
  FreeArc.CPP

  Second-level plugin module for FAR Manager 1.70 and MultiArc plugin

  Copyright (c) 1996-2000 Eugene Roshal
  Copyrigth (c) 2000-2002 FAR group
  Copyleft  (l) 2005-2008 Bulat Ziganshin

  TO DO: ����� ������/������ ������ ���������� �� ���
         join_name - ���������, ��� �� �� ������� �� ������� ������
*/
/* Revision: 22.06.2008 $ */

#include <windows.h>
#include <string.h>
#include <dos.h>
#include <io.h>
#include <fcntl.h>
#include <stdio.h>
#include <time.h>
#include <setjmp.h>
#include <malloc.h>
#define _FAR_USE_FARFINDDATA
#include "plugin.hpp"
#include "fmt.hpp"

// ��� ��������� ������ �� ��������� ���������� - longjmp ������������� ��������� �������� ������ � ������������ ������
static jmp_buf jumper;
#define CHECK(e,a,b)             {if(!(a)) longjmp(jumper,1);}
#define FreeAndNil(ptr)          {delete (ptr); (ptr)=NULL;}
#include "ArcStructure.h"

/***********************************************************************
 *** �������� ��������� ************************************************
 ***********************************************************************
*/

static ARCHIVE *arcinfo;
static DIRECTORY_BLOCK *dirblock;
static int current_block, current_file_in_block, current_data_block;
static FILESIZE SFXSize;
// Info that is computed once [per solid block] and then returned for each file [in the solid block]
static int Solid;       // "Solid" flag
static int Encrypted;   // Set if file is encrypted
static int DictSize;    // Dictionary size or 0 if unknown
static int UnpVer;      // Version to unpack (HighNumber*256+LowNumber) or 0 if unknown


// Close archive file and release memory. Used after archive reading and on errors
void cleanup()
{
  FreeAndNil (dirblock);
  FreeAndNil (arcinfo);
}

BOOL WINAPI _export IsArchive(const char *Name,const unsigned char *Data,int DataSize)
{
  // ����� � ������ ���������, � ����� �� - ������ ����������
  for( int I=0; I <= (int)(DataSize-20); I++ )
  {
    if (*(uint32*)(Data+I) == aSIGNATURE)
    {
      UnpVer=(Data[I+4]*10+Data[I+5])*256 + Data[I+6]*10+Data[I+7];  // ������ ���������, ��������� ��� ���������� ������
      SFXSize = I;
      return TRUE;
    }
  }
  return FALSE;
}

BOOL WINAPI _export OpenArchive(const char *Name,int *Type)
{
  if (setjmp(jumper) != 0)
    {cleanup(); return FALSE;}  // ���� �� ������ ��� ������������� ������ � ����� �� ���������� ��������

  // OEM -> UTF8
  char utf8name[MY_FILENAME_MAX*4];
  oem_to_utf8 (Name, utf8name);

  // ��������� ��������� ������
  arcinfo = new ARCHIVE (FILENAME (utf8name));
  arcinfo->read_structure (NULL, NULL);
  *Type   = 0;

  // ������� ���, ����� ��������� ����� GetArcItem ����� � ������ ������� �����
  current_block=-1; dirblock=NULL;
  return TRUE;
}

int WINAPI _export GetArcItem(struct PluginPanelItem *Item,struct ArcItemInfo *Info)
{
  if (setjmp(jumper) != 0)
    {cleanup(); return GETARC_BROKEN;}  // ���� �� ������ ��� ������������� ������ � ����� �� ���������� ��������

  // ������� ��������� ���� �������� ������, ���� ��� ����� �� �������� ��� �����������
  if( current_block < 0 || ++current_file_in_block >= dirblock->total_files)
  {
    FreeAndNil (dirblock);
    for(;;)
    {
      if( ++current_block == arcinfo->control_blocks_descriptors.size )
      {
        return GETARC_EOF;
      }

      // ���� ��� ���� �������� - ��������� ��� � ������ �� �����
      BLOCK& descriptor = arcinfo->control_blocks_descriptors [current_block];
      if (descriptor.type == DIR_BLOCK)
      {
         dirblock = new DIRECTORY_BLOCK (*arcinfo, descriptor, NULL, NULL);
         current_file_in_block = current_data_block = 0;
         if (dirblock->total_files>0)  break;
         FreeAndNil (dirblock);
      }
    }
    //printf("%d files\n", dirblock->total_files);
  }

  // �������� �������� �����
  int i = current_file_in_block;
  Item->FindData.dwFileAttributes = dirblock->isdir[i]? FILE_ATTRIBUTE_DIRECTORY : 0;
  UnixTimeToFileTime (dirblock->time[i], &Item->FindData.ftLastWriteTime);
  Item->FindData.nFileSizeHigh = ((uint64) dirblock->size[i]) >> 32;
  Item->FindData.nFileSizeLow  = dirblock->size[i];
  char utf8name[MY_FILENAME_MAX*4]; WCHAR utf16name[MY_FILENAME_MAX*2];
  dirblock->fullname (i, utf8name);
  utf8_to_utf16 (utf8name, utf16name);
  CharToOemW (utf16name, Item->FindData.cFileName);
  Item->CRC32  = dirblock->crc[i];
  Info->UnpVer = UnpVer;

  // ������ �������� ���������� �� �������� �����-�����
  int &b = current_data_block;
  // �������� ����� �����-����� ���� �� ����� �� ��������� ������������� ��� ����
  if (current_file_in_block >= dirblock->block_end(b))
    b++;
  // ���� ��� ������ ���� � �����-����� - ������ block-related ����������
  if (current_file_in_block == dirblock->block_start(b))
  { // ������� �� ������ ���� � ����� ���� ��� ����������� ������
    uint64 packed = dirblock->data_block[b].compsize;
    Item->PackSizeHigh = packed >> 32;
    Item->PackSize     = packed;
    // �������� ���������� � �����-����� ��� ������������� � �� ����� ������� �� ����� �����-�����
    Solid     = dirblock->block_start(b)+1 != dirblock->block_end(b);
    Encrypted = compressorIsEncrypted_Guess   (dirblock->data_block[b].compressor);
    DictSize  = compressorGetDecompressionMem (dirblock->data_block[b].compressor);
  }
  // �������� ���� �����������, ��������� ��� ��������� ������� ����� � �����-�����
  Info->Solid     = Solid;
  Info->Encrypted = Encrypted;
  Info->DictSize  = DictSize;

  return GETARC_SUCCESS;
}

BOOL WINAPI _export CloseArchive(struct ArcInfo *Info)
{
  if (setjmp(jumper) != 0)
    {cleanup(); return FALSE;}  // ���� �� ������ ��� ������������� ������ � ����� �� ���������� ��������

  Info->SFXSize = SFXSize;
  Info->Comment = arcinfo->arcComment.size>0;
  Info->Lock    = arcinfo->arcLocked;
  iterate_array (i, arcinfo->control_blocks_descriptors) {
    Info->Recovery  |=  arcinfo->control_blocks_descriptors[i].type==RECOVERY_BLOCK;
  }
  cleanup();
  return TRUE;
}

DWORD WINAPI _export GetSFXPos(void)
{
  return SFXSize;
}


BOOL WINAPI _export GetFormatName(int Type,char *FormatName,char *DefaultExt)
{
  if (Type==0)
  {
    strcpy(FormatName, "FreeArc");
    strcpy(DefaultExt, FREEARC_FILE_EXTENSION);
    return(TRUE);
  }
  return(FALSE);
}


BOOL WINAPI _export GetDefaultCommands(int Type,int Command,char *Dest)
{
  if (Type==0)
  {
    static char *Commands[]={
    /*Extract               */"arc x --noarcext -y -fn {-p%%P} -kb  {-ap%%R} {%%S} -- %%A @%%LNM",
    /*Extract without paths */"arc e --noarcext -y -fn {-p%%P} -kb           {%%S} -- %%A @%%LNM",
    /*Test                  */"arc t --noarcext -y -fn {-p%%P}               {%%S} -- %%A @%%LNM",
    /*Delete                */"arc d --noarcext -y -fn {-p%%P} {-w%%W}       {%%S} -- %%A @%%LNM",
    /*Comment archive       */"arc c --noarcext -y     {-p%%P} {-w%%W}       {%%S} -- %%A",
    /*Comment files         */"",
    /*Convert to SFX        */"arc s  --noarcext -y {-p%%P} {-w%%W}          {%%S} -- %%A",
    /*Lock archive          */"arc k  --noarcext -y {-p%%P} {-w%%W}          {%%S} -- %%A",
    /*Protect archive       */"arc rr --noarcext -y {-p%%P} {-w%%W}          {%%S} -- %%A",
    /*Recover archive       */"arc r  --noarcext -y {-p%%P} {-w%%W}          {%%S} -- %%A",
    /*Add files             */"arc a  --noarcext -y {-p%%P} {-w%%W} {-ap%%R} {%%S} -- %%A @%%LN",
    /*Move files            */"arc mf --noarcext -y {-p%%P} {-w%%W} {-ap%%R} {%%S} -- %%A @%%LN",
    /*Add files and folders */"arc a  --noarcext -y {-p%%P} {-w%%W} {-ap%%R} {%%S} -- %%A @%%LN",
    /*Move files and folders*/"arc m  --noarcext -y {-p%%P} {-w%%W} {-ap%%R} {%%S} -- %%A @%%LN",
    /*"All files" mask      */"*"
    };
    if (Command < sizeof(Commands)/sizeof(Commands[0]))
    {
      strcpy(Dest,Commands[Command]);
      return(TRUE);
    }
  }
  return(FALSE);
}
