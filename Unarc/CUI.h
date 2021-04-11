class CUI : public BASEUI
{
private:
  char outdir[MY_FILENAME_MAX*4];  //unicode: utf-8 encoding
  uint64 total_files, total_bytes, total_packed;
public:
  void DisplayHeader (char* header);
  bool ProgressFile  (bool isdir, const char *operation, FILENAME filename, uint64 filesize);
  void EndProgress   ();
  bool AllowProcessing (char cmd, int silent, FILENAME arcname, char* comment, int cmtsize, FILENAME outdir);
  FILENAME GetOutDir();
  char AskOverwrite (FILENAME filename, uint64 size, time_t modified);
  char AskPassword  (char *pwd, int pwdbuf_size);

  void ListHeader (COMMAND &);
  void ListFooter (COMMAND &);
  void ListFiles (DIRECTORY_BLOCK *, COMMAND &);
};

void CUI::DisplayHeader (char* header)
{
  printf ("%s", header);
}

bool CUI::ProgressFile (bool isdir, const char *operation, FILENAME filename, uint64 filesize)
{
  printf (isdir?  "%s %s" STR_PATH_DELIMITER "\n"  :  "%s %s (%.0lf bytes)\n",
          operation, filename, double(filesize));
  return TRUE;
}

void CUI::EndProgress()
{
  printf ("All OK");
}

FILENAME CUI::GetOutDir()
{
  return outdir;
}

bool CUI::AllowProcessing (char cmd, int silent, FILENAME arcname, char* comment, int cmtsize, FILENAME _outdir)
{
  strcpy (outdir, _outdir);
  printf (". %s archive: %s\n",                       // ������� ��� ��������������� ������
    cmd=='l'||cmd=='v'? "Listing" : cmd=='t' ? "Testing" : "Extracting", drop_dirname(arcname));
  if (cmtsize>0)                                      // ������� �������� �����������
#ifdef FREEARC_WIN
{
    // Convert comment from UTF-8 to OEM encoding before printing
    char *oemname = (char*) malloc(cmtsize+1);
    strncpy (oemname, comment, cmtsize);
    oemname[cmtsize] = 0;
    utf8_to_oem (oemname, oemname);
    printf ("%s\n", oemname);
    free (oemname);
}
#else
    printf("%*.*s\n", cmtsize, cmtsize, comment);
#endif

#ifdef FREEARC_SFX
  // � SFX ���������� ��������� �������� ������������ ����� ������� ����������
  if (!silent)
  {
    char answer[256];
    printf ("Continue extraction (y/n)? ");
    gets (answer);
    if (! (strequ(answer,"y") || strequ(answer,"Y")))
    {
      printf ("Extraction aborted!\n");
      return FALSE;
    }
    printf("\n");
  }
#endif
  return TRUE;
}

char CUI::AskOverwrite (FILENAME filename, uint64 size, time_t modified)
{
  char help[] = "Valid answers: Y - yes, N - no, A - overwrite all, S - skip all, Q - quit\n";
  again: printf ("Overwrite %s ?\n(Y)es / (N)o / (A)lways / (S)kip all / (Q)uit? ", filename);
  char answer[256];  gets (answer);  *answer = tolower(*answer);
  if (strlen(answer)!=1 || !strchr("ynasq", *answer))  {printf (help);  goto again;}
  if (*answer=='q') {printf ("Extraction aborted\n");  exit(1);}
  return *answer;
}

char CUI::AskPassword  (char *pwd, int pwdbuf_size)
{
  printf("Enter password: ");
  gets (pwd);
  return 'y';
}


/******************************************************************************
** ���������� ������� ��������� �������� ������ *******************************
******************************************************************************/
void CUI::ListHeader (COMMAND &command)
{
  if (command.cmd=='l')
      printf ("Date/time                  Size Filename\n"
              "----------------------------------------\n");
  else
      printf ("Date/time              Attr            Size          Packed      CRC Filename\n"
              "-----------------------------------------------------------------------------\n");
  total_files=total_bytes=total_packed=0;
}

void CUI::ListFooter (COMMAND &command)
{
  if (command.cmd=='l')
      printf ("----------------------------------------\n");
  else
      printf ("-----------------------------------------------------------------------------\n");
  printf ("%.0lf files, %.0lf bytes, %.0lf compressed\n", double(total_files), double(total_bytes), double(total_packed));
}

void CUI::ListFiles (DIRECTORY_BLOCK *dirblock, COMMAND &command)
{
  int  b=0;                // current_data_block
  bool Encrypted = FALSE;  // ������� �����-���� ����������?
  uint64 packed=0;
  iterate_var (i, dirblock->total_files) {
    // �������� ����� �����-����� ���� �� ����� �� ��������� ������������� ��� ����
    if (i >= dirblock->block_end(b))
      b++;
    // ���� ��� ������ ���� � �����-����� - ������ block-related ����������
    if (i == dirblock->block_start(b))
    { // ������� �� ������ ���� � ����� ���� ��� ����������� ������
      packed = dirblock->data_block[b].compsize;
      // �������� ���������� � �����-����� ��� ������������� � �� ����� ������� �� ����� �����-�����
      char *c = dirblock->data_block[b].compressor;
      Encrypted = strstr (c, "+aes-")!=NULL || strstr (c, "+serpent-")!=NULL || strstr (c, "+blowfish-")!=NULL || strstr (c, "+twofish-")!=NULL;
    }


    if (command.accept_file (dirblock, i)) { //   ���� ���� ���� ��������� ����������
      unsigned long long filesize = dirblock->size[i];
      char timestr[100];  FormatDateTime (timestr, 100, dirblock->time[i]);

      if (command.cmd=='l')
          printf (dirblock->isdir[i]? "%s       -dir-" : "%s %11.0lf", timestr, double(filesize));
      else
          printf ("%s %s %15.0lf %15.0lf %08x", timestr, dirblock->isdir[i]? ".D.....":".......", double(filesize), double(packed), dirblock->crc[i]);
      printf ("%c", Encrypted? '*':' ');

      // Print filename using console encoding
      static char filename[MY_FILENAME_MAX*4];
      dirblock->fullname (i, filename);
      static MYFILE file;  file.setname (filename);
      printf ("%s\n", file.displayname());

      total_files++;
      total_bytes  += filesize;
      total_packed += packed;    packed = 0;
    }
  }
}
