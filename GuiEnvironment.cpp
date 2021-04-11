#include <stdio.h>
#include <sys/stat.h>
#include <utime.h>
#include <limits.h>
#include <memory.h>
#include "Environment.h"
#include "Compression/Compression.h"

#ifdef FREEARC_WIN

#include <shlobj.h>

static int CALLBACK BrowseCallbackProc(HWND hwnd, UINT uMsg, LPARAM lParam, LPARAM lpData)
{
  if(uMsg == BFFM_INITIALIZED)
    PostMessage(hwnd, BFFM_SETSELECTION, TRUE, lpData);

  return 0;
}

// Дать пользователю выбрать каталог
int BrowseForFolder(TCHAR *prompt, TCHAR *in_filename, TCHAR *out_filename)
{
  BROWSEINFO bi;
  bi.hwndOwner = GetActiveWindow();
  bi.lParam = (LONG)in_filename;
  bi.lpszTitle = prompt;
  bi.lpfn = BrowseCallbackProc;
  bi.pidlRoot = NULL;
  bi.pszDisplayName = out_filename;
  bi.ulFlags = BIF_RETURNONLYFSDIRS | BIF_USENEWUI;

  LPITEMIDLIST pItemIdList = SHBrowseForFolder(&bi);

  int result = 0;
  if(pItemIdList != NULL)
  {
    if (SHGetPathFromIDList(pItemIdList, out_filename))
      result = 1;

    IMalloc *iMalloc = 0;
    if(SUCCEEDED(SHGetMalloc(&iMalloc)))
    {
      iMalloc->Free(pItemIdList);
      iMalloc->Release();
    }
  }
  return result;
}


// Дать пользователю выбрать файл
int BrowseForFile(TCHAR *prompt, TCHAR *filters, TCHAR *in_filename, TCHAR *out_filename)
{
  OPENFILENAME ofn;
  ZeroMemory (&ofn, sizeof(ofn));
  ofn.lStructSize = sizeof(ofn);
  ofn.hwndOwner   = GetActiveWindow();
  ofn.lpstrFilter = filters;
  ofn.lpstrFile   = out_filename;
  ofn.nMaxFile    = MY_FILENAME_MAX*2;
  ofn.lpstrTitle  = prompt;

  // If "initial filename" looks like "something\." or "something\", it's just initial dir, really
  if (in_filename[_tcslen(in_filename)-1] == _T('.')  &&
      in_filename[_tcslen(in_filename)-2] == _T('\\'))
  {
    in_filename[_tcslen(in_filename)-1] == _T('\0');
    ofn.lpstrInitialDir = in_filename;
    _tcscpy (out_filename, _T(""));
  }
  else if (in_filename[_tcslen(in_filename)-1] == _T('\\'))
  {
    ofn.lpstrInitialDir = in_filename;
    _tcscpy (out_filename, _T(""));
  }
  else
  {
    _tcscpy (out_filename, in_filename);
  }

  return GetOpenFileName(&ofn)? 1 : 0;
}

// Превратить время/дату файла в строку в соответствии с настройками locale или заданными форматами времени и даты
void GuiFormatDateTime (time_t t, char *buf, int bufsize, char *date_format, char *time_format)
{
  if (t<0)  t=INT_MAX;  // Иначе получаем вылет :(

  FILETIME ft1, ft2;
  UnixTimeToFileTime (t, &ft1);
  FileTimeToLocalFileTime (&ft1, &ft2);
  SYSTEMTIME datetime;
  FileTimeToSystemTime (&ft2, &datetime);

  GetDateFormatA(LOCALE_USER_DEFAULT, 0, &datetime, date_format, buf, bufsize);
  char *p = str_end(buf);
  *p++ = ' ';
  GetTimeFormatA(LOCALE_USER_DEFAULT, 0, &datetime, time_format, p, bufsize - (p-buf));
}

// Получить тип файла по его расширению
void GuiGetFileType (TCHAR *ext, TCHAR *buf)
{
  SHFILEINFO sfi;
  if (SHGetFileInfoW(ext, FILE_ATTRIBUTE_NORMAL, &sfi, sizeof(sfi), SHGFI_TYPENAME | SHGFI_USEFILEATTRIBUTES))
    _tcscpy(buf, sfi.szTypeName);
  else
    _tcscpy(buf, _T(""));
}

#endif // Windows/Unix
