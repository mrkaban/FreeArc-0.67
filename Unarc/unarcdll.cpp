// Обработка сбоев при распаковке архива
#undef  CHECK
#define CHECK(e,a,b)   {if (!(a))  {char *s=(char*)malloc(MY_FILENAME_MAX*4);  if (s)  sprintf b;  UnarcQuit(e,s);}}
void UnarcQuit (int errcode, char* errmsg);

// Доступ к структуре архива
#include "ArcStructure.h"

#include "../Compression/MultiThreading.h"
#include "unarcdll.h"

// Доступ к парсингу командной строки и выполнению операций над архивом
#include "ArcCommand.h"
#include "ArcProcess.h"

// Экстренный выход из программы в случае ошибки
void UnarcQuit (int errcode, char* errmsg)
{
  CurrentProcess->quit (errcode, errmsg);
}

// Запомним хендл unarc.dll, необходимый нам для правильной загрузки других dll
extern "C" BOOL WINAPI DllMain (HINSTANCE hinstDll, DWORD fdwReason, LPVOID lpvReserved)
{
  if (fdwReason == DLL_PROCESS_ATTACH)
    hinstUnarcDll = hinstDll;
  return TRUE;
}


/******************************************************************************
** Описание интерфейса с программой, использующей DLL *************************
******************************************************************************/
class DLLUI : public BASEUI
{
private:
  char outdir[MY_FILENAME_MAX*4];  //unicode: utf-8 encoding
public:
  COMMAND *command;
  bool     callback;
  Mutex    mutex;
  Event    DoEvent, EventDone;

  char *what; Number n1, n2; int result; char *str;
  int event (char *_what, Number _n1, Number _n2, char *_str);

  DLLUI (COMMAND *_command, bool _callback)  :  command(_command), callback(_callback) {}
  bool AllowProcessing (char cmd, int silent, FILENAME arcname, char* comment, int cmtsize, FILENAME outdir);
  FILENAME GetOutDir ();
  bool BeginProgress (uint64 totalBytes);
  bool ProgressRead  (uint64 readBytes);
  bool ProgressWrite (uint64 writtenBytes);
  bool ProgressFile  (bool isdir, const char *operation, FILENAME filename, uint64 filesize);
  char AskOverwrite  (FILENAME filename, uint64 size, time_t modified);
  char AskPassword   (char *pwd, int pwdbuf_size);
  void Abort         (COMMAND *cmd, int errcode, char* errmsg);
};


/******************************************************************************
** Реализация интерфейса с программой, использующей DLL ***********************
******************************************************************************/
int DLLUI::event (char *_what, Number _n1, Number _n2, char *_str)
{
  Lock _(mutex);
  what = _what;
  n1   = _n1;
  n2   = _n2;
  str  = _str;

  DoEvent.Signal();
  EventDone.Wait();
  return result;
}

bool DLLUI::BeginProgress (uint64 totalBytes)
{
  return callback? event ("total", totalBytes>>20, totalBytes, "") >= 0
                 : TRUE;
}

bool DLLUI::ProgressRead (uint64 readBytes)
{
  return callback? event ("read", readBytes>>20, readBytes, "") >= 0
                 : TRUE;
}

bool DLLUI::ProgressWrite (uint64 writtenBytes)
{
  return callback? event ("write", writtenBytes>>20, writtenBytes, "") >= 0
                 : TRUE;
}

bool DLLUI::ProgressFile (bool isdir, const char *operation, FILENAME filename, uint64 filesize)
{
  return callback? event ("filename", filesize>>20, filesize, filename) >= 0
                 : TRUE;
}

FILENAME DLLUI::GetOutDir()
{
  return outdir;
}

bool DLLUI::AllowProcessing (char cmd, int silent, FILENAME arcname, char* comment, int cmtsize, FILENAME _outdir)
{
  strcpy (outdir, _outdir);
  return TRUE;
}

char DLLUI::AskOverwrite (FILENAME filename, uint64 size, time_t modified)
{
  return callback? event ("overwrite?", size>>20, size, filename)
                 : 's';
}

char DLLUI::AskPassword (char *pwd, int pwdbuf_size)
{
  return callback? event ("password?", pwdbuf_size, 0, pwd)
                 : 'n';
}

void DLLUI::Abort (COMMAND *cmd, int errcode, char* errmsg)
{
  event ("error", errcode, 0, errmsg);
}


/******************************************************************************
** Реализация функционала DLL *************************************************
******************************************************************************/
static THREAD_FUNC_RET_TYPE THREAD_FUNC_CALL_TYPE decompress_thread (void *paramPtr)
{
  uint64 total_files, origsize, compsize;
  DLLUI *ui = (DLLUI*) paramPtr;
  // Выполнить разобранную команду
  if (ui->command->cmd=='l')
  {
    PROCESS (ui->command, ui, total_files, origsize, compsize);
    ui->event ("total_files", total_files,  0,        "");
    ui->event ("origsize",    origsize>>20, origsize, "");
    ui->event ("compsize",    compsize>>20, compsize, "");
  }
  else
    PROCESS (ui->command, ui);
  ui->what = "return";
  ui->DoEvent.Signal();
  return 0;
}

int __cdecl FreeArcExtract (cbtype *callback, ...)
{
  jmpready = FALSE;

  va_list argptr;
  va_start(argptr, callback);

  int argc=0;
  char *argv[1000] = {"c:\\unarc.dll"};  //// Здесь будет искаться arc.ini!

  for (int i=1; i<1000; i++)
  {
    argc = i;
    argv[i] = va_arg(argptr, char*);
    if (argv[i]==NULL || argv[i][0]==0)
      {argv[i]=NULL; break;}
  }
  va_end(argptr);

  int errcode = FREEARC_OK;
  COMMAND command (argc, argv);    // Распарсить команду
  if (command.ok) {                // Если парсинг был удачен и можно выполнить команду
    command.Prepare();
    Thread thread;
    DLLUI *ui  =  new DLLUI (&command, callback!=NULL);
    thread.Create (decompress_thread, ui);   //   Выполнить разобранную команду

    for(;;)
    {
      ui->DoEvent.Wait();
      if (strequ (ui->what, "return"))   break;
      ui->result = callback? callback (ui->what, ui->n1, ui->n2, ui->str) : FREEARC_ERRCODE_NOT_IMPLEMENTED;
      if (strequ (ui->what, "quit"))     errcode = ui->n1;   // error code of command
      ui->EventDone.Signal();
    }
    thread.Wait();
  }
  return errcode? errcode : (command.ok? FREEARC_OK : FREEARC_ERRCODE_GENERAL);
}

