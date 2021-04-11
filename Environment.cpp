#define _WIN32_WINNT 0x0501
#include <stdio.h>
#include <sys/stat.h>
#include <utime.h>
#include <limits.h>
#include <memory.h>
#include "Environment.h"
#include "Compression/Compression.h"

// Изменим настройки RTS, включив compacting GC начиная с 40 mb:
char *ghc_rts_opts = "-c1 -M4000m -K80m                       ";


/* ********************************************************************************************************
*  FreeArc.dll
***********************************************************************************************************/

#ifdef FREEARC_DLL
#include "Compression/_TABI/tabi.h"

extern "C" {
#include "HsFFI.h"
#include "Arc_stub.h"
void __stginit_Main();

BOOL APIENTRY DllMain (HANDLE hModule, DWORD  ul_reason_for_call, LPVOID lpReserved)
{
   return TRUE;
}

enum { MESSAGE_TOTAL_FILES=1
     , MESSAGE_TOTAL_ORIGINAL
     , MESSAGE_PROGRESS_ORIGINAL
     , MESSAGE_PROGRESS_COMPRESSED
     , MESSAGE_PROGRESS_MESSAGE
     , MESSAGE_PROGRESS_FILENAME
     , MESSAGE_WARNING_MESSAGE
     , MESSAGE_VOLUME_FILENAME
     , MESSAGE_CAN_BE_EXTRACTED

     , MESSAGE_PASSWORD_BUF
     , MESSAGE_PASSWORD_SIZE
     , MESSAGE_ARCINFO_TYPE
     , MESSAGE_ARCINFO_FILES
     , MESSAGE_ITEMINFO
     , MESSAGE_ARCINFO_NAME
     };

struct ItemInfo
{
    wchar_t *diskname;    // Filename on disk (only for "can_be_extracted?" request)
    wchar_t *filename;    // Filename of this item
    int64 original;       // Bytes, uncompressed
    int64 compressed;     // Bytes, compressed
    uint  time;           // Datetime stamp
    uint  attr;           // DOS attributes
    uint  is_folder;
    uint  crc;            // CRC32
    uint  is_encrypted;
};


typedef int CallBackFunc (WPARAM wParam, LPARAM lParam);
CallBackFunc *FreeArcCallback;

#define Send(msg,param) FreeArcCallback(msg, (LPARAM) (param))


// Function called by dotnet_FreeArcExecute() to display progress indicator and interact with user
int Callback_for_FreeArcExecute (TABI_ELEMENT* params)
{
    TABI_MAP p(params);
    char *request = p._str("request");   // Operation requested from callback
    if (strequ(request, "total"))
    {
        // Общий объём файлов, которые предстоит упаковать/распаковать
        int64 files      = p._longlong("files");          // Number of files
        int64 original   = p._longlong("original");       // Total size of files
        Send (MESSAGE_TOTAL_FILES,    &files);
        Send (MESSAGE_TOTAL_ORIGINAL, &original);
    }
    else if (strequ(request, "progress"))
    {
        // Информируем пользователя о ходе упаковки/распаковки
        int64 original   = p._longlong("original");       // Bytes, uncompressed
        int64 compressed = p._longlong("compressed");     // Bytes, compressed
        Send (MESSAGE_PROGRESS_ORIGINAL,   &original);
        Send (MESSAGE_PROGRESS_COMPRESSED, &compressed);
    }
    else if (strequ(request, "file"))
    {
        // Начало упаковки/распаковки нового файла
        wchar_t *message   = p._wstr("message");
        wchar_t *filename  = p._wstr("filename");
        Send (MESSAGE_PROGRESS_MESSAGE,  message);
        Send (MESSAGE_PROGRESS_FILENAME, filename);
    }
    else if (strequ(request, "warning") || strequ(request, "error"))
    {
        // Сообщшение о [не]критической ошибке
        wchar_t *message   = p._wstr("message");
        Send (MESSAGE_WARNING_MESSAGE, message);
    }
    else if (strequ(request, "volume"))
    {
        // Начало нового тома
        wchar_t *filename  = p._wstr("filename");       // Filename of new archive volume
        Send (MESSAGE_VOLUME_FILENAME, filename);
    }
    else if (strequ(request, "can_be_extracted?"))
    {
        // Можно ли извлечь этот файл?
        ItemInfo i;
        i.diskname      = p._wstr    ("diskname");       // Filename on disk
        i.filename      = p._wstr    ("filename");       // Filename of this item
        i.original      = p._longlong("original");       // Bytes, uncompressed
        i.compressed    = p._longlong("compressed");     // Bytes, compressed
        i.time          = p._unsigned("time");           // Datetime stamp
        i.attr          = p._unsigned("attr");           // DOS attributes
        i.is_folder     = p._bool    ("is_folder?");
        i.crc           = p._unsigned("crc");            // CRC32
        i.is_encrypted  = p._bool    ("is_encrypted?");
        return Send (MESSAGE_CAN_BE_EXTRACTED, &i);
    }
    else if (strequ(request, "ask_password"))
    {
        // Запрос пароля расшифровки
        wchar_t *password_buf  = (wchar_t *) p._ptr("password_buf");    // Buffer for password
        int      password_size =             p._int("password_size");   // Buffer size
        Send (MESSAGE_PASSWORD_BUF,  password_buf);
        Send (MESSAGE_PASSWORD_SIZE, password_size);
    }
    else if (strequ(request, "archive"))
    {
        // Общая информация об архиве
        wchar_t *arcname  = p._wstr("arcname");
        char    *arctype  = p._str ("arctype");
        int64    files    = p._longlong("files");
        Send (MESSAGE_ARCINFO_NAME,  arcname);
        Send (MESSAGE_ARCINFO_TYPE,  arctype);
        Send (MESSAGE_ARCINFO_FILES, &files);
    }
    else if (strequ(request, "item"))
    {
        // Информация об очередном файле внутри архива
        ItemInfo i;
        i.diskname      = p._wstr    ("filename");       // Filename of this item
        i.filename      = p._wstr    ("filename");       // Filename of this item
        i.original      = p._longlong("original");       // Bytes, uncompressed
        i.compressed    = p._longlong("compressed");     // Bytes, compressed
        i.time          = p._unsigned("time");           // Datetime stamp
        i.attr          = p._unsigned("attr");           // DOS attributes
        i.is_folder     = p._bool    ("is_folder?");
        i.crc           = p._unsigned("crc");            // CRC32
        i.is_encrypted  = p._bool    ("is_encrypted?");
        Send (MESSAGE_ITEMINFO, &i);
    }
    return 0;
}

// Initialize Haskell runtime
void haskell_init (void)
{
  static bool initialized = FALSE;
  if (!initialized)
  {
    initialized = TRUE;
    int argc = 1;
    char* argv[] = {"ghcDll", NULL}; // argv must end with NULL
    char** args = argv;
    hs_init(&argc, &args);
    hs_add_root(__stginit_Main);
  }
}

int FreeArcExecute (TABI_DYNAMAP arg)
{
  haskell_init();
  return haskell_FreeArcExecute(arg);
}

int FreeArcOpenArchive (TABI_DYNAMAP arg)
{
  haskell_init();
  return haskell_FreeArcOpenArchive(arg);
}

int dotnet_FreeArcExecute (wchar_t** arg, CallBackFunc *f)
{
  FreeArcCallback = f;
  return FreeArcExecute(TABI_DYNAMAP ("command", (void*)arg) ("callback", Callback_for_FreeArcExecute));
}

int dotnet_FreeArcOpenArchive (wchar_t* arcname, CallBackFunc *f)
{
  FreeArcCallback = f;
  return FreeArcOpenArchive(TABI_DYNAMAP ("arcname", arcname) ("callback", Callback_for_FreeArcExecute));
}

} // extern "C"
#endif // FREEARC_DLL


/* ********************************************************************************************************
*  Find largest contiguous memory block available and dump information about all available memory blocks
***********************************************************************************************************/

void memstat(void);

struct LargestMemoryBlock
{
  void   *p;
  size_t size;
  LargestMemoryBlock();
  ~LargestMemoryBlock()         {free();}
  size_t total();
  void alloc(size_t n);
  void free();
  void test();
};

LargestMemoryBlock::LargestMemoryBlock() : p(NULL)
{
  size_t a=0, b=UINT_MAX;
  while (b-a>1) {
    free();
    size_t c=(a+b)/2;
    alloc(c);
    if(p) a=c;  else b=c;
  }
}

size_t LargestMemoryBlock::total()
{
  if (size >= 10*mb) {               // Don't count too small memory blocks
    LargestMemoryBlock next;
    return size + next.total();
  } else {
    return 0;
  }
}

void LargestMemoryBlock::test()
{
  if ((size>>20)>0) {
    printf("Allocated %4d mb, addr=%p\n", size>>20, p);
    LargestMemoryBlock next;
    next.test();
  } else {
    memstat();
  }
}

void TestMalloc (void)
{
  memstat();
  printf("\n");
  LargestMemoryBlock m;
  m.test();
}


#ifdef FREEARC_WIN

#include <HsFFI.h>
#include <io.h>
#include <wchar.h>
#include <sys/stat.h>

extern "C" HsInt    __w_find_sizeof       ( void ) { return sizeof(struct _wfinddatai64_t); };
extern "C" unsigned __w_find_attrib       ( struct _wfinddatai64_t* st ) { return st->attrib;      }
extern "C" time_t   __w_find_time_create  ( struct _wfinddatai64_t* st ) { return st->time_create; }
extern "C" time_t   __w_find_time_access  ( struct _wfinddatai64_t* st ) { return st->time_access; }
extern "C" time_t   __w_find_time_write   ( struct _wfinddatai64_t* st ) { return st->time_write;  }
extern "C" __int64  __w_find_size         ( struct _wfinddatai64_t* st ) { return st->size;        }
extern "C" wchar_t* __w_find_name         ( struct _wfinddatai64_t* st ) { return st->name;        }

extern "C" HsInt          __w_stat_sizeof ( void ) { return sizeof(struct _stati64); }
extern "C" unsigned short __w_stat_mode   ( struct _stati64* st ) { return st->st_mode;  }
extern "C" time_t         __w_stat_ctime  ( struct _stati64* st ) { return st->st_ctime; }
extern "C" time_t         __w_stat_atime  ( struct _stati64* st ) { return st->st_atime; }
extern "C" time_t         __w_stat_mtime  ( struct _stati64* st ) { return st->st_mtime; }
extern "C" __int64        __w_stat_size   ( struct _stati64* st ) { return st->st_size;  }


#include <windows.h>
#include <reason.h>
#include <shlobj.h>
#include <stdio.h>
#include <conio.h>
#include <time.h>

// Provide VirtualAlloc operations for testing
void LargestMemoryBlock::alloc(size_t n) {p = VirtualAlloc (0, size=n, MEM_RESERVE, PAGE_READWRITE);};
void LargestMemoryBlock::free ()         {VirtualFree (p, 0, MEM_RELEASE); p=NULL;};


// Use to convert bytes to MB
#define DIV (1024*1024)

// Specify the width of the field in which to print the numbers.
// The asterisk in the format specifier "%*I64d" takes an integer
// argument and uses it to pad and right justify the number.
#define WIDTH 4

void memstat (void)
{
  MEMORYSTATUSEX statex;

  statex.dwLength = sizeof (statex);

  GlobalMemoryStatusEx (&statex);

  printf ("There is  %*ld percent of memory in use.\n",
          WIDTH, statex.dwMemoryLoad);
  printf ("There are %*I64d total Mbytes of physical memory.\n",
          WIDTH, statex.ullTotalPhys/DIV);
  printf ("There are %*I64d free Mbytes of physical memory.\n",
          WIDTH, statex.ullAvailPhys/DIV);
  printf ("There are %*I64d total Mbytes of paging file.\n",
          WIDTH, statex.ullTotalPageFile/DIV);
  printf ("There are %*I64d free Mbytes of paging file.\n",
          WIDTH, statex.ullAvailPageFile/DIV);
  printf ("There are %*I64d total Mbytes of virtual memory.\n",
          WIDTH, statex.ullTotalVirtual/DIV);
  printf ("There are %*I64d free Mbytes of virtual memory.\n",
          WIDTH, statex.ullAvailVirtual/DIV);

  // Show the amount of extended memory available.

  printf ("There are %*I64d free Mbytes of extended memory.\n",
          WIDTH, statex.ullAvailExtendedVirtual/DIV);
}

#else

// Provide malloc operations for testing
void LargestMemoryBlock::alloc(size_t n) {p=malloc(size=n);};
void LargestMemoryBlock::free ()         {::free(p); p=NULL;};

void memstat (void)
{
}

#endif


#ifdef FREEARC_WIN

CFILENAME GetExeName (CFILENAME buf, int bufsize)
{
  GetModuleFileNameW (NULL, buf, bufsize);
  return buf;
}


unsigned GetMaxBlockToAlloc (void)
{
  MEMORYSTATUS stat;
  stat.dwLength = sizeof(stat);
  GlobalMemoryStatus(&stat);

  LargestMemoryBlock block;
  return mymin(block.size, stat.dwAvailPageFile) - 5*mb;
}

unsigned GetTotalMemoryToAlloc (void)
{
  MEMORYSTATUS stat;
  stat.dwLength = sizeof(stat);
  GlobalMemoryStatus(&stat);

  LargestMemoryBlock block;
  return mymin(block.total(), stat.dwAvailPageFile) - 5*mb;
}

// Delete entrire subtree from Windows Registry
DWORD RegistryDeleteTree(HKEY hStartKey, LPTSTR pKeyName)
{
   const int MAX_KEY_LENGTH = MAX_PATH;
   DWORD   dwRtn, dwSubKeyLength;
   TCHAR   szSubKey[MAX_KEY_LENGTH];
   HKEY    hKey;

   // Do not allow NULL or empty key name
   if (pKeyName && lstrlen(pKeyName))
   {
      if((dwRtn = RegOpenKeyEx (hStartKey, pKeyName, 0, KEY_ENUMERATE_SUB_KEYS | DELETE, &hKey))  ==  ERROR_SUCCESS)
      {
         while (dwRtn == ERROR_SUCCESS )
         {
            dwSubKeyLength = MAX_KEY_LENGTH;
            dwRtn=RegEnumKeyEx(
                           hKey,
                           0,       // always index zero
                           szSubKey,
                           &dwSubKeyLength,
                           NULL,
                           NULL,
                           NULL,
                           NULL
                         );

            if(dwRtn == ERROR_NO_MORE_ITEMS)
            {
               dwRtn = RegDeleteKey(hStartKey, pKeyName);
               break;
            }
            else if(dwRtn == ERROR_SUCCESS)
               dwRtn=RegistryDeleteTree(hKey, szSubKey);
         }
         RegCloseKey(hKey);
         // Do not save return code because error
         // has already occurred
      }
   }
   else
      dwRtn = ERROR_BADKEY;

   return dwRtn;
}

int MyGetAppUserDataDirectory (CFILENAME buf)
{
  return SHGetFolderPathW(NULL, CSIDL_APPDATA, NULL, 0 /*SHGFP_TYPE_CURRENT*/, buf);
}

// Инициировать выключение компьютера
int PowerOffComputer()
{
   HANDLE hToken;
   TOKEN_PRIVILEGES tkp;

   // Get a token for this process.

   if (!OpenProcessToken(GetCurrentProcess(),
        TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &hToken))
      return( FALSE );

   // Get the LUID for the shutdown privilege.

   LookupPrivilegeValue(NULL, SE_SHUTDOWN_NAME,
        &tkp.Privileges[0].Luid);

   tkp.PrivilegeCount = 1;  // one privilege to set
   tkp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;

   // Get the shutdown privilege for this process.

   AdjustTokenPrivileges(hToken, FALSE, &tkp, 0,
        (PTOKEN_PRIVILEGES)NULL, 0);

   if (GetLastError() != ERROR_SUCCESS)
      return FALSE;

   // Shut down the system and force all applications to close.

   if (!ExitWindowsEx(EWX_POWEROFF, SHTDN_REASON_MAJOR_APPLICATION | SHTDN_REASON_MINOR_MAINTENANCE | SHTDN_REASON_FLAG_PLANNED))
      return FALSE;

   //shutdown was successful
   return TRUE;
}


// Заполняет буфер строкой с описанием версии ОС
void GetOSDisplayString(char* buf)
{
   strcpy(buf, "Unknown Windows version");

   typedef void (WINAPI *PGNSI)(LPSYSTEM_INFO);

   OSVERSIONINFOEX osvi;
   SYSTEM_INFO si;
   PGNSI pGNSI;

   ZeroMemory(&si, sizeof(SYSTEM_INFO));
   ZeroMemory(&osvi, sizeof(OSVERSIONINFOEX));

   osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);

   if( !GetVersionEx ((OSVERSIONINFO *) &osvi) )
      return;

   // Call GetNativeSystemInfo if supported or GetSystemInfo otherwise.

   pGNSI = (PGNSI) GetProcAddress(
      GetModuleHandle(TEXT("kernel32.dll")),
      "GetNativeSystemInfo");
   if(NULL != pGNSI)
      pGNSI(&si);
   else GetSystemInfo(&si);

   if ( VER_PLATFORM_WIN32_NT==osvi.dwPlatformId &&
        osvi.dwMajorVersion > 4 )
   {
      // Test for the specific product.

      if ( osvi.dwMajorVersion == 6 )
      {
         if( osvi.dwMinorVersion == 0 )
         {
            if( osvi.wProductType == VER_NT_WORKSTATION )
                strcpy(buf, "Windows Vista");
            else strcpy(buf, "Windows Server 2008");
         }

         if ( osvi.dwMinorVersion == 1 )
         {
            if( osvi.wProductType == VER_NT_WORKSTATION )
                strcpy(buf, "Windows 7");
            else strcpy(buf, "Windows Server 2008 R2");
         }

         if(osvi.dwMinorVersion == 2)
         {
            if( osvi.wProductType == VER_NT_WORKSTATION )
                strcpy(buf, "Windows 8");
            else strcpy(buf, "Windows Server 2012");
         }
      }

      if ( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2 )
      {
         if( GetSystemMetrics(SM_SERVERR2) )
            strcpy(buf,  "Windows Server 2003 R2");
         else if ( osvi.wSuiteMask & VER_SUITE_STORAGE_SERVER )
            strcpy(buf,  "Windows Storage Server 2003");
         else if( osvi.wProductType == VER_NT_WORKSTATION &&
                  si.wProcessorArchitecture==PROCESSOR_ARCHITECTURE_AMD64)
         {
            strcpy(buf,  "Windows XP Professional x64 Edition");
         }
         else strcpy(buf, "Windows Server 2003");
      }

      if ( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1 )
      {
         strcpy(buf, "Windows XP");
      }

      if ( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0 )
      {
         strcpy(buf, "Windows 2000");
      }

      sprintf(str_end(buf), " (%d.%d)", int(osvi.dwMajorVersion), int(osvi.dwMinorVersion));

      if ( osvi.dwMajorVersion >= 6 )
      {
         if ( si.wProcessorArchitecture==PROCESSOR_ARCHITECTURE_AMD64 )
            strcat(buf,  ", 64-bit");
         else if (si.wProcessorArchitecture==PROCESSOR_ARCHITECTURE_INTEL )
            strcat(buf, ", 32-bit");
      }
   }
}


// Operations on mutex shared by all FreeArc instances
HANDLE myCreateMutex  (char*  name)        {return CreateMutexA (NULL, FALSE, name);}
void   myCloseMutex   (HANDLE hMutex)      {CloseHandle(hMutex);}
void   myWaitMutex    (HANDLE hMutex)      {WaitForSingleObject (hMutex, INFINITE);}
void   myGrabMutex    (HANDLE hMutex)      {WaitForSingleObject (hMutex, 0);}
void   myReleaseMutex (HANDLE hMutex)      {ReleaseMutex(hMutex);}


#else // For Unix:


#include <unistd.h>

CFILENAME GetExeName (CFILENAME buf, int bufsize)
{
  int len = readlink("/proc/self/exe", buf, bufsize-1);
  if (len<0)  len=0;
  buf[len] = '\0';
  return buf;
}

unsigned GetMaxBlockToAlloc (void)
{
  //struct sysinfo si;
  //  sysinfo(&si);
  return INT_MAX;
}

unsigned GetTotalMemoryToAlloc (void)
{
  return INT_MAX;
}

// Инициировать выключение компьютера
int PowerOffComputer()
{
  system ("shutdown now");
  return TRUE;
}

#endif // Windows/Unix


void FormatDateTime (char *buf, int bufsize, time_t t)
{
  if (t<0)  t=INT_MAX;  // Иначе получаем вылет :(
  struct tm *p;
  p = localtime(&t);
  strftime (buf, bufsize, "%Y-%m-%d %H:%M:%S", p);
}

// Максимальная длина имени файла
int long_path_size (void)
{
  return MY_FILENAME_MAX;
}


// Вернуть имя файла без имени каталога
FILENAME basename (FILENAME fullname)
{
  char *basename = fullname;
  for (char* p=fullname; *p; p++)
    if (in_set (*p, ALL_PATH_DELIMITERS))
      basename = p+1;
  return basename;
}

// От-xor-ить два блока данных
void memxor (char *dest, char *src, uint size)
{
  if (size) do
      *dest++ ^= *src++;
  while (--size);
}


/* ***************************************************************************
*                                                                            *
* Random system values collection routine from CryptLib by Peter Gutmann     *
* [ftp://ftp.franken.de/pub/crypt/cryptlib/cl331.zip]                        *
*                                                                            *
*****************************************************************************/

/* The size of the intermediate buffer used to accumulate polled data */
#define RANDOM_BUFSIZE	4096

// Handling random data buffer
#define initRandomData(rand_buf, rand_size)  \
                                 char *rand_ptr=(rand_buf), *rand_end=(rand_buf)+(rand_size)
#define addRandomData(ptr,size)  (memcpy (rand_ptr, (ptr), mymin((size),rand_end-rand_ptr)), rand_ptr+=mymin((size),rand_end-rand_ptr))
#define addRandomLong(value)     {long n=(value); addRandomData(&n, sizeof(long));}
#define addRandomValue(value)    addRandomLong((long) value)


/* Map a value that may be 32 or 64 bits depending on the platform to a long */
#if defined( _MSC_VER ) && ( _MSC_VER >= 1400 )
  #define addRandomHandle( handle ) \
		  addRandomLong( PtrToUlong( handle ) )
#else
  #define addRandomHandle	addRandomValue
#endif /* 32- vs. 64-bit VC++ */


// This routine fills buffer with system-generated pseudo-random data
// and returns number of bytes filled
int systemRandomData (char *rand_buf, int rand_size)
{
#ifdef FREEARC_WIN

	FILETIME  creationTime, exitTime, kernelTime, userTime;
	DWORD minimumWorkingSetSize, maximumWorkingSetSize;
	LARGE_INTEGER performanceCount;
	MEMORYSTATUS memoryStatus;
	HANDLE handle;
	POINT point;

	initRandomData (rand_buf, rand_size);

	/* Get various basic pieces of system information: Handle of active
	   window, handle of window with mouse capture, handle of clipboard owner
	   handle of start of clpboard viewer list, pseudohandle of current
	   process, current process ID, pseudohandle of current thread, current
	   thread ID, handle of desktop window, handle  of window with keyboard
	   focus, whether system queue has any events, cursor position for last
	   message, 1 ms time for last message, handle of window with clipboard
	   open, handle of process heap, handle of procs window station, types of
	   events in input queue, and milliseconds since Windows was started.
	   Since a HWND/HANDLE can be a 64-bit value on a 64-bit platform, we
	   have to use a mapping macro that discards the high 32 bits (which
	   presumably won't be of much interest anyway) */
	addRandomHandle( GetActiveWindow() );
	addRandomHandle( GetCapture() );
	addRandomHandle( GetClipboardOwner() );
	addRandomHandle( GetClipboardViewer() );
	addRandomHandle( GetCurrentProcess() );
	addRandomValue( GetCurrentProcessId() );
	addRandomHandle( GetCurrentThread() );
	addRandomValue( GetCurrentThreadId() );
	addRandomHandle( GetDesktopWindow() );
	addRandomHandle( GetFocus() );
	addRandomValue( GetInputState() );
	addRandomValue( GetMessagePos() );
	addRandomValue( GetMessageTime() );
	addRandomHandle( GetOpenClipboardWindow() );
	addRandomHandle( GetProcessHeap() );
	addRandomHandle( GetProcessWindowStation() );
	addRandomValue( GetTickCount() );

	/* Get multiword system information: Current caret position, current
	   mouse cursor position */
	GetCaretPos( &point );
	addRandomData( &point, sizeof( POINT ) );
	GetCursorPos( &point );
	addRandomData( &point, sizeof( POINT ) );

	/* Get percent of memory in use, bytes of physical memory, bytes of free
	   physical memory, bytes in paging file, free bytes in paging file, user
	   bytes of address space, and free user bytes */
	memoryStatus.dwLength = sizeof( MEMORYSTATUS );
	GlobalMemoryStatus( &memoryStatus );
	addRandomData( &memoryStatus, sizeof( MEMORYSTATUS ) );

	/* Get thread and process creation time, exit time, time in kernel mode,
	   and time in user mode in 100ns intervals */
	handle = GetCurrentThread();
	GetThreadTimes( handle, &creationTime, &exitTime, &kernelTime, &userTime );
	addRandomData( &creationTime, sizeof( FILETIME ) );
	addRandomData( &exitTime, sizeof( FILETIME ) );
	addRandomData( &kernelTime, sizeof( FILETIME ) );
	addRandomData( &userTime, sizeof( FILETIME ) );
	handle = GetCurrentProcess();
	GetProcessTimes( handle, &creationTime, &exitTime, &kernelTime, &userTime );
	addRandomData( &creationTime, sizeof( FILETIME ) );
	addRandomData( &exitTime, sizeof( FILETIME ) );
	addRandomData( &kernelTime, sizeof( FILETIME ) );
	addRandomData( &userTime, sizeof( FILETIME ) );

	/* Get the minimum and maximum working set size for the current process */
	GetProcessWorkingSetSize( handle, &minimumWorkingSetSize, &maximumWorkingSetSize );
	addRandomValue( minimumWorkingSetSize );
	addRandomValue( maximumWorkingSetSize );

	/* The following are fixed for the lifetime of the process */
       	/* Get name of desktop, console window title, new window position and
       	   size, window flags, and handles for stdin, stdout, and stderr */
       	STARTUPINFO startupInfo;
       	startupInfo.cb = sizeof( STARTUPINFO );
       	GetStartupInfo( &startupInfo );
       	addRandomData( &startupInfo, sizeof( STARTUPINFO ) );

	/* The performance of QPC varies depending on the architecture it's
	   running on and on the OS, the MS documentation is vague about the
	   details because it varies so much.  Under Win9x/ME it reads the
	   1.193180 MHz PIC timer.  Under NT/Win2K/XP it may or may not read the
	   64-bit TSC depending on the HAL and assorted other circumstances,
	   generally on machines with a uniprocessor HAL
	   KeQueryPerformanceCounter() uses a 3.579545MHz timer and on machines
	   with a multiprocessor or APIC HAL it uses the TSC (the exact time
	   source is controlled by the HalpUse8254 flag in the kernel).  That
	   choice of time sources is somewhat peculiar because on a
	   multiprocessor machine it's theoretically possible to get completely
	   different TSC readings depending on which CPU you're currently
	   running on, while for uniprocessor machines it's not a problem.
	   However, the kernel appears to synchronise the TSCs across CPUs at
	   boot time (it resets the TSC as part of its system init), so this
	   shouldn't really be a problem.  Under WinCE it's completely platform-
	   dependant, if there's no hardware performance counter available, it
	   uses the 1ms system timer.

	   Another feature of the TSC (although it doesn't really affect us here)
	   is that mobile CPUs will turn off the TSC when they idle, Pentiums
	   will change the rate of the counter when they clock-throttle (to
	   match the current CPU speed), and hyperthreading Pentiums will turn
	   it off when both threads are idle (this more or less makes sense,
	   since the CPU will be in the halted state and not executing any
	   instructions to count).

	   To make things unambiguous, we detect a CPU new enough to call RDTSC
	   directly by checking for CPUID capabilities, and fall back to QPC if
	   this isn't present */
       	if( QueryPerformanceCounter( &performanceCount ) )
       		addRandomData( &performanceCount,
       					   sizeof( LARGE_INTEGER ) );
       	else
       		/* Millisecond accuracy at best... */
       		addRandomValue( GetTickCount() );

        return rand_ptr-rand_buf;

#else // For Unix:

	FILE *f = fopen ("/dev/urandom", "rb");

	if (f == NULL)
	{
		perror ("Cannot open /dev/urandom");
		return 0;
	}

	if (file_read (f, rand_buf, rand_size) != rand_size)
	{
		perror ("Read from /dev/urandom failed");
		fclose (f);
		return 0;
	}

	fclose (f);
	return rand_size;

#endif // Windows/Unix

}

/****************************************************************************
*
*                                           Random system values collection *
*
****************************************************************************/
