// This file replaces p7zip's myPrivate.h that cannot be used when multiple .cpp files are compiled as one unit (using #include)

void WINAPI RtlSecondsSince1970ToFileTime( DWORD Seconds, FILETIME * ft );

extern "C" int global_use_utf16_conversion;
#ifdef HAVE_LSTAT
extern "C" int global_use_lstat;
#endif

const char *my_getlocale(void);

#ifndef NWTU_DEFINED
#define NWTU_DEFINED
//#ifdef NEED_NAME_WINDOWS_TO_UNIX
#define nameWindowToUnix(lpFileName) ((lpFileName)[0] == 'c' && (lpFileName)[1] == ':'? (lpFileName)+2 : (lpFileName))
//#endif
#endif
