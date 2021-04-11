#include <stdio.h>
#include <windows.h>
#define strequ !strcmp

// Loads function from a unarc.dll
FARPROC LoadFromDLL (char *funcname)
{
  static HMODULE unarc_dll = 0;
  if (!unarc_dll)
    unarc_dll = LoadLibrary("unarc.dll");
  return GetProcAddress (unarc_dll, funcname);
}

typedef int Number;
typedef int __stdcall cbtype (char *what, Number int1, Number int2, char *str);

int __stdcall callback (char *what, Number int1, Number int2, char *str)
{
  if (strequ (what, "password?"))
    {printf("Enter password:"); gets(str);}

  printf("callback(\"%s\", %d, %d, \"%s\")\n", what, int1, int2, str);
  if (strequ (what, "password?"))
    return 'y';
  else if (strequ (what, "overwrite?"))
    return 'y';
  else
    return 1;
}

main (int argc, char **argv)
{
  if (argc<=2) {printf("Usage: UnarcDllExample.exe [l|e|x|t] [options] [--] archive [files]\n"); return 1;}
  char *command[100];
  memcpy (command, argv, argc*sizeof(char*));
  command[argc] = NULL;

  // Load function from unarc.dll
  int (*FreeArcExtract) (cbtype *callback, ...)  =  (int (*) (cbtype *callback, ...))  LoadFromDLL("FreeArcExtract");
  if (!FreeArcExtract) {printf("Cannot load FreeArcExtract() from unarc.dll\n"); return 1;}

  int result;

  // Command to execute.
  // NULL-terminated UTF8-encoded strings.
  // Every option/filename should be a separate string.
  // Started with command to execute and options, then "--", then archive name and filenames.
  // Execute command using callback() to interact with user. Return value is 0 for success, error code otherwise
  result = FreeArcExtract(callback, command[ 1], command[ 2], command[ 3], command[ 4], command[ 5], command[ 6], command[ 7], command[ 8], command[ 9], command[10],
                                    command[11], command[12], command[13], command[14], command[15], command[16], command[17], command[18], command[19], command[20], NULL);
  if (result) {printf("FreeArcExtract() failed with return code %d\n", result); return 1;}

  printf("FreeArcExtract() was successful\n");

  // Unload unarc.dll
  void (*UnloadDLL) (void)  =  (void (*) (void))  LoadFromDLL("UnloadDLL");
  if (!UnloadDLL) {printf("Cannot load UnloadDLL() from unarc.dll\n"); return 1;}
  (*UnloadDLL) ();

  return 0;
}
