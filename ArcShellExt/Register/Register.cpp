#include <stdio.h>
#include <tchar.h>
#include <windows.h>

// Version of FreeArc.exe->Register.exe call interface
#define API_VERSION "0.60"

// Delete entrire subtree from Windows Registry
DWORD RegistryDeleteTree (HKEY hStartKey, LPTSTR pKeyName)
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


BOOL RegistryCreateKey (HKEY hRootKey, TCHAR *szSubKey, TCHAR *szName, TCHAR *szData)
{
  HKEY     hKey;
  LRESULT  lResult;
  DWORD    dwDisp;

  lResult = RegCreateKeyEx(hRootKey, szSubKey, 0, NULL, REG_OPTION_NON_VOLATILE, KEY_WRITE, NULL, &hKey, &dwDisp);
  if(NOERROR == lResult)
  {
    lResult = RegSetValueEx(hKey, szName, 0, REG_SZ, (LPBYTE)szData, (lstrlen(szData) + 1) * sizeof(TCHAR));
    RegCloseKey(hKey);
    return TRUE;
  }
  else
  {
    return FALSE;
  }
}

BOOL RunDll (const TCHAR *path, const TCHAR *funcname_w)
{
  HMODULE dll = LoadLibraryW(path);
  const char *funcname = 0==_tcscmp(funcname_w, TEXT("DllRegisterServer"))? "DllRegisterServer" : "DllUnregisterServer";
  FARPROC func = GetProcAddress (dll, funcname);
  if (func)  (*func)();
  return func!=NULL;
}

int main(void)
{
  int argc;
  WCHAR **argv = CommandLineToArgvW (GetCommandLineW(), &argc);
  const WCHAR *FreeArcAPI = argv[1]? argv[1]:TEXT("???");

  if (_tcscmp(FreeArcAPI, TEXT(API_VERSION)))
  {
    TCHAR msg[1000];
    wsprintf(msg, TEXT("Incompatible versions: FreeArc %.40s vs Register.exe %.40s"), FreeArcAPI, TEXT(API_VERSION));

    MessageBoxW (NULL,
                 msg,
                 L"Register.exe",
                 MB_OK);
    exit(1);
  }

  if (FAILED(OleInitialize(NULL)))
  {
    MessageBoxW (NULL,
               L"OleInitialize failed.",
               L"Register.exe",
               MB_TASKMODAL | MB_ICONINFORMATION);
    exit(2);
  }

  for(int i=2;i<argc;i++)
  {
    if (0==_tcscmp(argv[i], TEXT("RegistryDeleteTree")) && i+1 < argc)
      RegistryDeleteTree (HKEY_CLASSES_ROOT, argv[i+1]), i+=1;
    else if (0==_tcscmp(argv[i], TEXT("RegistryCreateKey")) && i+3 < argc)
      RegistryCreateKey (HKEY_CLASSES_ROOT, argv[i+1], argv[i+2], argv[i+3]), i+=3;
    else if (0==_tcscmp(argv[i], TEXT("RunDll")) && i+2 < argc)
      RunDll (argv[i+1], argv[i+2]), i+=2;
    else {
      MessageBoxW (NULL,
                 L"Unsupported command.",
                 L"Register.exe",
                 MB_TASKMODAL | MB_ICONINFORMATION);
      break;
    }
  }


//  RegistryDeleteTree (HKEY_CLASSES_ROOT, TEXT("FreeArc.arc"));
//  RegistryCreateKey  (HKEY_CLASSES_ROOT, TEXT("FreeArc.arc"), TEXT(""), TEXT("test"));
  return 0;
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
  return main();
}

