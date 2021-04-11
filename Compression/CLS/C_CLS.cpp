#include <stdio.h>
#include <string.h>
extern "C" {
#include "C_CLS.h"
}


/*-------------------------------------------------*/
/* Реализация класса CLS_METHOD                    */
/*-------------------------------------------------*/

int cb(void* _instance, int op, void *ptr, int n)
{
    CLS_METHOD *instance = (CLS_METHOD*)_instance;
    switch(op)
    {
    case CLS_GET_PARAMSTR:
        strcpy((char*)ptr, instance->params);      //// add overflow checking!
        return CLS_OK;
    case CLS_FULL_READ:                            // FA read/write operations are full
    case CLS_PARTIAL_READ:
        return instance->callback("read", ptr, n, instance->auxdata);
    case CLS_FULL_WRITE:
    case CLS_PARTIAL_WRITE:
        return instance->callback("write", ptr, n, instance->auxdata);
    case CLS_MALLOC:
        *(void**)ptr = malloc(n);
        return *(void**)ptr? CLS_OK : CLS_ERROR_NOT_ENOUGH_MEMORY;
    case CLS_FREE:
        free(ptr);
        return CLS_OK;
    default:
        return CLS_ERROR_NOT_IMPLEMENTED;
    }
}

// Функция распаковки
int CLS_METHOD::decompress (CALLBACK_FUNC *_callback, void *_auxdata)
{
    callback = _callback;
    auxdata  = _auxdata;
    return ClsMain(CLS_DECOMPRESS, cb, this);
}

#ifndef FREEARC_DECOMPRESS_ONLY

// Функция упаковки
int CLS_METHOD::compress (CALLBACK_FUNC *_callback, void *_auxdata)
{
    callback = _callback;
    auxdata  = _auxdata;
    return ClsMain(CLS_COMPRESS, cb, this);
}

#endif  // !defined (FREEARC_DECOMPRESS_ONLY)

// Записать в buf[MAX_METHOD_STRLEN] строку, описывающую метод сжатия и его параметры (функция, обратная к parse_CLS)
void CLS_METHOD::ShowCompressionMethod (char *buf, bool purify)
{
    if (strequ(params,""))
      then strcpy (buf, name);
      else sprintf(buf, "%s:%s", name, params);
}


// ПОДДЕРЖКА ПРОИЗВОЛЬНЫХ ВНЕШНИХ УПАКОВЩИКОВ **********************************************************************

// Конструирует объект типа CLS_METHOD с заданными параметрами упаковки
// или возвращает NULL, если это другой метод сжатия или допущена ошибка в параметрах
COMPRESSION_METHOD* parse_CLS (char** parameters, void *method_template)
{
  if (strequ (parameters[0], ((CLS_METHOD*)method_template)->name)) {
    // Если название метода (нулевой параметр) соответствует названию проверяемого CLS метода, то запомним остальные параметры и возвратим его
    CLS_METHOD *p = new CLS_METHOD (*(CLS_METHOD*)method_template);
    // Save method parameters
    join (parameters+1, COMPRESSION_METHOD_PARAMETERS_DELIMITER, p->params, sizeof(p->params));
    return p;
  } else {
    return NULL;   // Это не метод CLS
  }
}


static int registered_methods = 0;
static HMODULE CLS_Handles[1000];
static void (*Saved_BeforeUnloadDLL)();

static void CLS_BeforeUnloadDLL()
{
  while (registered_methods)
  {
    registered_methods--;
    CLS_MAIN *ClsMain  =  (CLS_MAIN*) GetProcAddress (CLS_Handles [registered_methods], "ClsMain");
    ClsMain  &&  ClsMain(CLS_DONE, 0, 0);
    FreeLibrary (CLS_Handles [registered_methods]);
  }
  (*Saved_BeforeUnloadDLL)();
}

// Add CLS-enabled compressors from cls-*.dll
int AddClsCompressors()
{
    Saved_BeforeUnloadDLL = BeforeUnloadDLL;
    BeforeUnloadDLL = &CLS_BeforeUnloadDLL;

#ifdef FREEARC_WIN  // Non-Windows platforms aren't yet supported

    // Get program's executable/unarc.dll filename
    _MEMORY_BASIC_INFORMATION mbi;
    VirtualQuery ((void*)AddClsCompressors, &mbi, sizeof(MEMORY_BASIC_INFORMATION));

    TCHAR *path = new TCHAR[MY_FILENAME_MAX];  char *method_name = new char[MY_FILENAME_MAX];
    GetModuleFileName ((HINSTANCE__*)mbi.AllocationBase, path, MY_FILENAME_MAX);

    // Replace basename part with "cls-*.dll"
    TCHAR *basename = _tcsrchr (path, _T('\\')) + 1;
    _tcscpy (basename, _T("cls-*.dll"));

    // Find all cls-*.dll from program's directory
    WIN32_FIND_DATA FindData;
    HANDLE ff = FindFirstFile(path, &FindData);
    for (BOOL found = (ff!=INVALID_HANDLE_VALUE);  found;  found = FindNextFile(ff, &FindData))
    {
        // Put full DLL filename into `path`
        _tcscpy (basename, FindData.cFileName);

        // If DLL contains ClsMain() function - register it as dll-based compressor
        HMODULE dll = LoadLibrary(path);
        CLS_MAIN *ClsMain = (CLS_MAIN*) GetProcAddress (dll, "ClsMain");
        if (ClsMain)
        {
            // Call CLS module self-initialization
            ClsMain(CLS_INIT, 0, 0);

            // Register new CLS method in the global list of compression methods, "cls-TeSt.dll" will be registered as "test" compression method
            t_str_end(path)[-4] = _T('\0');            // remove ".dll" suffix
            _tcslwr (basename);
            utf16_to_utf8 (basename+4, method_name);   // skip "cls-" prefix
            CLS_METHOD *cls = new CLS_METHOD (method_name, ClsMain);
            AddExternalCompressionMethod (parse_CLS, cls);

            // Save DLL handle for unloading
            CLS_Handles[registered_methods] = dll;
            registered_methods++;
        }
    }
    delete[] path;  delete[] method_name;
#endif
    return registered_methods;
}


// Register all DLL-based compression algorithms
static int CLS_x = AddClsCompressors();

