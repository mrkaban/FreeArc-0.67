#include <time.h>
#include "Compression/Common.h"

#ifdef  __cplusplus
extern "C" {
#endif

// Environment.cpp
int long_path_size (void);                                 // Максимальная длина имени файла
void FormatDateTime (char *buf, int bufsize, time_t t);    // Отформатировать время/дату для команды листинга
CFILENAME GetExeName (CFILENAME buf, int bufsize);         // Вернуть имя исполняемого файла программы
int MyGetAppUserDataDirectory (CFILENAME buf);             // Вернуть %APPDATA%
unsigned GetMaxBlockToAlloc (void);                        // Макс. объём блока, который мы можем выделить в адресном пространстве нашего процесса
unsigned GetTotalMemoryToAlloc (void);                     // Общий объём памяти который мы можем выделить в адресном пространстве нашего процесса
void TestMalloc (void);                                    // Печатает статистику свободной памяти
int PowerOffComputer();                                    // Инициировать выключение компьютера
void GetOSDisplayString(char* buf);                        // Заполняет буфер строкой с описанием версии ОС
void memxor (char *dest, char *src, uint size);            // От-xor-ить два блока данных

#ifdef FREEARC_WIN
// Operations on mutex shared by all FreeArc instances
HANDLE myCreateMutex  (char*  name);
void   myCloseMutex   (HANDLE hMutex);
void   myWaitMutex    (HANDLE hMutex);
void   myGrabMutex    (HANDLE hMutex);
void   myReleaseMutex (HANDLE hMutex);

DWORD RegistryDeleteTree(HKEY hStartKey, LPTSTR pKeyName);  // Delete entrire subtree from Windows Registry
#endif

// GuiEnvironment.cpp
int BrowseForFolder(TCHAR *prompt, TCHAR *in_filename, TCHAR *out_filename);                      // Дать пользователю выбрать каталог
int BrowseForFile(TCHAR *prompt, TCHAR *filters, TCHAR *in_filename, TCHAR *out_filename);        // Дать пользователю выбрать файл
void GuiFormatDateTime (time_t t, char *buf, int bufsize, char *date_format, char *time_format);  // Превратить время/дату файла в строку в соответствии с настройками locale или заданными форматами времени и даты
void GuiGetFileType (TCHAR *ext, TCHAR *buf);                                                     // Получить тип файла по его расширению

#ifdef  __cplusplus
}
#endif
