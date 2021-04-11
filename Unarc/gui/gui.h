#ifndef _WA_GUI_INTERFACE
#define _WA_GUI_INTERFACE

#include <windows.h>
#include <tchar.h>
#include <time.h>


#ifndef FREEARC_WIN
typedef unsigned long long uint64;
#define MY_FILENAME_MAX MAX_PATH
#endif


class GUI : public BASEUI
{
public:
	static bool isInitialized;

	WORD button;
	TCHAR destinationDirectory[MY_FILENAME_MAX*2];
	char destinationDirectory_utf8[MY_FILENAME_MAX*4];
	char *pwd;
	int pwdSize;
	HWND hWndProgress;
	volatile bool progressResult;
	volatile char replaceDialogResult;
	volatile char AskPasswordResult;
	HICON hIcon;
	HICON hFileIcon;
	HANDLE thread;
	TCHAR filename[MY_FILENAME_MAX*2];
	TCHAR *msg;    // memory for formatting various messages before placing them to the screen
	uint64 totalBytes;
	uint64 readBytes;
	uint64 writtenBytes;
	uint64 lastWrittenBytes;
	int silent;
	HANDLE hEvent;
	TCHAR replacedFileName[MY_FILENAME_MAX*2];
	uint64 replacedFileSize;
	time_t replacedFileModified;
	DWORD tickCountOfBegin;

	GUI();
	~GUI();
	void DisplayHeader(char* header);
	bool AllowProcessing(char cmd, int silent, TCHAR *arcname, char* utf8comment, int cmtsize, char* utf8outdir);
	char*GetOutDir();
	bool BeginProgress(uint64 totalBytes);
	bool ProgressRead (uint64 readBytes);
	bool ProgressWrite(uint64 writtenBytes);
	bool ProgressFile (bool isdir, const char *operation, TCHAR *filename, uint64 filesize);
	bool ShowProgress();
	void EndProgress(COMMAND*);
	char AskOverwrite(TCHAR *filename, uint64 size, time_t modified);
	char AskPassword(char *pwd, int pwdbuf_size);
};

#endif
