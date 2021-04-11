/***********************************************************
 *                         ALL2ARC
 ***********************************************************
 *  Author: Radek "Black_Fox" Liska
 * License: GPL v2
 *
 * This program aims at <any archive> -> FreeArc
 * conversion using 7-zip's capabilities. Windows only.
 ***********************************************************
 *
 * Possible methods of use:
 *   all2arc (FreeArc options) <one supported archive>
 *   all2arc (FreeArc options) -- <many supported archives>
 *
 *  Note: You need 7zG.exe and 7z.dll in your path.
 * Note2: Use this tool at your own risk, no guarantees!
 *
 ***********************************************************
 *
 *  TO DO:
 * --------------------
 * -proper extension stripping (tar.bz2 vs tbz2)
 * -allow user to recompress into more formats 7z allows (7z, zip...) 
 *
 *  History:
 * ---------
 *  2009-10-21 v0.61
 *    Obtain archive name only once (second pass was superfluous and also wrong)
 *    Fixed a bug in .tar.xyz detection when processing multiple archives
 *    Changed behavior of "Overwrite archive?" - "NO" skips one file and "CANCEL" skips all files
 *  
 *  2009-10-15 v0.60
 *    Set modification time of the new archive to be the same as in original one
 *  
 *  2009-08-15 v0.52
 *    Cancel all operations after any of external programs fails
 *    For temporary files use directory obtained from TEMP environment variable
 *
 *  2009-06-07 v0.51
 *    Unicode support tested with CZE, SVK, RUS, CHN and JPN characters in filename => works
 *
 *    Following changes by Bulat Ziganshin, thanks:
 *     Fixed wrong allocation size, all crashes should be gone now
 *     Optimized temporary directory creation
 *     Multiple small commandline improvements
 *
 *  2009-05-27 v0.5
 *    Preliminary Unicode support
 *    Added processing of multiple archives at once ("all2arc.exe -- archive1.rar archive2.7z")
 *    Possibility to pass arguments to FreeArc ("all2arc.exe -mx -md128 archive1.rar")
 *    Compilation parameters improvements, resulting in smaller file (thanks Bulat)
 *    Showing errors/questions in GUI through MessageBox
 *    Improved check for existing archives
 *    Improved cleaning up of temp folders in case of failure
 *
 *  2009-05-12 v0.21
 *    Fixed context menu
 *
 *  2009-05-11 v0.2
 *    GCC 4.4.0 used for compilation, which uncovered some warnings - silenced
 *    Improved functionality for tar.sth archives
 *    Unique temp directories are now created instead of hardcoded "temp"
 *
 *  2009-05-07 v0.1
 *    First semi-public version
 *
 ******************************************************/

#include <cstdlib>
#include <cstdio>
#include <cstdarg>

#include <string>


/*#ifdef _MBCS
    #undef _MBCS
#endif*/
#define _UNICODE
#define UNICODE

#define _WIN32_WINNT 0x500
#include <windows.h>

#define MAX_LEN 1024

using namespace std;

#ifdef DEBUG
    bool showDebug = true;
    #define MessageBoxW(a,b,c,d) fwprintf(stderr, b)
#else
    bool showDebug = false;
#endif


/* deletes directory, including all contained files */
bool deleteDirectory(LPCWSTR lpszDir, bool noRecycleBin = true) {
  wint_t len = wcslen(lpszDir);
  wchar_t *pszFrom = new wchar_t[len+2];
  wcscpy(pszFrom, lpszDir);
  pszFrom[len] = 0;
  pszFrom[len+1] = 0;

  SHFILEOPSTRUCT fileop;
  fileop.hwnd   = NULL;      // no status display
  fileop.wFunc  = FO_DELETE; // delete operation
  fileop.pFrom  = pszFrom;   // source file name as double null terminated string
  fileop.pTo    = NULL;      // no destination needed
  fileop.fFlags = FOF_NOCONFIRMATION | FOF_SILENT;  // do not prompt the user

  if(!noRecycleBin) {
      fileop.fFlags |= FOF_ALLOWUNDO;
  }

  fileop.fAnyOperationsAborted = FALSE;
  fileop.lpszProgressTitle     = NULL;
  fileop.hNameMappings         = NULL;

  int ret = SHFileOperation(&fileop);
  delete [] pszFrom;
  if (fileop.fAnyOperationsAborted) {
      MessageBoxW(GetConsoleWindow(), L"some deleting operations were aborted!\n", L"All2Arc", MB_OK | MB_ICONWARNING);
  }

  return (ret == 0);
} //thx http://www.codeguru.com/forum/showthread.php?t=239271


/* executes an application and waits for it to end */
int runApp(const wchar_t *pszCommandLine, const wchar_t *pszWorkingDir, DWORD *returnVal) {
    STARTUPINFO         siStartupInfo;
    PROCESS_INFORMATION piProcessInfo;
    memset(&siStartupInfo, 0, sizeof(siStartupInfo));
    memset(&piProcessInfo, 0, sizeof(piProcessInfo));
    siStartupInfo.cb = sizeof(siStartupInfo);

    wchar_t *buf = new wchar_t[wcslen(pszCommandLine)+1];
    wcscpy(buf, pszCommandLine);
    
    if(CreateProcess(NULL,                       // App name
                     buf,                        // (App name) + arguments
                     NULL,                       // Process attributes
                     NULL,                       // Thread attributes
                     FALSE,                      // Inherit handles
                     NORMAL_PRIORITY_CLASS,      // Creation flags
                     NULL,                       // Environment
                     pszWorkingDir,              // Current directory
                     &siStartupInfo,             // Startup info
                     &piProcessInfo) == FALSE) { // Process info
        return GetLastError();
    } else {
        //wait until process ends
        WaitForSingleObject(piProcessInfo.hProcess, INFINITE);

        //get process exit code
        DWORD exitCode;
        if (!GetExitCodeProcess(piProcessInfo.hProcess, &exitCode)) {
            exitCode = 1;
        }
        *returnVal = exitCode;
    }
    delete [] buf;
    return 0;
}


/* Checks whether the file exists */
bool fileExists(const wchar_t *filename) {
    return (GetFileAttributes(filename) != 0xFFFFFFFF);
}


/* Deletes temporary directories */
void deleteTempFolders(const wchar_t *tempfolder) {
    wchar_t szDirectory [MAX_PATH];
    wcscpy(szDirectory, tempfolder);
    if (showDebug) wprintf(L">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
    if (showDebug) wprintf(L"deleting >>%s<<\n",szDirectory);
    if (!(deleteDirectory(szDirectory))) {
        wchar_t szlastError [MAX_LEN];
        swprintf(szlastError, L"tempfolder deletion failed with code %d\n", GetLastError());
        MessageBoxW(GetConsoleWindow(), szlastError, L"All2Arc", MB_OK | MB_ICONWARNING );
    }
    return;
}


/****************************************************************/
int main() {
    /* gcc doesn't know wmain, let's work it around */
    int argc;
    wchar_t **argv = CommandLineToArgvW(GetCommandLineW(), &argc);
    
    /* define variables */
    HWND hwndConsole = GetConsoleWindow();
    wstring filename,workingdir,freearcworkingdir,tempdir,commandline,freearcparams,archivename;
    int lastError = ERROR_SUCCESS;
    wchar_t msgbox [MAX_LEN]; //here are stored error messages which will be displayed in MessageBox
    bool addToCmd = true; //are we still adding parameters before processing files?
    bool compressedTar; //is it a compressed TAR?
    DWORD exitCode; //holds return codes from external applications
    HANDLE fileHnd, archiveHnd; //file handles to set modify times appropriately
    FILETIME modifyTme;

    /* check parameter count */
    if (argc < 2) {
        MessageBox(hwndConsole, L"All2Arc v0.60\nusage: all2arc (FreeArc parameters) [archive | -- archives]\n",
                                L"All2Arc", MB_OK | MB_ICONWARNING);
        return EXIT_FAILURE;
    }

    /* repeat for each file/parameter*/
    for (int k = 1; k < argc; k++) {

        compressedTar = false;
        exitCode = 0;
        fileHnd = INVALID_HANDLE_VALUE;
        archiveHnd = INVALID_HANDLE_VALUE;

        /* separate FreeArc parameters from archives to recompress */
        if (addToCmd && (k < argc-1)) {
            if (!wcscmp(argv[k],L"--")) {
                addToCmd = false;
            } else {
                freearcparams = freearcparams + L" " + argv[k];
            }
            continue;
        }
        
        
        if (showDebug) wprintf(L">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
        if (showDebug) wprintf(L"NOW PROCESSING ARCHIVE: %s\n", argv[k]);
        
        
        /* check file existence */
        filename = argv[k];
        if (!fileExists(filename.c_str())) {
            MessageBox(hwndConsole, L"file does not exist!\n", L"All2Arc", MB_OK | MB_ICONWARNING);
            return EXIT_FAILURE;
        }


        /* get original archive handle and modification time */
        if ((fileHnd = CreateFile(filename.c_str(), GENERIC_READ, FILE_SHARE_READ|FILE_SHARE_WRITE, 
                                  NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL)) == INVALID_HANDLE_VALUE) {
            swprintf(msgbox, L"Original archive handle retrieval failed with code %d\n", GetLastError());
            MessageBox(hwndConsole, msgbox, L"All2Arc", MB_OK | MB_ICONWARNING);
            return EXIT_FAILURE;
        }
        if (!GetFileTime(fileHnd, NULL, NULL, &modifyTme)) {
            swprintf(msgbox, L"Modified time retrieval failed with code %d\n", GetLastError());
            MessageBox(hwndConsole, msgbox, L"All2Arc", MB_OK | MB_ICONWARNING);
            return EXIT_FAILURE;
        }
        CloseHandle(fileHnd);


        /* check if we have tar.something archive */
        //http://en.wikipedia.org/wiki/Tar_(file_format)
        wchar_t *a[] = {(wchar_t*)L".tar.gz",(wchar_t*)L".tar.bz2",(wchar_t*)L".tar.lzma",(wchar_t*)L".tar.Z"/*,
                      (wchar_t*)".tgz",(wchar_t*)".tbz",(wchar_t*)".tbz2",(wchar_t*)".tb2",(wchar_t*)".taz"*/};
        if (showDebug) wprintf(L">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
        unsigned int arrsize = sizeof(a)/sizeof(*a);
        for (unsigned int x = 0; x < arrsize; x++) {
            if (!compressedTar) {
                int pos1 = filename.rfind(a[x]);
                //if ((found something at all) && (it was really the extension))
                compressedTar = (pos1 >= 0 && pos1 - (filename.size()-wcslen(a[x])) == 0);
            } else {
                if (showDebug) wprintf(L"dual-layer archive found\n");
                break;
            }
        }


        /* get archive name and check whether such file already exists */
        if (compressedTar) {
            archivename = filename.substr(0,filename.rfind(L"."));
            archivename = archivename.substr(0,archivename.rfind(L".")) + L".arc";
        } else {
            archivename = filename.substr(0,filename.rfind(L".")) + L".arc";
        }
        /* offer overwriting */
        if (fileExists(archivename.c_str())) {
            if (showDebug) {
                MessageBox(hwndConsole, L"archive already exists, cannot overwrite!\n", L"All2Arc", MB_OK | MB_ICONWARNING);
                return EXIT_FAILURE;
            } else {
                swprintf(msgbox, L"Archive \"%ls\" already exists, overwrite?\n", archivename.c_str());
                int ret = MessageBox(NULL, msgbox, L"All2Arc", MB_ICONWARNING | MB_YESNOCANCEL | MB_DEFBUTTON2);
                switch (ret) {
                    case IDCANCEL: return EXIT_FAILURE; break;
                    case IDNO: continue; break;
                    case IDYES:
                        if (!DeleteFile(archivename.c_str())) {
                            swprintf(msgbox, L"archive deletion failed with code %d\n", GetLastError());
                            MessageBox(hwndConsole, msgbox, L"All2Arc", MB_OK | MB_ICONWARNING);
                            return EXIT_FAILURE;
                        }
                        break;
                }
            }
        }


        /* properly set workingdir & filename */
        workingdir = filename = argv[k];
        if (workingdir.rfind(L"\\") == string::npos) {
            wchar_t szDirectory [MAX_PATH];
            GetCurrentDirectory(sizeof(szDirectory) - 1, szDirectory);
            workingdir.assign(szDirectory);
            workingdir += L"\\";
        } else {
            workingdir = workingdir.substr(0,workingdir.rfind(L"\\")+1);
        }
        if (filename.rfind(L"\\") != string::npos) {
            filename = filename.substr(filename.rfind(L"\\")+1, filename.size()-filename.rfind(L"\\")-1);
        }
        
        if (showDebug) wprintf(L"archivename = %s\n",archivename.c_str());


        /* create unique temp directory */
        //try to use environment variable
        if (_wgetenv(L"TEMP")) {
            tempdir.assign(_wgetenv(L"TEMP"));
            if (tempdir.find(L";") != string::npos) {
                tempdir = tempdir.substr(0,tempdir.find(L";"));
            }
            if (showDebug) wprintf(L"TEMP env. var. exists: %s\n",tempdir.c_str());
        } else {
            //we need this for step 2/3
            tempdir = workingdir;
        }
        //create a file with unique name
        wchar_t *pszTempFile = new wchar_t [MAX_PATH];
        if (!GetTempFileName(tempdir.c_str(),L"arc",0,pszTempFile)) {
            swprintf(msgbox, L"tempdir creation step 1/3 failed with code %d\n", GetLastError());
            MessageBox(hwndConsole, msgbox, L"All2Arc", MB_OK | MB_ICONWARNING);
            return EXIT_FAILURE;
        }
        tempdir.assign(pszTempFile);
        delete [] pszTempFile;
        //delete the file and use its name...
        if (!DeleteFile(tempdir.c_str())) {
            swprintf(msgbox, L"tempdir creation step 2/3 failed with code %d\n", GetLastError());
            MessageBox(hwndConsole, msgbox, L"All2Arc", MB_OK | MB_ICONWARNING);
            return EXIT_FAILURE;
        }
        //...to create a directory
        if (!CreateDirectory(tempdir.c_str(),NULL)) {
            swprintf(msgbox, L"tempdir creation step 3/3 failed with code %d\n", GetLastError());
            MessageBox(hwndConsole, msgbox, L"All2Arc", MB_OK | MB_ICONWARNING);
            return EXIT_FAILURE;
        }
        if (showDebug) wprintf(L"unique temp dir is %s\n",tempdir.c_str());


        /* decompress archive with 7-zip */
        commandline = L"7zG x -o\"" + tempdir + L"\" -- \"" + workingdir + filename + L"\"";
        if (showDebug) wprintf(L">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
        if (showDebug) wprintf(L"1st commandline = %s\n",commandline.c_str());
        lastError = runApp(commandline.c_str(), NULL, &exitCode);
        if ((lastError != ERROR_SUCCESS || exitCode > 0)) {
            swprintf(msgbox, L"7zG 1st execution failed with codes %d/%d\n", lastError, exitCode);
            MessageBox(hwndConsole, msgbox, L"All2Arc", MB_OK | MB_ICONWARNING);
            deleteTempFolders(tempdir.c_str());
            return EXIT_FAILURE;
        }


        /* if there's tar remaining, decompress it once more */
        if (compressedTar) {
            filename = filename.substr(0,filename.rfind(L"."));
            commandline = L"7zG x -o\"" + tempdir + L"\\temp\" -- \"" +
                                          tempdir + L"\\" + filename + L"\"";
            if (showDebug) wprintf(L">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
            if (showDebug) wprintf(L"2nd commandline = %s\n",commandline.c_str());
            lastError = runApp(commandline.c_str(), NULL, &exitCode);
            if ((lastError != ERROR_SUCCESS || exitCode > 0)) {
                swprintf(msgbox, L"7zG 2nd execution failed with codes %d/%d\n", lastError, exitCode);
                MessageBox(hwndConsole, msgbox, L"All2Arc", MB_OK | MB_ICONWARNING);
                deleteTempFolders(tempdir.c_str());
                return EXIT_FAILURE;
            }
        }


        /* compress uncompressed files with FreeArc */
        commandline = L"FreeArc create" + freearcparams + L" -r -- " + L"\"" + workingdir + archivename + L"\"";
        freearcworkingdir = tempdir + ((compressedTar)? L"\\temp\\" : L"\\");
        if (showDebug) wprintf(L">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
        if (showDebug) wprintf(L"last commandline = %s\n",commandline.c_str());
        if (showDebug) wprintf(L" FreeArc workdir = %s\n",freearcworkingdir.c_str());
        lastError = runApp(commandline.c_str(),freearcworkingdir.c_str(), &exitCode);
        if ((lastError != ERROR_SUCCESS || exitCode > 0)) {
            swprintf(msgbox, L"FreeArc execution failed with codes %d/%d\n", lastError, exitCode);
            MessageBox(hwndConsole, msgbox, L"All2Arc", MB_OK | MB_ICONWARNING);
            deleteTempFolders(tempdir.c_str());
            return EXIT_FAILURE;
        }
        
        
        /* set output archive modification time, only warn and don't fail when unsuccessful */
        if ((archiveHnd = CreateFile(archivename.c_str(), GENERIC_READ|GENERIC_WRITE, FILE_SHARE_READ|FILE_SHARE_WRITE, 
                                  NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL)) == INVALID_HANDLE_VALUE) {
            swprintf(msgbox, L"Converted archive handle retrieval failed with code %d\n", GetLastError());
            MessageBox(hwndConsole, msgbox, L"All2Arc", MB_OK | MB_ICONWARNING);
            return EXIT_FAILURE;
        }
        if (!SetFileTime(archiveHnd, NULL, NULL, &modifyTme)) {
            swprintf(msgbox, L"Modified time update failed with code %d\n", GetLastError());
            MessageBox(hwndConsole, msgbox, L"All2Arc", MB_OK | MB_ICONINFORMATION);
        }
        CloseHandle(archiveHnd);


        /* clean up temp folders */
        deleteTempFolders(tempdir.c_str());
    }

    wprintf(L"--All operations finished successfully!--\n");
    return EXIT_SUCCESS;
}
