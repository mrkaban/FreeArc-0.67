unarc.dll exports single function FreeArcExtract, implementing extraction of FreeArc archives, as well as
archive testing and getting basic archive statistics. You can look at examples of using unarc.dll
using various programming laguages (C++, Delphi...) in appropriate subdirectories.

It should be called in the following way:

errcode = FreeArcExtract(callback, command[1], command[2], command[3], ...);

where errcode is the error code returned (FREEARC_OK==0 on success, error codes are FREEARC_ERRCODE_* in the Common.h)
command[1]... - words of the command, that should be executed, it should be finished with either NULL or "".
The syntax of supported commands can be seen by running unarc.exe without parameters.
callback - your function that will be called from FreeArcExtract, it may be NULL.

Usage example:

int errcode = FreeArcExtract(callback, "x", "-o+", "--", "a.arc", "*.obj", "*.lib", NULL);

You can try to call FreeArcExtract with various command lines using CPP/UnarcDllExample.exe.
You should supply to it unarc command line, and it will print parameters of callback() calls performed by FreeArcExtract.

Now the following callbacks are performed:

callback("total", TotalBytes>>20, TotalBytes, "") - performed at the start of unpacking and passes total archive size

callback("read",  ReadBytes>>20,  ReadBytes,  "") - called multiple times due unpacking and passes position inside archive,
allowing you to display progress indicator as readBytes/totalBytes (note that it will be correct only when entire archive is extracted).

callback("write", WrittenBytes>>20, WrittenBytes, "") - called multiple times due unpacking and passes total size of data already decompressed.
If you know how much data woulkd be decompressed due operation, it will allow you to display more accurate progress indicator.

callback("filename", Filesize>>20, Filesize, Filename) - called at the start of unpacking of file named Filename and having size Filesize.

callback("overwrite?", Filesize>>20, Filesize, Filename) - asks for permission for overwriting of existing disk file with file from archive
named Filename and having size Filesize. Value returned from the callback should be one of the following:
    'y' - allow overwriting
    'n' - don't allow overwriting
    'a' - allow overwriting of all files, don't ask anymore
    's' - don't allow overwriting of any file, don't ask anymore
    'q' - abort operation

callback("password?", PasswordBuf_Size, 0, PasswordBuf) - requests password, that should be placed as UTF8Z string to the buffer PasswordBuf
having PasswordBuf_Size bytes. If password is wrong, this callback will be called again and again until you will provide correct password or
return 'n' from the call. Value returned from the callback should be one of the following:
    'y' - password placed to the buffer
    'n' - there is no (more) passwords
    'q' - abort operation

callback("error", Errcode, 0, Errmsg) - informs you that error was encountered due operation, passing error code Errcode (see FREEARC_ERRCODE_*
in Common.h) and textual error description Errmsg.

Negative value returned from callbacks other than "overwrite?", "password?" and "error" considered as errcode of error encountered when
performing callback, and unpacking stops immediately. Value returned from the "error" callback is ignored.

If callback==NULL, then callback "overwrite?" returns value 's', callback "password?" returns 'n', other callbacks return 0.



unarc.dll, unlike unarc.exe, doesn't support the 'v' command and assigns different meaning to the 'l' command: it provides archive statistics
using the following callbacks:

callback("total_files", TotalFiles, 0, "") - number of files in the archive
callback("origsize", OrigSize>>20, OrigSize, "") - total original (uncompressed) size of files in the archive
callback("compsize", CompSize>>20, CompSize, "") - total compressed size of files in the archive

Value returned from these callbacks are ignored.
