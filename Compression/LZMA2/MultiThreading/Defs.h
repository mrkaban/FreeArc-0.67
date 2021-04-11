// Windows/Defs.h

#ifndef __WINDOWS_DEFS_H
#define __WINDOWS_DEFS_H

#ifdef _WIN32
inline bool BOOLToBool(BOOL v) { return (v != FALSE); }
inline BOOL BoolToBOOL(bool v) { return (v ? TRUE: FALSE); }
#endif

#endif
