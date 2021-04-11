#ifdef __cplusplus
extern "C" {
#endif

#ifdef _WIN32
#include "ThreadsWin32.h"
#else
#include "ThreadsUnix.h"
#endif

#ifdef __cplusplus
}
#endif
