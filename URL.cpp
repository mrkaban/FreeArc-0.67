#include "URL.h"
#include "Compression/Compression.h"

#define FREEARC_USER_AGENT "FreeArc/0.67"

// ****************************************************************************
// ** URL reading library
// ****************************************************************************

#ifdef FREEARC_NOURL

void  url_setup_proxy (char *_proxy)                           {}
void  url_setup_bypass_list (char *_bypass_list)               {}
URL*  url_open  (char *_url)                                   {return NULL;}
int   url_readp (URL *url, int64 offset, char *buf, int size)  {return -1;}
void  url_close (URL *url)                                     {}


#elif defined(FREEARC_WIN)

HINTERNET hInternet   = NULL;
char*     proxy       = NULL;
char*     bypass_list = "<local>";
int64 url_detect_size (URL *url);
int   url_readp (URL *url, int64 offset, char *buf, int size);


// DynaLoad function from wininet.dll
static FARPROC wininet_load (char *funcname, int i)
{
  static bool loaded = FALSE;
  static HMODULE dll = NULL;
  static FARPROC f[10];

  if (f[i])
    return f[i];

  if (!loaded)
  {
    loaded = TRUE;
    dll = LoadLibraryA("wininet.dll");
  }

  f[i] = GetProcAddress (dll, funcname);
  return f[i];
}

#define InternetCloseHandle  ((BOOL      WINAPI (*) (HINTERNET))                                                        wininet_load("InternetCloseHandle",0))
#define InternetOpenA        ((HINTERNET WINAPI (*) (LPCSTR,DWORD,LPCSTR,LPCSTR,DWORD))                                 wininet_load("InternetOpenA",      1))
#define InternetConnectA     ((HINTERNET WINAPI (*) (HINTERNET,LPCSTR,INTERNET_PORT,LPCSTR,LPCSTR,DWORD,DWORD,DWORD))   wininet_load("InternetConnectA",   2))
#define InternetOpenUrlA     ((HINTERNET WINAPI (*) (HINTERNET,LPCSTR,LPCSTR,DWORD,DWORD,DWORD))                        wininet_load("InternetOpenUrlA",   3))
#define HttpQueryInfoA       ((BOOL      WINAPI (*) (HINTERNET,DWORD,PVOID,PDWORD,PDWORD))                              wininet_load("HttpQueryInfoA",     4))
#define InternetReadFile     ((BOOL      WINAPI (*) (HINTERNET,PVOID,DWORD,PDWORD))                                     wininet_load("InternetReadFile",   5))
#define FtpOpenFileA         ((HINTERNET WINAPI (*) (HINTERNET,LPCSTR,DWORD,DWORD,DWORD))                               wininet_load("FtpOpenFileA",       6))
#define FtpGetFileSize       ((DWORD     WINAPI (*) (HINTERNET,LPDWORD))                                                wininet_load("FtpGetFileSize",     7))
#define FtpCommandA          ((BOOL      WINAPI (*) (HINTERNET,BOOL,DWORD,LPCSTR,DWORD_PTR,HINTERNET*))                 wininet_load("FtpCommandA",        8))


void url_setup_proxy (char *_proxy)
{
    proxy = strequ(_proxy,"--") ? NULL : strdup_msg (_proxy);
    hInternet && InternetCloseHandle (hInternet),  hInternet=NULL;
}


void url_setup_bypass_list (char *_bypass_list)
{
    bypass_list = _bypass_list[0]? strdup_msg (_bypass_list) : (char*)"<local>";
    hInternet && InternetCloseHandle (hInternet),  hInternet=NULL;
}


// Open file with given url and return handle for operations on the file
URL *url_init (char *_url)
{
    URL *url = (URL*) malloc(sizeof(URL));
    if (!url)  return NULL;
    url->url      = strdup_msg (_url);
    url->hConnect = NULL;
    url->curpos   = 0;
    url->hURL     = NULL;

    // �������������� WinInet
    hInternet = hInternet? hInternet :
           InternetOpenA(
             FREEARC_USER_AGENT,
             proxy? INTERNET_OPEN_TYPE_PROXY : INTERNET_OPEN_TYPE_PRECONFIG,
             proxy, bypass_list,
             0);
    if (!hInternet)  {url_close(url); return NULL;}

    // True, ���� ��� ftp url
    url->isFTP  =  (start_with (_url, "ftp://") != NULL);

    if (url->isFTP) {
        char *server = url->url + 6;
        url->file = strchr (server, '/');
        *url->file++ = '\0';

        char *user = NULL, *password = NULL;
        if (strchr (server, '@')) {
            user = server;
            server = strchr (server, '@');
            *server++ = '\0';
            password = strchr (user, ':');
            *password++ = '\0';
        }

        int portnum = INTERNET_DEFAULT_FTP_PORT;
        if (strchr (server, ':')) {
            char *port = strchr (server, ':');
            *port++ = '\0';
            portnum = atoi(port);
        }

        // ������ FTP ������
        url->hConnect = InternetConnectA(
                          hInternet,
                          server,
                          portnum,
                          user, password,
                          INTERNET_SERVICE_FTP,
                          0,
                          0);
        if (!url->hConnect)   {url_close(url); return NULL;}
    }

    return url;
}


//  4.1Gb - http://download.opensuse.org/distribution/10.3/iso/dvd/openSUSE-10.3-GM-DVD-i386.iso
//  4.1Gb - ftp://ftp.linuxcenter.ru/iso/Linuxcenter-games-collection-v2-dvd/lc-games-dvd.iso
int64 url_detect_size (URL *url)
{
    if (url) {
        if (url->isFTP)
        {
/*
            // Fast method of getting filesize
            WIN32_FIND_DATA FindFileData;
            HINTERNET hURL = FtpFindFirstFile (url->hConnect, url->file, &FindFileData, 0, 0);
            InternetCloseHandle (hURL);
            if (hURL && FindFileData.nFileSizeLow!=(DWORD)-1)
                return (int64(FindFileData.nFileSizeHigh)<<32)+FindFileData.nFileSizeLow;
*/
            // Slow method of getting filesize
            HINTERNET hURL = FtpOpenFileA (url->hConnect, url->file, GENERIC_READ, FTP_TRANSFER_TYPE_BINARY, 0);
            DWORD SizeHigh, SizeLow = FtpGetFileSize (hURL, &SizeHigh);
            InternetCloseHandle (hURL);
            //if (SizeLow==-1)  return 0;  - filesize may be exactly 4gb-1

            // DIRTY HACK!
            // Because for files >4gb truncated size is returned (with SizeHigh==0),
            // we try to download further parts of file in order to find its real size
            int64 Size;
            for(;;)
            {
                Size = (int64(SizeHigh)<<32) + SizeLow;
                char tmp[10];
                int res = url_readp (url, Size, tmp, 10);
                if (res<0)   return res;
                if (res<10)  break;
                SizeHigh++;
            }

            return Size;
        }

        // HTTP
        HINTERNET
        hURL = InternetOpenUrlA(
                 hInternet,             // session handle
                 url->url,              // URL to access
                 0, 0, INTERNET_FLAG_EXISTING_CONNECT, 0);

        DWORD dwStatusCode = 0;
        DWORD dwLengthStatusCode = sizeof(dwStatusCode);

        BOOL bQuery1 = HttpQueryInfoA(
            hURL,
            HTTP_QUERY_STATUS_CODE | HTTP_QUERY_FLAG_NUMBER,
            &dwStatusCode,
            &dwLengthStatusCode,
            NULL);

        char DataSize[100];
        DWORD dwLengthDataSize = sizeof(DataSize)-1;

        BOOL bQuery2 = HttpQueryInfoA(
            hURL,
            HTTP_QUERY_CONTENT_LENGTH,
            &DataSize,
            &dwLengthDataSize,
            NULL);

        InternetCloseHandle (hURL);

        DataSize[dwLengthDataSize] = '\0';
        return bQuery1&&dwStatusCode==HTTP_STATUS_OK? (bQuery2? atoll(DataSize) : -2) : -1;
    }

    return -1;
}


// Check existence of given url
int url_exists (char *_url)
{
    URL *url = url_init(_url);

    int size = url_detect_size (url);
    url_close(url);

    return size>=0 || size==-2;
}


// Open file with given url, fill url->size and return handle for operations on the file
URL *url_open (char *_url)
{
    URL *url = url_init(_url);

    url->IsCheckNews  =  (strstr(_url,"freearc.org/CheckNews.") != NULL);

    url->size = URL_BUFSIZE;
    if (url->IsCheckNews) {
        url->size = url_readp (url, 0, url->buf, URL_BUFSIZE);
        url_seek (url, 0); }
    if (url->size >= URL_BUFSIZE)
        url->size = url_detect_size (url);
    if (url->size < 0)
        {url_close(url); return NULL;}

    return url;
}


int url_readp (URL *url, int64 offset, char *buf, int size)
{
    if (size==0)  return 0;
    if (!url)     return -1;

    // Small files/reads for CheckNews are served directly from url->buf
    if (url->IsCheckNews && offset==0 && url->size<URL_BUFSIZE) {
        int bytes = min(size,min(url->size,URL_BUFSIZE));
        memcpy (buf, url->buf, bytes);
        return bytes;
    }

    // End of data that should be read by the new operation.
    // If we asked exactly for 64kb/256kb/8mb - this is probably just beginning of large block,
    // so we switch to reading all the data until file end
    int64 endpos = size==BUFFER_SIZE || size==LARGE_BUFFER_SIZE || size==HUGE_BUFFER_SIZE
                     ? 0 : offset+size;

    // Make connection to desired url. Continue previous read operation
    // if it finished exactly where we want to start, otherwise close its handle
    if (url->hURL && url->curpos!=offset)
        url_reset(url);

    BOOL new_hURL = (url->hURL==NULL);
    if (new_hURL) {
        if (url->isFTP)
        {
            char Rest[100];
            sprintf (Rest, "REST %.0lf", double(offset));
            FtpCommandA (url->hConnect, FALSE, FTP_TRANSFER_TYPE_BINARY, Rest, 0, NULL);
            url->hURL = FtpOpenFileA (url->hConnect, url->file, GENERIC_READ, FTP_TRANSFER_TYPE_BINARY, 0);
        } else {
            // Ask server for required range of bytes
            char Range[100];
            if (endpos==0)
              then sprintf (Range, "Range: bytes=%.0lf-\r\n",      double(offset));
              else sprintf (Range, "Range: bytes=%.0lf-%.0lf\r\n", double(offset), double(endpos-1));
            url->hURL = InternetOpenUrlA (hInternet, url->url, Range,DWORD(-1L), INTERNET_FLAG_EXISTING_CONNECT, 0);
        }
    }
    if (!url->hURL)  return -1;

    int bytes;  // ������� ���� ��� ���������
    for (bytes=0; bytes<size;)
    {
        DWORD dwBytesRead;
        // ������ ������
        InternetReadFile (url->hURL,  buf, size-bytes,  &dwBytesRead);

        // ����� �� ����� ��� ������ ��� ����������
        if (dwBytesRead == 0)
            break;

        buf         += dwBytesRead;
        bytes       += dwBytesRead;
        url->curpos += dwBytesRead;
    }

    // Close read handle if we've read all the data asked by calling procedure
    if (url->curpos == endpos)  url_reset(url);
    // If not all the data are read and we have chances to read something more - try it!
    if (!url->IsCheckNews && bytes<size && !(new_hURL && bytes==0))
    {
        url_reset(url);
        int ret = url_readp (url, offset+bytes, buf, size-bytes);
        return ret>=0? bytes+ret : ret;
    }
     return bytes;
}


void url_reset (URL *url)
{
    if (!url) return;
    InternetCloseHandle (url->hURL);
    url->hURL = NULL;
}


void url_close (URL *url)
{
    if (!url) return;
    free (url->url);
    InternetCloseHandle (url->hURL);
    InternetCloseHandle (url->hConnect);
    free (url);
}


#else // Unix


char *proxy = NULL;
void  url_setup_proxy (char *_proxy)
{
    proxy = strequ(_proxy,"--") ? NULL : strdup_msg (_proxy);
}

void  url_setup_bypass_list (char *_bypass_list)
{
}

static size_t NoWriteCallback(void *ptr, size_t size, size_t nmemb, void *data)
{
    return -1;
}

static size_t WriteMemoryCallback(void *ptr, size_t size, size_t nmemb, void *data)
{
    char **buf = (char **) data;
    memcpy (*buf, ptr, size*nmemb);
    *buf += size*nmemb;
    return size*nmemb;
}

URL* url_open (char *_url)
{
    static bool curl_inited = FALSE;
    if (!curl_inited)  curl_global_init(CURL_GLOBAL_ALL), curl_inited=TRUE;

    URL *url = (URL*) malloc(sizeof(URL));
    if (!url)  return NULL;
    url->url      = strdup_msg (_url);
    url->curpos   = 0;

    /* init the curl session */
    url->curl_handle = curl_easy_init();

    /* set proxy if it was specified by user */
    if (proxy)  curl_easy_setopt (url->curl_handle, CURLOPT_PROXY, proxy);

    /* some servers don't like requests that are made without a user-agent
       field, so we provide one */
    curl_easy_setopt (url->curl_handle, CURLOPT_USERAGENT, FREEARC_USER_AGENT);

    /* specify URL to get */
    curl_easy_setopt (url->curl_handle, CURLOPT_URL, url->url);

    // Get file size
    curl_easy_setopt (url->curl_handle, CURLOPT_WRITEFUNCTION, NoWriteCallback);
    CURLcode res = curl_easy_perform (url->curl_handle);
    if (CURLE_OK!=res && CURLE_WRITE_ERROR!=res)   {url_close(url); return NULL;}

    long response;
    curl_easy_getinfo (url->curl_handle, CURLINFO_RESPONSE_CODE, &response);
    if (response!=200 && response!=301 && response!=302)   {url_close(url); return NULL;}

    double size;
    res = curl_easy_getinfo (url->curl_handle, CURLINFO_CONTENT_LENGTH_DOWNLOAD, &size);
    if (CURLE_OK != res)   {url_close(url); return NULL;}
    url->size = (int64)size;

    /* send all data read to this function  */
    curl_easy_setopt (url->curl_handle, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);

    return url;
}

// Check existence of given url
int url_exists (char *_url)
{
    URL *url = url_open(_url);
    url_close(url);
    return url!=NULL;
}

int url_readp (URL *url, int64 offset, char *buf, int size)
{
    if (size==0)  return 0;
    if (!url)     return -1;

    /* specify byte range to get */
    char Range[100];
    sprintf (Range, "%.0lf-%.0lf", double(offset), double(offset+size-1));
    curl_easy_setopt (url->curl_handle, CURLOPT_RANGE, Range);

    /* we pass address of 'current write pointer' variable to the callback function */
    char *ptr = buf;
    curl_easy_setopt (url->curl_handle, CURLOPT_WRITEDATA, (void *)&ptr);

    /* get it! */
    curl_easy_perform (url->curl_handle);

    return ptr-buf;
}

void url_reset (URL *url)
{
}

void url_close (URL *url)
{
    if (!url) return;
    /* cleanup curl stuff */
    curl_easy_cleanup (url->curl_handle);

    free (url->url);
    free (url);
}


#endif // Windows/Unix


int64 url_size (URL *url)                      {return url? url->size   : 0;}
int64 url_pos  (URL *url)                      {return url? url->curpos : 0;}
void  url_seek (URL *url, int64 newpos)        {if (url)  url->curpos = newpos, url_reset(url);}
int   url_read (URL *url, char *buf, int size) {return url? url_readp (url, url->curpos, buf, size) : -1;}
