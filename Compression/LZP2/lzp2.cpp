/*
   LZP2 - Fast LZP compression algorithm.
   Copyright (C) 2012, Bulat Ziganshin.
   BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)
   You can contact the author at:
   - my email: bulat.ziganshin@gmail.com
   - my homepage: http://freearc.org/Research.aspx
   - LZP2 source repository: http://freearc.org:8002/freearc/file - go to Compression/LZP2

   Compilation defines:
   LZPE             if enabled, use chars 128..255 to encode match lengths; may be used only for English texts
   HASHBYTES=2..4   number of bytes used to find match
   MINLEN           minimum match length
   HASHLOG          logarithm of hash size (in elements, each element is a char*)
   MAX_OFFSET       maximum match offset (in order to improve caching)
*/

#include <io.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

//#define LZPE                    /* Use chars 128..255 to encode match lengths; may be used only for English texts */

#ifndef HASHBYTES
#define HASHBYTES   2             /* hashed bytes */
#endif
#ifndef MINLEN
#define MINLEN      2             /* minimum match length */
#endif
#ifndef HASHLOG
#define HASHLOG     16
#endif
#define HASHSIZE    (1<<HASHLOG)  /* number of hash elements */
#ifndef MAX_OFFSET
#define MAX_OFFSET  65536         /* maximum match offset (in order to improve caching) */
#endif

// Calculate HASHLOG-bits wide hash of HASHBYTES bytes preceding p
#define calchash(p) (((*(unsigned*)(p-HASHBYTES) & ((1<<(HASHBYTES*8))-1))*123456791) >> (32-HASHLOG))

int compress (char* inbuf, int insize, char* outbuf)
{
    char* hash[HASHSIZE];
    for (int i=0; i<HASHSIZE; i++)  hash[i] = 0;

    char *outptr=outbuf, *ptr=inbuf+HASHBYTES, *last_ptr=inbuf, *bufend=inbuf+insize;

    while (ptr<bufend)
    {
        unsigned h = calchash(ptr);
        char* match = hash[h];  hash[h] = ptr;
        int len=0;
        if (match && ptr-match<MAX_OFFSET)
        {
            while (ptr+len<bufend && ptr[len]==match[len])
                len++;
        }
        if (len>MINLEN || ptr+len>=bufend)
        {
            // Output literal last_ptr...ptr and match with length len
            int litlen=ptr-last_ptr; int orig_len=len, orig_litlen = litlen;
//fprintf(stderr, " %d: %d/%d\n", last_ptr-inbuf, litlen, len);
            len -= MINLEN;

#define LITTLE(n,MAX)  ((n)>=MAX? MAX:(n))
#define BIG(n,MAX)     if((n)>=MAX) {(n)-=MAX; while ((n)>127)  *outptr++ = 128+(n)%128, (n)/=128;  *outptr++ = (n);}

#ifndef LZPE
            *outptr++ = LITTLE(litlen,15)*16 + LITTLE(len,15);
            BIG (litlen,15);
            BIG (len,15);
            memcpy(outptr, last_ptr, orig_litlen);  outptr+=orig_litlen;
#else
            memcpy(outptr, last_ptr, orig_litlen);  outptr+=orig_litlen;
            *outptr++ = 128+LITTLE(len,127);
            BIG (len,127);
#endif
            last_ptr = ptr += orig_len;
        }
        else
        {
            ptr++;
        }
    }
    return outptr-outbuf;
}



const int BUFSIZE = 16<<20;
char inbuf[BUFSIZE+16], outbuf[BUFSIZE+BUFSIZE/4+512];

int main()
{
#ifdef _WIN32
    setmode (fileno(stdin), O_BINARY);
    setmode (fileno(stdout),O_BINARY);
#endif

    for(;;)
    {
        int insize = fread (inbuf, 1, BUFSIZE, stdin);
        if (insize==0)  break;
        int outsize = compress (inbuf, insize, outbuf);
        fwrite (outbuf, 1, outsize, stdout);
    }

    return 0;
}
