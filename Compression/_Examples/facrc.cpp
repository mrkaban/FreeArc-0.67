#include "../Compression.h"
#include "../MultiThreading.h"

const int MAXFILENAME = 260;
const int QUEUE_SIZE  = 1000*1000;
      int NumThreads  = 32;
      int BufSize     = 1*mb;


SyncQueue<char*>  file_queue;

THREAD_FUNC_RET_TYPE THREAD_FUNC_CALL_TYPE fill_queue_with_filenames (void *)
{
  file_queue.SetSize(QUEUE_SIZE);
  char str[MAXFILENAME];
  while (fgets (str, MAXFILENAME, stdin))
  {
    int len = strlen(str);
    if (str[len-1]=='\n')
      str[--len] = '\0';
    char *filename = (char*) malloc(len+1);
    strcpy (filename, str);
    file_queue.Put(filename);
  }
  iterate (NumThreads, file_queue.Put(NULL););
  return 0;
}


THREAD_FUNC_RET_TYPE THREAD_FUNC_CALL_TYPE read_files (void *)
{
  void *buf = malloc(BufSize);
  while(char *filename = file_queue.Get())
  {
    FILE *f = fopen(filename,"rb");
//    puts(filename);
    free(filename);
    while(fread(buf,1,BufSize,f) > 0);
    fclose(f);
  }
  return 0;
}


main (int argc, char **argv)
{
  if (argc==1)
  {
    char BufSizeStr[100];
    showMem (BufSize, BufSizeStr);
    printf ("FACRC: file hashing utility\n"
            "  -tN: use N threads (default %d)\n"
            "  -bN: read buffer in each thread (default %s)\n"
            , NumThreads, BufSizeStr);
    return 0;
  }

  while (*++argv)
  {
    int error = 0;
         if (start_with(*argv,"-t"))     NumThreads   = parseInt (*argv+2, &error);
    else if (start_with(*argv,"-b"))     BufSize      = parseMem (*argv+2, &error, 'm');
    if (error)  {printf("Bad option: %s\n", *argv); return 1;}
  }

  Thread *thread = new Thread[NumThreads];
  thread[0].Create (fill_queue_with_filenames, NULL);
  iterate (NumThreads, thread[i].Create (read_files, NULL));
  iterate (NumThreads, thread[i].Wait());
  return 0;
}
