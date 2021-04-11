#include "LZMA2/MultiThreading/Thread.h"
#include "LZMA2/MultiThreading/Synchronization.h"

// *************************************************************************************************
// *** Queue with thread-safe Put/Get operations ***************************************************
// *************************************************************************************************
template <class T>
class SyncQueue
{
    Mutex mutex;               // Protects operations on queue
    ManualEvent ObjectAdded;   // Signals that object was added to queue
    T  *a;                     // Array of objects
    int size;                  // Size of array
    int head, tail;            // Indices of queue head and tail

    // Number of objects currently in queue
    int ObjectsInQueue()  {return (tail-head+size) % size;};

public:
    SyncQueue(): a(NULL) {};
    void Close() { FreeAndNil(a); }
    ~SyncQueue() { Close(); }

    // Set queue size - the class relies on assumption that client code
    // will never try to put excessive elements to the queue
    int SetSize (int max_elements)
    {
        Close();
        head = tail = 0;
        size = max_elements+1;   // +1 simplifies distinguishing between full and empty queues
        a = (T*) malloc(size * sizeof(T));
        return  (a==NULL? FREEARC_ERRCODE_NOT_ENOUGH_MEMORY : 0);
    }

    // Add object to the queue
    void Put (T o)
    {
        Lock _(mutex);
        if (ObjectsInQueue() == 0)
            ObjectAdded.Signal();
        a[tail] = o;
        tail = (tail+1) % size;
    }

    // Remove object from the queue
    T Get()
    {
        for(;;)
        {
            {
                Lock _(mutex);
                if (ObjectsInQueue() > 0)
                {
                    int tmp = head;
                    head = (head+1) % size;
                    if (ObjectsInQueue() == 0)
                        ObjectAdded.Reset();
                    return a[tmp];
                }
            }
            // Sleep until new object will be added to the queue
            ObjectAdded.Wait();
        }
    }
};



// *************************************************************************************************
// *** Background thread processing data ***********************************************************
// *************************************************************************************************

// General idea of background thread
struct BackgroundThread
{
    Thread BackgroundThreadPtr;
    void start();
    virtual void run() = 0;
    virtual ~BackgroundThread() {};
};

static THREAD_FUNC_RET_TYPE THREAD_FUNC_CALL_TYPE RunBackgroundThread (void *param)  {((BackgroundThread*) param) -> run(); return 0;}

inline void BackgroundThread::start()
{
    BackgroundThreadPtr.Create(RunBackgroundThread, this);
}


// Background thread performing process() on enqueued jobs
template <class Job>
struct ProcessingThread : BackgroundThread
{
    SyncQueue<Job> WorkerQueue, FinishedQueue;

    void SetMaxJobs (int max_jobs)
    {
        WorkerQueue.SetSize(max_jobs+2);
        FinishedQueue.SetSize(max_jobs+2);
    }

    virtual void run()
    {
        SetCompressionThreadPriority();                 // Понизить приоритет треда (рас)паковки чтоб не завешивать машину
        for(;;)
        {
            Job job = WorkerQueue.Get();
            job.process();
            FinishedQueue.Put(job);
        }
    }

    void Put (Job job)   {WorkerQueue.Put(job);}             // Enqueue job for processing
    Job  Get ()          {return FinishedQueue.Get();}       // Get processed job
};



// *************************************************************************************************
// *** Multiple background threads performing process() on enqueued jobs ***************************
// *************************************************************************************************

template <class Job>
struct MultipleProcessingThreads
{
    struct Task         {Job job;  Event *event;};

    int                 NumThreads, MaxJobs;
    Thread*             WorkerThreadsPtr;
    SyncQueue<Task>     WorkerQueue, FinishedQueue;
    SyncQueue<Event*>   FreeEvents;
    Event               WorkerThreadFinished;

    MultipleProcessingThreads()  {NumThreads=MaxJobs=0;}

    // Enqueue job for processing
    void Put (Job job)
    {
        Event *event = FreeEvents.Get();
        Task task = {job,event};
        WorkerQueue.Put(task);
        FinishedQueue.Put(task);
    }

    // Get processed job
    Job Get()
    {
        Task task = FinishedQueue.Get();
        task.event->Wait();
        FreeEvents.Put(task.event);
        return task.job;
    }

    // Thread performing jobs from queue
    void WorkerThread()
    {
        SetCompressionThreadPriority();                 // Понизить приоритет треда (рас)паковки чтоб не завешивать машину
        for(;;)
        {
            Task task = WorkerQueue.Get();
            if (task.event==NULL) break;
            task.job.process();
            task.event->Signal();
        }
        WorkerThreadFinished.Signal();
    }

    // Alloc resources and start b/g threads
    int start();

    // Finish b/g threads and free resources
    void finish()
    {
        for (int i=0; i<NumThreads; i++) {
            Job job = {0};
            Task task = {job, NULL};
            WorkerQueue.Put(task);
            WorkerThreadFinished.Wait();
        }
        delete[] WorkerThreadsPtr;
        for (int i=0; i<MaxJobs; i++)
            delete FreeEvents.Get();
    }
};

// Main function of Worker thread
template <class Job>  static THREAD_FUNC_RET_TYPE THREAD_FUNC_CALL_TYPE
MultipleProcessingThreads_RunWorkerThread (void *param)  {((MultipleProcessingThreads<Job>*)param) -> WorkerThread(); return 0;}

// Alloc resources and start b/g threads
template <class Job>
int MultipleProcessingThreads<Job>::start()
{
    if (MaxJobs<=0 || NumThreads<=0)
        return -1;  // Errcode

    WorkerQueue  .SetSize (MaxJobs+1);   // +1 for a NULL job signalling thread to finish
    FinishedQueue.SetSize (MaxJobs);
    FreeEvents   .SetSize (MaxJobs);
    for (int i=0; i<MaxJobs; i++)
        FreeEvents.Put (new Event);

    // Create worker threads
    WorkerThreadsPtr = new Thread[NumThreads];
    for (int i=0; i<NumThreads; i++)
        WorkerThreadsPtr[i].Create (MultipleProcessingThreads_RunWorkerThread<Job>, this);

    return 0;  // All OK
}



#ifndef MULTI_THREADING_BASICS
// *************************************************************************************************
// *** Multithreading block-wise compressor or decompressor ****************************************
// *************************************************************************************************

// Base (de)compression job
struct CompressionJob
{
    char* InBuf;                                          // Buffer pointing to input (original) data
    char* OutBuf;                                         // Buffer pointing to output (processed) data
    int   InSize;                                         // Amount of data in inbuf
    int   OutSize;                                        // Amount of data in outbuf
    Event ReadyToWrite;                                   // Signals that data were processed and need to be written
    int   result;                                         // Return code of last performed operation
};

// Multithreaded compressor performing multiple jobs simultaneously
template <class Job=CompressionJob, typename Worker=void*>  struct MTCompressor
{
    int              NumThreads;                          // Number of worker threads
    int              NumBuffers;                          // Number of I/O buffers and jobs
    bool             ImmediatelyFreeInbuf;

    Job*             Jobs;
    Thread*          WorkerThreadsPtr;
    Thread           WriterThreadPtr;

    SyncQueue<Job*>  FreeJobs;                            // Queue of free jobs
    SyncQueue<Job*>  WorkerJobs;                          // Queue of (de)compression jobs
    SyncQueue<Job*>  WriterJobs;                          // Queue of jobs for Writer thread, ordered by input chunks

    char             **buf0, **bufs;                      // Full list of I/O buffers
    SyncQueue<char*> InputBuffers;                        // Queue of free input buffers
    SyncQueue<char*> OutputBuffers;                       // Queue of free output buffers

    volatile int ErrCode;                                 // Error code to return or 0
    virtual  int SetErrCode (int e)  {if (e<0 && ErrCode==0)  ErrCode=e;  return e;}

    virtual int  AllocateBuffers (int BufferSize);        // Allocate I/O buffers of given size
    virtual int  Init();                                  // Creates threads and jobs
    virtual int  Run();                                   // Main function performing all work
    virtual void Process (Job &job, Worker &worker) = 0;  // Perform one (de)compression operation
    virtual void Write   (Job &job)                 = 0;  // Write output buffer and perform post-processing if required

    virtual int  ReaderThread()                     = 0;  // User-defined main thread reading input data
    virtual void WorkerThread();                          // Thread performing jobs from queue
    virtual void WriterThread();                          // Thread writing output data
    virtual int  WorkerInit (Worker &worker) {return 0;}  // Init WorkerThread local data, returning non-zero error code on fail
    virtual void WorkerDone (Worker &worker) {}           // Release WorkerThread local data

    MTCompressor (int _NumThreads = -1,  int _NumExtraBuffers = -1)
    {                                                                             // default values
        NumThreads  =                 (_NumThreads>0?        _NumThreads       :  GetCompressionThreads());
        NumBuffers  =  NumThreads  +  (_NumExtraBuffers>=0?  _NumExtraBuffers  :  2);
        ImmediatelyFreeInbuf = true;  ErrCode = 0;  Jobs = NULL;  WorkerThreadsPtr = NULL;  bufs = buf0 = NULL;
    }
    virtual ~MTCompressor() {}
};

// Main function of Worker/Writer thread
static THREAD_FUNC_RET_TYPE THREAD_FUNC_CALL_TYPE RunWorkerThread (void *param)  {((MTCompressor<CompressionJob>*) param) -> WorkerThread(); return 0;}
static THREAD_FUNC_RET_TYPE THREAD_FUNC_CALL_TYPE RunWriterThread (void *param)  {((MTCompressor<CompressionJob>*) param) -> WriterThread(); return 0;}

// Allocate I/O buffers of given size
template <class Job, typename Worker>  int MTCompressor<Job,Worker>::AllocateBuffers (int BufferSize)
{
    SetErrCode (InputBuffers. SetSize(NumBuffers));
    SetErrCode (OutputBuffers.SetSize(NumBuffers));
    if (ErrCode)  return ErrCode;

    bufs = buf0 = new char* [NumBuffers*2];
    if (bufs==NULL)   return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;

    // Allocate I/O buffers
    for (int i=0; i < NumBuffers; i++)
    {
        char *buf = (char*) BigAlloc(BufferSize);
        if (buf==NULL)   return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
        InputBuffers.Put(buf);   *bufs++ = buf;

        buf = (char*) BigAlloc(BufferSize);
        if (buf==NULL)   return FREEARC_ERRCODE_NOT_ENOUGH_MEMORY;
        OutputBuffers.Put(buf);  *bufs++ = buf;
    }

    return 0;
}

// Main function performing all work
template <class Job, typename Worker>  int MTCompressor<Job,Worker>::Init()
{
    // Extend queues to hold all the jobs (including NULL jobs at EOF)
    SetErrCode (FreeJobs.  SetSize(NumBuffers*2));
    SetErrCode (WorkerJobs.SetSize(NumBuffers*2));
    SetErrCode (WriterJobs.SetSize(NumBuffers*2));

    // Create job objects and put them to the FreeJobs queue
    Jobs = new Job[NumBuffers];
    for (int i=0; i < NumBuffers; i++)
        FreeJobs.Put (&Jobs[i]);

    // Create compression threads
    WorkerThreadsPtr = new Thread[NumThreads];
    for (int i=0; i < NumThreads; i++)
        WorkerThreadsPtr[i].Create (RunWorkerThread, this);

    // Create writer thread
    WriterThreadPtr.Create (RunWriterThread, this);

    return ErrCode;
}

// Main function performing all work
template <class Job, typename Worker>  int MTCompressor<Job,Worker>::Run()
{
    if (SetErrCode(Init()))  return ErrCode;


    // *****************************************************************************************************
    SetErrCode (ReaderThread());   // Main cycle: reading data and sending compression jobs to other threads
    // *****************************************************************************************************


    // Send EOF message to all other threads
    for (int i=0; i < NumThreads; i++)   WorkerJobs.Put(NULL);
    WriterJobs.Put(NULL);

    // Wait for all threads to finish
    for (int i=0; i < NumThreads; i++)   WorkerThreadsPtr[i].Wait();
    WriterThreadPtr.Wait();

    // Free memory
    while (bufs > buf0)   BigFree(*--bufs);
    delete [] buf0;
    delete [] WorkerThreadsPtr;
    delete [] Jobs;

    // Return error code or 0
    return ErrCode;
}

// Thread performing jobs from queue
template <class Job, typename Worker>  void MTCompressor<Job,Worker>::WorkerThread()
{
    SetCompressionThreadPriority();                 // Понизить приоритет треда (рас)паковки чтоб не завешивать машину
    Worker local;  SetErrCode (WorkerInit (local));
    for(;;)
    {
        Job *job = WorkerJobs.Get();                // Acquire next (de)compression job
        if (job == NULL  ||  ErrCode)       break;  // Quit on EOF or error in other thread
        job->OutBuf = OutputBuffers.Get();          // Acquire output buffer
        Process (*job, local);                      // *** COMPRESS OR DECOMPRESS THE DATA ***
        SetErrCode (job->result);                   // Set errcode if we have an error
        if (ImmediatelyFreeInbuf)
            InputBuffers.Put (job->InBuf);          // Input buffer is no more required for this job
        job->ReadyToWrite.Signal();                 // Signal to Writer thread
    }
    WorkerDone (local);
}

// Thread writing output data
template <class Job, typename Worker>  void MTCompressor<Job,Worker>::WriterThread()
{
    for(;;)
    {
        Job *job = WriterJobs.Get();                // Acquire next writer job
        if (job == NULL)                    break;  // Break on EOF
        job->ReadyToWrite.Wait();                   // Wait until (de)compression will be finished
        Write (*job);                               // *** WRITE OUTPUT DATA ***
        if (SetErrCode(job->result) < 0)    break;  // Break on error (after we've wrote data - assuming they were produced before error)
        if (!ImmediatelyFreeInbuf)
            InputBuffers.Put (job->InBuf);          // Input buffer is no more required for this job
        OutputBuffers.Put (job->OutBuf);            // Output buffer is no more required for this job
        FreeJobs.Put(job);                          // Free the job
    }
    if (bufs)
        InputBuffers.Put(NULL);                     // Wake up ReaderThread so that it can check ErrCode
    FreeJobs.Put(NULL);                             // -.-
}

#endif
