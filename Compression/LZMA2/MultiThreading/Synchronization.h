// Windows/Synchronization.h

#ifndef __WINDOWS_SYNCHRONIZATION_H
#define __WINDOWS_SYNCHRONIZATION_H

#include "Defs.h"

extern "C"
{
#include "../C/Threads.h"
}

#ifdef _WIN32
#include "Handle.h"
#endif

class Uncopyable {
protected:
  Uncopyable() {} // allow construction
  ~Uncopyable() {} // and destruction of derived objects...
private:
  Uncopyable(const Uncopyable&);             // ...but prevent copying
  Uncopyable& operator=(const Uncopyable&);
};


class CBaseEvent // FIXME : private Uncopyable
{
protected:
  ::CEvent _object;
public:
  bool IsCreated() { return Event_IsCreated(&_object) != 0; }
#ifdef _WIN32
  operator HANDLE() { return _object; }
#endif
  CBaseEvent() { Event_Construct(&_object); }
  ~CBaseEvent() { Close(); }
  WRes Close() { return Event_Close(&_object); }
  #ifdef _WIN32
  WRes Create(bool manualReset, bool initiallyOwn, LPCTSTR name = NULL,
      LPSECURITY_ATTRIBUTES securityAttributes = NULL)
  {
    _object = ::CreateEvent(securityAttributes, BoolToBOOL(manualReset),
        BoolToBOOL(initiallyOwn), name);
    if (_object != 0)
      return 0;
    return ::GetLastError();
  }
  WRes Open(DWORD desiredAccess, bool inheritHandle, LPCTSTR name)
  {
    _object = ::OpenEvent(desiredAccess, BoolToBOOL(inheritHandle), name);
    if (_object != 0)
      return 0;
    return ::GetLastError();
  }
  #endif

  WRes Signal() { return Event_Set(&_object); }
  // bool Pulse() { return BOOLToBool(::PulseEvent(_handle)); }
  WRes Reset() { return Event_Reset(&_object); }
  WRes Wait() { return Event_Wait(&_object); }
};

class ManualEvent: public CBaseEvent
{
public:
  ManualEvent() { Create(); }
  WRes Create(bool initiallyOwn = false)
  {
    return ManualResetEvent_Create(&_object, initiallyOwn ? 1: 0);
  }
  WRes CreateIfNotCreated()
  {
    if (IsCreated())
      return 0;
    return ManualResetEvent_CreateNotSignaled(&_object);
  }
  #ifdef _WIN32
  WRes CreateWithName(bool initiallyOwn, LPCTSTR name)
  {
    return CBaseEvent::Create(true, initiallyOwn, name);
  }
  #endif
};

class Event: public CBaseEvent
{
public:
  Event() { Create(); }
  WRes Create()
  {
    return AutoResetEvent_CreateNotSignaled(&_object);
  }
  WRes CreateIfNotCreated()
  {
    if (IsCreated())
      return 0;
    return AutoResetEvent_CreateNotSignaled(&_object);
  }
};

class Semaphore : private Uncopyable
{
  ::CSemaphore _object;
public:
  Semaphore() { Semaphore_Construct(&_object); }
  ~Semaphore() { Close(); }
  WRes Close() {  return Semaphore_Close(&_object); }
#ifdef _WIN32
  operator HANDLE() { return _object; }
#endif
  WRes Create(UInt32 initiallyCount, UInt32 maxCount)
  {
    return Semaphore_Create(&_object, initiallyCount, maxCount);
  }
  WRes Release() { return Semaphore_Release1(&_object); }
  WRes Release(UInt32 releaseCount) { return Semaphore_ReleaseN(&_object, releaseCount); }
  WRes Wait() { return Semaphore_Wait(&_object); }
};

class Mutex : private Uncopyable
{
  ::CCriticalSection _object;
public:
  Mutex() { CriticalSection_Init(&_object); }
  ~Mutex() { CriticalSection_Delete(&_object); }
  void Enter() { CriticalSection_Enter(&_object); }
  void Leave() { CriticalSection_Leave(&_object); }
};

class Lock : private Uncopyable
{
  Mutex *_object;
  void Unlock()  { _object->Leave(); }
public:
  Lock(Mutex &object): _object(&object) {_object->Enter(); }
  ~Lock() { Unlock(); }
};

#endif

