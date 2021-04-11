// Client7z.cpp
#define EXTERNAL_CODECS

#include <windows.h>
#include <stdio.h>
#include <tchar.h>
#include "tabi.h"

// Required for SetProperties in p7zip code
#define COMPRESS_MT
// For correct filename translation in Unix
#define LOCALE_IS_UTF8


#ifdef FREEARC_WIN
#  include <initguid.h>
#else
   // Because IID_IUnknown is already defined in LZMA code
#  define __COMMON_MYINITGUID_H
#  define INITGUID
#  include "Common/MyGuidDef.h"

#  define ENV_UNIX
#endif

#include "Common/IntToString.h"
#include "Common/StringConvert.h"

#include "Windows/DLL.h"
#include "Windows/FileDir.h"
#include "Windows/FileFind.h"
#include "Windows/FileName.h"
#include "Windows/PropVariant.h"
#include "Windows/PropVariantConversions.h"

#include "7zip/Common/ProgressUtils.h"
#include "7zip/UI/Common/LoadCodecs.h"
#include "7zip/UI/Common/OpenArchive.h"
#include "7zip/UI/Common/ExtractMode.h"
#include "7zip/UI/Console/OpenCallbackConsole.h"
#include "7zip/Archive/IArchive.h"
#include "7zip/Archive/Common/OutStreamWithCRC.h"
#include "7zip/IPassword.h"
#include "7zip/MyVersion.h"

// Inline required source files
#include "Common/MyVector.cpp"
#include "Common/IntToString.cpp"
#include "Common/MyString.cpp"
#include "Common/StringConvert.cpp"
#include "Common/Wildcard.cpp"
#include "Common/UTFConvert.cpp"
#include "Common/MyWindows.cpp"
#include "Common/StringToInt.cpp"
#include "Windows/PropVariant.cpp"
#include "Windows/PropVariantConversions.cpp"
#include "Windows/DLL.cpp"
#include "Windows/Error.cpp"
#include "Windows/FileIO.cpp"
#include "Windows/FileDir.cpp"
#include "Windows/FileFind.cpp"
#include "Windows/FileName.cpp"
#include "Windows/Time.cpp"
#ifdef FREEARC_WIN
#include "Windows/Registry.cpp"
#else
#include "myWindows/wine_date_and_time.cpp"
#endif
#ifndef FREEARC_WIN
#include "../C/Threads.c"
#endif
#include "7zip/Common/FileStreams.cpp"
#include "7zip/Common/StreamUtils.cpp"
#include "7zip/Common/ProgressUtils.cpp"
#include "7zip/UI/Console/ConsoleClose.cpp"
#include "7zip/UI/Common/DefaultName.cpp"
#include "7zip/UI/Common/LoadCodecs.cpp"
#include "7zip/UI/Common/OpenArchive.cpp"
#include "7zip/UI/Common/ArchiveOpenCallback.cpp"
#include "7zip/UI/Common/ExtractingFilePath.cpp"
#include "7zip/UI/Common/SetProperties.cpp"


//////////////////////////////////////////////////////////////
// Some common definitions


using namespace NWindows;

HINSTANCE g_hInstance;
int g_CodePage = -1;


// Флаг, устанавливаемый в 1 когда нужно экстренно прервать вполняемую операцию
int BreakFlag = 0;
extern "C" void c_szSetBreakFlag (int flag)
{
  BreakFlag = flag;
}



//////////////////////////////////////////////////////////////
// Archive Open callback class


class COpenCallbackConsoleZ: public IOpenCallbackUI
{
public:
  INTERFACE_IOpenCallbackUI(;)

  #ifndef _NO_CRYPTO
  bool PasswordIsDefined;
  bool PasswordWasAsked;
  UString Password;
  TABI_FUNCTION *cb;
  COpenCallbackConsoleZ(TABI_FUNCTION *_cb): cb(_cb), PasswordIsDefined(false), PasswordWasAsked(false) {}
  #endif
};


HRESULT COpenCallbackConsoleZ::Open_CheckBreak()
{
  if (BreakFlag)
    return E_ABORT;
  return S_OK;
}

HRESULT COpenCallbackConsoleZ::Open_SetTotal(const UInt64 *, const UInt64 *)
{
  return Open_CheckBreak();
}

HRESULT COpenCallbackConsoleZ::Open_SetCompleted(const UInt64 *, const UInt64 *)
{
  return Open_CheckBreak();
}

HRESULT COpenCallbackConsoleZ::Open_CryptoGetTextPassword(BSTR *password)
{
  PasswordWasAsked = true;
  RINOK(Open_CheckBreak());

  if (!PasswordIsDefined)
  {
    int password_size = 1000;
    wchar_t password_buf[password_size];
    cb(TABI_DYNAMAP ("request","ask_password") ("password_buf", (void*)password_buf) ("password_size", password_size));
    Password = password_buf;
    PasswordIsDefined = true;
  }
  return StringToBstr(Password, password);
}

HRESULT COpenCallbackConsoleZ::Open_GetPasswordIfAny(UString &password)
{
  if (PasswordIsDefined)
    password = Password;
  return S_OK;
}

bool COpenCallbackConsoleZ::Open_WasPasswordAsked()
{
  return PasswordWasAsked;
}

void COpenCallbackConsoleZ::Open_ClearPasswordWasAskedFlag()
{
  PasswordWasAsked = false;
}



//////////////////////////////////////////////////////////////
// Exported functions


static CCodecs *codecs = NULL;
static CIntVector formatIndices;

// Load archive formats from 7z.dll
static void szInitLibrary()
{
  UString ArcType = L"";

  if (!codecs)
  {
#ifndef FREEARC_WIN
    global_use_utf16_conversion = 1;
#endif
    codecs = new CCodecs;
    CMyComPtr<ICompressCodecsInfo> *compressCodecsInfo = new CMyComPtr<ICompressCodecsInfo> (codecs);
    HRESULT result = codecs->Load();
    if (result != S_OK)
      throw "CSystemException(result)";

    if (!codecs->FindFormatForArchiveType(ArcType, formatIndices))
      throw "kUnsupportedArcTypeMessage";
  }
}


// Open existing archive
extern "C" int c_szOpenArchive (TABI_ELEMENT* params)
{
  try {
    TABI_MAP p(params);
    wchar_t       *archiveName  =        (wchar_t *)(p._ptr("arcname"));
    CArchiveLink* &arc          =  *(CArchiveLink**)(p._ptr("archive"));

    szInitLibrary();
    CArchiveLink *archiveLink = new CArchiveLink;
    COpenCallbackConsoleZ *openCallback = new COpenCallbackConsoleZ(p._callback("callback"));

    bool stdInMode = false;
    try {
      HRESULT result = archiveLink->Open2(codecs, formatIndices, stdInMode, NULL, archiveName, openCallback);
      if (result != S_OK)
        throw "can't open file as archive";
    } catch (...) {
        throw "can't open file as archive";
    }

    arc = archiveLink;

    return 0;
  } catch (const char *msg) {
//    sprintf(errmsg, "c_szOpenArchive: %s", msg);
    return 1;
  } catch (int code) {
//    sprintf(errmsg, "c_szOpenArchive: error %d", code);
    return 1;
  }
}


// Close archive
extern "C" UInt32  c_szArcClose (CArchiveLink &arc, char *errmsg)
{
  try {
    arc.Close();
    return 0;
  } catch (const char *msg) {
    sprintf(errmsg, "c_szArcClose: %s", msg);
    return 1;
  } catch (int code) {
    sprintf(errmsg, "c_szArcClose: error %d", code);
    return 1;
  }
}


// Number of files in archive
extern "C" UInt32  c_szArcItems (CArchiveLink &arc, UInt32 *value, char *errmsg)
{
  try {
    CMyComPtr<IInArchive> archive = arc.GetArchive();
    archive->GetNumberOfItems(value);
    return 0;
  } catch (const char *msg) {
    sprintf(errmsg, "c_szArcItems: %s", msg);
    return 1;
  } catch (int code) {
    sprintf(errmsg, "c_szArcItems: error %d", code);
    return 1;
  }
}


// Get numeric property of file in archive
extern "C" UInt32  c_szArcGetInt64Property (CArchiveLink &arc, Int32 index, PROPID propID, UInt64 *value, char *errmsg)
{
  try {
    CMyComPtr<IInArchive> archive = arc.GetArchive();
    NCOM::CPropVariant prop;
    if ((index==-1?  archive->GetArchiveProperty(propID, &prop) : archive->GetProperty(index, propID, &prop)) != S_OK)
      throw "GetProperty failed";
    if (prop.vt == VT_EMPTY)
         *value = 0; //throw "empty value";
    else *value = ConvertPropVariantToUInt64(prop);
    return 0;
  } catch (const char *msg) {
    sprintf(errmsg, "c_szArcGetInt64Property: %s", msg);
    return 1;
  } catch (int code) {
    sprintf(errmsg, "c_szArcGetInt64Property: error %d", code);
    return 1;
  }
}


// Get boolean property of file in archive
extern "C" UInt32  c_szArcGetBoolProperty (CArchiveLink &arc, Int32 index, PROPID propID, UInt32 *value, char *errmsg)
{
  try {
    CMyComPtr<IInArchive> archive = arc.GetArchive();
    NCOM::CPropVariant prop;
    if ((index==-1?  archive->GetArchiveProperty(propID, &prop) : archive->GetProperty(index, propID, &prop)) != S_OK)
      throw "GetProperty failed";
    switch (prop.vt)
    {
      case VT_EMPTY: *value = 0;  break;
      case VT_BOOL:  *value = VARIANT_BOOLToBool(prop.boolVal) ? 1 : 0;  break;
      default:       throw "not an Empty or Bool value";
    }
    return 0;
  } catch (const char *msg) {
    sprintf(errmsg, "c_szArcGetBoolProperty: %s", msg);
    return 1;
  } catch (int code) {
    sprintf(errmsg, "c_szArcGetBoolProperty: error %d", code);
    return 1;
  }
}


// Get string property of file in archive
extern "C" UInt32  c_szArcGetStrProperty (CArchiveLink &arc, Int32 index, PROPID propID, wchar_t *value, UInt32 valueSize, char *errmsg)
{
  try {
    CMyComPtr<IInArchive> archive = arc.GetArchive();
    NCOM::CPropVariant prop;
    if (index==-1  &&  propID==kpidType)
        prop = codecs->Formats[arc.Arcs.Back().FormatIndex].Name;
    else if ((index==-1?  archive->GetArchiveProperty(propID, &prop) : archive->GetProperty(index, propID, &prop)) != S_OK)
      throw "GetProperty failed";
    switch (prop.vt)
    {
      case VT_EMPTY: value[0] = 0;
                     break;

      case VT_BSTR: {int i;
                     for (i=0; i+1<valueSize && prop.bstrVal[i]; i++)
                       value[i] = prop.bstrVal[i];
                     value[i] = 0;
                     break;}

      default:       throw "not an Empty or BStr value";
    }
    return 0;
  } catch (const char *msg) {
    sprintf(errmsg, "c_szArcGetStrProperty: %s", msg);
    return 1;
  } catch (int code) {
    sprintf(errmsg, "c_szArcGetStrProperty: error %d", code);
    return 1;
  }
}


static BOOL IsFileTimeZero(CONST FILETIME *lpFileTime)
{
  return (lpFileTime->dwLowDateTime == 0) && (lpFileTime->dwHighDateTime == 0);
}

// Get datetime property of file in archive
extern "C" UInt32  c_szArcGetTimeProperty (CArchiveLink &arc, Int32 index, PROPID propID, UInt32 *value, char *errmsg)
{
  try {
    CMyComPtr<IInArchive> archive = arc.GetArchive();
    NCOM::CPropVariant prop;
    if ((index==-1?  archive->GetArchiveProperty(propID, &prop) : archive->GetProperty(index, propID, &prop)) != S_OK)
      throw "GetProperty failed";
    if (prop.vt != VT_FILETIME)
      throw "not a Filetime value";
    if (IsFileTimeZero(&prop.filetime))
      *value = 0;
    else
    {
      //FILETIME localFileTime;
      //if (!FileTimeToLocalFileTime(&prop.filetime, &localFileTime))
      //  throw "FileTimeToLocalFileTime failed";
      if (!NTime::FileTimeToUnixTime(prop.filetime, *value))
        throw "FileTimeToUnixTime failed";
    }
    return 0;
  } catch (const char *msg) {
    sprintf(errmsg, "c_szArcGetTimeProperty: %s", msg);
    return 1;
  } catch (int code) {
    sprintf(errmsg, "c_szArcGetTimeProperty: error %d", code);
    return 1;
  }
}













//////////////////////////////////////////////////////////////
// Archive Extracting callback class #1
#include "7zip/UI/Common/IFileExtractCallback.h"

static const wchar_t *kUniversalWildcard = L"*";


class CExtractCallbackConsole:
  public IExtractCallbackUI,
  public ICompressProgressInfo,
  #ifndef _NO_CRYPTO
  public ICryptoGetTextPassword,
  #endif
  public CMyUnknownImp
{
public:
  MY_QUERYINTERFACE_BEGIN2(IFolderArchiveExtractCallback)
  MY_QUERYINTERFACE_ENTRY(ICompressProgressInfo)
  #ifndef _NO_CRYPTO
  MY_QUERYINTERFACE_ENTRY(ICryptoGetTextPassword)
  #endif
  MY_QUERYINTERFACE_END
  MY_ADDREF_RELEASE

  STDMETHOD(SetTotal)(UInt64 total);
  STDMETHOD(SetCompleted)(const UInt64 *completeValue);

  // IFolderArchiveExtractCallback
  STDMETHOD(AskOverwrite)(
      const wchar_t *existName, const FILETIME *existTime, const UInt64 *existSize,
      const wchar_t *newName, const FILETIME *newTime, const UInt64 *newSize,
      Int32 *answer);
  STDMETHOD (PrepareOperation)(const wchar_t *name, bool isFolder, Int32 askExtractMode, const UInt64 *position);

  STDMETHOD(MessageError)(const wchar_t *message);
  STDMETHOD(SetOperationResult)(Int32 operationResult, bool encrypted);

  HRESULT BeforeOpen(const wchar_t *name);
  HRESULT OpenResult(const wchar_t *name, HRESULT result, bool encrypted);
  HRESULT ThereAreNoFiles();
  HRESULT ExtractResult(HRESULT result);

  // ICompressProgressInfo
  STDMETHOD(SetRatioInfo)(const UInt64 *inSize, const UInt64 *outSize);

  #ifndef _NO_CRYPTO
  HRESULT SetPassword(const UString &password);
  STDMETHOD(CryptoGetTextPassword)(BSTR *password);

  bool PasswordIsDefined;
  UString Password;

  #endif

  UInt64 NumArchives;
  UInt64 NumArchiveErrors;
  UInt64 NumFileErrors;
  UInt64 NumFileErrorsInCurrentArchive;

  void Init(TABI_FUNCTION *_cb)
  {
    NumArchives = 0;
    NumArchiveErrors = 0;
    NumFileErrors = 0;
    NumFileErrorsInCurrentArchive = 0;
    cb = _cb;
    old_inSize = old_outSize = 0;
  }

  TABI_FUNCTION *cb;
  UInt64 old_inSize;
  UInt64 old_outSize;
};


using namespace NWindows;
using namespace NFile;
using namespace NDirectory;

// static const char *kCantAutoRename = "can not create file with auto name\n";
// static const char *kCantRenameFile = "can not rename existing file\n";
// static const char *kCantDeleteOutputFile = "can not delete output file ";
static const char *kError = "ERROR: ";
static const char *kMemoryExceptionMessage = "Can't allocate required memory!";

static const char *kProcessing = "Processing archive: ";
static const char *kEverythingIsOk = "Everything is Ok";
static const char *kNoFiles = "No files to process";

static const char *kUnsupportedMethod = "Unsupported Method";
static const char *kCrcFailed = "CRC Failed";
static const char *kCrcFailedEncrypted = "CRC Failed in encrypted file. Wrong password?";
static const char *kDataError = "Data Error";
static const char *kDataErrorEncrypted = "Data Error in encrypted file. Wrong password?";
static const char *kUnknownError = "Unknown Error";

STDMETHODIMP CExtractCallbackConsole::SetTotal(UInt64 x)
{
  return S_OK;
}

STDMETHODIMP CExtractCallbackConsole::SetCompleted(const UInt64 *x)
{
  return S_OK;
}

STDMETHODIMP CExtractCallbackConsole::AskOverwrite(
    const wchar_t *existName, const FILETIME *, const UInt64 *index,
    const wchar_t *, const FILETIME *, const UInt64 *,
    Int32 *answer)
{
  if (BreakFlag)
    return E_ABORT;
  *answer = cb(TABI_DYNAMAP ("request","can_be_extracted?") ("outname", existName) ("index", *index));
  return S_OK;
}

STDMETHODIMP CExtractCallbackConsole::PrepareOperation(const wchar_t *name, bool isFolder, Int32 askExtractMode, const UInt64 *position)
{
  if (BreakFlag)
    return E_ABORT;
  cb(TABI_DYNAMAP ("request","filename") ("filename", name) ("is_folder?", isFolder) ("mode", askExtractMode));
  return S_OK;
}

STDMETHODIMP CExtractCallbackConsole::SetRatioInfo(const UInt64 *inSize, const UInt64 *outSize)
{
  if (BreakFlag)
    return E_ABORT;
  cb(TABI_DYNAMAP ("request","progress") ("compressed", *inSize - old_inSize) ("original", *outSize - old_outSize));
  old_inSize  = *inSize;
  old_outSize = *outSize;
  return S_OK;
}

STDMETHODIMP CExtractCallbackConsole::MessageError(const wchar_t *message)
{
  return S_OK;
}

STDMETHODIMP CExtractCallbackConsole::SetOperationResult(Int32 operationResult, bool encrypted)
{
  if (BreakFlag)
    return E_ABORT;
  cb(TABI_DYNAMAP ("request","filedone") ("operationResult", operationResult) ("encrypted?", encrypted));
  return S_OK;
}

#ifndef _NO_CRYPTO

HRESULT CExtractCallbackConsole::SetPassword(const UString &password)
{
  PasswordIsDefined = true;
  Password = password;
  return S_OK;
}

STDMETHODIMP CExtractCallbackConsole::CryptoGetTextPassword(BSTR *password)
{
  if (!PasswordIsDefined)
  {
    int password_size = 1000;
    wchar_t password_buf[password_size];
    cb(TABI_DYNAMAP ("request","ask_password") ("password_buf", (void*)password_buf) ("password_size", password_size));
    Password = password_buf;
    PasswordIsDefined = true;
  }
  return StringToBstr(Password, password);
}

#endif

HRESULT CExtractCallbackConsole::BeforeOpen(const wchar_t *name)
{
  return S_OK;
}

HRESULT CExtractCallbackConsole::OpenResult(const wchar_t * /* name */, HRESULT result, bool encrypted)
{
  return S_OK;
}

HRESULT CExtractCallbackConsole::ThereAreNoFiles()
{
  return S_OK;
}

HRESULT CExtractCallbackConsole::ExtractResult(HRESULT result)
{
  return S_OK;
}





//////////////////////////////////////////////////////////////
// Archive Extracting callback class #2
#define __ARCHIVE_EXTRACT_CALLBACK_H

class CArchiveExtractCallback:
  public IArchiveExtractCallback,
  // public IArchiveVolumeExtractCallback,
  public ICryptoGetTextPassword,
  public ICompressProgressInfo,
  public CMyUnknownImp
{
  const CArc *_arc;
  const NWildcard::CCensorNode *_wildcardCensor;
  CMyComPtr<IFolderArchiveExtractCallback> _extractCallback2;
  CMyComPtr<ICompressProgressInfo> _compressProgress;
  CMyComPtr<ICryptoGetTextPassword> _cryptoGetTextPassword;
  UString _directoryPath;
  NExtract::NPathMode::EEnum _pathMode;
  NExtract::NOverwriteMode::EEnum _overwriteMode;

  UString _diskFilePath;
  UString _filePath;
  UInt64 _position;
  bool _isSplit;

  bool _extractMode;

  bool WriteCTime;
  bool WriteATime;
  bool WriteMTime;

  bool _encrypted;

  struct CProcessedFileInfo
  {
    FILETIME CTime;
    FILETIME ATime;
    FILETIME MTime;
    UInt32 Attrib;

    bool CTimeDefined;
    bool ATimeDefined;
    bool MTimeDefined;
    bool AttribDefined;

    bool IsDir;
  } _fi;

  UInt32 _index;
  UInt64 _curSize;
  bool _curSizeDefined;
  COutFileStream *_outFileStreamSpec;
  CMyComPtr<ISequentialOutStream> _outFileStream;

  COutStreamWithCRC *_crcStreamSpec;
  CMyComPtr<ISequentialOutStream> _crcStream;

  UStringVector _removePathParts;

  bool _stdOutMode;
  bool _testMode;
  bool _crcMode;
  bool _multiArchives;

  CMyComPtr<ICompressProgressInfo> _localProgress;
  UInt64 _packTotal;
  UInt64 _unpTotal;

  void CreateComplexDirectory(const UStringVector &dirPathParts, UString &fullPath);
  HRESULT GetTime(int index, PROPID propID, FILETIME &filetime, bool &filetimeIsDefined);
  HRESULT GetUnpackSize();

public:

  CLocalProgress *LocalProgressSpec;

  UInt64 NumFolders;
  UInt64 NumFiles;
  UInt64 UnpackSize;
  UInt32 CrcSum;

  MY_UNKNOWN_IMP2(ICryptoGetTextPassword, ICompressProgressInfo)
  // COM_INTERFACE_ENTRY(IArchiveVolumeExtractCallback)

  INTERFACE_IArchiveExtractCallback(;)

  STDMETHOD(SetRatioInfo)(const UInt64 *inSize, const UInt64 *outSize);

  // IArchiveVolumeExtractCallback
  // STDMETHOD(GetInStream)(const wchar_t *name, ISequentialInStream **inStream);

  STDMETHOD(CryptoGetTextPassword)(BSTR *password);

  CArchiveExtractCallback():
      WriteCTime(true),
      WriteATime(true),
      WriteMTime(true),
      _multiArchives(false)
  {
    LocalProgressSpec = new CLocalProgress();
    _localProgress = LocalProgressSpec;
  }

  void InitForMulti(bool multiArchives,
      NExtract::NPathMode::EEnum pathMode,
      NExtract::NOverwriteMode::EEnum overwriteMode)
  {
    _multiArchives = multiArchives;
    _pathMode = pathMode;
    _overwriteMode = overwriteMode;
    NumFolders = NumFiles = UnpackSize = 0;
    CrcSum = 0;
  }

  void Init(
      const NWildcard::CCensorNode *wildcardCensor,
      const CArc *arc,
      IFolderArchiveExtractCallback *extractCallback2,
      bool stdOutMode, bool testMode, bool crcMode,
      const UString &directoryPath,
      const UStringVector &removePathParts,
      UInt64 packSize);

  bool optionKeepBroken;
};


static const wchar_t *kCantAutoRename = L"ERROR: Can not create file with auto name";
static const wchar_t *kCantRenameFile = L"ERROR: Can not rename existing file ";
static const wchar_t *kCantDeleteOutputFile = L"ERROR: Can not delete output file ";

void CArchiveExtractCallback::Init(
    const NWildcard::CCensorNode *wildcardCensor,
    const CArc *arc,
    IFolderArchiveExtractCallback *extractCallback2,
    bool stdOutMode, bool testMode, bool crcMode,
    const UString &directoryPath,
    const UStringVector &removePathParts,
    UInt64 packSize)
{
  _wildcardCensor = wildcardCensor;

  _stdOutMode = stdOutMode;
  _testMode = testMode;
  _crcMode = crcMode;
  _unpTotal = 1;
  _packTotal = packSize;

  _extractCallback2 = extractCallback2;
  _compressProgress.Release();
  _extractCallback2.QueryInterface(IID_ICompressProgressInfo, &_compressProgress);

  LocalProgressSpec->Init(extractCallback2, true);
  LocalProgressSpec->SendProgress = false;


  _removePathParts = removePathParts;
  _arc = arc;
  _directoryPath = directoryPath;
  NFile::NName::NormalizeDirPathPrefix(_directoryPath);
}

STDMETHODIMP CArchiveExtractCallback::SetTotal(UInt64 size)
{
  COM_TRY_BEGIN
  _unpTotal = size;
  if (!_multiArchives && _extractCallback2)
    return _extractCallback2->SetTotal(size);
  return S_OK;
  COM_TRY_END
}

static void NormalizeVals(UInt64 &v1, UInt64 &v2)
{
  const UInt64 kMax = (UInt64)1 << 31;
  while (v1 > kMax)
  {
    v1 >>= 1;
    v2 >>= 1;
  }
}

static UInt64 MyMultDiv64(UInt64 unpCur, UInt64 unpTotal, UInt64 packTotal)
{
  NormalizeVals(packTotal, unpTotal);
  NormalizeVals(unpCur, unpTotal);
  if (unpTotal == 0)
    unpTotal = 1;
  return unpCur * packTotal / unpTotal;
}

STDMETHODIMP CArchiveExtractCallback::SetCompleted(const UInt64 *completeValue)
{
  COM_TRY_BEGIN
  if (!_extractCallback2)
    return S_OK;

  if (_multiArchives)
  {
    if (completeValue != NULL)
    {
      UInt64 packCur = LocalProgressSpec->InSize + MyMultDiv64(*completeValue, _unpTotal, _packTotal);
      return _extractCallback2->SetCompleted(&packCur);
    }
  }
  return _extractCallback2->SetCompleted(completeValue);
  COM_TRY_END
}

STDMETHODIMP CArchiveExtractCallback::SetRatioInfo(const UInt64 *inSize, const UInt64 *outSize)
{
  COM_TRY_BEGIN
  return _localProgress->SetRatioInfo(inSize, outSize);
  COM_TRY_END
}

void CArchiveExtractCallback::CreateComplexDirectory(const UStringVector &dirPathParts, UString &fullPath)
{
  fullPath = _directoryPath;
  for (int i = 0; i < dirPathParts.Size(); i++)
  {
    if (i > 0)
      fullPath += wchar_t(NFile::NName::kDirDelimiter);
    fullPath += dirPathParts[i];
    NFile::NDirectory::MyCreateDirectory(fullPath);
  }
}

HRESULT CArchiveExtractCallback::GetTime(int index, PROPID propID, FILETIME &filetime, bool &filetimeIsDefined)
{
  filetimeIsDefined = false;
  NCOM::CPropVariant prop;
  RINOK(_arc->Archive->GetProperty(index, propID, &prop));
  if (prop.vt == VT_FILETIME)
  {
    filetime = prop.filetime;
    filetimeIsDefined = (filetime.dwHighDateTime != 0 || filetime.dwLowDateTime != 0);
  }
  else if (prop.vt != VT_EMPTY)
    return E_FAIL;
  return S_OK;
}

HRESULT CArchiveExtractCallback::GetUnpackSize()
{
  NCOM::CPropVariant prop;
  RINOK(_arc->Archive->GetProperty(_index, kpidSize, &prop));
  _curSizeDefined = (prop.vt != VT_EMPTY);
  if (_curSizeDefined)
    _curSize = ConvertPropVariantToUInt64(prop);
  return S_OK;
}

STDMETHODIMP CArchiveExtractCallback::GetStream(UInt32 index, ISequentialOutStream **outStream, Int32 askExtractMode)
{
  COM_TRY_BEGIN
  _crcStream.Release();
  *outStream = 0;
  _outFileStream.Release();

  _encrypted = false;
  _isSplit = false;
  _curSize = 0;
  _curSizeDefined = false;
  _index = index;

  UString fullPath;

  IInArchive *archive = _arc->Archive;
  RINOK(_arc->GetItemPath(index, fullPath));
  RINOK(IsArchiveItemFolder(archive, index, _fi.IsDir));

  _filePath = fullPath;

  {
    NCOM::CPropVariant prop;
    RINOK(archive->GetProperty(index, kpidPosition, &prop));
    if (prop.vt != VT_EMPTY)
    {
      if (prop.vt != VT_UI8)
        return E_FAIL;
      _position = prop.uhVal.QuadPart;
      _isSplit = true;
    }
  }

  RINOK(GetArchiveItemBoolProp(archive, index, kpidEncrypted, _encrypted));

  RINOK(GetUnpackSize());

  if (_wildcardCensor)
  {
    if (!_wildcardCensor->CheckPath(fullPath, !_fi.IsDir))
      return S_OK;
  }

  if (askExtractMode == NArchive::NExtract::NAskMode::kExtract && !_testMode)
  {
    if (_stdOutMode)
    {
      CMyComPtr<ISequentialOutStream> outStreamLoc = new CStdOutFileStream;
      *outStream = outStreamLoc.Detach();
      return S_OK;
    }

    {
      NCOM::CPropVariant prop;
      RINOK(archive->GetProperty(index, kpidAttrib, &prop));
      if (prop.vt == VT_UI4)
      {
        _fi.Attrib = prop.ulVal;
        _fi.AttribDefined = true;
      }
      else if (prop.vt == VT_EMPTY)
        _fi.AttribDefined = false;
      else
        return E_FAIL;
    }

    RINOK(GetTime(index, kpidCTime, _fi.CTime, _fi.CTimeDefined));
    RINOK(GetTime(index, kpidATime, _fi.ATime, _fi.ATimeDefined));
    RINOK(GetTime(index, kpidMTime, _fi.MTime, _fi.MTimeDefined));

    bool isAnti = false;
    RINOK(_arc->IsItemAnti(index, isAnti));

    UStringVector pathParts;
    SplitPathToParts(fullPath, pathParts);

    if (pathParts.IsEmpty())
      return E_FAIL;
    int numRemovePathParts = 0;
    switch(_pathMode)
    {
      case NExtract::NPathMode::kFullPathnames:
        break;
      case NExtract::NPathMode::kCurrentPathnames:
      {
        numRemovePathParts = _removePathParts.Size();
        if (pathParts.Size() <= numRemovePathParts)
          return E_FAIL;
        for (int i = 0; i < numRemovePathParts; i++)
          if (_removePathParts[i].CompareNoCase(pathParts[i]) != 0)
            return E_FAIL;
        break;
      }
      case NExtract::NPathMode::kNoPathnames:
      {
        numRemovePathParts = pathParts.Size() - 1;
        break;
      }
    }
    pathParts.Delete(0, numRemovePathParts);
    MakeCorrectPath(pathParts);
    UString processedPath = MakePathNameFromParts(pathParts);
    if (!isAnti)
    {
      if (!_fi.IsDir)
      {
        if (!pathParts.IsEmpty())
          pathParts.DeleteBack();
      }

      if (!pathParts.IsEmpty())
      {
        UString fullPathNew;
        CreateComplexDirectory(pathParts, fullPathNew);
        if (_fi.IsDir)
          NFile::NDirectory::SetDirTime(fullPathNew,
            (WriteCTime && _fi.CTimeDefined) ? &_fi.CTime : NULL,
            (WriteATime && _fi.ATimeDefined) ? &_fi.ATime : NULL,
            (WriteMTime && _fi.MTimeDefined) ? &_fi.MTime : (_arc->MTimeDefined ? &_arc->MTime : NULL));
      }
    }


    UString fullProcessedPath = _directoryPath + processedPath;

    if (_fi.IsDir)
    {
      _diskFilePath = fullProcessedPath;
      if (isAnti)
        NFile::NDirectory::MyRemoveDirectory(_diskFilePath);
      return S_OK;
    }

    if (!_isSplit)
    {
      Int32 overwiteResult;
      UInt64 index64 = index;
      RINOK(_extractCallback2->AskOverwrite(fullProcessedPath, NULL, &index64, NULL, NULL, NULL, &overwiteResult));
      if (overwiteResult==0)
        return S_OK;  // no overwrite
    }
    if (!isAnti)
    {
      _outFileStreamSpec = new COutFileStream;
      CMyComPtr<ISequentialOutStream> outStreamLoc(_outFileStreamSpec);
      if (!_outFileStreamSpec->Open(fullProcessedPath, _isSplit ? OPEN_ALWAYS: CREATE_ALWAYS))
      {
        // if (::GetLastError() != ERROR_FILE_EXISTS || !isSplit)
        {
          UString message = L"can not open output file " + fullProcessedPath;
          RINOK(_extractCallback2->MessageError(message));
          return S_OK;
        }
      }
      if (_isSplit)
      {
        RINOK(_outFileStreamSpec->Seek(_position, STREAM_SEEK_SET, NULL));
      }
      _outFileStream = outStreamLoc;
      *outStream = outStreamLoc.Detach();
    }
    _diskFilePath = fullProcessedPath;
  }
  else
  {
    *outStream = NULL;
  }
  return S_OK;
  COM_TRY_END
}

STDMETHODIMP CArchiveExtractCallback::PrepareOperation(Int32 askExtractMode)
{
  COM_TRY_BEGIN
  _extractMode = false;
  switch (askExtractMode)
  {
    case NArchive::NExtract::NAskMode::kExtract:
      if (_testMode)
        askExtractMode = NArchive::NExtract::NAskMode::kTest;
      else
        _extractMode = true;
      break;
  };
  return _extractCallback2->PrepareOperation(_filePath, _fi.IsDir,
      askExtractMode, _isSplit ? &_position: 0);
  COM_TRY_END
}

STDMETHODIMP CArchiveExtractCallback::SetOperationResult(Int32 operationResult)
{
  COM_TRY_BEGIN
  switch(operationResult)
  {
    case NArchive::NExtract::NOperationResult::kOK:
    case NArchive::NExtract::NOperationResult::kUnSupportedMethod:
    case NArchive::NExtract::NOperationResult::kCRCError:
    case NArchive::NExtract::NOperationResult::kDataError:
      break;
    default:
      _outFileStream.Release();
      return E_FAIL;
  }
  if (_outFileStream)
  {
    _outFileStreamSpec->SetTime(
        (WriteCTime && _fi.CTimeDefined) ? &_fi.CTime : NULL,
        (WriteATime && _fi.ATimeDefined) ? &_fi.ATime : NULL,
        (WriteMTime && _fi.MTimeDefined) ? &_fi.MTime : (_arc->MTimeDefined ? &_arc->MTime : NULL));
    _curSize = _outFileStreamSpec->ProcessedSize;
    _curSizeDefined = true;
    RINOK(_outFileStreamSpec->Close());
    _outFileStream.Release();
  }
  if (operationResult == NArchive::NExtract::NOperationResult::kOK  ||  optionKeepBroken)
  {
    if (!_curSizeDefined)
      GetUnpackSize();
    if (_curSizeDefined)
      UnpackSize += _curSize;
    if (_fi.IsDir)
      NumFolders++;
    else
      NumFiles++;

    if (_extractMode && _fi.AttribDefined)
      NFile::NDirectory::MySetFileAttributes(_diskFilePath, _fi.Attrib);
  } else {
    NFile::NDirectory::DeleteFileAlways(_diskFilePath);
  }
  RINOK(_extractCallback2->SetOperationResult(operationResult, _encrypted));
  return S_OK;
  COM_TRY_END
}

STDMETHODIMP CArchiveExtractCallback::CryptoGetTextPassword(BSTR *password)
{
  COM_TRY_BEGIN
  if (!_cryptoGetTextPassword)
  {
    RINOK(_extractCallback2.QueryInterface(IID_ICryptoGetTextPassword,
        &_cryptoGetTextPassword));
  }
  return _cryptoGetTextPassword->CryptoGetTextPassword(password);
  COM_TRY_END
}



//////////////////////////////////////////////////////////////
// Archive extraction function
#include "7zip/UI/Common/Extract.cpp"


static HRESULT DecompressArchiveZ(
    const CArc &arc,
    UInt32* filelist,
    int files,
    UInt64 packSize,
    const CExtractOptions &options,
    IExtractCallbackUI *callback,
    CArchiveExtractCallback *extractCallbackSpec,
    UString &errorMessage,
    UInt64 &stdInProcessed)
{
  stdInProcessed = 0;
  IInArchive *archive = arc.Archive;
  if (!options.StdInMode)
  {
    if (files == 0)
    {
      callback->ThereAreNoFiles();
      return S_OK;
    }
  }

  UStringVector removePathParts;

  UString outDir = options.OutputDir;
  outDir.Replace(L"*", GetCorrectFsPath(arc.DefaultName));
  #ifdef _WIN32
  outDir.TrimRight();
  outDir = GetCorrectFullFsPath(outDir);
  #endif

  if (!outDir.IsEmpty())
    if (!NFile::NDirectory::CreateComplexDirectory(outDir))
    {
      HRESULT res = ::GetLastError();
      if (res == S_OK)
        res = E_FAIL;
      errorMessage = ((UString)L"Can not create output directory ") + outDir;
      return res;
    }

  extractCallbackSpec->InitForMulti(false, options.PathMode, options.OverwriteMode);
  extractCallbackSpec->Init(NULL, &arc, callback, options.StdOutMode, options.TestMode, options.CalcCrc, outDir, removePathParts, packSize);

  RINOK(SetProperties(archive, options.Properties));

  HRESULT result;
  Int32 testMode = (options.TestMode && !options.CalcCrc) ?  1 : 0;
  if (options.StdInMode)
  {
    result = archive->Extract(NULL, (UInt32)(Int32)-1, testMode, extractCallbackSpec);
    NCOM::CPropVariant prop;
    if (archive->GetArchiveProperty(kpidPhySize, &prop) == S_OK)
      if (prop.vt == VT_UI8 || prop.vt == VT_UI4)
        stdInProcessed = ConvertPropVariantToUInt64(prop);
  }
  else
    result = archive->Extract(filelist, files, testMode, extractCallbackSpec);

  return callback->ExtractResult(result);
}



//////////////////////////////////////////////////////////////
// Archive extraction


// Test/extract files from archive
extern "C" int c_szExtract (TABI_ELEMENT* params)
{
  try {
    TABI_MAP p(params);
    char         *command   =                  p._str("cmd");
    CArchiveLink &arc       = *(CArchiveLink*)(p._ptr("archive"));
    wchar_t     *OutputDir  =      (wchar_t *)(p._ptr("OutputDir"));
    UInt32      *filelist   =      (UInt32  *)(p._ptr("filelist"));
    int          files      =                  p._int("files");
    bool         keepBroken =                  p._bool("keepBroken?");

    CArchiveExtractCallback *extractCallbackSpec = new CArchiveExtractCallback;
    CMyComPtr<IArchiveExtractCallback> ecs(extractCallbackSpec);
    extractCallbackSpec->optionKeepBroken = keepBroken;

    CExtractCallbackConsole *extractCallback = new CExtractCallbackConsole;
    CMyComPtr<IFolderArchiveExtractCallback> ec = extractCallback;
    extractCallback->PasswordIsDefined = false;

    extractCallback->Init(p._callback("callback"));

    CExtractOptions options;
    options.StdInMode = false;
    options.StdOutMode = false;
    options.PathMode = strcmp(command,"e")==0? NExtract::NPathMode::kNoPathnames : NExtract::NPathMode::kFullPathnames;
    options.TestMode = strcmp(command,"t")==0;
    options.OverwriteMode = NExtract::NOverwriteMode::kAskBefore;
    options.OutputDir = OutputDir;
    options.YesToAll = false;
    options.CalcCrc = false;

    UString errorMessage;
    UInt64 packProcessed;
    UInt64 packSize = 0;

    HRESULT result = DecompressArchiveZ(arc.Arcs.Back(), filelist, files, packSize, options,
                                        extractCallback, extractCallbackSpec, errorMessage, packProcessed);

    if (result != S_OK)
      throw "extract error";

    return 0;
  } catch (const char *msg) {
    printf("\nc_szExtract: %s\n", msg);
//    sprintf(errmsg, "c_szExtract: %s", msg);
    return 1;
  }
}









//////////////////////////////////////////////////////////////
// Archive creation
#include "7zip/Compress/CopyCoder.cpp"
#include "7zip/UI/Common/TempFiles.cpp"
#include "7zip/UI/Common/UpdateAction.cpp"
#include "7zip/UI/Common/UpdatePair.cpp"
#include "7zip/UI/Common/SortUtils.cpp"

static TABI_FUNCTION *cb;
static int            arc_files;            // Количество файлов из оригинального архива,
static UInt32*        arc_filelist;         //   их номера,
static wchar_t**      new_names    = NULL;  //   и новые имена
static wchar_t**      disk_names   = NULL;  // Файлы для упаковки с диска
static wchar_t**      stored_names = NULL;  //   и имена, которые им дать в архиве
static int            last_index = -1;
#include "Client7zUpdate.cpp"

static UInt64 old_inSize, old_outSize;

#include "7zip/UI/Console/UpdateCallbackConsole.h"
HRESULT CUpdateCallbackConsole::OpenResult(const wchar_t *name, HRESULT result)
{
  return S_OK;
}

HRESULT CUpdateCallbackConsole::StartScanning()
{
  return S_OK;
}

HRESULT CUpdateCallbackConsole::ScanProgress(UInt64 /* numFolders */, UInt64 /* numFiles */, const wchar_t * /* path */)
{
  return S_OK;
}

HRESULT CUpdateCallbackConsole::CanNotFindError(const wchar_t *name, DWORD systemError)
{
  return S_OK;
}

HRESULT CUpdateCallbackConsole::FinishScanning()
{
  return S_OK;
}

HRESULT CUpdateCallbackConsole::StartArchive(const wchar_t *name, bool updating)
{
  return S_OK;
}

HRESULT CUpdateCallbackConsole::FinishArchive()
{
  return S_OK;
}

HRESULT CUpdateCallbackConsole::CheckBreak()
{
  if (BreakFlag)
    return E_ABORT;
  return S_OK;
}

HRESULT CUpdateCallbackConsole::Finilize()
{
  return S_OK;
}

HRESULT CUpdateCallbackConsole::SetNumFiles(UInt64 numFiles)
{
  return S_OK;
}

HRESULT CUpdateCallbackConsole::SetTotal(UInt64 size)   // This "total" is "capacity" actual, it's larger than sum of files' origsize
{
  return S_OK;
}

HRESULT CUpdateCallbackConsole::SetCompleted(const UInt64 *completeValue)
{
  RINOK(CheckBreak());
  return S_OK;
}

HRESULT CUpdateCallbackConsole::SetRatioInfo(const UInt64 *inSize, const UInt64 *outSize)
{
  RINOK(CheckBreak());
  cb(TABI_DYNAMAP ("request","progress") ("original", *inSize - old_inSize) ("compressed", *outSize - old_outSize));
  old_inSize  = *inSize;
  old_outSize = *outSize;
  return S_OK;
}

HRESULT CUpdateCallbackConsole::GetStream(const wchar_t *name, bool isAnti)
{
  RINOK(CheckBreak());
  cb(TABI_DYNAMAP ("request","filename") ("filename", name) ("is_folder?", false) ("mode", NArchive::NExtract::NAskMode::kExtract));
  //cb(TABI_DYNAMAP ("request","filename") ("filename", last_index>=0? disk_names[last_index] : name) ("is_folder?", false) ("mode", NArchive::NExtract::NAskMode::kExtract));
  last_index = -1;
  return S_OK;
}

HRESULT CUpdateCallbackConsole::OpenFileError(const wchar_t *name, DWORD systemError)
{
  return S_FALSE;
}

HRESULT CUpdateCallbackConsole::SetOperationResult(Int32 )
{
  return S_OK;
}

HRESULT CUpdateCallbackConsole::CryptoGetTextPassword2(Int32 *passwordIsDefined, BSTR *password)
{
  RINOK(CheckBreak());
  *passwordIsDefined = PasswordIsDefined;
  return StringToBstr(Password, password);
}

HRESULT CUpdateCallbackConsole::CryptoGetTextPassword(BSTR *password)
{
  RINOK(CheckBreak());
  return StringToBstr(Password, password);
}


//////////////////////////////////////////////////////////////
// EnumDirItems.cpp

using namespace NWindows;
using namespace NFile;
using namespace NName;

void AddDirFileInfo(int phyParent, int logParent,
    const NFind::CFileInfoW &fi, CObjectVector<CDirItem> &dirItems)
{
  CDirItem di;
  di.Size = fi.Size;
  di.CTime = fi.CTime;
  di.ATime = fi.ATime;
  di.MTime = fi.MTime;
  di.Attrib = fi.Attrib;
  di.PhyParent = phyParent;
  di.LogParent = logParent;
  di.Name = fi.Name;
  dirItems.Add(di);
}

UString CDirItems::GetPrefixesPath(const CIntVector &parents, int index, const UString &name) const
{
  return UString();
}

UString CDirItems::GetPhyPath(int index) const
{
  return disk_names[index];
}

UString CDirItems::GetLogPath(int index) const
{
  last_index = index;
  return stored_names[index];
}

void CDirItems::ReserveDown()
{
  Items.ReserveDown();
}

int CDirItems::AddPrefix(int phyParent, int logParent, const UString &prefix)
{
  return 0;
}

void CDirItems::DeleteLastPrefix()
{
}

void CDirItems::EnumerateDirectory(int phyParent, int logParent, const UString &phyPrefix,
    UStringVector &errorPaths, CRecordVector<DWORD> &errorCodes)
{
}

HRESULT EnumerateItems(
    const NWildcard::CCensor &censor,
    CDirItems &dirItems,
    IEnumDirItemCallback *callback,
    UStringVector &errorPaths,
    CRecordVector<DWORD> &errorCodes)
{
  for (wchar_t **filename = disk_names; *filename; filename++)
  {
    NFind::CFileInfoW fi;
    if (!fi.Find(*filename))  throw "file to compress not found";
    AddDirFileInfo(-1,-1,fi,dirItems.Items);
  }
  dirItems.ReserveDown();
  return S_OK;
}





//////////////////////////////////////////////////////////////
// Create archive
extern "C" int c_szCompress (TABI_ELEMENT* params)
{
  try {
    szInitLibrary();

    TABI_MAP p(params);
    char     *update_type         =             (p._str ("update_type"));   // Archive updating operation (a, u, f, s)
    wchar_t **compression_options = (wchar_t **)(p._ptr ("options"));       // Compression options
    wchar_t  *arcType             =             (p._wstr("arctype"));       // Archive type (7z, zip...)
    wchar_t  *in_arcname          =             (p._wstr("in_arcname"));
    wchar_t  *out_arcname         =             (p._wstr("out_arcname"));
              arc_files           =             (p._int ("files"));          // Количество файлов из оригинального архива,
              arc_filelist        = (UInt32 *)  (p._ptr ("filelist"));       //   их номера,
              new_names           = (wchar_t **)(p._ptr ("new_names"));      //   и новые имена
              disk_names          = (wchar_t **)(p._ptr ("disk_names"));     // Файлы для упаковки с диска
              stored_names        = (wchar_t **)(p._ptr ("stored_names"));   //   и имена, которые им дать в архиве
    wchar_t  *password            =             (p._wstr("password"));       // Пароль шифрования
    int       volumes             =             (p._int ("volumes"));        // Размер массива volume_sizes
    UInt64   *volume_sizes        = (UInt64 *)  (p._ptr ("volume_sizes"));;  // Размер томов архива
    wchar_t  *sfxModule           =             (p._wstr("sfx"));            // Файл sfx-модуля, "-" для удаления существующего модуля, или "--" чтобы ничего не менять

    last_index = -1;

    CUpdateOptions options;
    options.UpdateArchiveItself = false;

    options.Commands.Clear();
    CUpdateArchiveCommand uc;
    uc.UserArchivePath = out_arcname;
    uc.ActionSet = 0==strcmp(update_type,"a")? NUpdateArchive::kAddActionSet
                 : 0==strcmp(update_type,"u")? NUpdateArchive::kUpdateActionSet
                 : 0==strcmp(update_type,"f")? NUpdateArchive::kFreshActionSet
                 : 0==strcmp(update_type,"s")? NUpdateArchive::kSynchronizeActionSet
                 : throw "bad update_type";
    options.Commands.Add(uc);

    for (wchar_t **option = compression_options; *option; option++)
    {
      //wprintf(L"\n 7z option = %s \n", *option);
      wchar_t *value = wcschr (*option, L'=');
      if (value)  *value++ = L'\0';  else value = L"";
      CProperty property = {*option, value};
      options.MethodMode.Properties.Add(property);
    }
    for (int i=0; i<volumes; i++)
      options.VolumesSizes.Add(volume_sizes[i]);

    if (wcscmp(sfxModule, L"-")==0)                   // remove SFX module from the archive
    {
      CProperty property = {L"rsfx", L"on"};
      options.MethodMode.Properties.Add(property);
    }
    else if (wcscmp(sfxModule, L"--"))                // add SFX module to the archive
    {
      options.SfxMode = true;
      options.SfxModule = sfxModule;
    }


    cb = p._callback("callback"); old_inSize = old_outSize = 0;

    COpenCallbackConsoleZ openCallback(p._callback("callback"));

    CUpdateCallbackConsole callback;
    callback.PasswordIsDefined = (*password != 0);
    callback.AskPassword = false;
    callback.Password = password;

    CUpdateErrorInfo errorInfo;

    NWildcard::CCensor censor;  // fake censor

    // Подобрать формат архива по опции -t и имени создаваемого архива
    CIntVector formatIndices;
    if (!codecs->FindFormatForArchiveType(arcType, formatIndices))
      throw "unsupported archive type";
    if (!options.Init(codecs, formatIndices, in_arcname))
      throw kUpdateIsNotSupoorted;

    HRESULT result = UpdateArchive(codecs,
        censor, options,
        errorInfo, &openCallback, &callback);

    if (result != S_OK)
      throw "compression error";

    return 0;
  } catch (const char *msg) {
    printf("\nc_szCompress: %s\n", msg);
//    sprintf(errmsg, "c_szCompress: %s", msg);
    return 1;
  }
}



//////////////////////////////////////////////////////////////
// Check that given archive type (7z, zip...) supported by 7z.dll
extern "C" int c_szCheckType (wchar_t *arcType)
{
  szInitLibrary();
  CIntVector formatIndices;
  return (codecs->FindFormatForArchiveType(arcType, formatIndices)? 1 : 0);
}

// Return default extension for given arcType
extern "C" int c_szDefaultExtension (wchar_t *arcType, wchar_t *extension, UInt32 extensionSize, char *errmsg)
{
  szInitLibrary();
  CIntVector formatIndices;
  wcscpy(extension, L"");
  if (codecs->FindFormatForArchiveType(arcType, formatIndices))
  {
    for (int i=0; i<formatIndices.Size(); i++)
    {
      int arcTypeIndex = formatIndices[0];
      if (arcTypeIndex >= 0)
      {
        UString typeExt = codecs->Formats[arcTypeIndex].GetMainExt();
        if (typeExt > L"")
        {
          wcscat(extension, L".");
          wcscat(extension, typeExt);
        }
      }
    }
    return 0;
  }
  else
  {
    sprintf(errmsg, "c_szDefaultExtension: unsupported archive type");
    return 1;
  }
}

// Return possible arcType what 7z.dll is able to CREATE for given arcname w/o path, or ""
extern "C" int c_szFindFormatForArchiveName (wchar_t *arcname, wchar_t *arcType, UInt32 arcTypeSize, char *errmsg)
{
  szInitLibrary();
  int i = codecs->FindFormatForArchiveName(arcname);
  wcscpy (arcType, i>=0? codecs->Formats[i].Name : L"");   // i<0 == archive type wasn't recognized
  return 0;
}
