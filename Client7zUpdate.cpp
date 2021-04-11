// Update.cpp

#include "7zip/UI/Common/StdAfx.h"

#include "7zip/UI/Common/Update.h"

#include "Common/IntToString.h"
#include "Common/StringConvert.h"

#ifdef _WIN32
#include "Windows/DLL.h"
#endif

#include "Windows/FileDir.h"
#include "Windows/FileFind.h"
#include "Windows/FileName.h"
#include "Windows/PropVariant.h"
#include "Windows/PropVariantConversions.h"
#include "Windows/Time.h"

#include "7zip/Common/FileStreams.h"

#include "7zip/Compress/CopyCoder.h"

#include "7zip/UI/Common/DirItem.h"
#include "7zip/UI/Common/EnumDirItems.h"
#include "7zip/UI/Common/OpenArchive.h"
#include "7zip/UI/Common/UpdateProduce.h"

#include "7zip/UI/Common/EnumDirItems.h"
#include "7zip/UI/Common/SetProperties.h"
#include "7zip/UI/Common/TempFiles.h"
#include "7zip/UI/Common/UpdateCallback.h"

static const char *kUpdateIsNotSupoorted =
  "update operations are not supported for this archive";

using namespace NWindows;
using namespace NCOM;
using namespace NFile;
using namespace NName;

static const wchar_t *kTempFolderPrefix = L"7zE";

using namespace NUpdateArchive;

class COutMultiVolStream:
  public IOutStream,
  public CMyUnknownImp
{
  int _streamIndex; // required stream
  UInt64 _offsetPos; // offset from start of _streamIndex index
  UInt64 _absPos;
  UInt64 _length;

  struct CSubStreamInfo
  {
    COutFileStream *StreamSpec;
    CMyComPtr<IOutStream> Stream;
    UString Name;
    UInt64 Pos;
    UInt64 RealSize;
  };
  CObjectVector<CSubStreamInfo> Streams;
public:
  // CMyComPtr<IArchiveUpdateCallback2> VolumeCallback;
  CRecordVector<UInt64> Sizes;
  UString Prefix;
  CTempFiles *TempFiles;

  void Init()
  {
    _streamIndex = 0;
    _offsetPos = 0;
    _absPos = 0;
    _length = 0;
  }

  HRESULT Close();

  MY_UNKNOWN_IMP1(IOutStream)

  STDMETHOD(Write)(const void *data, UInt32 size, UInt32 *processedSize);
  STDMETHOD(Seek)(Int64 offset, UInt32 seekOrigin, UInt64 *newPosition);
  STDMETHOD(SetSize)(Int64 newSize);
};

// static NSynchronization::CCriticalSection g_TempPathsCS;

HRESULT COutMultiVolStream::Close()
{
  HRESULT res = S_OK;
  for (int i = 0; i < Streams.Size(); i++)
  {
    CSubStreamInfo &s = Streams[i];
    if (s.StreamSpec)
    {
      HRESULT res2 = s.StreamSpec->Close();
      if (res2 != S_OK)
        res = res2;
    }
  }
  return res;
}

STDMETHODIMP COutMultiVolStream::Write(const void *data, UInt32 size, UInt32 *processedSize)
{
  if (processedSize != NULL)
    *processedSize = 0;
  while(size > 0)
  {
    if (_streamIndex >= Streams.Size())
    {
      CSubStreamInfo subStream;

      wchar_t temp[16];
      ConvertUInt32ToString(_streamIndex + 1, temp);
      UString res = temp;
      while (res.Length() < 3)
        res = UString(L'0') + res;
      UString name = Prefix + res;
      subStream.StreamSpec = new COutFileStream;
      subStream.Stream = subStream.StreamSpec;
      if (!subStream.StreamSpec->Create(name, false))
        return ::GetLastError();
      {
        // NSynchronization::CCriticalSectionLock lock(g_TempPathsCS);
        TempFiles->Paths.Add(name);
      }

      subStream.Pos = 0;
      subStream.RealSize = 0;
      subStream.Name = name;
      Streams.Add(subStream);
      cb(TABI_DYNAMAP ("request","volume") ("filename", (const wchar_t*)name));      //// modified by zbr
      continue;
    }
    CSubStreamInfo &subStream = Streams[_streamIndex];

    int index = _streamIndex;
    if (index >= Sizes.Size())
      index = Sizes.Size() - 1;
    UInt64 volSize = Sizes[index];

    if (_offsetPos >= volSize)
    {
      _offsetPos -= volSize;
      _streamIndex++;
      continue;
    }
    if (_offsetPos != subStream.Pos)
    {
      // CMyComPtr<IOutStream> outStream;
      // RINOK(subStream.Stream.QueryInterface(IID_IOutStream, &outStream));
      RINOK(subStream.Stream->Seek(_offsetPos, STREAM_SEEK_SET, NULL));
      subStream.Pos = _offsetPos;
    }

    UInt32 curSize = (UInt32)MyMin((UInt64)size, volSize - subStream.Pos);
    UInt32 realProcessed;
    RINOK(subStream.Stream->Write(data, curSize, &realProcessed));
    data = (void *)((Byte *)data + realProcessed);
    size -= realProcessed;
    subStream.Pos += realProcessed;
    _offsetPos += realProcessed;
    _absPos += realProcessed;
    if (_absPos > _length)
      _length = _absPos;
    if (_offsetPos > subStream.RealSize)
      subStream.RealSize = _offsetPos;
    if (processedSize != NULL)
      *processedSize += realProcessed;
    if (subStream.Pos == volSize)
    {
      _streamIndex++;
      _offsetPos = 0;
    }
    if (realProcessed == 0 && curSize != 0)
      return E_FAIL;
    break;
  }
  return S_OK;
}

STDMETHODIMP COutMultiVolStream::Seek(Int64 offset, UInt32 seekOrigin, UInt64 *newPosition)
{
  if (seekOrigin >= 3)
    return STG_E_INVALIDFUNCTION;
  switch(seekOrigin)
  {
    case STREAM_SEEK_SET:
      _absPos = offset;
      break;
    case STREAM_SEEK_CUR:
      _absPos += offset;
      break;
    case STREAM_SEEK_END:
      _absPos = _length + offset;
      break;
  }
  _offsetPos = _absPos;
  if (newPosition != NULL)
    *newPosition = _absPos;
  _streamIndex = 0;
  return S_OK;
}

STDMETHODIMP COutMultiVolStream::SetSize(Int64 newSize)
{
  if (newSize < 0)
    return E_INVALIDARG;
  int i = 0;
  while (i < Streams.Size())
  {
    CSubStreamInfo &subStream = Streams[i++];
    if ((UInt64)newSize < subStream.RealSize)
    {
      RINOK(subStream.Stream->SetSize(newSize));
      subStream.RealSize = newSize;
      break;
    }
    newSize -= subStream.RealSize;
  }
  while (i < Streams.Size())
  {
    {
      CSubStreamInfo &subStream = Streams.Back();
      subStream.Stream.Release();
      NDirectory::DeleteFileAlways(subStream.Name);
    }
    Streams.DeleteBack();
  }
  _offsetPos = _absPos;
  _streamIndex = 0;
  _length = newSize;
  return S_OK;
}

static const wchar_t *kDefaultArchiveType = L"7z";
static const wchar_t *kSFXExtension =
  #ifdef _WIN32
    L"exe";
  #else
    L"";
  #endif

bool CUpdateOptions::Init(const CCodecs *codecs, const CIntVector &formatIndices, const UString &arcPath)
{
  if (formatIndices.Size() > 1)
    return false;
  int arcTypeIndex = -1;
  if (formatIndices.Size() != 0)
    arcTypeIndex = formatIndices[0];
  if (arcTypeIndex >= 0)
    MethodMode.FormatIndex = arcTypeIndex;
  else
  {
    MethodMode.FormatIndex = codecs->FindFormatForArchiveName(arcPath);
    // It works incorrectly for update command if archive has some non-default extension!
    if (MethodMode.FormatIndex < 0)
      MethodMode.FormatIndex = codecs->FindFormatForArchiveType(kDefaultArchiveType);
  }
  if (MethodMode.FormatIndex < 0)
    return false;
  const CArcInfoEx &arcInfo = codecs->Formats[MethodMode.FormatIndex];
  if (!arcInfo.UpdateEnabled)
    return false;
  UString typeExt = arcInfo.GetMainExt();
  UString ext = typeExt;
  if (SfxMode)
    ext = kSFXExtension;
  ArchivePath.BaseExtension = ext;
  ArchivePath.VolExtension = typeExt;
  ArchivePath.ParseFromPath(arcPath);
  for (int i = 0; i < Commands.Size(); i++)
  {
    CUpdateArchiveCommand &uc = Commands[i];
    uc.ArchivePath.BaseExtension = ext;
    uc.ArchivePath.VolExtension = typeExt;
    uc.ArchivePath.ParseFromPath(uc.UserArchivePath);
  }
  return true;
}

/*
struct CUpdateProduceCallbackImp: public IUpdateProduceCallback
{
  const CObjectVector<CArcItem> *_arcItems;
  IUpdateCallbackUI *_callback;

  CUpdateProduceCallbackImp(const CObjectVector<CArcItem> *a,
      IUpdateCallbackUI *callback): _arcItems(a), _callback(callback) {}
  virtual HRESULT ShowDeleteFile(int arcIndex);
};

HRESULT CUpdateProduceCallbackImp::ShowDeleteFile(int arcIndex)
{
  return _callback->ShowDeleteFile((*_arcItems)[arcIndex].Name);
}
*/

static HRESULT Compress(
    CCodecs *codecs,
    const CActionSet &actionSet,
    IInArchive *archive,
    const CCompressionMethodMode &compressionMethod,
    CArchivePath &archivePath,
    const CObjectVector<CArcItem> &arcItems,
    bool shareForWrite,
    bool stdInMode,
    /* const UString & stdInFileName, */
    bool stdOutMode,
    const CDirItems &dirItems,
    bool sfxMode,
    const UString &sfxModule,
    const CRecordVector<UInt64> &volumesSizes,
    CTempFiles &tempFiles,
    CUpdateErrorInfo &errorInfo,
    IUpdateCallbackUI *callback)
{
  CMyComPtr<IOutArchive> outArchive;
  if (archive != NULL)
  {
    CMyComPtr<IInArchive> archive2 = archive;
    HRESULT result = archive2.QueryInterface(IID_IOutArchive, &outArchive);
    if (result != S_OK)
      throw kUpdateIsNotSupoorted;
  }
  else
  {
    RINOK(codecs->CreateOutArchive(compressionMethod.FormatIndex, outArchive));

    #ifdef EXTERNAL_CODECS
    {
      CMyComPtr<ISetCompressCodecsInfo> setCompressCodecsInfo;
      outArchive.QueryInterface(IID_ISetCompressCodecsInfo, (void **)&setCompressCodecsInfo);
      if (setCompressCodecsInfo)
      {
        RINOK(setCompressCodecsInfo->SetCompressCodecsInfo(codecs));
      }
    }
    #endif
  }
  if (outArchive == 0)
    throw kUpdateIsNotSupoorted;

  NFileTimeType::EEnum fileTimeType;
  UInt32 value;
  RINOK(outArchive->GetFileTimeType(&value));

  switch(value)
  {
    case NFileTimeType::kWindows:
    case NFileTimeType::kUnix:
    case NFileTimeType::kDOS:
      fileTimeType = (NFileTimeType::EEnum)value;
      break;
    default:
      return E_FAIL;
  }

  CRecordVector<CUpdatePair2> updatePairs2;

  {
    CRecordVector<CUpdatePair> updatePairs;
    GetUpdatePairInfoList(dirItems, arcItems, fileTimeType, updatePairs); // must be done only once!!!
    // CUpdateProduceCallbackImp upCallback(&arcItems, callback);
    UpdateProduce(updatePairs, actionSet, updatePairs2, NULL /* &upCallback */);
  }

  UInt32 numFiles = 0;
  for (int i = 0; i < updatePairs2.Size(); i++)
    if (updatePairs2[i].NewData)
      numFiles++;

  RINOK(callback->SetNumFiles(numFiles));

  //// added by zbr: Count total amount and size of files to compress
  {
    UInt64 totalSize = 0;
    for (int i = 0; i < updatePairs2.Size(); i++)
    {
      if (updatePairs2[i].ExistOnDisk())
        totalSize += dirItems.Items[updatePairs2[i].DirIndex].Size;
      else if (updatePairs2[i].ExistInArchive())
        totalSize += arcItems[updatePairs2[i].ArcIndex].Size;
    }
    cb(TABI_DYNAMAP ("request","total") ("files", updatePairs2.Size()) ("original", totalSize));
  }

  CArchiveUpdateCallback *updateCallbackSpec = new CArchiveUpdateCallback;
  CMyComPtr<IArchiveUpdateCallback> updateCallback(updateCallbackSpec);

  updateCallbackSpec->ShareForWrite = shareForWrite;
  updateCallbackSpec->StdInMode = stdInMode;
  updateCallbackSpec->Callback = callback;
  updateCallbackSpec->DirItems = &dirItems;
  updateCallbackSpec->ArcItems = &arcItems;
  updateCallbackSpec->UpdatePairs = &updatePairs2;
  updateCallbackSpec->Archive = archive;                   //// added by zbr: file may be renamed, Archive required to copy all file properties except for name

  CMyComPtr<ISequentialOutStream> outStream;

  const UString &archiveName = archivePath.GetFinalPath();
  if (!stdOutMode)
  {
    UString resultPath;
    int pos;
    if (!NFile::NDirectory::MyGetFullPathName(archiveName, resultPath, pos))
      throw 1417161;
    NFile::NDirectory::CreateComplexDirectory(resultPath.Left(pos));
  }

  COutFileStream *outStreamSpec = NULL;
  COutMultiVolStream *volStreamSpec = NULL;

  if (volumesSizes.Size() == 0)
  {
    if (stdOutMode)
      outStream = new CStdOutFileStream;
    else
    {
      outStreamSpec = new COutFileStream;
      outStream = outStreamSpec;
      bool isOK = false;
      UString realPath;
      for (int i = 0; i < (1 << 16); i++)
      {
        if (archivePath.Temp)
        {
          if (i > 0)
          {
            wchar_t s[16];
            ConvertUInt32ToString(i, s);
            archivePath.TempPostfix = s;
          }
          realPath = archivePath.GetTempPath();
        }
        else
          realPath = archivePath.GetFinalPath();
        if (outStreamSpec->Create(realPath, false))
        {
          tempFiles.Paths.Add(realPath);
          isOK = true;
          break;
        }
        if (::GetLastError() != ERROR_FILE_EXISTS)
          break;
        if (!archivePath.Temp)
          break;
      }
      if (!isOK)
      {
        errorInfo.SystemError = ::GetLastError();
        errorInfo.FileName = realPath;
        errorInfo.Message = L"7-Zip cannot open file";
        return E_FAIL;
      }
    }
  }
  else
  {
    if (stdOutMode)
      return E_FAIL;
    volStreamSpec = new COutMultiVolStream;
    outStream = volStreamSpec;
    volStreamSpec->Sizes = volumesSizes;
    volStreamSpec->Prefix = archivePath.GetFinalPath() + UString(L".");
    volStreamSpec->TempFiles = &tempFiles;
    volStreamSpec->Init();

    /*
    updateCallbackSpec->VolumesSizes = volumesSizes;
    updateCallbackSpec->VolName = archivePath.Prefix + archivePath.Name;
    if (!archivePath.VolExtension.IsEmpty())
      updateCallbackSpec->VolExt = UString(L'.') + archivePath.VolExtension;
    */
  }

  RINOK(SetProperties(outArchive, compressionMethod.Properties));

  if (sfxMode)
  {
    CInFileStream *sfxStreamSpec = new CInFileStream;
    CMyComPtr<IInStream> sfxStream(sfxStreamSpec);
    if (!sfxStreamSpec->Open(sfxModule))
    {
      errorInfo.SystemError = ::GetLastError();
      errorInfo.Message = L"7-Zip cannot open SFX module";
      errorInfo.FileName = sfxModule;
      return E_FAIL;
    }

    CMyComPtr<ISequentialOutStream> sfxOutStream;
    COutFileStream *outStreamSpec = NULL;
    if (volumesSizes.Size() == 0)
      sfxOutStream = outStream;
    else
    {
      outStreamSpec = new COutFileStream;
      sfxOutStream = outStreamSpec;
      UString realPath = archivePath.GetFinalPath();
      if (!outStreamSpec->Create(realPath, false))
      {
        errorInfo.SystemError = ::GetLastError();
        errorInfo.FileName = realPath;
        errorInfo.Message = L"7-Zip cannot open file";
        return E_FAIL;
      }
    }
    RINOK(NCompress::CopyStream(sfxStream, sfxOutStream, NULL));
    if (outStreamSpec)
    {
      RINOK(outStreamSpec->Close());
    }
  }

  HRESULT result = outArchive->UpdateItems(outStream, updatePairs2.Size(), updateCallback);
  callback->Finilize();
  RINOK(result);
  if (outStreamSpec)
    result = outStreamSpec->Close();
  else if (volStreamSpec)
    result = volStreamSpec->Close();
  return result;
}

HRESULT EnumerateInArchiveItems(const NWildcard::CCensor &censor,
    const CArc &arc,
    CObjectVector<CArcItem> &arcItems)
{
  UInt32 numItems;
  IInArchive *archive = arc.Archive;
  RINOK(archive->GetNumberOfItems(&numItems));

  //// modified by zbr: Put into arcItems[] all files numbered in arc_filelist[]
  arcItems.Clear();
  arcItems.Reserve(arc_files);
  for (UInt32 arc_i=0; arc_i<arc_files; arc_i++)
  {
    UInt32 i = arc_filelist[arc_i];

    CArcItem ai;
    ai.Censored = true;
    if (new_names && new_names[arc_i] && new_names[arc_i][0])
      ai.Name = new_names[arc_i];                                //// added by zbr: file may be renamed
    else
      RINOK(arc.GetItemPath(i, ai.Name));
    RINOK(IsArchiveItemFolder(archive, i, ai.IsDir));
    RINOK(arc.GetItemMTime(i, ai.MTime, ai.MTimeDefined));

    {
      CPropVariant prop;
      RINOK(archive->GetProperty(i, kpidSize, &prop));
      ai.SizeDefined = (prop.vt != VT_EMPTY);
      if (ai.SizeDefined)
        ai.Size = ConvertPropVariantToUInt64(prop);
    }

    {
      CPropVariant prop;
      RINOK(archive->GetProperty(i, kpidTimeType, &prop));
      if (prop.vt == VT_UI4)
      {
        ai.TimeType = (int)(NFileTimeType::EEnum)prop.ulVal;
        switch(ai.TimeType)
        {
          case NFileTimeType::kWindows:
          case NFileTimeType::kUnix:
          case NFileTimeType::kDOS:
            break;
          default:
            return E_FAIL;
        }
      }
    }

    ai.IndexInServer = i;
    arcItems.Add(ai);
  }
  return S_OK;
}


static HRESULT UpdateWithItemLists(
    CCodecs *codecs,
    CUpdateOptions &options,
    IInArchive *archive,
    const CObjectVector<CArcItem> &arcItems,
    CDirItems &dirItems,
    CTempFiles &tempFiles,
    CUpdateErrorInfo &errorInfo,
    IUpdateCallbackUI2 *callback)
{
  for(int i = 0; i < options.Commands.Size(); i++)
  {
    CUpdateArchiveCommand &command = options.Commands[i];
    if (options.StdOutMode)
    {
      RINOK(callback->StartArchive(L"stdout", archive != 0));
    }
    else
    {
      RINOK(callback->StartArchive(command.ArchivePath.GetFinalPath(),
          i == 0 && options.UpdateArchiveItself && archive != 0));
    }

    RINOK(Compress(
        codecs,
        command.ActionSet, archive,
        options.MethodMode,
        command.ArchivePath,
        arcItems,
        options.OpenShareForWrite,
        options.StdInMode,
        /* options.StdInFileName, */
        options.StdOutMode,
        dirItems,
        options.SfxMode, options.SfxModule,
        options.VolumesSizes,
        tempFiles,
        errorInfo, callback));

    RINOK(callback->FinishArchive());
  }
  return S_OK;
}

#if defined(_WIN32) && !defined(UNDER_CE)
class CCurrentDirRestorer
{
  UString _path;
public:
  CCurrentDirRestorer() { NFile::NDirectory::MyGetCurrentDirectory(_path); }
  ~CCurrentDirRestorer() { RestoreDirectory();}
  bool RestoreDirectory() { return BOOLToBool(NFile::NDirectory::MySetCurrentDirectory(_path)); }
};
#endif

struct CEnumDirItemUpdateCallback: public IEnumDirItemCallback
{
  IUpdateCallbackUI2 *Callback;
  HRESULT ScanProgress(UInt64 numFolders, UInt64 numFiles, const wchar_t *path)
  {
    return Callback->ScanProgress(numFolders, numFiles, path);
  }
};

#ifdef _WIN32
typedef ULONG (FAR PASCAL MY_MAPISENDDOCUMENTS)(
  ULONG_PTR ulUIParam,
  LPSTR lpszDelimChar,
  LPSTR lpszFilePaths,
  LPSTR lpszFileNames,
  ULONG ulReserved
);
typedef MY_MAPISENDDOCUMENTS FAR *MY_LPMAPISENDDOCUMENTS;
#endif

HRESULT UpdateArchive(
    CCodecs *codecs,
    const NWildcard::CCensor &censor,
    CUpdateOptions &options,
    CUpdateErrorInfo &errorInfo,
    IOpenCallbackUI *openCallback,
    IUpdateCallbackUI2 *callback)
{
  if (options.StdOutMode && options.EMailMode)
    return E_FAIL;

  if (options.VolumesSizes.Size() > 0 && (options.EMailMode || options.SfxMode))
    return E_NOTIMPL;

  if (options.SfxMode)
  {
    CProperty property;
    property.Name = L"rsfx";
    property.Value = L"on";
    options.MethodMode.Properties.Add(property);
    if (options.SfxModule.IsEmpty())
    {
      errorInfo.Message = L"SFX file is not specified";
      return E_FAIL;
    }
    UString name = options.SfxModule;
    #ifdef UNDER_CE
    if (!NFind::DoesFileExist(name))
    #else
    if (!NDirectory::MySearchPath(NULL, name, NULL, options.SfxModule))
    #endif
    {
      errorInfo.SystemError = ::GetLastError();
      errorInfo.Message = L"7-Zip cannot find specified SFX module";
      errorInfo.FileName = name;
      return E_FAIL;
    }
  }

  const UString archiveName = options.ArchivePath.GetFinalPath();

  CArchiveLink archiveLink;
  NFind::CFileInfoW archiveFileInfo;

  if (archiveFileInfo.Find(archiveName))
  {
    if (archiveFileInfo.IsDir())
      throw "there is no such archive";
    if (options.VolumesSizes.Size() > 0)
      return E_NOTIMPL;
    CIntVector formatIndices;
    if (options.MethodMode.FormatIndex >= 0)
      formatIndices.Add(options.MethodMode.FormatIndex);
    HRESULT result = archiveLink.Open2(codecs, formatIndices, false, NULL, archiveName, openCallback);
    if (result == E_ABORT)
      return result;
    RINOK(callback->OpenResult(archiveName, result));
    RINOK(result);
    if (archiveLink.VolumePaths.Size() > 1)
    {
      errorInfo.SystemError = (DWORD)E_NOTIMPL;
      errorInfo.Message = L"Updating for multivolume archives is not implemented";
      return E_NOTIMPL;
    }

    CArc &arc = archiveLink.Arcs.Back();
    arc.MTimeDefined = !archiveFileInfo.IsDevice;
    arc.MTime = archiveFileInfo.MTime;
  }
  else
  {
    /*
    if (archiveType.IsEmpty())
      throw "type of archive is not specified";
    */
  }

  CDirItems dirItems;
  if (options.StdInMode)
  {
    CDirItem di;
    di.Name = options.StdInFileName;
    di.Size = (UInt64)(Int64)-1;
    di.Attrib = 0;
    NTime::GetCurUtcFileTime(di.MTime);
    di.CTime = di.ATime = di.MTime;
    dirItems.Items.Add(di);
  }
  else
  {
    bool needScanning = false;
    for(int i = 0; i < options.Commands.Size(); i++)
      if (options.Commands[i].ActionSet.NeedScanning())
        needScanning = true;
    if (needScanning)
    {
      CEnumDirItemUpdateCallback enumCallback;
      enumCallback.Callback = callback;
      RINOK(callback->StartScanning());
      UStringVector errorPaths;
      CRecordVector<DWORD> errorCodes;
      HRESULT res = EnumerateItems(censor, dirItems, &enumCallback, errorPaths, errorCodes);
      for (int i = 0; i < errorPaths.Size(); i++)
      {
        RINOK(callback->CanNotFindError(errorPaths[i], errorCodes[i]));
      }
      if (res != S_OK)
      {
        if (res != E_ABORT)
          errorInfo.Message = L"Scanning error";
        return res;
      }
      RINOK(callback->FinishScanning());
    }
  }

  UString tempDirPrefix;
  bool usesTempDir = false;

  #ifdef _WIN32
  NDirectory::CTempDirectoryW tempDirectory;
  if (options.EMailMode && options.EMailRemoveAfter)
  {
    tempDirectory.Create(kTempFolderPrefix);
    tempDirPrefix = tempDirectory.GetPath();
    NormalizeDirPathPrefix(tempDirPrefix);
    usesTempDir = true;
  }
  #endif

  CTempFiles tempFiles;

  bool createTempFile = false;

  bool thereIsInArchive = archiveLink.IsOpen;

  if (!options.StdOutMode && options.UpdateArchiveItself)
  {
    CArchivePath &ap = options.Commands[0].ArchivePath;
    ap = options.ArchivePath;
    // if ((archive != 0 && !usesTempDir) || !options.WorkingDir.IsEmpty())
    if ((thereIsInArchive || !options.WorkingDir.IsEmpty()) && !usesTempDir && options.VolumesSizes.Size() == 0)
    {
      createTempFile = true;
      ap.Temp = true;
      if (!options.WorkingDir.IsEmpty())
      {
        ap.TempPrefix = options.WorkingDir;
        NormalizeDirPathPrefix(ap.TempPrefix);
      }
    }
  }

  for(int i = 0; i < options.Commands.Size(); i++)
  {
    CArchivePath &ap = options.Commands[i].ArchivePath;
    if (usesTempDir)
    {
      // Check it
      ap.Prefix = tempDirPrefix;
      // ap.Temp = true;
      // ap.TempPrefix = tempDirPrefix;
    }
    if (i > 0 || !createTempFile)
    {
      const UString &path = ap.GetFinalPath();
      if (NFind::DoesFileOrDirExist(path))
      {
        errorInfo.SystemError = 0;
        errorInfo.Message = L"The file already exists";
        errorInfo.FileName = path;
        return E_FAIL;
      }
    }
  }

  CObjectVector<CArcItem> arcItems;
  if (thereIsInArchive)
  {
    RINOK(EnumerateInArchiveItems(censor, archiveLink.Arcs.Back(), arcItems));
  }

  RINOK(UpdateWithItemLists(codecs, options,
      thereIsInArchive ? archiveLink.GetArchive() : 0,
      arcItems, dirItems,
      tempFiles, errorInfo, callback));

  if (thereIsInArchive)
  {
    RINOK(archiveLink.Close());
    archiveLink.Release();
  }

  tempFiles.Paths.Clear();
  if (createTempFile)
  {
    try
    {
      CArchivePath &ap = options.Commands[0].ArchivePath;
      const UString &tempPath = ap.GetTempPath();
      if (thereIsInArchive)
        if (!NDirectory::DeleteFileAlways(archiveName))
        {
          errorInfo.SystemError = ::GetLastError();
          errorInfo.Message = L"7-Zip cannot delete the file";
          errorInfo.FileName = archiveName;
          return E_FAIL;
        }
      if (!NDirectory::MyMoveFile(tempPath, archiveName))
      {
        errorInfo.SystemError = ::GetLastError();
        errorInfo.Message = L"7-Zip cannot move the file";
        errorInfo.FileName = tempPath;
        errorInfo.FileName2 = archiveName;
        return E_FAIL;
      }
    }
    catch(...)
    {
      throw;
    }
  }

  #if defined(_WIN32) && !defined(UNDER_CE)
  if (options.EMailMode)
  {
    NDLL::CLibrary mapiLib;
    if (!mapiLib.Load(TEXT("Mapi32.dll")))
    {
      errorInfo.SystemError = ::GetLastError();
      errorInfo.Message = L"7-Zip cannot load Mapi32.dll";
      return E_FAIL;
    }
    MY_LPMAPISENDDOCUMENTS fnSend = (MY_LPMAPISENDDOCUMENTS)mapiLib.GetProc("MAPISendDocuments");
    if (fnSend == 0)
    {
      errorInfo.SystemError = ::GetLastError();
      errorInfo.Message = L"7-Zip cannot find MAPISendDocuments function";
      return E_FAIL;
    }
    UStringVector fullPaths;
    int i;
    for(i = 0; i < options.Commands.Size(); i++)
    {
      CArchivePath &ap = options.Commands[i].ArchivePath;
      UString arcPath;
      if (!NFile::NDirectory::MyGetFullPathName(ap.GetFinalPath(), arcPath))
      {
        errorInfo.SystemError = ::GetLastError();
        errorInfo.Message = L"GetFullPathName error";
        return E_FAIL;
      }
      fullPaths.Add(arcPath);
    }
    CCurrentDirRestorer curDirRestorer;
    for(i = 0; i < fullPaths.Size(); i++)
    {
      UString arcPath = fullPaths[i];
      UString fileName = ExtractFileNameFromPath(arcPath);
      AString path = GetAnsiString(arcPath);
      AString name = GetAnsiString(fileName);
      // Warning!!! MAPISendDocuments function changes Current directory
      fnSend(0, ";", (LPSTR)(LPCSTR)path, (LPSTR)(LPCSTR)name, 0);
    }
  }
  #endif
  return S_OK;
}




// UpdateProduce.cpp
using namespace NUpdateArchive;

static const char *kUpdateActionSetCollision = "Internal collision in update action set";

void UpdateProduce(
    const CRecordVector<CUpdatePair> &updatePairs,
    const CActionSet &actionSet,
    CRecordVector<CUpdatePair2> &operationChain,
    IUpdateProduceCallback *callback)
{
  for (int i = 0; i < updatePairs.Size(); i++)
  {
    const CUpdatePair &pair = updatePairs[i];

    CUpdatePair2 up2;
    up2.IsAnti = false;
    up2.DirIndex = pair.DirIndex;
    up2.ArcIndex = pair.ArcIndex;
    up2.NewData = up2.NewProps = true;

    switch(actionSet.StateActions[pair.State])
    {
      case NPairAction::kIgnore:
        /*
        if (pair.State != NPairState::kOnlyOnDisk)
          IgnoreArchiveItem(m_ArchiveItems[pair.ArcIndex]);
        // cout << "deleting";
        */
        if (callback)
          callback->ShowDeleteFile(pair.ArcIndex);
        continue;

      case NPairAction::kCopy:
        if (pair.State == NPairState::kOnlyOnDisk)
          throw kUpdateActionSetCollision;
        up2.NewData = false;                                      //// modified by zbr: file may be renamed
        break;

      case NPairAction::kCompress:
        if (pair.State == NPairState::kOnlyInArchive ||
            pair.State == NPairState::kNotMasked)
          throw kUpdateActionSetCollision;
        break;

      case NPairAction::kCompressAsAnti:
        up2.IsAnti = true;
        break;
    }
    operationChain.Add(up2);
  }
  operationChain.ReserveDown();
}




// UpdateCallback.cpp
using namespace NWindows;

CArchiveUpdateCallback::CArchiveUpdateCallback():
  Callback(0),
  ShareForWrite(false),
  StdInMode(false),
  DirItems(0),
  ArcItems(0),
  UpdatePairs(0),
  NewNames(0)
  {}


STDMETHODIMP CArchiveUpdateCallback::SetTotal(UInt64 size)
{
  COM_TRY_BEGIN
  return Callback->SetTotal(size);
  COM_TRY_END
}

STDMETHODIMP CArchiveUpdateCallback::SetCompleted(const UInt64 *completeValue)
{
  COM_TRY_BEGIN
  return Callback->SetCompleted(completeValue);
  COM_TRY_END
}

STDMETHODIMP CArchiveUpdateCallback::SetRatioInfo(const UInt64 *inSize, const UInt64 *outSize)
{
  COM_TRY_BEGIN
  return Callback->SetRatioInfo(inSize, outSize);
  COM_TRY_END
}


/*
STATPROPSTG kProperties[] =
{
  { NULL, kpidPath, VT_BSTR},
  { NULL, kpidIsDir, VT_BOOL},
  { NULL, kpidSize, VT_UI8},
  { NULL, kpidCTime, VT_FILETIME},
  { NULL, kpidATime, VT_FILETIME},
  { NULL, kpidMTime, VT_FILETIME},
  { NULL, kpidAttrib, VT_UI4},
  { NULL, kpidIsAnti, VT_BOOL}
};

STDMETHODIMP CArchiveUpdateCallback::EnumProperties(IEnumSTATPROPSTG **)
{
  return CStatPropEnumerator::CreateEnumerator(kProperties, sizeof(kProperties) / sizeof(kProperties[0]), enumerator);
}
*/

STDMETHODIMP CArchiveUpdateCallback::GetUpdateItemInfo(UInt32 index,
      Int32 *newData, Int32 *newProps, UInt32 *indexInArchive)
{
  COM_TRY_BEGIN
  RINOK(Callback->CheckBreak());
  const CUpdatePair2 &up = (*UpdatePairs)[index];
  if (newData != NULL) *newData = BoolToInt(up.NewData);
  if (newProps != NULL) *newProps = BoolToInt(up.NewProps);
  if (indexInArchive != NULL)
  {
    *indexInArchive = (UInt32)-1;
    if (up.ExistInArchive())
      *indexInArchive = (ArcItems == 0) ? up.ArcIndex : (*ArcItems)[up.ArcIndex].IndexInServer;
  }
  return S_OK;
  COM_TRY_END
}

STDMETHODIMP CArchiveUpdateCallback::GetProperty(UInt32 index, PROPID propID, PROPVARIANT *value)
{
  COM_TRY_BEGIN
  const CUpdatePair2 &up = (*UpdatePairs)[index];
  NWindows::NCOM::CPropVariant prop;

  if (propID == kpidIsAnti)
  {
    prop = up.IsAnti;
    prop.Detach(value);
    return S_OK;
  }

  if (up.IsAnti)
  {
    switch(propID)
    {
      case kpidIsDir:
      case kpidPath:
        break;
      case kpidSize:
        prop = (UInt64)0;
        prop.Detach(value);
        return S_OK;
      default:
        prop.Detach(value);
        return S_OK;
    }
  }

  if (up.ExistOnDisk())
  {
    const CDirItem &di = DirItems->Items[up.DirIndex];
    switch(propID)
    {
      case kpidPath:  prop = DirItems->GetLogPath(up.DirIndex); break;
      case kpidIsDir:  prop = di.IsDir(); break;
      case kpidSize:  prop = di.Size; break;
      case kpidAttrib:  prop = di.Attrib; break;
      case kpidCTime:  prop = di.CTime; break;
      case kpidATime:  prop = di.ATime; break;
      case kpidMTime:  prop = di.MTime; break;
    }
  }
  else
  {
    if (propID == kpidPath)
    {
      if (up.NewNameIndex >= 0)
      {
        prop = (*NewNames)[up.NewNameIndex];
        prop.Detach(value);
        return S_OK;
      }
    }
    if (up.ExistInArchive() && Archive)
    {
      if (propID == kpidPath  &&  ArcItems)          //// added by zbr: file may be renamed
      {
        prop = (*ArcItems)[up.ArcIndex].Name;
        prop.Detach(value);
        return S_OK;
      }

      UInt32 indexInArchive;
      if (ArcItems == 0)
        indexInArchive = up.ArcIndex;
      else
        indexInArchive = (*ArcItems)[up.ArcIndex].IndexInServer;
      return Archive->GetProperty(indexInArchive, propID, value);
    }
  }
  prop.Detach(value);
  return S_OK;
  COM_TRY_END
}

STDMETHODIMP CArchiveUpdateCallback::GetStream(UInt32 index, ISequentialInStream **inStream)
{
  COM_TRY_BEGIN
  const CUpdatePair2 &up = (*UpdatePairs)[index];
  if (!up.NewData)
    return E_FAIL;

  RINOK(Callback->CheckBreak());
  RINOK(Callback->Finilize());

  if (up.IsAnti)
  {
    return Callback->GetStream((*ArcItems)[up.ArcIndex].Name, true);
  }
  const CDirItem &di = DirItems->Items[up.DirIndex];
  RINOK(Callback->GetStream(DirItems->GetLogPath(up.DirIndex), false));

  if (di.IsDir())
    return S_OK;

  if (StdInMode)
  {
    CStdInFileStream *inStreamSpec = new CStdInFileStream;
    CMyComPtr<ISequentialInStream> inStreamLoc(inStreamSpec);
    *inStream = inStreamLoc.Detach();
  }
  else
  {
    CInFileStream *inStreamSpec = new CInFileStream;
    CMyComPtr<ISequentialInStream> inStreamLoc(inStreamSpec);
    const UString path = DirItems->GetPhyPath(up.DirIndex);
    if (!inStreamSpec->OpenShared(path, ShareForWrite))
    {
      return Callback->OpenFileError(path, ::GetLastError());
    }
    *inStream = inStreamLoc.Detach();
  }
  return S_OK;
  COM_TRY_END
}

STDMETHODIMP CArchiveUpdateCallback::SetOperationResult(Int32 operationResult)
{
  COM_TRY_BEGIN
  return Callback->SetOperationResult(operationResult);
  COM_TRY_END
}

STDMETHODIMP CArchiveUpdateCallback::GetVolumeSize(UInt32 index, UInt64 *size)
{
  if (VolumesSizes.Size() == 0)
    return S_FALSE;
  if (index >= (UInt32)VolumesSizes.Size())
    index = VolumesSizes.Size() - 1;
  *size = VolumesSizes[index];
  return S_OK;
}

STDMETHODIMP CArchiveUpdateCallback::GetVolumeStream(UInt32 index, ISequentialOutStream **volumeStream)
{
  COM_TRY_BEGIN
  wchar_t temp[16];
  ConvertUInt32ToString(index + 1, temp);
  UString res = temp;
  while (res.Length() < 2)
    res = UString(L'0') + res;
  UString fileName = VolName;
  fileName += L'.';
  fileName += res;
  fileName += VolExt;
  COutFileStream *streamSpec = new COutFileStream;
  CMyComPtr<ISequentialOutStream> streamLoc(streamSpec);
  if (!streamSpec->Create(fileName, false))
    return ::GetLastError();
  *volumeStream = streamLoc.Detach();
  return S_OK;
  COM_TRY_END
}

STDMETHODIMP CArchiveUpdateCallback::CryptoGetTextPassword2(Int32 *passwordIsDefined, BSTR *password)
{
  COM_TRY_BEGIN
  return Callback->CryptoGetTextPassword2(passwordIsDefined, password);
  COM_TRY_END
}

STDMETHODIMP CArchiveUpdateCallback::CryptoGetTextPassword(BSTR *password)
{
  COM_TRY_BEGIN
  return Callback->CryptoGetTextPassword(password);
  COM_TRY_END
}
