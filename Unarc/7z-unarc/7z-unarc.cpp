// ZHandler.cpp
#define INITGUID

#include "StdAfx.h"

#include "Common/ComTry.h"

#include "Windows/PropVariant.h"

#include "../Common/ProgressUtils.h"
#include "../Common/RegisterArc.h"
#include "../Common/StreamUtils.h"

#include "../Compress/ZDecoder.h"

#include "Common/DummyOutStream.h"



//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Inline required source files
#include "7zip/Common/ProgressUtils.cpp"
#include "7zip/Common/StreamUtils.cpp"
#include "Common/DummyOutStream.cpp"
#include "Windows/PropVariant.cpp"
#include "ArchiveExports.cpp"

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//#include "7zip/Archive/DllExports.cpp"
#include "../../Windows/NtCheck.h"
HINSTANCE g_hInstance;

#define NT_CHECK_FAIL_ACTION return FALSE;

extern "C"
BOOL WINAPI DllMain(HINSTANCE hInstance, DWORD dwReason, LPVOID /*lpReserved*/)
{
  if (dwReason == DLL_PROCESS_ATTACH)
  {
    g_hInstance = hInstance;
    NT_CHECK;
  }
  return TRUE;
}

STDAPI CreateArchiver(const GUID *classID, const GUID *iid, void **outObject);

STDAPI CreateObject(const GUID *clsid, const GUID *iid, void **outObject)
{
  return CreateArchiver(clsid, iid, outObject);
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////




namespace NArchive {
namespace NUnarc {

class CHandler:
  public IInArchive,
  public CMyUnknownImp
{
  CMyComPtr<IInStream> _stream;
  UInt64 _streamStartPosition;
  UInt64 _packSize;
  Byte _properties;
public:
  MY_UNKNOWN_IMP1(IInArchive)
  INTERFACE_IInArchive(;)
};

STATPROPSTG kProps[] =
{
  { NULL, kpidPackSize, VT_UI8}
};

IMP_IInArchive_Props
IMP_IInArchive_ArcProps_NO

STDMETHODIMP CHandler::GetNumberOfItems(UInt32 *numItems)
{
  *numItems = 1;
  return S_OK;
}

STDMETHODIMP CHandler::GetProperty(UInt32 /* index */, PROPID propID,  PROPVARIANT *value)
{
  NWindows::NCOM::CPropVariant prop;
  switch(propID)
  {
    case kpidPackSize: prop = _packSize; break;
  }
  prop.Detach(value);
  return S_OK;
}

static const int kSignatureSize = 3;

STDMETHODIMP CHandler::Open(IInStream *stream,
    const UInt64 * /* maxCheckStartPosition */,
    IArchiveOpenCallback * /* openArchiveCallback */)
{
  COM_TRY_BEGIN
  {
/*
    RINOK(stream->Seek(0, STREAM_SEEK_CUR, &_streamStartPosition));
    Byte buffer[kSignatureSize];
    RINOK(ReadStream_FALSE(stream, buffer, kSignatureSize));
    if (buffer[0] != 0x1F || buffer[1] != 0x9D)
      return S_FALSE;
    _properties = buffer[2];

    UInt64 endPosition;
    RINOK(stream->Seek(0, STREAM_SEEK_END, &endPosition));
    _packSize = endPosition - _streamStartPosition - kSignatureSize;
*/
    _stream = stream;
  }
  return S_OK;
  COM_TRY_END
}

STDMETHODIMP CHandler::Close()
{
  _stream.Release();
  return S_OK;
}


STDMETHODIMP CHandler::Extract(const UInt32 *indices, UInt32 numItems,
    Int32 testMode, IArchiveExtractCallback *extractCallback)
{
  COM_TRY_BEGIN
  if (numItems == 0)
    return S_OK;
  if (numItems != (UInt32)-1 && (numItems != 1 || indices[0] != 0))
    return E_INVALIDARG;

  extractCallback->SetTotal(_packSize);

  UInt64 currentTotalPacked = 0;

  RINOK(extractCallback->SetCompleted(&currentTotalPacked));

  CMyComPtr<ISequentialOutStream> realOutStream;
  Int32 askMode = testMode ?
      NExtract::NAskMode::kTest :
      NExtract::NAskMode::kExtract;

  RINOK(extractCallback->GetStream(0, &realOutStream, askMode));

  if (!testMode && !realOutStream)
    return S_OK;

  extractCallback->PrepareOperation(askMode);

  CDummyOutStream *outStreamSpec = new CDummyOutStream;
  CMyComPtr<ISequentialOutStream> outStream(outStreamSpec);
  outStreamSpec->SetStream(realOutStream);
  outStreamSpec->Init();
  realOutStream.Release();

  CLocalProgress *lps = new CLocalProgress;
  CMyComPtr<ICompressProgressInfo> progress = lps;
  lps->Init(extractCallback, true);

  RINOK(_stream->Seek(_streamStartPosition + kSignatureSize, STREAM_SEEK_SET, NULL));

  CMyComPtr<ICompressCoder> decoder;
  //NCompress::NZ::CDecoder *decoderSpec = new NCompress::NZ::CDecoder;
  //decoder = decoderSpec;

  //HRESULT result = decoderSpec->SetDecoderProperties2(&_properties, 1);
  HRESULT result;

  int opResult;
  if (result != S_OK)
    opResult = NExtract::NOperationResult::kUnSupportedMethod;
  else
  {
    result = decoder->Code(_stream, outStream, NULL, NULL, progress);
    if (result == S_FALSE)
      opResult = NExtract::NOperationResult::kDataError;
    else
    {
      RINOK(result);
      opResult = NExtract::NOperationResult::kOK;
    }
  }
  outStream.Release();
  return extractCallback->SetOperationResult(opResult);
  COM_TRY_END
}

static IInArchive *CreateArc() { return new CHandler; }

static CArcInfo g_ArcInfo =
  { L"FreeArc", L"arc", 0, 0x80, {65,114,67,1}, 4, false, CreateArc, 0 };

REGISTER_ARC(FreeArc)

}}
