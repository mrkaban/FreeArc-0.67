@echo off
@call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\bin\vcvars32.bat"
::del DeflateCodec*.obj
nmake
::copy DeflateCodec.dll C:\!\FreeArchiver\Tests\Codecs\DeflateCodec.dll
