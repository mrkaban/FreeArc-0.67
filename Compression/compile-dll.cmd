@echo off
@call "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" x86
@t make -j mkdir fazip.exe -fmakefile-dll CC=cl \"OPT_FLAGS=-Ox -arch:SSE2\" MODE=-msvc2013-x86
@call "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" x86_amd64
@t make -j mkdir fazip64.exe EXE=fazip64.exe -fmakefile-dll CC=cl \"OPT_FLAGS=-Ox\" MODE=-msvc2013-x64
@
@::call "C:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x86
@::t make -j mkdir fazip.exe -fmakefile-dll CC=cl \"OPT_FLAGS=-Ox -arch:SSE2\" MODE=-msvc2012-x86
@::call "C:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x86_amd64
@::t make -j mkdir fazip64.exe EXE=fazip64.exe -fmakefile-dll CC=cl \"OPT_FLAGS=-Ox\" MODE=-msvc2012-x64
@
@::call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" x86
@::t make -j mkdir fazip.exe -fmakefile-dll CC=cl \"OPT_FLAGS=-Ox -arch:SSE2\" MODE=-msvc2010-x86
@::call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" x86_amd64
@::t make -j mkdir fazip64.exe EXE=fazip64.exe -fmakefile-dll CC=cl \"OPT_FLAGS=-Ox\" MODE=-msvc2010-x64
@
@
@
@::call "C:\Program Files (x86)\Intel\Parallel Studio 2011\ips-vars.cmd" ia32
@::t make -j mkdir fazip.exe -fmakefile-dll CC=icl LINKER=xilink \"OPT_FLAGS=-w -O3 -Qipo -arch:SSE2\" MODE=-icl2011-x86
@::call "C:\Program Files (x86)\Intel\Parallel Studio 2011\ips-vars.cmd" ia32_intel64
@::t make -j mkdir fazip64.exe EXE=fazip64.exe -fmakefile-dll CC=icl LINKER=xilink \"OPT_FLAGS=-w -O3 -Qipo\" MODE=-icl2011-x64
@
@::call "C:\Program Files (x86)\Intel\Composer XE 2013 SP1\bin\ipsxe-comp-vars.bat" ia32
@::t make -j mkdir fazip.exe -fmakefile-dll CC=icl LINKER=xilink \"OPT_FLAGS=-w -O3 -Qipo -arch:SSE2\" MODE=-icl2013sp1-x86
@::call "C:\Program Files (x86)\Intel\Composer XE 2013 SP1\bin\ipsxe-comp-vars.bat" intel64
@::t make -j mkdir fazip64.exe EXE=fazip64.exe -fmakefile-dll CC=icl LINKER=xilink \"OPT_FLAGS=-w -O3 -Qipo\" MODE=-icl2013sp1-x64
@
@del facompress.exp facompress.lib facompress_mt.exp facompress_mt.lib
