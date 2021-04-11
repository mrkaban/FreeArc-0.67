@rem Example of using PROFILING compilation to check the speeds of various algorithm parts
@set INFILE=D:\Testing\office.dll
call compile.cmd "-DPROFILING=1"
Delta  %INFILE%
Delta  %INFILE%
call compile.cmd "-DPROFILING=2"
Delta  %INFILE%
Delta  %INFILE%
call compile.cmd "-DPROFILING=3"
Delta  %INFILE%
Delta  %INFILE%
call compile.cmd "-DPROFILING=4"
Delta  %INFILE%
Delta  %INFILE%
