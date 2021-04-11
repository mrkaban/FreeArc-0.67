@ %* Z:\Testing\1
@ if errorlevel 1 echo %* >>Z:\Testing\errlog
@ srep32i.exe -d Z:\Testing\1 Z:\Testing\2
@ if errorlevel 1 echo -d %* >>Z:\Testing\errlog
@ del Z:\Testing\1 Z:\Testing\2
