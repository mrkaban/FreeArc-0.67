@for %%a in ( 4 1 ) do srep64g -a%%a -m1 %* z:\vhd nul
@for %%a in ( 16 4 1 ) do srep64g -a%%a -m1 -c4k %* z:\vhd nul
@for %%a in ( 16 4 1 ) do srep32g -a%%a -m1 -c4k %* z:\vhd nul
