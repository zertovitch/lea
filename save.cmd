rem --------------------------
rem Nice date YYYY-MM-DD_HH.MM
rem --------------------------

set year=%date:~-4,4%

set month=%date:~-7,2%
if "%month:~0,1%" equ " " set month=0%month:~1,1%

set day=%date:~-10,2%
if "%day:~0,1%" equ " " set day=0%day:~1,1%

set hour=%time:~0,2%
if "%hour:~0,1%" equ " " set hour=0%hour:~1,1%

set min=%time:~3,2%

set nice_date=%year%-%month%-%day%
set nice_date=%year%-%month%-%day%_%hour%.%min%


rem --------------------------

set root=lea

cd..

set files=%root%/*.ad* %root%/*.gpr %root%/lea.rc %root%/lea.h %root%/lea.rbj
set files=%files% %root%/*.txt %root%/save.cmd %root%/build.cmd %root%/pack_data.cmd %root%/*.pra
set files=%files% %root%/ico_bmp/*.ico %root%/ico_bmp/*.bmp %root%/ico_bmp/*.ppt

zipada -ep2 %root%/lea-%nice_date% %files%

cd %root%