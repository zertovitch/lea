@echo off

rem  Use another compiler.
rem  was for "-march=i686" for MinGW 4.7.2
echo Option "%1"

set target=_MinGW
if (%1)==() set target=

if not exist _lea_data.zip sample_catalogue.exe
if not exist lea.rbj windres lea.rc lea.rbj
if not exist memorymodule.o gcc -c -Os memorymodule.c

if exist lea_without_data.exe del lea_without_data.exe

gprbuild -P lea         -XLEA_Build_Mode=Debug%target%

if %errorlevel% == 9009 goto error

copy /B lea_without_data.exe + _lea_data.zip lea_debug%target%.exe
copy /B lea_without_data.exe lea_debug_without_data.exe

if exist lea_without_data.exe del lea_without_data.exe

gprbuild -P lea lea_without_data.adb -XLEA_Build_Mode=Small%target%
copy /B lea_without_data.exe + _lea_data.zip "lea (ver)%target%.exe"
copy /B lea_without_data.exe lea_small_without_data.exe

copy /B "lea (ver)%target%.exe" lea.exe

echo Press Return
pause
goto :eof

:error

echo.
echo The GNAT Ada compiler was not found in the PATH!
echo.
echo Check https://www.adacore.com/download for GNAT
echo or https://alire.ada.dev/ for ALIRE.
echo The LEA project is available as an ALIRE crate.
echo.
echo Press Return
pause
