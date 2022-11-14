@echo off

rem  Use another compiler.
rem  was for "-march=i686" for MinGW 4.7.2
echo Option "%1"

set target=_MinGW
if (%1)==() set target=

if not exist _lea_data.zip sample_catalogue.exe
if not exist lea.rbj windres lea.rc lea.rbj
if not exist memorymodule.o gcc -c memorymodule.c

del lea.exe
gprbuild -P lea         -XBuild_Mode=Debug%target%
copy /B lea.exe + _lea_data.zip lea_debug%target%.exe
copy /B lea.exe lea_debug_without_data.exe

del lea.exe
gprbuild -P lea lea.adb -XBuild_Mode=Fast%target%
copy /B lea.exe + _lea_data.zip "lea (ver)%target%.exe"
copy /B lea.exe lea_fast_without_data.exe

copy /B "lea (ver)%target%.exe" lea.exe
