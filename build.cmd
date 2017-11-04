echo Option "%1"
rem e.g. "-march=i686" for MinGW 4.7.2

set target=_MinGW
if (%1)==() set target=

del lea.exe
gprbuild -P lea -XBuild_Mode=Debug%target%
copy lea.exe lea_debug%target%.exe

del lea.exe
mkdir obj\fast
gprbuild -P lea -XBuild_Mode=Fast%target%
copy lea.exe lea_fast%target%.exe
