rem Use another compiler.
rem was for "-march=i686" for MinGW 4.7.2
echo Option "%1"

set target=_MinGW
if (%1)==() set target=

if not exist sci.zip zipada -eps sci.zip SciLexer.dll

del lea.exe
gprbuild -P lea -XBuild_Mode=Debug%target%
copy /B lea.exe + sci.zip lea_debug%target%.exe

del lea.exe
mkdir obj\fast
gprbuild -P lea -XBuild_Mode=Fast%target%
copy /B lea.exe + sci.zip "lea (ver)%target%.exe"
copy /B "lea (ver)%target%.exe" lea.exe
