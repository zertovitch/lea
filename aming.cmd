rem  Special build for TDM-GCC (formerly MinGW), not GNAT GPL.
rem  This brings in the GMGPL license instead of GPL, which
rem  allows for distributing the executable without the sources.

rem Add TDM-GCC (or MinGW) bin directory in front of the PATH.
rem This is specific your installation of TDM-GCC / MinGW.

call ming_set

rem make Ming object directories if not yet existing
md obj\fast_ming
md obj\debug_ming
copy obj\debug\debug.pra       obj\debug_ming

cd gwindows

call build "-march=i686"
