gprbuild -P lea 
copy lea.exe lea_debug.exe
del lea.exe
gprbuild -P lea -XBuild_Mode=Fast
copy lea.exe lea_fast.exe