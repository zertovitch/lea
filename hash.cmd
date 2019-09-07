if     '%1' == '' set exe_hash=lea.exe
if not '%1' == '' set exe_hash=%1

CertUtil -hashfile %exe_hash% SHA256  >%1.hash.txt
CertUtil -hashfile %exe_hash% SHA1   >>%1.hash.txt
CertUtil -hashfile %exe_hash% MD5    >>%1.hash.txt
