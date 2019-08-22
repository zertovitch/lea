if     '%1' == '' set exe_hash=lea.exe
if not '%1' == '' set exe_hash=%1

CertUtil -hashfile %exe_hash% SHA256
CertUtil -hashfile %exe_hash% SHA1
CertUtil -hashfile %exe_hash% MD5
