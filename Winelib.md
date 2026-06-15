LEA Winelib Port
================

LEA-specific changes for Winelib.  See gwindows/docs/Winelib.txt
for the general framework porting documentation.


MemoryLoadLibrary Stub
----------------------

  LEA's installer references MemoryLoadLibrary from the
  MemoryModule C library (Windows-only).  A no-op C stub is
  provided for Winelib (memorymodule_stub.c), linked via lea.gpr.


Generic Body Trampoline ('Code_Address)
---------------------------------------

  Subprogram 'Address taken inside a generic package body may
  produce a sysv_abi stack trampoline.  When Wine calls that
  address expecting ms_abi, the ABI mismatch causes an immediate
  SIGSEGV.

  Fix: use 'Code_Address instead of 'Address.  This bypasses the
  trampoline and returns the real function address which has the
  ms_abi attribute applied.

  This applies to any callback registered with Win32 from inside
  a generic body.


Wine Path vs POSIX Path (To_Native_Path)
----------------------------------------

  Ada.Command_Line.Command_Name returns a Wine path
  (Z:\home\user\...\app.exe.so) but GNAT's Ada.Streams.Stream_IO
  uses POSIX open() which doesn't understand Wine drive letters.

  Win32_Types.To_Native_Path strips the drive prefix and converts
  backslashes on Winelib; no-op on Windows.


Appended ZIP Archive
--------------------

  The appended-ZIP technique (cat exe.so data.zip > app.exe.so)
  works identically with ELF as with PE -- ZIP scans from the end.
