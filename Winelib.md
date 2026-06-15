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

  Win32 file dialogs and Ada.Command_Line.Command_Name return
  Wine paths (Z:\home\user\...\app.exe.so) but GNAT's runtime
  (Ada.Streams.Stream_IO, Ada.Directories) uses POSIX open()
  which doesn't understand Wine drive letters.

  Win32_Types.To_Native_Path (available for both String and
  Wide_String) strips the drive prefix and converts backslashes
  on Winelib; no-op on Windows.  Applied in:

  - lea_gwin-embedded_texts.adb: executable path for ZIP loading
  - lea_gwin-mdi_main.adb: file paths from Open dialog and drop
  - lea_gwin-mdi_child.adb: file path from Save As dialog


Building and Running
--------------------

  There are two GPR variants for building LEA.  Both produce
  lea_without_data.exe (launcher script) and
  lea_without_data.exe.so (ELF shared object).

  ADALIB_DIR must be set for linking (see gwindows/docs/Winelib.txt).

  Option 1: lea_project_tree.gpr (recommended)

    Uses project references to resolve all dependencies
    automatically.  Pass -aP for each project directory and
    -cargs -fPIC so dependent projects are also compiled as
    position-independent code:

      gprbuild -P lea_project_tree.gpr -XPLATFORM=Winelib \
        -XADALIB_DIR=$ADALIB_DIR \
        -aP../gwindows -aP../hac -aP../ini-files -aP../zip-ada \
        -p -j0 -cargs -fPIC

  Option 2: lea.gpr (flat, manual source paths)

    Requires GNAT_SOURCE_PATH listing all dependency source
    directories, separated by semicolons.  Assuming the standard
    repository layout with all projects as siblings:

      export GNAT_SOURCE_PATH="\
        ../gwindows/framework;../gwindows/contrib;\
        ../gnatcom/framework;../gnatcom/framework/coding/winelib;\
        ../hac/src;../hac/src/compile;../hac/src/compile/emit;\
        ../hac/src/execute;../hac/src/manage;\
        ../ini-files;\
        ../zip-ada/zip_lib"

      gprbuild -P lea.gpr -XPLATFORM=Winelib \
        -XADALIB_DIR=$ADALIB_DIR -p -j0

  After building with either option, create lea.exe with the
  embedded help and samples ZIP appended:

    cp lea_without_data.exe.so lea.exe.so
    cat _lea_data.zip >> lea.exe.so
    sed 's/lea_without_data\.exe\.so/lea.exe.so/' lea_without_data.exe > lea.exe
    chmod +x lea.exe
    ./lea.exe
