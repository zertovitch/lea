LEA Winelib Port
================

LEA-specific changes for Winelib.  See gwindows/docs/Winelib.txt
for the general framework porting documentation.


MemoryModule (vendored)
-----------------------

  LEA loads SciLexer.dll from a ZIP archive appended to the
  executable, using the MemoryModule C library to load the DLL
  from memory.  The source is vendored directly:

    MemoryModule.c, MemoryModule.h  — from github.com/fancycode/MemoryModule
    MEMORYMODULE_LICENSE            — MPL-2.0 license text

  On Windows, the pre-compiled memorymodule.o (COFF) is linked.
  On Winelib, compile the vendored source with winegcc:

    winegcc -c -O2 -fPIC MemoryModule.c -o memorymodule.o

  Both GPR variants (lea.gpr and lea_project_tree.gpr) link
  memorymodule.o on both platforms.


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

  - lea_gwin-installer.adb: executable path for Scintilla ZIP loading
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

    ./make_lea_exe.sh
    ./lea.exe
