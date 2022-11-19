with GWindows.Message_Boxes;

with UnZip.Streams;
with Zip;
with Zip_Streams;

with Ada.Command_Line,
     Ada.Strings.Unbounded;
with Interfaces.C;
with System;

package body LEA_GWin.Installer is

  --  "MemoryModule is a library that can be used to load a DLL
  --   completely from memory - without storing on the disk first."
  --
  --  https://github.com/fancycode/MemoryModule
  --
  pragma Linker_Options ("..\..\MemoryModule.o");

  use Ada.Strings.Unbounded;

  procedure Memory_Load_Library (pointer : System.Address; size : Interfaces.C.size_t);
  pragma Import (C, Memory_Load_Library, "MemoryLoadLibrary");

  procedure Load_Scintilla_DLL_from_Memory is
    use Ada.Command_Line, GWindows.Message_Boxes;
    lea_exe       : constant String := Command_Name;
    scintilla_dll : constant String := "SciLexer.dll";
    z : Zip.Zip_info;
    m : Zip_Streams.Memory_Zipstream;
    b : Unbounded_String;
    p : String_Access;
  begin
    Zip.Load (z, lea_exe);
    UnZip.Streams.Extract (m, z, scintilla_dll);
    Zip_Streams.Get (m, b);
    --  For pointer p, the memory is heap-allocated until
    --  the termination of lea.exe:
    p := new String (1 .. Length (b));
    p.all := To_String (b);
    Memory_Load_Library (p (1)'Address, p.all'Length);
  exception
    when others =>
      Message_Box
        ("LEA startup",
         "Installation error: cannot unpack """ & S2G (scintilla_dll) & '"' & NL &
         "from Zip archive appended to ""lea.exe""." & NL &
         "Path = " & S2G (Command_Name),
          OK_Box,
          Error_Icon
        );
      raise;
  end Load_Scintilla_DLL_from_Memory;

end LEA_GWin.Installer;
