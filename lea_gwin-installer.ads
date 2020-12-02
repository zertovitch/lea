--  This package deals with:
--
--    - Unpacking missing SciLexer.dll for current session
--    - Unpacking SciLexer.dll and spawn lea.exe into a target directory (either
--      all-user program directory, or current user's program directory).

package LEA_GWin.Installer is

  procedure Unpack_DLL (target : String := "");

  --  TBD: Unpacking SciLexer.dll and spawn lea.exe into a target directory

end LEA_GWin.Installer;
