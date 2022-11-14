--  This package deals with:
--
--    - Loading SciLexer.dll from Zip file attached to lea.exe, for current session.
--
--    - Spawning lea.exe into a target directory (either all-user program
--      all-user program directory, or current user's program directory).
--      See AZip http://azip.sf.net for an example.

package LEA_GWin.Installer is

  procedure Load_Scintilla_DLL_from_Memory;

  --  TBD: Spawn lea.exe into a target directory.

end LEA_GWin.Installer;
