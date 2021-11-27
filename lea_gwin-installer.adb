with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

with UnZip;                             use UnZip;

with Ada.Command_Line;                  use Ada.Command_Line;

package body LEA_GWin.Installer is

  procedure Unpack_DLL (target : String := "") is
    lea_exe       : constant String := Command_Name;
    scintilla_dll : constant String := "SciLexer.dll";
    bs : Integer;
  begin
    for i in reverse lea_exe'Range loop
      if lea_exe (i) = '\' then
        bs := i;
        exit;
      end if;
    end loop;
    if target = "" then
      Extract (
        from   => lea_exe,
        what   => scintilla_dll,
        rename => lea_exe (lea_exe'First .. bs) & scintilla_dll
      );
    else
      Extract (
        from   => lea_exe,
        what   => scintilla_dll,
        rename => target & scintilla_dll
      );
    end if;
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
  end Unpack_DLL;

end LEA_GWin.Installer;
