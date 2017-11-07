with GWindows.Message_Boxes;      use GWindows.Message_Boxes;

with UnZip;                             use UnZip;

with Ada.Command_Line;                  use Ada.Command_Line;

package body LEA_GWin.Installer is

  procedure Unpack_DLL (target: String := "") is
    lea_exe: constant String := Command_Name;
    bs: Integer;
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
        what   => "SciLexer.dll",
        rename => lea_exe (lea_exe'First .. bs) & "SciLexer.dll"
      );
    else
      Extract (
        from   => lea_exe,
        what   => "SciLexer.dll",
        rename => target & "SciLexer.dll"
      );
    end if;
  exception
    when others =>
      Message_Box
        ("LEA",
         "Installation error: cannot unpack ""scilexer.dll"" from ""lea.exe""." & NL &
         "Path = " & S2G (Command_Name),
          OK_Box
        );
      raise;
  end Unpack_DLL;

end LEA_GWin.Installer;
