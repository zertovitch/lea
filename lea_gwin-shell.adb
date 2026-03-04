with LEA_GWin.Messages.IO_Pipe;

with GWindows.Application,
     GWindows.Pipes;

with Ada.Strings.Unbounded;

package body LEA_GWin.Shell is

   ----------------------------------
   -- Shell_Execute_to_LEA_Console --
   ----------------------------------

  procedure Output_a_Line_to_LEA_Console (s : String) is
  begin
    Messages.IO_Pipe.Put_Console (s);
    Messages.IO_Pipe.New_Line_Console;
    Messages.IO_Pipe.Scroll_Down_To_Last_Output_Line;
    GWindows.Application.Message_Check;
  end Output_a_Line_to_LEA_Console;

  procedure Execute_to_LEA_Console (command : String; result : out Integer) is
    p : GWindows.Pipes.Piped_Process renames Messages.IO_Pipe.shell_pipe;
  begin
    Messages.IO_Pipe.Change_Header ("Console - Shell");
    p.Start ("cmd.exe /c " & command, ".", Output_a_Line_to_LEA_Console'Unrestricted_Access);
    while p.Is_Alive loop
      p.Check_Progress;
      delay 0.01;
    end loop;
    Messages.IO_Pipe.Restore_Header;
    result := p.Last_Exit_Code;
  exception
    when GWindows.Pipes.Cannot_Create_Pipe =>
      Messages.IO_Pipe.Restore_Header;
      result := -1;
    when GWindows.Pipes.Cannot_Start =>
      Messages.IO_Pipe.Restore_Header;
      result := -1;
  end Execute_to_LEA_Console;

   ------------------------------
   -- Shell_Execute_to_VString --
   ------------------------------

  piped_output : HAT.VString;

  procedure Output_a_Line_to_VString (s : String) is
  begin
    Ada.Strings.Unbounded.Append (piped_output, s & ASCII.LF);
  end Output_a_Line_to_VString;

  procedure Execute_to_VString (command : String; result : out Integer; output : out HAT.VString) is
    p : GWindows.Pipes.Piped_Process renames Messages.IO_Pipe.shell_pipe;
  begin
    piped_output := HAT.Null_VString;
    p.Start ("cmd.exe /c " & command, ".", Output_a_Line_to_VString'Unrestricted_Access);
    while p.Is_Alive loop
      p.Check_Progress;
      delay 0.01;
    end loop;
    result := p.Last_Exit_Code;
    output := piped_output;
  exception
    when GWindows.Pipes.Cannot_Create_Pipe =>
      result := -1;
    when GWindows.Pipes.Cannot_Start =>
      result := -1;
  end Execute_to_VString;

end LEA_GWin.Shell;
