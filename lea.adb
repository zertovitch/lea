--  The GWindows* packages need to be visible to the compiler.
--  See installation instructions in the header part of the lea.gpr file.
--
with GWindows.Application;        use GWindows.Application;
with GWindows.Base;
with GWindows.GStrings;           use GWindows.GStrings;
with GWindows.Message_Boxes;      use GWindows.Message_Boxes;
with GWindows.Scintilla;
with GWindows.Types;

with LEA_GWin.MDI_Main;           use LEA_GWin, LEA_GWin.MDI_Main;
with LEA_GWin.Installer;          use LEA_GWin.Installer;

with Ada.Command_Line;            use Ada.Command_Line;
with Ada.Exceptions;

with GNAT.Traceback.Symbolic;

procedure LEA is

  Top : LEA_GWin.MDI_Main.MDI_Main_Type;

  procedure Interactive_crash (
    Window : in out GWindows.Base.Base_Window_Type'Class;
    E : Ada.Exceptions.Exception_Occurrence)
  is
    pragma Unreferenced (Window);
    small_insult : constant String :=
        Ada.Exceptions.Exception_Name (E) & ASCII.LF &
        Ada.Exceptions.Exception_Message (E);
    insult : constant String :=
        small_insult & ASCII.LF &
        GNAT.Traceback.Symbolic.Symbolic_Traceback (E);
  begin
    GWindows.Base.On_Exception_Handler (Handler => null);  --  Avoid infinite recursion!
    Message_Box
      ("Crash in LEA",
        To_GString_From_String (insult),
        OK_Box
      );
  end Interactive_crash;

  procedure LEA_start is
  begin
    GWindows.Base.On_Exception_Handler (Handler => Interactive_crash'Unrestricted_Access);
    Create_MDI_Top (Top, "LEA - starting");
    Top.Update_Title;
    Top.Focus;
    Message_Loop;
  end LEA_start;

begin
  --  Message_Box ("GWindows.Scintilla.Int is...",
  --    GWindows.Scintilla.Int'Size'Wide_Image & " bits");

  if GWindows.Scintilla.SCI_Lexer_DLL_Successfully_Loaded then
    LEA_start;
  else
    begin
      Unpack_DLL;
      GWindows.Scintilla.Try_Loading_Lexer_DLL;
      if GWindows.Scintilla.SCI_Lexer_DLL_Successfully_Loaded then
        LEA_start;
      else
        Message_Box
          ("LEA startup",
           "Installation error: file ""scilexer.dll"" is needed beside ""lea.exe""." & NL &
           "Either the file ""scilexer.dll"" doesn't exist, or there is a 32 vs. 64 bit mismatch." & NL &
           "This program is a" &
           GWindows.GStrings.To_GString_From_String (Integer'Image (GWindows.Types.Wparam'Size)) &
           " bit application." & NL &
           "Path = " & S2G (Command_Name),
           OK_Box,
           Error_Icon
          );
      end if;
    exception
      when others =>
        null;
    end;
  end if;
end LEA;
