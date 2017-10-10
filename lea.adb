--  The GWindows* packages need to be visible to the compiler.
--  See installation instructions in the header part of the lea.gpr file.
--
with GWindows.Application;        use GWindows.Application;
with GWindows.Base;               use GWindows.Base;
with GWindows.GStrings;           use GWindows.GStrings;
with GWindows.Message_Boxes;      use GWindows.Message_Boxes;

with LEA_GWin.MDI_Main;           use LEA_GWin.MDI_Main;

with Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with GWindows.Scintilla;

procedure LEA is

  Top: LEA_GWin.MDI_Main.MDI_Main_Type;

  procedure Interactive_crash(
    Window : in out GWindows.Base.Base_Window_Type'Class;
    E: Ada.Exceptions.Exception_Occurrence)
  is
    pragma Unreferenced (Window);
    small_insult: constant String:=
        Ada.Exceptions.Exception_Name (E) & ASCII.LF &
        Ada.Exceptions.Exception_Message (E);
    insult: constant String:=
        small_insult & ASCII.LF &
        GNAT.Traceback.Symbolic.Symbolic_Traceback(E);
  begin
    GWindows.Base.On_Exception_Handler (Handler => null); -- Avoid infinite recursion!
    Message_Box
      ("Crash in LEA",
        To_GString_From_String(insult),
        OK_Box
      );
  end Interactive_crash;

begin
  GWindows.Base.On_Exception_Handler (Handler => Interactive_crash'Unrestricted_Access);
  Create_MDI_Top (Top, "LEA");
  if GWindows.Scintilla.SCI_Lexer_DLL_Successfully_Loaded then
    Message_Loop;
  else
    Message_Box
      ("LEA",
       "Installation error: file ""scilexer.dll"" is needed beside ""lea.exe""",
        OK_Box
      );
  end if;
end LEA;
