--  The GWindows* packages need to be available to the compiler.
--  See installation instructions in the header part of the lea.gpr file.
--
with GWindows.Application,
     GWindows.Base,
     GWindows.GStrings,
     GWindows.Message_Boxes,
     GWindows.Scintilla,
     GWindows.Types;

with LEA_GWin.MDI_Main,
     LEA_GWin.Installer;

with Ada.Command_Line,
     Ada.Exceptions;

with GNAT.Traceback.Symbolic;

procedure LEA_without_data is

  Top : LEA_GWin.MDI_Main.MDI_Main_Type;

  use LEA_GWin;
  use GWindows.Message_Boxes;

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
        GWindows.GStrings.To_GString_From_String (insult),
        OK_Box
      );
  end Interactive_crash;

  procedure LEA_start is
  begin
    GWindows.Base.On_Exception_Handler (Handler => Interactive_crash'Unrestricted_Access);
    Top.Create_MDI_Top ("LEA - starting");
    Top.Update_Title;
    Top.Focus;
    GWindows.Application.Message_Loop;
  end LEA_start;

begin
  --  Message_Box ("GWindows.Scintilla.Int is...",
  --    GWindows.Scintilla.Int'Size'Wide_Image & " bits");

  if GWindows.Scintilla.SCI_Lexer_DLL_Successfully_Loaded then
    --  The file scilexer.dll was found and has been loaded.
    null;
  else
    --  Unpack scilexer.dll as memory stream and into
    --  a string. Load it to the Windows system from there.
    LEA_GWin.Installer.Load_Scintilla_DLL_from_Memory;
  end if;
  LEA_start;
end LEA_without_data;
