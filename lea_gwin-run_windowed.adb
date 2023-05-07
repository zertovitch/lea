with LEA_Common;

with LEA_GWin.MDI_Main;
with LEA_GWin.Messages.IO_Pipe;

with LEA_Resource_GUI;

with HAC_Sys.Defs,
     HAC_Sys.PCode.Interpreter;

with HAT;

with GWindows.Application,
     GWindows.Base,
     GWindows.Common_Controls,
     GWindows.Message_Boxes;

with Ada.Calendar,
     Ada.Strings.Unbounded,
     Ada.Strings.Wide_Fixed;

procedure LEA_GWin.Run_Windowed (Window : in out MDI_Child.MDI_Child_Type) is

  function Fake_Argument_Count return Natural is
  begin
    return 0;  --  !! TBD: Add a mode where the arguments are prompted.
  end Fake_Argument_Count;

  function Fake_Argument (Number : Positive) return String is
  begin
    return Integer'Image (Number);  --  !! TBD: Add a mode where the arguments are prompted.
  end Fake_Argument;

  function HAC_Command_Name return String is
  begin
    return G2S (GU2G (Window.ID.File_Name));
  end HAC_Command_Name;

  procedure Fake_Shell_Execute (Command : String; Result : out Integer) is
  begin
    Result := -1 + 0 * Command'Length;  --  !! TBD: pipe the console I/O (as in GWenerator)
  end Fake_Shell_Execute;

  procedure Fake_Shell_Execute_Output (Command : String; Result : out Integer; Output : out HAT.VString) is
  begin
    Result := -1 + 0 * Command'Length;  --  !! TBD: pipe the console I/O (as in GWenerator)
    Output := HAT.Null_VString;
  end Fake_Shell_Execute_Output;

  MDI_Main  : LEA_GWin.MDI_Main.MDI_Main_Type  renames Window.MDI_Root.all;
  ml : LEA_GWin.Messages.Message_List_Type renames MDI_Main.Message_Panel.Message_List;

  use LEA_Common, HAC_Sys.PCode.Interpreter, Ada.Strings.Unbounded;

  post_mortem : Post_Mortem_Data;

  procedure Show_Error is

    count : Natural := 0;

    procedure Show_Line_Information (
      File_Name   : String;   --  Example: hac-pcode-interpreter.adb
      Block_Name  : String;   --  Example: HAC.PCode.Interpreter.Do_Write_Formatted
      Line_Number : Positive
    )
    is
      use HAC_Sys.Defs, Ada.Strings, Ada.Strings.Wide_Fixed;
      diagnostic : Diagnostic_Kit;
    begin
      diagnostic.file_name := To_Unbounded_String (File_Name);
      diagnostic.line      := Line_Number;
      ml.Insert_Item (
        Trim (Integer'Wide_Image (Line_Number), Left),
        count
      );
      --  Here we set a payload in order to get the source file and position
      --  when selecting a row in the error / warnings message list.
      ml.Item_Data
        (count,
         new Diagnostic_Kit'(diagnostic));  --  Copy `diagnostic` into a new heap allocated object.
      ml.Set_Sub_Item (S2G (Block_Name), count, 1);
      count := count + 1;
    end Show_Line_Information;

    procedure ML_Trace_Back is new Show_Trace_Back (Show_Line_Information);

    use GWindows.Message_Boxes;

  begin
    if Is_User_Abort (post_mortem.Unhandled) then
      Message_Box
        (MDI_Main,
         "User abort",
         S2G ("HAC Virtual Machine stopped by user"),
         Icon => Information_Icon);
    else
      Message_Box
        (MDI_Main,
         "Unhandled exception - run-time error",
         S2G ("HAC Virtual Machine: raised " & Image (post_mortem.Unhandled)) & NL & NL &
         S2G (Message (post_mortem.Unhandled)),
         Icon => Exclamation_Icon);
    end if;
    ml.Clear;
    ml.Set_Column ("Line", 0, 100);
    ml.Set_Column ("Trace-back: approximate location", 1, 800);
    ml.Set_Column ("", 2, 0);
    --
    ML_Trace_Back (post_mortem.Unhandled);
    --
    ml.Insert_Item ("-------", count);
    ml.Set_Sub_Item ("----- Exception information -----", count, 1);
    ml.Insert_Item ("Name", count + 1);
    ml.Set_Sub_Item (S2G (Image (post_mortem.Unhandled)), count + 1, 1);
    ml.Insert_Item ("Message", count + 2);
    ml.Set_Sub_Item (S2G (Message (post_mortem.Unhandled)), count + 2, 1);
  end Show_Error;

  --  The following is copied and adapted from AZip's progress bar.
  --
  progress_box : LEA_Resource_GUI.Progress_box_Type;
  --
  procedure Abort_clicked (dummy : in out GWindows.Base.Base_Window_Type'Class) is
  begin
    LEA_GWin.Messages.IO_Pipe.is_aborted_flag := True;
    --  Will propagate user_abort upon next Boxed_Feedback.
  end Abort_clicked;
  --
  tick  : Ada.Calendar.Time;
  start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
  hidden_progress_box : Boolean := True;
  --
  procedure Boxed_Feedback (
    Stack_Current, Stack_Total : in     Natural;
    Wall_Clock                 : in     Ada.Calendar.Time;
    User_Abort                 :    out Boolean
  )
  is
    use Ada.Calendar, GWindows.Application;
  begin
    --  Don't show the progress box for programms running very shortly.
    --  For those programs, the box would flash nastily without this delay.
    if hidden_progress_box and then Wall_Clock - start > 1.0 then
      progress_box.Redraw;
      progress_box.Show;
      hidden_progress_box := False;
    end if;

    --  Display only at most every n-th/100 second.
    --  Otherwise Windows may be overflown by messages and it would
    --  slow down the interpreter.
    if Wall_Clock - tick >= 0.04  then
      progress_box.Stack_Bar.Position
        (Integer (100.0 * Long_Float (Stack_Current) / Long_Float (Stack_Total)));
      Message_Check;
      tick := Wall_Clock;
    end if;
    User_Abort := LEA_GWin.Messages.IO_Pipe.is_aborted_flag;
  end Boxed_Feedback;

  package Windowed_Console is new
    Console_Traits
       (LEA_GWin.Messages.IO_Pipe.End_Of_File_Console,
        LEA_GWin.Messages.IO_Pipe.End_Of_Line_Console,
        LEA_GWin.Messages.IO_Pipe.Get_Needs_Skip_Line,
        LEA_GWin.Messages.IO_Pipe.Get_Console,
        LEA_GWin.Messages.IO_Pipe.Get_Console,
        LEA_GWin.Messages.IO_Pipe.Get_Console,  --  For Get_Console (C)
        LEA_GWin.Messages.IO_Pipe.Get_Console,  --  For Get_Immediate_Console (C)
        LEA_GWin.Messages.IO_Pipe.Get_Line_Console,
        LEA_GWin.Messages.IO_Pipe.Skip_Line_Console,
        LEA_GWin.Messages.IO_Pipe.Put_Console,
        LEA_GWin.Messages.IO_Pipe.Put_Console,
        LEA_GWin.Messages.IO_Pipe.Put_Console,
        LEA_GWin.Messages.IO_Pipe.Put_Console,
        LEA_GWin.Messages.IO_Pipe.Put_Console,
        LEA_GWin.Messages.IO_Pipe.New_Line_Console);

  package LEA_System_Calls is new
    System_Calls_Traits
       (Fake_Argument_Count,
        Fake_Argument,
        HAC_Command_Name,
        Fake_Shell_Execute,
        Fake_Shell_Execute_Output,
        HAT.Directory_Separator);

  procedure Windowed_interpret is new
    HAC_Sys.PCode.Interpreter.Interpret
       (Boxed_Feedback,
        Windowed_Console,
        LEA_System_Calls);

  use Ada.Calendar, GWindows.Application, GWindows.Common_Controls;

begin
  LEA_GWin.Messages.IO_Pipe.is_aborted_flag := False;
  case Window.MDI_Root.opt.toolset is
    when HAC_mode =>
      --  !!  Check if anything compiled ?
      ml.Clear;
      ml.Set_Column ("Console - Running...", 0, 800);
      ml.Set_Column ("", 1, 0);
      ml.Set_Column ("", 2, 0);
      LEA_GWin.Messages.IO_Pipe.Set_current_IO_pipe (Window.MDI_Root.Message_Panel.Message_List);
      tick := Clock - 5.0;  --  Ensure refresh code in Boxed_Feedback is executed soon
      progress_box.Create_Full_Dialog (Window);
      progress_box.Stack_Bar.Position (0);
      progress_box.Stop_VM_Button.Hide;
      progress_box.Stop_VM_Button_permanent.Show;
      progress_box.Stop_VM_Button_permanent.On_Click_Handler (Abort_clicked'Unrestricted_Access);
      progress_box.Center;
      Window.MDI_Root.Disable;
      Windowed_interpret (Window.BD, post_mortem);  --  Running the HAC program happens here.
      ml.Set_Column ("Console", 0, ml.Column_Width (0));
      --  Scroll to last output line:
      ml.Ensure_Visible (Integer'Max (0, ml.Item_Count - 1), Full);
      Window.MDI_Root.Enable;
      Window.MDI_Root.Focus;
      if Is_Exception_Raised (post_mortem.Unhandled) then
        Show_Error;
      end if;
    when GNAT_mode =>
      null;
  end case;
end LEA_GWin.Run_Windowed;
