with LEA_Common,
     LEA_Common.User_options;

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

with Interfaces;

procedure LEA_GWin.Run_Windowed (Window : in out MDI_Child.MDI_Child_Type) is

  use Ada.Strings.Unbounded, HAC_Sys.PCode.Interpreter, LEA_Common, GWindows.Message_Boxes;

  main    : LEA_GWin.MDI_Main.MDI_Main_Type renames Window.mdi_root.all;
  Options : LEA_Common.User_options.Option_Pack_Type renames LEA_Common.User_options.Options;

  procedure HAC_VM_Interpret is

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
      return G2S (GU2G (Window.ID.file_name));
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

    ml : LEA_GWin.Messages.Message_List_Type renames main.Message_Panel.Message_List;

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
        kit : Diagnostic_Kit;
      begin
        kit.file_name     := To_Unbounded_String (File_Name);
        kit.location.line := Line_Number;
        ml.Insert_Item (Trim (Line_Number'Wide_Image, Left), count);
        --  Here, we set a payload in order to get the source file and position
        --  when selecting a row in the error / warnings message list.
        ml.Item_Data
          (count,
           new Diagnostic_Kit'(kit));  --  Copy `kit` into a new heap allocated object.
        ml.Set_Sub_Item (S2G (Block_Name), count, 1);
        count := count + 1;
      end Show_Line_Information;

      procedure ML_Trace_Back is new Show_Trace_Back (Show_Line_Information);

    begin
      if Is_User_Abort (post_mortem.Unhandled) then
        Message_Box
          (main,
           "User abort",
           "HAC Virtual Machine stopped by user",
           Icon => Information_Icon);
      else
        Message_Box
          (main,
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

    procedure Scroll_Down_To_Last_Input_Line is
      use GWindows.Common_Controls;
    begin
      ml.Ensure_Visible (Integer'Max (0, ml.Item_Count - 1), Full);
    end Scroll_Down_To_Last_Input_Line;

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
    infrequent : Interfaces.Unsigned_8 := 0;
    --
    procedure Boxed_Feedback
      (Stack_Current, Stack_Total : in     Natural;
       Wall_Clock                 : in     Ada.Calendar.Time;
       User_Abort                 :    out Boolean)
    is
      use Ada.Calendar, GWindows.Application, Interfaces;
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
        infrequent := infrequent + 1;  --  No overflow.
        if (infrequent and 7) = 0 then
          Scroll_Down_To_Last_Input_Line;
        end if;
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

    procedure Windowed_HAC_VM_Interpret is new
      HAC_Sys.PCode.Interpreter.Interpret
         (Boxed_Feedback,
          Windowed_Console,
          LEA_System_Calls);

    use Ada.Calendar, GWindows.Application;

  begin
    ml.Clear;
    ml.Set_Column ("Console - Running...", 0, 800);
    ml.Set_Column ("", 1, 0);
    ml.Set_Column ("", 2, 0);
    LEA_GWin.Messages.IO_Pipe.Set_current_IO_pipe (Window.mdi_root.Message_Panel.Message_List);
    tick := Clock - 5.0;  --  Ensure refresh code in Boxed_Feedback is executed soon
    progress_box.Create_Full_Dialog (Window);
    progress_box.Stack_Bar.Position (0);
    progress_box.Stop_VM_Button.Hide;
    progress_box.Stop_VM_Button_permanent.Show;
    progress_box.Stop_VM_Button_permanent.On_Click_Handler (Abort_clicked'Unrestricted_Access);
    progress_box.Center;
    Window.mdi_root.Disable;
    --  Running of the HAC program happens here:
    Windowed_HAC_VM_Interpret (Window.mdi_root.BD, post_mortem);
    ml.Set_Column ("Console", 0, ml.Column_Width (0));
    Scroll_Down_To_Last_Input_Line;
    Window.mdi_root.Enable;
    Window.mdi_root.Focus;
    if Is_Exception_Raised (post_mortem.Unhandled) then
      Show_Error;
    end if;
  end HAC_VM_Interpret;

begin
  LEA_GWin.Messages.IO_Pipe.is_aborted_flag := False;
  case Options.toolset is
    when HAC_mode =>
      --  !!  Check if anything compiled ?
      if Window.mdi_root.BD.CD.Is_Executable then
        HAC_VM_Interpret;
      else
        Message_Box
          (main,
           "Run",
           "Can only execute a parameterless procedure",
           Icon => Information_Icon);
      end if;
    when GNAT_mode =>
      null;
  end case;
end LEA_GWin.Run_Windowed;
