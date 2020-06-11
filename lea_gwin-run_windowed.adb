with LEA_Common;

with LEA_GWin.MDI_Main;
with LEA_GWin.Messages.IO_Pipe;

with LEA_Resource_GUI;                  use LEA_Resource_GUI;

with HAC.PCode.Interpreter, HAC_Pack;

with GWindows.Application;              use GWindows.Application;
with GWindows.Base;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

with Ada.Calendar;
with Ada.Strings.Wide_Fixed;            use Ada.Strings, Ada.Strings.Wide_Fixed;

procedure LEA_GWin.Run_Windowed (MDI_Child : in out MDI_Child_Type) is

  function Fake_Argument_Count return Natural is
  begin
    return 0;  --  !! TBD: Add a mode where the arguments are prompted.
  end;

  function Fake_Argument (Number : Positive) return String is
  begin
    return Integer'Image (Number);  --  !! TBD: Add a mode where the arguments are prompted.
  end;

  function Fake_Shell_Execute (Command : String) return Integer is
  begin
    return -1 + 0 * Command'Length;  --  !! TBD: pipe the console I/O (as in GWenerator)
  end;

  MDI_Main  : LEA_GWin.MDI_Main.MDI_Main_Type  renames MDI_Child.MDI_Parent.all;
  ml : LEA_GWin.Messages.Message_List_Type renames MDI_Main.Message_Panel.Message_List;

  use LEA_Common, HAC.PCode.Interpreter;

  unhandled : Exception_Propagation_Data;

  procedure Show_Error is

    count : Natural := 0;

    procedure Show_Line_Information (
      File_Name   : String;   --  Example: hac-pcode-interpreter.adb
      Block_Name  : String;   --  Example: HAC.PCode.Interpreter.Do_Write_Formatted
      Line_Number : Positive
    )
    is
      extended_repair : LEA_GWin.Messages.Editor_repair_information;
    begin
      extended_repair.file  := G2GU (S2G (File_Name));
      extended_repair.line  := Line_Number - 1;  --  Scintilla's lines are 0-based
      extended_repair.col_a := 0;
      extended_repair.col_z := 0;
      ml.Insert_Item (
        Trim (Integer'Wide_Image (Line_Number), Left),
        count
      );
      --  Here we set a payload in order to get the source file and position
      --  when selecting a row in the error / warnings message list.
      ml.Item_Data(
        count,
        new LEA_GWin.Messages.Editor_repair_information'(extended_repair)
      );
      ml.Set_Sub_Item (S2G (Block_Name), count, 1);
      count := count + 1;
    end Show_Line_Information;

    procedure ML_Trace_Back is new Show_Trace_Back (Show_Line_Information);

  begin
    Message_Box (MDI_Main,
      "Unhandled exception - run-time error",
      S2G ("HAC VM: raised " & Image (unhandled)),
      Icon => Exclamation_Icon
    );
    ml.Clear;
    ml.Set_Column ("Line", 0, 60);
    ml.Set_Column ("Trace-back location", 1, 800);
    ML_Trace_Back (unhandled);
  end Show_Error;

  --  The following is copied and adapted from AZip's progress bar.
  --
  progress_box: Progress_box_Type;
  is_aborted: Boolean:= False;
  --
  procedure Abort_clicked ( dummy : in out GWindows.Base.Base_Window_Type'Class ) is
    pragma Warnings(off, dummy);
  begin
    is_aborted:= True;  --  Will propagate user_abort upon next Boxed_Feedback.
  end Abort_clicked;
  --
  tick: Ada.Calendar.Time;
  --
  procedure Boxed_Feedback (
    Stack_Current, Stack_Total : in     Natural;
    Wall_Clock                 : in     Ada.Calendar.Time;
    User_Abort                 :    out Boolean
  )
  is
    use Ada.Calendar;
  begin
    --  Display only at most every n-th/100 second.
    --  Otherwise Windows may be overflown by messages and it would
    --  slow down the interpreter.
    if Wall_Clock - tick >= 0.04  then
      progress_box.Stack_Bar.Position(
        Integer (100.0 * Long_Float (Stack_Current) / Long_Float (Stack_Total))
      );
      Message_Check;
      tick := Wall_Clock;
    end if;
    User_Abort := is_aborted;
  end Boxed_Feedback;

  package Windowed_Console is new
    Console_Traits
      ( LEA_GWin.Messages.IO_Pipe.End_Of_File_Console,
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
        LEA_GWin.Messages.IO_Pipe.New_Line_Console
      );

  package LEA_System_Calls is new
    System_Calls_Traits
      ( Fake_Argument_Count,
        Fake_Argument,
        Fake_Shell_Execute,
        HAC_Pack.Directory_Separator
      );

  procedure Windowed_interpret is new
    HAC.PCode.Interpreter.Interpret
      ( Boxed_Feedback,
        Windowed_Console,
        LEA_System_Calls
      );

  use Ada.Calendar;

begin
  case MDI_Child.MDI_Parent.opt.toolset is
    when HAC_mode =>
      --  !!  Check if anything compiled ?
      ml.Clear;
      ml.Set_Column ("Console", 0, 800);
      LEA_GWin.Messages.IO_Pipe.Set_current_IO_pipe (MDI_Child.MDI_Parent.Message_Panel.Message_List);
      tick:= Clock - 5.0;  --  Ensure refresh code in Boxed_Feedback is executed soon
      progress_box.Create_Full_Dialog (MDI_Child);
      progress_box.Stack_Bar.Position (0);
      progress_box.Stop_VM_Button.Hide;
      progress_box.Stop_VM_Button_permanent.Show;
      progress_box.Stop_VM_Button_permanent.On_Click_Handler (Abort_clicked'Unrestricted_Access);
      progress_box.Center;
      progress_box.Redraw;
      progress_box.Show;
      for Windows_Woodoo in 1 .. 3 loop
        --  Rain dance to make Windows display the
        --  progress box from the beginning.
        progress_box.Redraw (Redraw_Now => True);
        delay 0.01;
        Message_Check;
        delay 0.03;
      end loop;
      MDI_Child.MDI_Parent.Disable;
      Windowed_interpret (MDI_Child.CD, unhandled);
      MDI_Child.MDI_Parent.Enable;
      MDI_Child.MDI_Parent.Focus;
      if Is_Exception_Raised (unhandled) then
        Show_Error;
      end if;
    when GNAT_mode =>
      null;
  end case;
end LEA_GWin.Run_Windowed;
