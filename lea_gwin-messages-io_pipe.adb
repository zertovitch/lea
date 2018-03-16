package body LEA_GWin.Messages.IO_Pipe is

  type ML_access is access all Message_List_Type;

  current_IO_pipe: ML_access := null;

  procedure Set_current_IO_pipe (ML: in out Message_List_Type) is
  begin
    current_IO_pipe := ML'Unchecked_Access;
  end Set_current_IO_pipe;

  procedure New_line_IO_pipe is
    last_line: constant Integer := current_IO_pipe.Item_Count -1;
  begin
    current_IO_pipe.Insert_Item ("", Index => last_line + 1);
  end New_line_IO_pipe;

  procedure Append_to_IO_pipe (new_text : String) is
    last_line: Integer := current_IO_pipe.Item_Count -1;
  begin
    if last_line < 0 then
      current_IO_pipe.Insert_Item ("", Index => 0);  --  First line
      last_line := 0;
    end if;
    current_IO_pipe.Set_Item(
      Text => current_IO_pipe.Text (Item => last_line, SubItem => 0) & S2G (new_text),
      Index => last_line
    );
  end Append_to_IO_pipe;

   -------------------------
   -- End_Of_File_Console --
   -------------------------

   function End_Of_File_Console return Boolean is
   begin
     return False;
   end End_Of_File_Console;

   -------------------------
   -- End_Of_Line_Console --
   -------------------------

   function End_Of_Line_Console return Boolean is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "End_Of_Line_Console unimplemented");
      raise Program_Error with "Unimplemented function End_Of_Line_Console";
      return End_Of_Line_Console;
   end End_Of_Line_Console;

   -----------------
   -- Get_Console --
   -----------------

   procedure Get_Console (i: out Integer; Width : Ada.Text_IO.Field := 0) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Console unimplemented");
      raise Program_Error with "Unimplemented procedure Get_Console";
   end Get_Console;

   -----------------
   -- Get_Console --
   -----------------

   procedure Get_Console (f: out Float; Width : Ada.Text_IO.Field := 0) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Console unimplemented");
      raise Program_Error with "Unimplemented procedure Get_Console";
   end Get_Console;

   -----------------
   -- Get_Console --
   -----------------

   procedure Get_Console (c: out Character) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Console unimplemented");
      raise Program_Error with "Unimplemented procedure Get_Console";
   end Get_Console;

   -----------------------
   -- Skip_Line_Console --
   -----------------------

   procedure Skip_Line_Console (Spacing : Ada.Text_IO.Positive_Count := 1) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Skip_Line_Console unimplemented");
      raise Program_Error with "Unimplemented procedure Skip_Line_Console";
   end Skip_Line_Console;

   -----------------
   -- Put_Console --
   -----------------

   procedure Put_Console
     (i     : Integer;
      Width : Ada.Text_IO.Field       := Ada.Integer_Text_IO.Default_Width;
      Base  : Ada.Text_IO.Number_Base := Ada.Integer_Text_IO.Default_Base)
   is
     s : String (1..Width);
   begin
     Ada.Integer_Text_IO.Put (s, i, Base);
     Append_to_IO_pipe(s);
   end Put_Console;

   -----------------
   -- Put_Console --
   -----------------

   procedure Put_Console
     (f    : Float;
      Fore : Integer := Ada.Float_Text_IO.Default_Fore;
      Aft  : Integer := Ada.Float_Text_IO.Default_Aft;
      Exp  : Integer := Ada.Float_Text_IO.Default_Exp)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Put_Console unimplemented");
      raise Program_Error with "Unimplemented procedure Put_Console";
   end Put_Console;

   -----------------
   -- Put_Console --
   -----------------

   procedure Put_Console
     (b: in Boolean;
      Width : Ada.Text_IO.Field    := HAC.PCode.Interpreter.Boolean_Text_IO.Default_Width;
      Set   : Ada.Text_IO.Type_Set := HAC.PCode.Interpreter.Boolean_Text_IO.Default_Setting)
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Put_Console unimplemented");
      raise Program_Error with "Unimplemented procedure Put_Console";
   end Put_Console;

   -----------------
   -- Put_Console --
   -----------------

   procedure Put_Console (c: in Character) is
   begin
     Append_to_IO_pipe ( (1 => c));
   end Put_Console;

   -----------------
   -- Put_Console --
   -----------------

   procedure Put_Console (s: in String) is
   begin
     Append_to_IO_pipe (s);
   end Put_Console;

   ----------------------
   -- New_Line_Console --
   ----------------------

   procedure New_Line_Console (Spacing : Ada.Text_IO.Positive_Count := 1) is
   begin
     for i in 1 .. Spacing loop
       New_line_IO_pipe;
     end loop;
   end New_Line_Console;

end LEA_GWin.Messages.IO_Pipe;
