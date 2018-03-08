package body LEA_GWin.Messages.IO_Pipe is

   -------------------------
   -- End_Of_File_Console --
   -------------------------

   function End_Of_File_Console return Boolean is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "End_Of_File_Console unimplemented");
      raise Program_Error with "Unimplemented function End_Of_File_Console";
      return End_Of_File_Console;
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
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Put_Console unimplemented");
      raise Program_Error with "Unimplemented procedure Put_Console";
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
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Put_Console unimplemented");
      raise Program_Error with "Unimplemented procedure Put_Console";
   end Put_Console;

   -----------------
   -- Put_Console --
   -----------------

   procedure Put_Console (s: in String) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Put_Console unimplemented");
      raise Program_Error with "Unimplemented procedure Put_Console";
   end Put_Console;

   ----------------------
   -- New_Line_Console --
   ----------------------

   procedure New_Line_Console (Spacing : Ada.Text_IO.Positive_Count := 1) is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "New_Line_Console unimplemented");
      raise Program_Error with "Unimplemented procedure New_Line_Console";
   end New_Line_Console;

end LEA_GWin.Messages.IO_Pipe;
