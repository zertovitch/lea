with LEA_GWin.Input_Boxes;              use LEA_GWin.Input_Boxes;

with GWindows.Application,
     GWindows.Message_Boxes;

with Ada.Calendar,
     Ada.Strings.Fixed;

package body LEA_GWin.Messages.IO_Pipe is

  type ML_access is access all Message_List_Type;

  --  A few global variables here. We assume there is only one main window.
  current_IO_pipe : ML_access := null;
  tick : Ada.Calendar.Time;  --  For display refresh

  procedure Set_current_IO_pipe (ML : in out Message_List_Type) is
  begin
    current_IO_pipe := ML'Unchecked_Access;
  end Set_current_IO_pipe;

  procedure New_line_IO_pipe is
    last_line : constant Integer := current_IO_pipe.Item_Count - 1;
  begin
    current_IO_pipe.Insert_Item ("", Index => last_line + 1);
  end New_line_IO_pipe;

  package RIO is new Ada.Text_IO.Float_IO (HAC_Sys.Defs.HAC_Float);

  procedure Append_to_IO_pipe (new_text : String) is
    last_line : Integer := current_IO_pipe.Item_Count - 1;
    use Ada.Calendar, GWindows.Common_Controls;
    now : constant Time := Clock;
  begin
    if last_line < 0 then
      current_IO_pipe.Insert_Item ("", Index => 0);  --  First line
      tick := Clock;
      last_line := 0;
    end if;
    current_IO_pipe.Set_Item (
      Text => current_IO_pipe.Text (Item => last_line, SubItem => 0) & S2G (new_text),
      Index => last_line
    );
    if now - tick >= 0.3333 then
      current_IO_pipe.Ensure_Visible (last_line, Partial);  --  Scroll to last line
      GWindows.Application.Message_Check;  --  Refresh display
      tick := now;
    end if;
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
    return False;
  end End_Of_Line_Console;

  function Get_Needs_Skip_Line return Boolean is
  begin
    return False;  --  The input is immediate with LEA.
  end Get_Needs_Skip_Line;

   -----------------
   -- Get_Console --
   -----------------

   procedure Get_Console (I : out HAC_Sys.Defs.HAC_Integer; Width : Ada.Text_IO.Field := 0) is
   pragma Unreferenced (Width);
   begin
     if current_IO_pipe = null then
       raise Program_Error with "IO pipe undefined";
     end if;
     I := HAC_Sys.Defs.HAC_Integer'Wide_Value (
       String_Input (current_IO_pipe.mdi_main_parent.all, "Integer Input")
     );
     --  An eventual error raises an exception like Ada.Text_IO.Get.
     Put_Console (I);  --  Reflect the input on the "console".
   end Get_Console;

   procedure Get_Console (F : out HAC_Sys.Defs.HAC_Float; Width : Ada.Text_IO.Field := 0) is
   pragma Unreferenced (Width);
   begin
     if current_IO_pipe = null then
       raise Program_Error with "IO pipe undefined";
     end if;
     F := HAC_Sys.Defs.HAC_Float'Wide_Value (
       String_Input (current_IO_pipe.mdi_main_parent.all, "Floating-point Input")
     );
     --  An eventual error raises an exception like Ada.Text_IO.Get.
     Put_Console (F);  --  Reflect the input on the "console".
   end Get_Console;

   procedure Get_Console (C : out Character) is
   begin
     if current_IO_pipe = null then
       raise Program_Error with "IO pipe undefined";
     end if;
     loop
       declare
         res  : constant GString :=
                  String_Input (current_IO_pipe.mdi_main_parent.all, "Character Input");
         res8 : constant String := G2S (res);  --  8-bit string
       begin
         if res8'Length > 0 then
           C := res8 (res8'First);
           Put_Console (C);  --  Reflect the input on the "console".
           return;
         end if;
       end;
     end loop;
   end Get_Console;

  function Get_Line_Console return String is
  begin
    if current_IO_pipe = null then
      raise Program_Error with "IO pipe undefined";
    end if;
    declare
      s : constant String := G2S (String_Input (current_IO_pipe.mdi_main_parent.all, "String Input"));
    begin
      --  Reflect the input on the "console".
      Put_Console (s);
      New_Line_Console;
      return s;
    end;
  end Get_Line_Console;

   -----------------------
   -- Skip_Line_Console --
   -----------------------

   procedure Skip_Line_Console (Spacing : Ada.Text_IO.Positive_Count := 1) is
   pragma Unreferenced (Spacing);
     use GWindows.Message_Boxes;
     Result : Message_Box_Result;
   begin
     if current_IO_pipe = null then
       raise Program_Error with "IO pipe undefined";
     end if;
     Skip_Line (
       current_IO_pipe.mdi_main_parent.all,
       "Press  Return key  or click  ""OK""." & NL & NL &
       "---" & NL & NL &
       "Press  Esc key  or click on  ""Cancel""" & NL & 
       "for aborting the program.",
       Result
     );
     New_Line_Console;  --  Reflect the new line on the "console".
     if Result = Cancel then
       is_aborted_flag := True;
     end if;
   end Skip_Line_Console;

   -----------------
   -- Put_Console --
   -----------------

   procedure Put_Console
     (I     : HAC_Sys.Defs.HAC_Integer;
      Width : Ada.Text_IO.Field       := Ada.Integer_Text_IO.Default_Width;
      Base  : Ada.Text_IO.Number_Base := Ada.Integer_Text_IO.Default_Base)
   is
     s : String (1 .. HAC_Sys.Defs.HAC_Integer'Size + 4);  --  Longest representation is in base 2.
     use Ada.Strings.Fixed, Ada.Strings;
   begin
     HAC_Sys.Defs.IIO.Put (s (1 .. Width), I, Base);
     Append_to_IO_pipe (s (1 .. Width));
   exception
     when Ada.Text_IO.Layout_Error =>  --  Cannot fit within 1 .. Width
       HAC_Sys.Defs.IIO.Put (s, I, Base);
       Append_to_IO_pipe (Trim (s, Left));
   end Put_Console;

   -----------------
   -- Put_Console --
   -----------------

   procedure Put_Console
     (F    : HAC_Sys.Defs.HAC_Float;
      Fore : Integer := Ada.Float_Text_IO.Default_Fore;
      Aft  : Integer := Ada.Float_Text_IO.Default_Aft;
      Exp  : Integer := Ada.Float_Text_IO.Default_Exp)
   is
     s : String (1 .. 100);
     l : Integer := Fore + 1 + Aft + 1 + Exp;
     use Ada.Strings.Fixed, Ada.Strings;
   begin
     if Exp = 0 then
       l := l - 1;  --  No 'E'
     end if;
     RIO.Put (s (1 .. l), F, Aft, Exp);
     Append_to_IO_pipe (s (1 .. l));
   exception
     when Ada.Text_IO.Layout_Error =>  --  Cannot fit within 1 .. l
       RIO.Put (s, F, Aft, Exp);
       Append_to_IO_pipe (Trim (s, Left));
   end Put_Console;

   procedure Put_Console
     (B     : Boolean;
      Width : Ada.Text_IO.Field    := HAC_Sys.Defs.BIO.Default_Width;
      Set   : Ada.Text_IO.Type_Set := HAC_Sys.Defs.BIO.Default_Setting)
   is
     s : String (1 .. 5);  --  Length of "FALSE"
     use Ada.Strings.Fixed, Ada.Strings;
   begin
     HAC_Sys.Defs.BIO.Put (s (1 .. Width), B, Set);
     Append_to_IO_pipe (s (1 .. Width));
   exception
     when Ada.Text_IO.Layout_Error =>  --  Cannot fit within 1 .. Width
       HAC_Sys.Defs.BIO.Put (s, B, Set);
       Append_to_IO_pipe (Trim (s, Left));
   end Put_Console;

   procedure Put_Console (C : in Character) is
   begin
     Append_to_IO_pipe ((1 => C));
   end Put_Console;

   -----------------
   -- Put_Console --
   -----------------

   procedure Put_Console (S : in String) is
   begin
     Append_to_IO_pipe (S);
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
