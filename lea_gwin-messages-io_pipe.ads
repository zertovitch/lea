--  This is an ad-hoc pipe for using a (hum... the global) message list
--  as a terminal.  It is foreseen for the HAC p-code interpreter.

with Ada.Text_IO,
     Ada.Integer_Text_IO,
     Ada.Float_Text_IO;

with HAC.Data;

package LEA_GWin.Messages.IO_Pipe is

  procedure Set_current_IO_pipe (ML: in out Message_List_Type);

  function End_Of_File_Console return Boolean;
  function End_Of_Line_Console return Boolean;
  function Get_Needs_Skip_Line return Boolean;
  procedure Get_Console (i: out Integer;            Width : Ada.Text_IO.Field := 0);
  procedure Get_Console (f: out HAC.Data.HAC_Float; Width : Ada.Text_IO.Field := 0);
  procedure Get_Console (c: out Character);
  function Get_Line_Console return String;
  procedure Skip_Line_Console (Spacing : Ada.Text_IO.Positive_Count := 1);
  --
  procedure Put_Console (
    i     : Integer;
    Width : Ada.Text_IO.Field       := Ada.Integer_Text_IO.Default_Width;
    Base  : Ada.Text_IO.Number_Base := Ada.Integer_Text_IO.Default_Base);
  procedure Put_Console (
    f    : HAC.Data.HAC_Float;
    Fore : Integer := Ada.Float_Text_IO.Default_Fore;
    Aft  : Integer := Ada.Float_Text_IO.Default_Aft;
    Exp  : Integer := Ada.Float_Text_IO.Default_Exp
  );
  procedure Put_Console (
    b     : Boolean;
    Width : Ada.Text_IO.Field    := HAC.Data.BIO.Default_Width;
    Set   : Ada.Text_IO.Type_Set := HAC.Data.BIO.Default_Setting);
  procedure Put_Console (c: in Character);
  procedure Put_Console (s: in String);
  procedure New_Line_Console (Spacing : Ada.Text_IO.Positive_Count := 1);

end LEA_GWin.Messages.IO_Pipe;
