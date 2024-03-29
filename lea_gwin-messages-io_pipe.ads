--  This is an ad-hoc pipe for using a (hum... the global) message list
--  as a terminal.  It is foreseen for the HAC p-code interpreter.

with Ada.Text_IO,
     Ada.Integer_Text_IO,
     Ada.Float_Text_IO;

with HAC_Sys.Defs;

package LEA_GWin.Messages.IO_Pipe is

  procedure Set_current_IO_pipe (ML : in out Message_List_Type);

  function End_Of_File_Console return Boolean;
  function End_Of_Line_Console return Boolean;
  function Get_Needs_Skip_Line return Boolean;
  procedure Get_Console (I : out HAC_Sys.Defs.HAC_Integer; Width : Ada.Text_IO.Field := 0);
  procedure Get_Console (F : out HAC_Sys.Defs.HAC_Float;   Width : Ada.Text_IO.Field := 0);
  procedure Get_Console (C : out Character);
  function Get_Line_Console return String;
  procedure Skip_Line_Console (Spacing : Ada.Text_IO.Positive_Count := 1);
  --
  procedure Put_Console (
    I     : HAC_Sys.Defs.HAC_Integer;
    Width : Ada.Text_IO.Field       := Ada.Integer_Text_IO.Default_Width;
    Base  : Ada.Text_IO.Number_Base := Ada.Integer_Text_IO.Default_Base);
  procedure Put_Console (
    F    : HAC_Sys.Defs.HAC_Float;
    Fore : Integer := Ada.Float_Text_IO.Default_Fore;
    Aft  : Integer := Ada.Float_Text_IO.Default_Aft;
    Exp  : Integer := Ada.Float_Text_IO.Default_Exp
  );
  procedure Put_Console (
    B     : Boolean;
    Width : Ada.Text_IO.Field    := HAC_Sys.Defs.BIO.Default_Width;
    Set   : Ada.Text_IO.Type_Set := HAC_Sys.Defs.BIO.Default_Setting);
  procedure Put_Console (C : in Character);
  procedure Put_Console (S : in String);
  procedure New_Line_Console (Spacing : Ada.Text_IO.Positive_Count := 1);

  --  A global variable here. We assume there is only one main window.
  is_aborted_flag : Boolean;

end LEA_GWin.Messages.IO_Pipe;
