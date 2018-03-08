--  This is an ad-hoc pipe for using a (hum... the global) message list
--  as a terminal.  It is forseen for the HAC compiler and p-code interpreter.

with Ada.Text_IO,
     Ada.Integer_Text_IO,
     Ada.Float_Text_IO;

with HAC.PCode.Interpreter;

package LEA_GWin.Messages.IO_Pipe is

    function End_Of_File_Console return Boolean;
    function End_Of_Line_Console return Boolean;
    procedure Get_Console (i: out Integer; Width : Ada.Text_IO.Field := 0);
    procedure Get_Console (f: out Float;   Width : Ada.Text_IO.Field := 0);
    procedure Get_Console (c: out Character);
    procedure Skip_Line_Console (Spacing : Ada.Text_IO.Positive_Count := 1);
    --
    procedure Put_Console (
      i     : Integer;
      Width : Ada.Text_IO.Field       := Ada.Integer_Text_IO.Default_Width;
      Base  : Ada.Text_IO.Number_Base := Ada.Integer_Text_IO.Default_Base);
    procedure Put_Console (
      f    : Float;
      Fore : Integer := Ada.Float_Text_IO.Default_Fore;
      Aft  : Integer := Ada.Float_Text_IO.Default_Aft;
      Exp  : Integer := Ada.Float_Text_IO.Default_Exp
    );
    procedure Put_Console (
      b: in Boolean;
      Width : Ada.Text_IO.Field    := HAC.PCode.Interpreter.Boolean_Text_IO.Default_Width;
      Set   : Ada.Text_IO.Type_Set := HAC.PCode.Interpreter.Boolean_Text_IO.Default_Setting);
    procedure Put_Console (c: in Character);
    procedure Put_Console (s: in String);
    procedure New_Line_Console (Spacing : Ada.Text_IO.Positive_Count := 1);

end LEA_GWin.Messages.IO_Pipe;
