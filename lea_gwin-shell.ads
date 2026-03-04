with HAT;

package LEA_GWin.Shell is

  procedure Execute_to_LEA_Console (command : String; result : out Integer);
  --
  --  IMPORTANT: Call Set_current_IO_Pipe prior to a call to that procedure!

  procedure Execute_to_VString (command : String; result : out Integer; output : out HAT.VString);

end LEA_GWin.Shell;
