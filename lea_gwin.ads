with GWindows;                          use GWindows;
with GWindows.Common_Dialogs;
with GWindows.GStrings;                 use GWindows.GStrings;

package LEA_GWin is

  function S2G (Value : String) return GString renames To_GString_From_String;
  function G2S (Value : GString) return String renames To_String;
  function GU2G (Value : GString_Unbounded) return GString renames To_GString_From_Unbounded;
  function G2GU (Value : GString) return GString_Unbounded renames To_GString_Unbounded;

  NL: constant GString:= S2G((ASCII.CR, ASCII.LF));

  Text_files_filters: GWindows.Common_Dialogs.Filter_Array:=  --  !! Global variable, move to MDI_Main
    ((G2GU ("Ada files"),                         G2GU ("*" )),  --  This is overwritten by options
     (G2GU ("Ada files (*.ads, *.adb)"),          G2GU ("*.ads;*.adb" )),
     (G2GU ("Ada specification files (*.ads)"),   G2GU ("*.ads" )),
     (G2GU ("Ada body files (*.adb)"),            G2GU ("*.adb" )),
     (G2GU ("GNAT project files (*.gpr)"),        G2GU ("*.gpr" )),
     (G2GU ("All files (*.*)"),                   G2GU ("*.*")));

  Project_files_filters: constant GWindows.Common_Dialogs.Filter_Array:=
    ((G2GU ("GNAT project files (*.gpr)"),        G2GU ("*.gpr" )),
     (G2GU ("ObjectAda project files (*.prj)"),   G2GU ("*.prj" )));

  App_default_font      : constant GString := "Courier New";
  App_default_font_size : constant := 10;

end LEA_GWin;
