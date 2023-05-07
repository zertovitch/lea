with LEA_Common;

with GWindows.Colors,
     GWindows.Common_Dialogs,
     GWindows.GStrings;

with Ada.Containers.Vectors;

package LEA_GWin is

  use GWindows, GWindows.GStrings;

  function S2G (Value : String) return GString renames To_GString_From_String;
  function G2S (Value : GString) return String renames To_String;
  function GU2G (Value : GString_Unbounded) return GString renames To_GString_From_Unbounded;
  function G2GU (Value : GString) return GString_Unbounded renames To_GString_Unbounded;

  NL : constant GString := S2G ((ASCII.CR, ASCII.LF));

  Initial_text_files_filters : constant GWindows.Common_Dialogs.Filter_Array :=
    ((G2GU ("Ada files (filter: see Options)"),          G2GU ("*")),
     (G2GU ("Ada files (*.ads, *.adb, *.ada)"),          G2GU ("*.ads;*.adb;*.ada")),
     (G2GU ("Ada specification files (*.ads, *.1.ada)"), G2GU ("*.ads;*.1.ada")),
     (G2GU ("Ada body files (*.adb, *.2.ada)"),          G2GU ("*.adb;*.2.ada")),
     (G2GU ("GNAT project files (*.gpr)"),               G2GU ("*.gpr")),
     (G2GU ("All files (*.*)"),                          G2GU ("*.*")));

  Project_files_filters : constant GWindows.Common_Dialogs.Filter_Array :=
    ((G2GU ("GNAT project files (*.gpr)"),      G2GU ("*.gpr")),
     (G2GU ("ObjectAda project files (*.prj)"), G2GU ("*.prj")));

  App_default_font      : constant GString := "Courier New";
  App_default_font_size : constant := 10;

  type ID_Type is record
    File_Name  : GString_Unbounded;  --  If File_Name = "" (no file), then...
    Short_Name : GString_Unbounded;  --  ... the Short_Name serves as identification.
  end record;

  function Equivalent (Id_1, Id_2 : ID_Type) return Boolean;

  package ID_Vectors is new Ada.Containers.Vectors (Natural, ID_Type);

  function Color_Convert
    (rgb : LEA_Common.Color_Themes.RGB_Type) return GWindows.Colors.Color_Type;

  function GWindows_Color_Theme
    (theme : LEA_Common.Color_Themes.Color_Theme_Type;
     topic : LEA_Common.Color_Themes.Color_Topic) return GWindows.Colors.Color_Type;

end LEA_GWin;
