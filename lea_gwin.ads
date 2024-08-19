with LEA_Common.Color_Themes;

with GWindows.Colors,
     GWindows.Common_Dialogs,
     GWindows.GStrings,
     GWindows.Menus;

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

  App_default_font      : constant GString := "Consolas";  --  Was: "Courier New";
  App_default_font_size : constant := 10;

  type ID_Type is record
    file_name  : GString_Unbounded;  --  If .file_name = "" (no file), then...
    short_name : GString_Unbounded;  --  ...  .short_name serves as identification.
  end record;

  function Equivalent (Id_1, Id_2 : ID_Type) return Boolean;

  function Color_Convert
    (rgb : LEA_Common.Color_Themes.RGB_Type) return GWindows.Colors.Color_Type;

--   function GWindows_Color_Theme
--     (theme : LEA_Common.Color_Themes.Color_Theme_Type;
--      topic : LEA_Common.Color_Themes.Color_Topic) return GWindows.Colors.Color_Type;

  bool_to_state : constant array (Boolean) of GWindows.Menus.State_Type :=
    (GWindows.Menus.Disabled, GWindows.Menus.Enabled);

  function Simple_Name (path : GString) return GString;

end LEA_GWin;
