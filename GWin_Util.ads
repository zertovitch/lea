--  Mix of various functionalities. Some of them would be good as part of GWindows.

with GWindows.Base;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.GControls.GSize_Bars;
with GWindows.GStrings;                 use GWindows.GStrings;
with GWindows.Static_Controls;
with GWindows.Windows;

use GWindows;

package GWin_Util is

  -- Tab:
  HT : constant GCharacter := GCharacter'Val (9);
  -- New Line:
  NL : constant GString    := GCharacter'Val (13) & GCharacter'Val (10);

  function To_Lower(Value : GString) return GString;
  --  Converts case of GString to Lower

  function To_URL_Encoding( s: String ) return String;

  function S2G (Value : String) return GString renames To_GString_From_String;

  boolean_to_state: constant array(Boolean) of Check_State_Type:=
    (True => Checked, False => Unchecked);

  procedure Use_GUI_Font(Window: in out GWindows.Base.Base_Window_Type'Class);

  function Exist(Name : String) return Boolean;

  -----------------------------------------
  -- Execute or start external processes --
  -----------------------------------------

  procedure Start(
    File       : in String;
    Parameter  : in String := "";
    Minimized  : in Boolean:= False
  );

  procedure Exec(name: String; param: String:= "");
  Exec_failed: exception;

  procedure Exec_Command(the_command: String);

  type Windows_family is (Win32s, Win9x, NT);
  procedure Get_Windows_version(
    major, minor: out Integer;
    family      : out Windows_family
  );
  cannot_get_Windows_version: exception;

  -- Toolbar styles (for GWindows.Common_Controls):
  TBSTYLE_WRAPABLE: constant:= 16#200#;
  TBSTYLE_FLAT    : constant:= 16#800#;
  TBSTYLE_LIST    : constant:= 16#1000#;

  function Temp_dir return String;

  function Minimized(Window: GWindows.Base.Base_Window_Type'Class)
    return Boolean;

  function Valid_Left_Top(Left, Top: Integer)
    return Boolean;

  function Find_short_path_name( long: String ) return String;

  ------------------------------
  --  Tabs - Property sheets  --
  ------------------------------

  -- 6-Jan-2007 : Solution to buttons in tabs creating an infinte loop
  --              in Windows' GUI system. This should be corrected
  --              inside of GWindows.
  --              By André van Splunter
  procedure Fix_Tabbed_control( Tab_Control: Tab_Window_Control_Type );

  -- The package Property_Tabs_Package is indeed a kind of generic object type
  -- An instance, like:
  --   package Tabbing is new GWin_Util.Property_Tabs_Package(Tab_subject,Lang.Msg);
  -- contains the Tab objects and Tabbing.Create
  -- manages the creation of tabs with the Ideal Microsoft Proportions
  -- and adds the OK, Cancel, (later) Apply buttons to the parent.

  generic
    type Tab_enumeration is (<>);
    with function Title(te: Tab_enumeration) return GString;
    ok_message    : GString;
    cancel_message: GString;
  package Property_Tabs_Package is
    -- Data:
    tab   : array(Tab_enumeration) of aliased GWindows.Windows.Window_Type;
    ok    : Default_Button_Type;
    cancel: Button_Type;
    procedure Create(Parent: in out GWindows.Base.Base_Window_Type'Class);
  end Property_Tabs_Package;

  --  ----------------------
  --  -- Fix Dialog style --
  --  ----------------------

  --  procedure Fix_Dialog(Dialog: in GWindows.Windows.Window_Type);

  ------------------------------------------
  --  Split bar including a visible grip  --
  ------------------------------------------

  type Splitter_with_dashes is new GWindows.GControls.GSize_Bars.GSize_Bar_Type with record
    Dashes : GWindows.Static_Controls.Label_Type;
  end record;

  overriding procedure Create
     (Window     : in out Splitter_with_dashes;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Location   : in     GWindows.Base.Dock_Type;
      Text       : in     GString                              := "";
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 3;
      Height     : in     Integer                              := 3;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);

end GWin_Util;
