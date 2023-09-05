with LEA_Common.Color_Themes;

package LEA_Common.User_options is

  use Ada.Strings.Wide_Unbounded;

  use_default : constant := -1;

  --  MRU (Most Recently Used) files names:

  subtype MRU_Range is Integer range 1 .. 9;

  type MRU_Item is record
    name : UTF_16_Unbounded_String := Null_Unbounded_Wide_String;
    line : Natural                 := 0;  --  = 0: undefined; > 0: last visited line.
  end record;

  type MRU_List is array (MRU_Range) of MRU_Item;

  type Backup_mode is (none, bak);

  --  The GUI-agnostic part of user options are stored in this record
  --
  type Option_Pack_Type is record
    view_mode    : View_Mode_Type                := Notepad;
    color_theme  : Color_Themes.Color_Theme_Type := Color_Themes.Default;
    backup       : Backup_mode                   := none;
    indentation  : Integer                       := 2;
    tab_width    : Integer                       := 2;
    right_margin : Integer                       := 100;  --  also called "vertical edge"
    show_special : Show_special_symbol_mode      := none;
    show_indent  : Boolean                       := False;
    auto_insert  : Boolean                       := True;
    toolset      : Toolset_mode_type             := HAC_mode;
    --  Horizontal portion of the window for the tree, when view_mode = Studio
    tree_portion : Float := 0.25;  --  !! will disappear
    --  Horizontal window portion of MDI *main*, active when view_mode = Studio
    project_tree_portion    : Float := 0.25;
    --  Vertical window portion of MDI *main*
    message_list_portion    : Float := 0.20;
    --  Horizontal window portion of MDI *child*, active when subprogram tree is shown
    subprogram_tree_portion : Float := 0.25;
    --
    win_left,
    win_top,
    win_width,
    win_height            : Integer := use_default;
    MDI_childen_maximized : Boolean := True;
    MDI_main_maximized    : Boolean := True;
    mru                   : MRU_List;
    ada_files_filter      : Unbounded_Wide_String :=
                              To_Unbounded_Wide_String ("*.ads;*.adb;*.ada;*.hac");
    --
    --  "Smart editor" features: mouse-hover tips, call tips, auto-complete, ...
    --  Note Sep-2023: it is in its erly days and not yet "bullet-proof"...
    smart_editor : Boolean := False;
  end record;

  procedure Toggle_show_special (o : in out Option_Pack_Type);

  -----------------
  -- Persistence --
  -----------------

  type Persistence_Key is
    (view_mode,      --  Notepad, Studio
     toolset_mode,   --  HAC, GNAT, ...
     color_theme,
     backup,
     indent, tab_width,
     edge,  --  right margin
     show_special,
     show_indent,
     auto_insert,
     win_left, win_top, win_width, win_height,
     maximized, children_maximized,
     tree_portion,  --  !! to be removed
     project_tree_portion,
     message_list_portion,
     subprogram_tree_portion,
     mru1, mru2, mru3, mru4, mru5, mru6, mru7, mru8, mru9,
     ada_files_filter,
     smart_editor);

  --  On Windows "vanilla", persistence is done usually through the registry.
  --  On Linux or Gtk (any platform) persistence is done usually in a config file.

  generic
    with function Read_Key (key : Persistence_Key) return Wide_String;
    with procedure Write_Key (key : Persistence_Key; value : Wide_String);
  package Persistence is
    procedure Load (opt : out Option_Pack_Type);
    procedure Save (opt : in  Option_Pack_Type);
  end Persistence;

  generic
    with procedure String_Output (key_name : String);
  procedure Show_Persistence_Keys;

end LEA_Common.User_options;
