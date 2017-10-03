package LEA_Common.User_options is

  use_default: constant:= -1;

  -- MRU (Most Recently Used) files names:
  type MRU_List is array(1..9) of UTF_16_Unbounded_String;

  -- The GUI-agnostic part of user options are stored in this record
  --
  type Option_Pack_Type is record
    view_mode    : View_Mode_Type:= Notepad;
    -- Horizontal portion of the window for the tree, when view_mode = Studio
    tree_portion : Float:= 0.25;
    win_left,
    win_top,
    win_width,
    win_height            : Integer := use_default;
    MDI_childen_maximized : Boolean := True;
    MDI_main_maximized    : Boolean := False;
    mru                   : MRU_List:= (others => Null_Unbounded_Wide_String);
  end record;

  -----------------
  -- Persistence --
  -----------------

  -- On Windows "vanilla", it is done through the registry
  -- On Linux or Gtk (any platform) it is done usually in a config file

  generic
    with function Read_key(topic: Wide_String) return Wide_String;
    with procedure Write_key(topic: Wide_String; value: Wide_String);
  package Persistence is
    procedure Load(opt: out Option_Pack_Type);
    procedure Save(opt: in  Option_Pack_Type);
  end;

end LEA_Common.User_options;
