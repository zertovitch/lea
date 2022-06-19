---------------------------------------------------------------------------
--  GUI contents of resource script file: LEA.rc
--  Transcription time: 2022/06/19  14:40:09
--  GWenerator project file: lea.gwen
--
--  Translated by the RC2GW or by the GWenerator tool.
--  URL: http://sf.net/projects/gnavi
--
--  This file contains only automatically generated code. Do not edit this.
--  Rework the resource script instead, and re-run the translator.
--  RC Grammar version: 14-Apr-2021
---------------------------------------------------------------------------

with GWindows.Types;                    use GWindows.Types;
with GWindows.Drawing;                  use GWindows.Drawing;
with GWindows.Drawing_Objects;
with GWindows.GStrings;                 use GWindows.GStrings;
with System;

pragma Warnings ("U");  --  turn off warnings for unused entity

package body LEA_Resource_GUI is

  --  ** Generated code begins here \/ \/ \/.

  --  Menu at line 29
  procedure Create_Full_Menu (New_Menu : in out Menu_MDI_Child_Type)
  is
  begin
    New_Menu.Main := Create_Menu;
    New_Menu.Popup_0001 := Create_Popup;
    Append_Menu (New_Menu.Main, "&File", New_Menu.Popup_0001);
    Append_Item (New_Menu.Popup_0001, "&New" & To_GString_From_String((1=>ASCII.HT)) & "Ctrl+N", IDM_New_File);
    Append_Item (New_Menu.Popup_0001, "&Open..." & To_GString_From_String((1=>ASCII.HT)) & "Ctrl+O", IDM_Open_File);
    New_Menu.Popup_0002 := Create_Popup;
    Append_Menu (New_Menu.Popup_0001, "Open &recent", New_Menu.Popup_0002);
    Append_Item (New_Menu.Popup_0002, "mru_1", IDM_MRU_1);
    Append_Item (New_Menu.Popup_0002, "mru_2", IDM_MRU_2);
    Append_Item (New_Menu.Popup_0002, "mru_3", IDM_MRU_3);
    Append_Item (New_Menu.Popup_0002, "mru_4", IDM_MRU_4);
    Append_Item (New_Menu.Popup_0002, "mru_5", IDM_MRU_5);
    Append_Item (New_Menu.Popup_0002, "mru_6", IDM_MRU_6);
    Append_Item (New_Menu.Popup_0002, "mru_7", IDM_MRU_7);
    Append_Item (New_Menu.Popup_0002, "mru_8", IDM_MRU_8);
    Append_Item (New_Menu.Popup_0002, "mru_9", IDM_MRU_9);
    Append_Item (New_Menu.Popup_0001, "Open containing &folder", IDM_Open_Containing_Folder);
    Append_Item (New_Menu.Popup_0001, "Save" & To_GString_From_String((1=>ASCII.HT)) & "Ctrl+S", IDM_Save_File);
    Append_Item (New_Menu.Popup_0001, "Save &as..." & To_GString_From_String((1=>ASCII.HT)) & "F12", IDM_Save_As);
    Append_Item (New_Menu.Popup_0001, "Sav&e all modified", IDM_Save_All);
    Append_Item (New_Menu.Popup_0001, "&Close" & To_GString_From_String((1=>ASCII.HT)) & "Ctrl+W / Ctrl+F4", IDM_Close);
    Append_Separator (New_Menu.Popup_0001);
    Append_Item (New_Menu.Popup_0001, "Open &Project", IDM_Open_Project);
    State (New_Menu.Popup_0001, Command, IDM_Open_Project, Disabled);
    Append_Separator (New_Menu.Popup_0001);
    Append_Item (New_Menu.Popup_0001, "&Quit" & To_GString_From_String((1=>ASCII.HT)) & "Alt+F4", IDM_QUIT);
    New_Menu.Popup_0003 := Create_Popup;
    Append_Menu (New_Menu.Main, "&Edit", New_Menu.Popup_0003);
    Append_Item (New_Menu.Popup_0003, "&Undo" & To_GString_From_String((1=>ASCII.HT)) & "Ctrl+Z", IDM_Undo);
    Append_Item (New_Menu.Popup_0003, "&Redo" & To_GString_From_String((1=>ASCII.HT)) & "Ctrl+Y", IDM_Redo);
    Append_Separator (New_Menu.Popup_0003);
    Append_Item (New_Menu.Popup_0003, "Cu&t to clipboard" & To_GString_From_String((1=>ASCII.HT)) & "Ctrl+X or Shift+Del", IDM_Cut);
    Append_Item (New_Menu.Popup_0003, "&Copy to clipboard" & To_GString_From_String((1=>ASCII.HT)) & "Ctrl+C or Ctrl+Ins", IDM_Copy);
    Append_Item (New_Menu.Popup_0003, "Copy message &list to clipboard", IDM_Copy_Messages);
    Append_Item (New_Menu.Popup_0003, "&Paste from clipboard" & To_GString_From_String((1=>ASCII.HT)) & "Ctrl+V or Shift+Ins", IDM_Paste);
    Append_Separator (New_Menu.Popup_0003);
    Append_Item (New_Menu.Popup_0003, "&Duplicate line or selection" & To_GString_From_String((1=>ASCII.HT)) & "Ctrl+D", IDM_Duplicate);
    Append_Separator (New_Menu.Popup_0003);
    Append_Item (New_Menu.Popup_0003, "Select &all" & To_GString_From_String((1=>ASCII.HT)) & "Ctrl+A", IDM_Select_all);
    Append_Separator (New_Menu.Popup_0003);
    Append_Item (New_Menu.Popup_0003, "&Indent" & To_GString_From_String((1=>ASCII.HT)) & "Tab", IDM_Indent);
    Append_Item (New_Menu.Popup_0003, "&Unindent" & To_GString_From_String((1=>ASCII.HT)) & "Shift+Tab", IDM_Unindent);
    Append_Separator (New_Menu.Popup_0003);
    Append_Item (New_Menu.Popup_0003, "Co&mment" & To_GString_From_String((1=>ASCII.HT)) & "F7", IDM_Comment);
    Append_Item (New_Menu.Popup_0003, "U&ncomment" & To_GString_From_String((1=>ASCII.HT)) & "Shift+F7", IDM_Uncomment);
    New_Menu.Popup_0004 := Create_Popup;
    Append_Menu (New_Menu.Main, "&Navigate", New_Menu.Popup_0004);
    Append_Item (New_Menu.Popup_0004, "Find && Replace..." & To_GString_From_String((1=>ASCII.HT)) & "Ctrl+F", IDM_Find);
    Append_Item (New_Menu.Popup_0004, "Find next" & To_GString_From_String((1=>ASCII.HT)) & "F3", IDM_Find_Next);
    Append_Item (New_Menu.Popup_0004, "Find previous" & To_GString_From_String((1=>ASCII.HT)) & "Shift+F3", IDM_Find_Previous);
    Append_Separator (New_Menu.Popup_0004);
    Append_Item (New_Menu.Popup_0004, "&Go to line..." & To_GString_From_String((1=>ASCII.HT)) & "Ctrl+G", IDM_Go_to_line);
    Append_Separator (New_Menu.Popup_0004);
    Append_Item (New_Menu.Popup_0004, "Next bookmark" & To_GString_From_String((1=>ASCII.HT)) & "F2", IDM_Next_bookmark);
    Append_Item (New_Menu.Popup_0004, "Previous bookmark" & To_GString_From_String((1=>ASCII.HT)) & "Shift+F2", IDM_Previous_bookmark);
    Append_Item (New_Menu.Popup_0004, "Toggle bookmark" & To_GString_From_String((1=>ASCII.HT)) & "Ctrl+F2", IDM_Toggle_bookmark);
    New_Menu.Popup_0005 := Create_Popup;
    Append_Menu (New_Menu.Main, "&Actions", New_Menu.Popup_0005);
    Append_Item (New_Menu.Popup_0005, "&Compile this file" & To_GString_From_String((1=>ASCII.HT)) & "Shift-F4", IDM_Compile_single);
    Append_Item (New_Menu.Popup_0005, "&Build application" & To_GString_From_String((1=>ASCII.HT)) & "F4", IDM_Build);
    Append_Item (New_Menu.Popup_0005, "Build and &run" & To_GString_From_String((1=>ASCII.HT)) & "F9", IDM_Build_and_run);
    Append_Separator (New_Menu.Popup_0005);
    Append_Item (New_Menu.Popup_0005, "Code sam&ple", IDM_Ada_Sample);
    New_Menu.Popup_0006 := Create_Popup;
    Append_Menu (New_Menu.Main, "&View", New_Menu.Popup_0006);
    Append_Item (New_Menu.Popup_0006, "Show special symbols", IDM_Show_special_symbols);
    Append_Item (New_Menu.Popup_0006, "Show indentation lines", IDM_Show_indentation_lines);
    Append_Separator (New_Menu.Popup_0006);
    Append_Item (New_Menu.Popup_0006, "&Notepad view", IDM_Notepad_view);
    Append_Item (New_Menu.Popup_0006, "&Studio view", IDM_Studio_view);
    State (New_Menu.Popup_0006, Command, IDM_Studio_view, Disabled);
    Append_Separator (New_Menu.Popup_0006);
    Append_Item (New_Menu.Popup_0006, "&HAC restricted Ada mode", IDM_HAC_Mode);
    Append_Item (New_Menu.Popup_0006, "&GNAT full Ada mode", IDM_GNAT_Mode);
    State (New_Menu.Popup_0006, Command, IDM_GNAT_Mode, Disabled);
    New_Menu.Popup_0007 := Create_Popup;
    Append_Menu (New_Menu.Main, "&Options", New_Menu.Popup_0007);
    Append_Item (New_Menu.Popup_0007, "&General options", IDM_General_options);
    New_Menu.Popup_0008 := Create_Popup;
    Append_Menu (New_Menu.Main, "&Window", New_Menu.Popup_0008);
    Append_Item (New_Menu.Popup_0008, "&Cascade", IDM_WINDOW_CASCADE);
    Append_Item (New_Menu.Popup_0008, "Tile &Horizontal", IDM_WINDOW_TILE_HORIZONTAL);
    Append_Item (New_Menu.Popup_0008, "Tile &Vertical", IDM_WINDOW_TILE_VERTICAL);
    Append_Item (New_Menu.Popup_0008, "&Close All", IDM_WINDOW_CLOSE_ALL);
    New_Menu.Popup_0009 := Create_Popup;
    Append_Menu (New_Menu.Main, "&Help", New_Menu.Popup_0009);
    Append_Item (New_Menu.Popup_0009, "&Quick help" & To_GString_From_String((1=>ASCII.HT)) & "F1", IDM_Quick_Help);
    Append_Item (New_Menu.Popup_0009, "LEA &Web page (contact, support)", IDM_Web);
    Append_Separator (New_Menu.Popup_0009);
    Append_Item (New_Menu.Popup_0009, "&About LEA", IDM_ABOUT);
  end Create_Full_Menu;  --  For type: Menu_MDI_Child_Type

  --  Menu at line 131
  procedure Create_Full_Menu (New_Menu : in out Menu_MDI_Main_Type)
  is
  begin
    New_Menu.Main := Create_Menu;
    New_Menu.Popup_0001 := Create_Popup;
    Append_Menu (New_Menu.Main, "&File", New_Menu.Popup_0001);
    Append_Item (New_Menu.Popup_0001, "&New" & To_GString_From_String((1=>ASCII.HT)) & "Ctrl+N", IDM_New_File);
    Append_Item (New_Menu.Popup_0001, "&Open..." & To_GString_From_String((1=>ASCII.HT)) & "Ctrl+O", IDM_Open_File);
    New_Menu.Popup_0002 := Create_Popup;
    Append_Menu (New_Menu.Popup_0001, "Open &recent", New_Menu.Popup_0002);
    Append_Item (New_Menu.Popup_0002, "mru_1", IDM_MRU_1);
    Append_Item (New_Menu.Popup_0002, "mru_2", IDM_MRU_2);
    Append_Item (New_Menu.Popup_0002, "mru_3", IDM_MRU_3);
    Append_Item (New_Menu.Popup_0002, "mru_4", IDM_MRU_4);
    Append_Item (New_Menu.Popup_0002, "mru_5", IDM_MRU_5);
    Append_Item (New_Menu.Popup_0002, "mru_6", IDM_MRU_6);
    Append_Item (New_Menu.Popup_0002, "mru_7", IDM_MRU_7);
    Append_Item (New_Menu.Popup_0002, "mru_8", IDM_MRU_8);
    Append_Item (New_Menu.Popup_0002, "mru_9", IDM_MRU_9);
    Append_Separator (New_Menu.Popup_0001);
    Append_Item (New_Menu.Popup_0001, "Open &Project", IDM_Open_Project);
    State (New_Menu.Popup_0001, Command, IDM_Open_Project, Disabled);
    Append_Separator (New_Menu.Popup_0001);
    Append_Item (New_Menu.Popup_0001, "&Quit" & To_GString_From_String((1=>ASCII.HT)) & "Ctrl+W / Alt+F4", IDM_QUIT);
    New_Menu.Popup_0003 := Create_Popup;
    Append_Menu (New_Menu.Main, "&Edit", New_Menu.Popup_0003);
    Append_Item (New_Menu.Popup_0003, "Copy message &list to clipboard", IDM_Copy_Messages);
    New_Menu.Popup_0004 := Create_Popup;
    Append_Menu (New_Menu.Main, "&Actions", New_Menu.Popup_0004);
    Append_Item (New_Menu.Popup_0004, "&Compile this file" & To_GString_From_String((1=>ASCII.HT)) & "Shift-F4", IDM_Compile_single);
    State (New_Menu.Popup_0004, Command, IDM_Compile_single, Disabled);
    Append_Item (New_Menu.Popup_0004, "&Build application" & To_GString_From_String((1=>ASCII.HT)) & "F4", IDM_Build);
    State (New_Menu.Popup_0004, Command, IDM_Build, Disabled);
    Append_Item (New_Menu.Popup_0004, "Build and &run" & To_GString_From_String((1=>ASCII.HT)) & "F9", IDM_Build_and_run);
    State (New_Menu.Popup_0004, Command, IDM_Build_and_run, Disabled);
    Append_Separator (New_Menu.Popup_0004);
    Append_Item (New_Menu.Popup_0004, "Code sam&ple", IDM_Ada_Sample);
    New_Menu.Popup_0005 := Create_Popup;
    Append_Menu (New_Menu.Main, "&View", New_Menu.Popup_0005);
    Append_Item (New_Menu.Popup_0005, "&Notepad view", IDM_Notepad_view);
    Append_Item (New_Menu.Popup_0005, "&Studio view", IDM_Studio_view);
    State (New_Menu.Popup_0005, Command, IDM_Studio_view, Disabled);
    Append_Separator (New_Menu.Popup_0005);
    Append_Item (New_Menu.Popup_0005, "&HAC restricted Ada mode", IDM_HAC_Mode);
    Append_Item (New_Menu.Popup_0005, "&GNAT full Ada mode", IDM_GNAT_Mode);
    State (New_Menu.Popup_0005, Command, IDM_GNAT_Mode, Disabled);
    New_Menu.Popup_0006 := Create_Popup;
    Append_Menu (New_Menu.Main, "&Options", New_Menu.Popup_0006);
    Append_Item (New_Menu.Popup_0006, "&General options", IDM_General_options);
    New_Menu.Popup_0007 := Create_Popup;
    Append_Menu (New_Menu.Main, "&Window", New_Menu.Popup_0007);
    Append_Item (New_Menu.Popup_0007, "&Cascade", IDM_WINDOW_CASCADE);
    Append_Item (New_Menu.Popup_0007, "Tile &Horizontal", IDM_WINDOW_TILE_HORIZONTAL);
    Append_Item (New_Menu.Popup_0007, "Tile &Vertical", IDM_WINDOW_TILE_VERTICAL);
    Append_Item (New_Menu.Popup_0007, "&Close All", IDM_WINDOW_CLOSE_ALL);
    New_Menu.Popup_0008 := Create_Popup;
    Append_Menu (New_Menu.Main, "&Help", New_Menu.Popup_0008);
    Append_Item (New_Menu.Popup_0008, "&Quick help" & To_GString_From_String((1=>ASCII.HT)) & "F1", IDM_Quick_Help);
    Append_Item (New_Menu.Popup_0008, "LEA &Web page (contact, support)", IDM_Web);
    Append_Separator (New_Menu.Popup_0008);
    Append_Item (New_Menu.Popup_0008, "&About LEA", IDM_ABOUT);
  end Create_Full_Menu;  --  For type: Menu_MDI_Main_Type

  --  Dialog at resource line 203

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out About_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned)
  is
    pragma Unmodified (Window);
    pragma Unmodified (dwExStyle);
    WS_SYSMENU : constant := 16#0008_0000#;
  begin
    dwStyle:= dwStyle and not WS_SYSMENU;
  end On_Pre_Create;

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out About_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "About LEA";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x, y, w, h: Integer;
  begin
    Dlg_to_Scn ( 0, 0, 289, 225, x,y,w,h);
    if Left   /= Use_Default then x := Left;   end if;
    if Top    /= Use_Default then y := Top;    end if;
    if Width  /= Use_Default then w := Width;  end if;
    if Height /= Use_Default then h := Height; end if;
    Create_As_Dialog(
      Window => Window_Type(Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then Client_Area_Width(Window, w); end if;
    if Height = Use_Default then Client_Area_Height(Window, h); end if;
    Use_GUI_Font(Window);
    Create_Contents(Window, True);
  end Create_Full_Dialog; -- About_box_Type

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out About_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionally resize Window as designed
     )
  is
    x,y,w,h : Integer;
  begin
    if resize then
    Dlg_to_Scn ( 0, 0, 289, 225, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn ( 12, 14, 87, 80, x,y,w,h);
    Create (Window.Static_0001, Window, Num_resource (LEA_Icon), x,y,w,h, GWindows.Static_Controls.Static_Size, Half_Sunken);
    Dlg_to_Scn ( 110, 14, 165, 8, x,y,w,h);
    Create_Label (Window, "LEA - a Lightweight Editor for Ada", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 110, 29, 165, 8, x,y,w,h);
    Create (Window.Copyright_label, Window, "Copyright © Gautier de Montmollin 2017 .. 2071", x,y,w,h, GWindows.Static_Controls.Left, None, ID => Copyright_label);
    Dlg_to_Scn ( 110, 44, 120, 8, x,y,w,h);
    Create_Label (Window, "MIT Open Source License", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 110, 61, 30, 8, x,y,w,h);
    Create_Label (Window, "Internet:", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 157, 61, 89, 8, x,y,w,h);
    Create (Window.LEA_URL, Window, "http://l-e-a.sf.net/", x,y,w,h, GWindows.Static_Controls.Left, None, ID => LEA_URL);
    Dlg_to_Scn ( 110, 81, 30, 8, x,y,w,h);
    Create_Label (Window, "Version:", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 157, 81, 118, 8, x,y,w,h);
    Create (Window.Version_label, Window, "(ver)", x,y,w,h, GWindows.Static_Controls.Left, None, ID => Version_label);
    Dlg_to_Scn ( 5, 105, 278, 93, x,y,w,h);
    Create( Window.Static_0006, Window, "Software made with the following free, open source components:", x, y, w, h);
    Dlg_to_Scn ( 23, 119, 100, 8, x,y,w,h);
    Create (Window.GNAT_URL, Window, "GNAT -  free Ada compiler", x,y,w,h, GWindows.Static_Controls.Left, None, ID => GNAT_URL);
    Dlg_to_Scn ( 132, 119, 147, 8, x,y,w,h);
    Create (Window.GNAT_Version, Window, "GNAT_Version", x,y,w,h, GWindows.Static_Controls.Left, None, ID => GNAT_Version);
    Dlg_to_Scn ( 23, 134, 100, 8, x,y,w,h);
    Create (Window.HAC_URL, Window, "HAC Ada Compiler", x,y,w,h, GWindows.Static_Controls.Left, None, ID => HAC_URL);
    Dlg_to_Scn ( 132, 134, 146, 8, x,y,w,h);
    Create (Window.HAC_Version, Window, "HAC Version", x,y,w,h, GWindows.Static_Controls.Left, None, ID => HAC_Version);
    Dlg_to_Scn ( 23, 149, 118, 8, x,y,w,h);
    Create (Window.GNAVI_URL, Window, "GNAVI / GWindows", x,y,w,h, GWindows.Static_Controls.Left, None, ID => GNAVI_URL);
    Dlg_to_Scn ( 23, 164, 100, 8, x,y,w,h);
    Create (Window.ZipAda_URL, Window, "Zip-Ada", x,y,w,h, GWindows.Static_Controls.Left, None, ID => ZipAda_URL);
    Dlg_to_Scn ( 132, 164, 146, 8, x,y,w,h);
    Create (Window.ZipAda_Version, Window, "Zip-Ada Version", x,y,w,h, GWindows.Static_Controls.Left, None, ID => ZipAda_Version);
    Dlg_to_Scn ( 23, 179, 170, 8, x,y,w,h);
    Create (Window.ResEdit_URL, Window, "ResEdit", x,y,w,h, GWindows.Static_Controls.Left, None, ID => ResEdit_URL);
    Dlg_to_Scn ( 87, 201, 115, 19, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDOK, Window, "Close", x,y,w,h, ID => IDOK);
    Create (Window.IDOK_permanent, Window, "Close", x,y,w,h, ID => IDOK);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDOK_permanent);
    else  --  Hide the closing button
      Hide (Window.IDOK);
    end if;
  end Create_Contents;  --  About_box_Type

  --  Dialog at resource line 231

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Go_to_line_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned)
  is
    pragma Unmodified (Window);
    pragma Unmodified (dwExStyle);
    WS_SYSMENU : constant := 16#0008_0000#;
  begin
    dwStyle:= dwStyle and not WS_SYSMENU;
  end On_Pre_Create;

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Go_to_line_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Go to line...";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x, y, w, h: Integer;
  begin
    Dlg_to_Scn ( 0, 0, 186, 47, x,y,w,h);
    if Left   /= Use_Default then x := Left;   end if;
    if Top    /= Use_Default then y := Top;    end if;
    if Width  /= Use_Default then w := Width;  end if;
    if Height /= Use_Default then h := Height; end if;
    Create_As_Dialog(
      Window => Window_Type(Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then Client_Area_Width(Window, w); end if;
    if Height = Use_Default then Client_Area_Height(Window, h); end if;
    Use_GUI_Font(Window);
    Create_Contents(Window, True);
  end Create_Full_Dialog; -- Go_to_line_box_Type

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Go_to_line_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionally resize Window as designed
     )
  is
    x,y,w,h : Integer;
  begin
    if resize then
    Dlg_to_Scn ( 0, 0, 186, 47, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn ( 23, 12, 80, 14, x,y,w,h);
    Create (Window.Line_value_box, Window, "", x,y,w,h, Horizontal_Scroll => True, Read_Only => False, ID => Line_value_box);
    Dlg_to_Scn ( 126, 25, 50, 17, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDCANCEL, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    Create (Window.IDCANCEL_permanent, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDCANCEL_permanent);
    else  --  Hide the closing button
      Hide (Window.IDCANCEL);
    end if;
    Dlg_to_Scn ( 126, 4, 50, 17, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDOK, Window, "Go !", x,y,w,h, ID => IDOK);
    Create (Window.IDOK_permanent, Window, "Go !", x,y,w,h, ID => IDOK);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDOK_permanent);
    else  --  Hide the closing button
      Hide (Window.IDOK);
    end if;
  end Create_Contents;  --  Go_to_line_box_Type

  --  Dialog at resource line 244

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out HAC_example_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned)
  is
    pragma Unmodified (Window);
    pragma Unmodified (dwExStyle);
    WS_SYSMENU : constant := 16#0008_0000#;
  begin
    dwStyle:= dwStyle and not WS_SYSMENU;
  end On_Pre_Create;

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out HAC_example_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "HAC Ada source code samples";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x, y, w, h: Integer;
  begin
    Dlg_to_Scn ( 0, 0, 472, 188, x,y,w,h);
    if Left   /= Use_Default then x := Left;   end if;
    if Top    /= Use_Default then y := Top;    end if;
    if Width  /= Use_Default then w := Width;  end if;
    if Height /= Use_Default then h := Height; end if;
    Create_As_Dialog(
      Window => Window_Type(Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then Client_Area_Width(Window, w); end if;
    if Height = Use_Default then Client_Area_Height(Window, h); end if;
    Use_GUI_Font(Window);
    Create_Contents(Window, True);
  end Create_Full_Dialog; -- HAC_example_box_Type

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out HAC_example_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionally resize Window as designed
     )
  is
    x,y,w,h : Integer;
  begin
    if resize then
    Dlg_to_Scn ( 0, 0, 472, 188, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn ( 9, 5, 96, 12, x,y,w,h);
    Create (Window.Label_HAC_topic, Window, "Topic", x,y,w,h, GWindows.Static_Controls.Left, None, ID => Label_HAC_topic);
    Dlg_to_Scn ( 145, 5, 96, 12, x,y,w,h);
    Create (Window.Label_HAC_sample, Window, "Sample", x,y,w,h, GWindows.Static_Controls.Left, None, ID => Label_HAC_sample);
    Dlg_to_Scn ( 9, 22, 123, 95, x,y,w,h);
    Create (Window.Topic_box, Window, x,y,w,h, False, ID => Topic_box);
    Dlg_to_Scn ( 144, 22, 319, 137, x,y,w,h);
    Create (Window.Zipped_file_box, Window, x,y,w,h, Multiple, Report_View, No_Sorting, False, Align_Left);
    Dlg_to_Scn ( 352, 166, 50, 16, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDOK, Window, "OK", x,y,w,h, ID => IDOK);
    Create (Window.IDOK_permanent, Window, "OK", x,y,w,h, ID => IDOK);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDOK_permanent);
    else  --  Hide the closing button
      Hide (Window.IDOK);
    end if;
    Dlg_to_Scn ( 413, 166, 50, 16, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDCANCEL, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    Create (Window.IDCANCEL_permanent, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDCANCEL_permanent);
    else  --  Hide the closing button
      Hide (Window.IDCANCEL);
    end if;
  end Create_Contents;  --  HAC_example_box_Type

  --  Dialog at resource line 260

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Option_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Options";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x, y, w, h: Integer;
  begin
    Dlg_to_Scn ( 0, 0, 253, 168, x,y,w,h);
    if Left   /= Use_Default then x := Left;   end if;
    if Top    /= Use_Default then y := Top;    end if;
    if Width  /= Use_Default then w := Width;  end if;
    if Height /= Use_Default then h := Height; end if;
    Create_As_Dialog(
      Window => Window_Type(Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then Client_Area_Width(Window, w); end if;
    if Height = Use_Default then Client_Area_Height(Window, h); end if;
    Use_GUI_Font(Window);
    Create_Contents(Window, True);
  end Create_Full_Dialog; -- Option_box_Type

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Option_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionally resize Window as designed
     )
  is
    x,y,w,h : Integer;
  begin
    if resize then
    Dlg_to_Scn ( 0, 0, 253, 168, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn ( 7, 10, 50, 15, x,y,w,h);
    Create_Label (Window, "Indentation", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 49, 9, 16, 15, x,y,w,h);
    Create (Window.Indentation_edit_box, Window, "", x,y,w,h, Horizontal_Scroll => True, Read_Only => False, ID => Indentation_edit_box);
    Dlg_to_Scn ( 7, 31, 110, 9, x,y,w,h);
    Create_Label (Window, "Tab width (symbol AND keystroke)", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 121, 30, 16, 15, x,y,w,h);
    Create (Window.Tab_width_edit_box, Window, "", x,y,w,h, Horizontal_Scroll => True, Read_Only => False, ID => Tab_width_edit_box);
    Dlg_to_Scn ( 159, 10, 52, 15, x,y,w,h);
    Create_Label (Window, "Right margin", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 213, 9, 31, 15, x,y,w,h);
    Create (Window.Right_margin_edit_box, Window, "", x,y,w,h, Horizontal_Scroll => True, Read_Only => False, ID => Right_margin_edit_box);
    Dlg_to_Scn ( 7, 52, 63, 9, x,y,w,h);
    Create_Label (Window, "Ada file extensions", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 83, 50, 162, 15, x,y,w,h);
    Create (Window.Ada_file_extension_edit_box, Window, "", x,y,w,h, Horizontal_Scroll => True, Read_Only => False, ID => Ada_file_extension_edit_box);
    Dlg_to_Scn ( 5, 99, 240, 28, x,y,w,h);
    Create( Window.Group_color_theme, Window, "Color theme", x, y, w, h);
    Dlg_to_Scn ( 60, 110, 108, 20, x,y,w,h);
    Create (Window.Color_theme_list_box, Window, x,y,w,h, False, ID => Color_theme_list_box);
    Dlg_to_Scn ( 5, 68, 240, 28, x,y,w,h);
    Create( Window.Group_Backup, Window, "Backup", x, y, w, h);
    Dlg_to_Scn ( 79, 78, 91, 11, x,y,w,h);
    Create (Window.Backup_bak_button, Window, "Simple (.bak)", x,y,w,h, ID => Backup_bak_button);
    Dlg_to_Scn ( 14, 80, 43, 8, x,y,w,h);
    Create (Window.Backup_none_button, Window, "None", x,y,w,h, ID => Backup_none_button);
    Dlg_to_Scn ( 190, 140, 50, 19, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDCANCEL, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    Create (Window.IDCANCEL_permanent, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDCANCEL_permanent);
    else  --  Hide the closing button
      Hide (Window.IDCANCEL);
    end if;
    Dlg_to_Scn ( 135, 140, 50, 19, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDOK, Window, "OK", x,y,w,h, ID => IDOK);
    Create (Window.IDOK_permanent, Window, "OK", x,y,w,h, ID => IDOK);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDOK_permanent);
    else  --  Hide the closing button
      Hide (Window.IDOK);
    end if;
  end Create_Contents;  --  Option_box_Type

  --  Dialog at resource line 285

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Progress_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned)
  is
    pragma Unmodified (Window);
    pragma Unmodified (dwExStyle);
    WS_SYSMENU : constant := 16#0008_0000#;
  begin
    dwStyle:= dwStyle and not WS_SYSMENU;
  end On_Pre_Create;

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Progress_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "HAC Virtual Machine is running...";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x, y, w, h: Integer;
  begin
    Dlg_to_Scn ( 0, 0, 177, 65, x,y,w,h);
    if Left   /= Use_Default then x := Left;   end if;
    if Top    /= Use_Default then y := Top;    end if;
    if Width  /= Use_Default then w := Width;  end if;
    if Height /= Use_Default then h := Height; end if;
    Create_As_Dialog(
      Window => Window_Type(Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then Client_Area_Width(Window, w); end if;
    if Height = Use_Default then Client_Area_Height(Window, h); end if;
    Use_GUI_Font(Window);
    Create_Contents(Window, True);
  end Create_Full_Dialog; -- Progress_box_Type

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Progress_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionally resize Window as designed
     )
  is
    x,y,w,h : Integer;
  begin
    if resize then
    Dlg_to_Scn ( 0, 0, 177, 65, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn ( 109, 7, 64, 55, x,y,w,h);
    Create( Window.Group_Stack, Window, "Stack usage", x, y, w, h);
    Dlg_to_Scn ( 124, 20, 36, 30, x,y,w,h);
    Create (Window.Stack_Bar, Window, x,y,w,h, Vertical, True);
    Dlg_to_Scn ( 3, 7, 99, 55, x,y,w,h);
    Create( Window.Group_VM_Inter, Window, "Interpreter control", x, y, w, h);
    Dlg_to_Scn ( 16, 26, 70, 19, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.Stop_VM_Button, Window, "Stop", x,y,w,h, ID => Stop_VM_Button);
    Create (Window.Stop_VM_Button_permanent, Window, "Stop", x,y,w,h, ID => Stop_VM_Button);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.Stop_VM_Button_permanent);
    else  --  Hide the closing button
      Hide (Window.Stop_VM_Button);
    end if;
  end Create_Contents;  --  Progress_box_Type

  --  Dialog at resource line 300

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Search_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Search";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x, y, w, h: Integer;
  begin
    Dlg_to_Scn ( 0, 0, 235, 164, x,y,w,h);
    if Left   /= Use_Default then x := Left;   end if;
    if Top    /= Use_Default then y := Top;    end if;
    if Width  /= Use_Default then w := Width;  end if;
    if Height /= Use_Default then h := Height; end if;
    Create_As_Dialog(
      Window => Window_Type(Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then Client_Area_Width(Window, w); end if;
    if Height = Use_Default then Client_Area_Height(Window, h); end if;
    Use_GUI_Font(Window);
    Create_Contents(Window, True);
  end Create_Full_Dialog; -- Search_box_Type

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Search_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionally resize Window as designed
     )
  is
    x,y,w,h : Integer;
  begin
    if resize then
    Dlg_to_Scn ( 0, 0, 235, 164, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn ( 11, 10, 50, 15, x,y,w,h);
    Create_Label (Window, "Find", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 63, 10, 160, 15, x,y,w,h);
    Create (Window.Model_find_box, Window, "", x,y,w,h, False, ID => Model_find_box);
    Dlg_to_Scn ( 11, 30, 50, 15, x,y,w,h);
    Create_Label (Window, "Replace with", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 63, 30, 160, 15, x,y,w,h);
    Create (Window.Model_replace_box, Window, "", x,y,w,h, False, ID => Model_replace_box);
    Dlg_to_Scn ( 149, 143, 74, 17, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.Close_search_box, Window, "Close", x,y,w,h, ID => Close_search_box);
    Create (Window.Close_search_box_permanent, Window, "Close", x,y,w,h, ID => Close_search_box);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.Close_search_box_permanent);
    else  --  Hide the closing button
      Hide (Window.Close_search_box);
    end if;
    Dlg_to_Scn ( 11, 53, 58, 18, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.Find_next_button, Window, "Find next", x,y,w,h, ID => Find_next_button);
    Create (Window.Find_next_button_permanent, Window, "Find next", x,y,w,h, ID => Find_next_button);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.Find_next_button_permanent);
    else  --  Hide the closing button
      Hide (Window.Find_next_button);
    end if;
    Dlg_to_Scn ( 82, 53, 58, 18, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.Find_previous_button, Window, "Find previous", x,y,w,h, ID => Find_previous_button);
    Create (Window.Find_previous_button_permanent, Window, "Find previous", x,y,w,h, ID => Find_previous_button);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.Find_previous_button_permanent);
    else  --  Hide the closing button
      Hide (Window.Find_previous_button);
    end if;
    Dlg_to_Scn ( 165, 53, 58, 18, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.Find_all_button, Window, "Find all", x,y,w,h, ID => Find_all_button);
    Create (Window.Find_all_button_permanent, Window, "Find all", x,y,w,h, ID => Find_all_button);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.Find_all_button_permanent);
    else  --  Hide the closing button
      Hide (Window.Find_all_button);
    end if;
    Dlg_to_Scn ( 20, 82, 110, 18, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.Replace_and_find_next_button, Window, "Replace and find next", x,y,w,h, ID => Replace_and_find_next_button);
    Create (Window.Replace_and_find_next_button_permanent, Window, "Replace and find next", x,y,w,h, ID => Replace_and_find_next_button);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.Replace_and_find_next_button_permanent);
    else  --  Hide the closing button
      Hide (Window.Replace_and_find_next_button);
    end if;
    Dlg_to_Scn ( 165, 82, 58, 18, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.Replace_all_button, Window, "Replace all", x,y,w,h, ID => Replace_all_button);
    Create (Window.Replace_all_button_permanent, Window, "Replace all", x,y,w,h, ID => Replace_all_button);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.Replace_all_button_permanent);
    else  --  Hide the closing button
      Hide (Window.Replace_all_button);
    end if;
    Dlg_to_Scn ( 11, 112, 81, 11, x,y,w,h);
    Create (Window.Whole_word, Window, "Whole word", x,y,w,h, ID => Whole_word);
    Dlg_to_Scn ( 11, 129, 81, 11, x,y,w,h);
    Create (Window.Match_case, Window, "Match case", x,y,w,h, ID => Match_case);
  end Create_Contents;  --  Search_box_Type

  --  Dialog at resource line 322

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out String_Prompt_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned)
  is
    pragma Unmodified (Window);
    pragma Unmodified (dwExStyle);
    WS_SYSMENU : constant := 16#0008_0000#;
  begin
    dwStyle:= dwStyle and not WS_SYSMENU;
  end On_Pre_Create;

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out String_Prompt_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "String_Prompt Title";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x, y, w, h: Integer;
  begin
    Dlg_to_Scn ( 0, 0, 378, 76, x,y,w,h);
    if Left   /= Use_Default then x := Left;   end if;
    if Top    /= Use_Default then y := Top;    end if;
    if Width  /= Use_Default then w := Width;  end if;
    if Height /= Use_Default then h := Height; end if;
    Create_As_Dialog(
      Window => Window_Type(Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then Client_Area_Width(Window, w); end if;
    if Height = Use_Default then Client_Area_Height(Window, h); end if;
    Use_GUI_Font(Window);
    Create_Contents(Window, True);
  end Create_Full_Dialog; -- String_Prompt_Type

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out String_Prompt_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionally resize Window as designed
     )
  is
    x,y,w,h : Integer;
  begin
    if resize then
    Dlg_to_Scn ( 0, 0, 378, 76, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn ( 7, 9, 368, 9, x,y,w,h);
    Create (Window.String_Prompt_Label, Window, "Enter some text...", x,y,w,h, GWindows.Static_Controls.Left, None, ID => String_Prompt_Label);
    Dlg_to_Scn ( 7, 28, 362, 21, x,y,w,h);
    Create (Window.String_Prompt_Edit_Box, Window, "", x,y,w,h, Horizontal_Scroll => True, Read_Only => False, ID => String_Prompt_Edit_Box);
    Dlg_to_Scn ( 235, 54, 62, 18, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDOK, Window, "OK", x,y,w,h, ID => IDOK);
    Create (Window.IDOK_permanent, Window, "OK", x,y,w,h, ID => IDOK);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDOK_permanent);
    else  --  Hide the closing button
      Hide (Window.IDOK);
    end if;
    Dlg_to_Scn ( 307, 54, 62, 18, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDCANCEL, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    Create (Window.IDCANCEL_permanent, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDCANCEL_permanent);
    else  --  Hide the closing button
      Hide (Window.IDCANCEL);
    end if;
  end Create_Contents;  --  String_Prompt_Type

  --  ** Generated code ends here /\ /\ /\.

  --  ** Some helper utilities (body).

  procedure Dlg_to_Scn (  --  Converts dialog coords to screen (pixel) coordinates.
    xd,yd,wd,hd :  in Integer;
    xs,ys,ws,hs : out Integer)
  is
    --  function GetDialogBaseUnits return Integer;
    --  pragma Import (StdCall, GetDialogBaseUnits, "GetDialogBaseUnits");
    --  baseunit, baseunitX, baseunitY: Integer;
    baseunitX: constant:= 6;
    baseunitY: constant:= 13;
  begin
    --  baseunit := GetDialogBaseUnits; -- this gives X=8, Y=16 (SYSTEM font)
    --  baseunitX := baseunit mod (2 ** 16);
    --  baseunitY := baseunit  / (2 ** 16);
    --  NB: the other way with MapDialogRect works only
    --    by full moon, hence the user-defined units.
    xs := (xd * baseunitX) / 4;
    ws := (wd * baseunitX) / 4;
    ys := (yd * baseunitY) / 8;
    hs := (hd * baseunitY) / 8;
  end Dlg_to_Scn;

  package Common_Fonts is
    GUI_Font : GWindows.Drawing_Objects.Font_Type;
    URL_Font : GWindows.Drawing_Objects.Font_Type;
    --  ^ These fonts are created once, at startup
    --    it avoid GUI resource leak under Windows 95/98/ME
    procedure Create_Common_Fonts;
    --  in initialisation part if this pkg becomes standalone
  end Common_Fonts;

  procedure Use_GUI_Font (Window: in out GWindows.Base.Base_Window_Type'Class)
  is
  begin
    --  Use Standard Windows GUI font instead of system font
    GWindows.Base.Set_Font (Window, Common_Fonts.GUI_Font);
  end Use_GUI_Font;

  function Num_resource (id: Natural) return GString is
    img: constant String:= Integer'Image(id);
  begin
    return To_GString_From_String('#' & img(img'First+1..img'Last));
  end Num_resource;

  package body Common_Fonts is

    procedure Create_Common_Fonts is

     type Face_Name_Type is array (1 .. 32) of GWindows.GChar_C;

     type LOGFONT is record
       lfHeight : Interfaces.C.long;
       lfWidth  : Interfaces.C.long;
       lfEscapement  : Interfaces.C.long;
       lfOrientation : Interfaces.C.long;
       lfWeight: Interfaces.C.long;
       lfItalic: Interfaces.C.char;
       lfUnderline: Interfaces.C.char;
       lfStrikeOut: Interfaces.C.char;
       lfCharSet: Interfaces.C.char;
       lfOutPrecision: Interfaces.C.char;
       lfClipPrecision: Interfaces.C.char;
       lfQuality: Interfaces.C.char;
       lfPitchAndFamily: Interfaces.C.char;
       lfFaceName: Face_Name_Type;
     end record;

     Log_of_current_font : aliased LOGFONT;

     subtype PVOID   is System.Address;                      --  winnt.h
     subtype LPVOID  is PVOID;                               --  windef.h

     function GetObject
       (hgdiobj  : GWindows.Types.Handle  := GWindows.Drawing_Objects.Handle(GUI_Font);
        cbBufferl: Interfaces.C.int       := LOGFONT'Size / 8;
        lpvObject: LPVOID                 := Log_of_current_font'Address)
       return Interfaces.C.int;
     pragma Import (StdCall, GetObject,
                      "GetObject" & Character_Mode_Identifier);

     function CreateFontIndirect
       (lpvObject: LPVOID                 := Log_of_current_font'Address)
       return GWindows.Types.Handle;
     pragma Import (StdCall, CreateFontIndirect,
                      "CreateFontIndirect" & Character_Mode_Identifier);

    begin
      GWindows.Drawing_Objects.Create_Stock_Font(
        GUI_Font,
        GWindows.Drawing_Objects.Default_GUI
      );
      if GetObject = 0 then
        GWindows.Drawing_Objects.Create_Font (URL_Font,
          "MS Sans Serif",
          14, Underline => True);
            --  !! ^ Not so nice (non-unsharpened font, size ~..., color ?)
      else
        Log_of_current_font.lfUnderline := Interfaces.C.char'Val(1);
        GWindows.Drawing_Objects.Handle (URL_Font, CreateFontIndirect);
      end if;
    end Create_Common_Fonts;

  end Common_Fonts;

begin
  Common_Fonts.Create_Common_Fonts;

  --  Last line of resource script file: 435

end LEA_Resource_GUI;
