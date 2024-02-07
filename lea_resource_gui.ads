---------------------------------------------------------------------------
--  GUI contents of resource script file: LEA.rc
--  Transcription time: 2024/02/07  21:12:21
--  GWenerator project file: lea.gwen
--
--  Translated by the RC2GW or by the GWenerator tool.
--  URL: http://sf.net/projects/gnavi
--
--  This file contains only automatically generated code. Do not edit this.
--  Rework the resource script instead, and re-run the translator.
--  RC Grammar version: 29-Jul-2022
---------------------------------------------------------------------------

with GWindows.Base;                     use GWindows.Base;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Buttons.Graphic;          use GWindows.Buttons.Graphic;
with GWindows.Buttons.Owner_Drawn;      use GWindows.Buttons.Owner_Drawn;
with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;
with GWindows.List_Boxes;               use GWindows.List_Boxes;
with GWindows.Combo_Boxes;              use GWindows.Combo_Boxes;
with GWindows.Static_Controls;          use GWindows.Static_Controls;
with GWindows.Scroll_Bars;              use GWindows.Scroll_Bars;
with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.Menus;                    use GWindows.Menus;
use GWindows;
with Interfaces.C;                      use Interfaces.C;

pragma Warnings ("U");  --  turn off warnings for unused entity

package LEA_Resource_GUI is

  type Menu_Fake_Type is tagged record
    Main : Menu_Type;  --  Root of the whole menu tree
    Popup_0001 : Menu_Type;   --  Popup level: 1; title: "Fake_Menu"
  end record;  --  Menu_Fake_Type

  --  Menu at line 36
  procedure Create_Full_Menu (New_Menu : in out Menu_Fake_Type);

  type Menu_MDI_Child_Type is tagged record
    Main : Menu_Type;  --  Root of the whole menu tree
    Popup_0001 : Menu_Type;   --  Popup level: 1; title: "&File"
    Popup_0002 : Menu_Type;   --  Popup level: 2; title: "Open &recent"
    Popup_0003 : Menu_Type;   --  Popup level: 1; title: "&Edit"
    Popup_0004 : Menu_Type;   --  Popup level: 1; title: "&Navigate"
    Popup_0005 : Menu_Type;   --  Popup level: 1; title: "&Actions"
    Popup_0006 : Menu_Type;   --  Popup level: 1; title: "&View"
    Popup_0007 : Menu_Type;   --  Popup level: 1; title: "&Options"
    Popup_0008 : Menu_Type;   --  Popup level: 1; title: "&Window"
    Popup_0009 : Menu_Type;   --  Popup level: 1; title: "&Help"
  end record;  --  Menu_MDI_Child_Type

  --  Menu at line 138
  procedure Create_Full_Menu (New_Menu : in out Menu_MDI_Child_Type);

  type Menu_MDI_Main_Type is tagged record
    Main : Menu_Type;  --  Root of the whole menu tree
    Popup_0001 : Menu_Type;   --  Popup level: 1; title: "&File"
    Popup_0002 : Menu_Type;   --  Popup level: 2; title: "Open &recent"
    Popup_0003 : Menu_Type;   --  Popup level: 1; title: "&Edit"
    Popup_0004 : Menu_Type;   --  Popup level: 1; title: "&Navigate"
    Popup_0005 : Menu_Type;   --  Popup level: 1; title: "&Actions"
    Popup_0006 : Menu_Type;   --  Popup level: 1; title: "&View"
    Popup_0007 : Menu_Type;   --  Popup level: 1; title: "&Options"
    Popup_0008 : Menu_Type;   --  Popup level: 1; title: "&Window"
    Popup_0009 : Menu_Type;   --  Popup level: 1; title: "&Help"
  end record;  --  Menu_MDI_Main_Type

  --  Menu at line 232
  procedure Create_Full_Menu (New_Menu : in out Menu_MDI_Main_Type);

  type About_box_Type is new Window_Type with record

    Static_0001 : Icon_Type;
    --  Label: IDC_STATIC
    Copyright_label : Label_Type;
    --  Label: IDC_STATIC
    --  Label: IDC_STATIC
    LEA_URL : Label_Type;
    --  Label: IDC_STATIC
    Version_label : Label_Type;
    Static_0006 : Group_Box_Type;
    GNAT_URL : Label_Type;
    GNAT_Version : Label_Type;
    HAC_URL : Label_Type;
    HAC_Version : Label_Type;
    GNAVI_URL : Label_Type;
    ZipAda_URL : Label_Type;
    ZipAda_Version : Label_Type;
    ResEdit_URL : Label_Type;
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
    Credits_Button : Dialog_Button_Type;    --  Closes parent window after click
    Credits_Button_permanent : Button_Type;  --  Doesn't close parent window after click
  end record; -- About_box_Type

  --  Dialog at resource line 264

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out About_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                            dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out About_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "About LEA";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out About_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Credits_Box_Type is new Window_Type with record

    Static_0001 : Group_Box_Type;
    --  Label: IDC_STATIC
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
  end record; -- Credits_Box_Type

  --  Dialog at resource line 275

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Credits_Box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Credits";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Credits_Box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Go_to_line_box_Type is new Window_Type with record

    Line_value_box : Edit_Box_Type;
    IDCANCEL : Dialog_Button_Type;    --  Closes parent window after click
    IDCANCEL_permanent : Button_Type;  --  Doesn't close parent window after click
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
  end record; -- Go_to_line_box_Type

  --  Dialog at resource line 288

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Go_to_line_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                            dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Go_to_line_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Go to line...";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Go_to_line_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type HAC_example_box_Type is new Window_Type with record

    Label_HAC_topic : Label_Type;
    Label_HAC_sample : Label_Type;
    Topic_box : List_Box_Type;
    Zipped_file_box : List_View_Control_Type;
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
    IDCANCEL : Dialog_Button_Type;    --  Closes parent window after click
    IDCANCEL_permanent : Button_Type;  --  Doesn't close parent window after click
  end record; -- HAC_example_box_Type

  --  Dialog at resource line 304

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out HAC_example_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                            dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out HAC_example_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "HAC Ada source code samples";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out HAC_example_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Option_box_Type is new Window_Type with record

    --  Label: 0
    Indentation_edit_box : Edit_Box_Type;
    --  Label: 0
    Right_margin_edit_box : Edit_Box_Type;
    --  Label: 0
    Tab_width_edit_box : Edit_Box_Type;
    Auto_Insert_Check_Box : Check_Box_Type;
    Smart_Editor_Check_Box : Check_Box_Type;
    --  Label: 0
    Ada_file_extension_edit_box : Edit_Box_Type;
    Group_color_theme : Group_Box_Type;
    Color_theme_list_box : Drop_Down_List_Box_Type;
    Group_Backup : Group_Box_Type;
    Backup_bak_button : Radio_Button_Type;
    Backup_none_button : Radio_Button_Type;
    IDCANCEL : Dialog_Button_Type;    --  Closes parent window after click
    IDCANCEL_permanent : Button_Type;  --  Doesn't close parent window after click
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
  end record; -- Option_box_Type

  --  Dialog at resource line 331

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Option_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Options";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Option_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Progress_box_Type is new Window_Type with record

    Group_Stack : Group_Box_Type;
    Stack_Bar : Progress_Control_Type;
    Group_VM_Inter : Group_Box_Type;
    Stop_VM_Button : Dialog_Button_Type;    --  Closes parent window after click
    Stop_VM_Button_permanent : Button_Type;  --  Doesn't close parent window after click
  end record; -- Progress_box_Type

  --  Dialog at resource line 345

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out Progress_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                            dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Progress_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "HAC Virtual Machine is running...";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Progress_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Reload_Files_Box_Type is new Window_Type with record

    --  Label: 0
    Select_All_Button : Dialog_Button_Type;    --  Closes parent window after click
    Select_All_Button_permanent : Button_Type;  --  Doesn't close parent window after click
    Unselect_All_Button : Dialog_Button_Type;    --  Closes parent window after click
    Unselect_All_Button_permanent : Button_Type;  --  Doesn't close parent window after click
    Changed_Files_List : List_View_Control_Type;
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
  end record; -- Reload_Files_Box_Type

  --  Dialog at resource line 361

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Reload_Files_Box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Files externally changed";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Reload_Files_Box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type Search_box_Type is new Window_Type with record

    --  Label: 0
    Model_find_box : Drop_Down_Combo_Box_Type;
    --  Label: 0
    Model_replace_box : Drop_Down_Combo_Box_Type;
    Close_search_box : Dialog_Button_Type;    --  Closes parent window after click
    Close_search_box_permanent : Button_Type;  --  Doesn't close parent window after click
    Find_next_button : Default_Dialog_Button_Type;    --  Closes parent window after click
    Find_next_button_permanent : Default_Button_Type;  --  Doesn't close parent window after click
    Find_previous_button : Dialog_Button_Type;    --  Closes parent window after click
    Find_previous_button_permanent : Button_Type;  --  Doesn't close parent window after click
    Find_all_button : Dialog_Button_Type;    --  Closes parent window after click
    Find_all_button_permanent : Button_Type;  --  Doesn't close parent window after click
    Replace_and_find_next_button : Dialog_Button_Type;    --  Closes parent window after click
    Replace_and_find_next_button_permanent : Button_Type;  --  Doesn't close parent window after click
    Replace_all_button : Dialog_Button_Type;    --  Closes parent window after click
    Replace_all_button_permanent : Button_Type;  --  Doesn't close parent window after click
    Whole_word : Check_Box_Type;
    Match_case : Check_Box_Type;
  end record; -- Search_box_Type

  --  Dialog at resource line 384

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Search_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Search";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Search_box_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  type String_Prompt_Type is new Window_Type with record

    String_Prompt_Label : Label_Type;
    String_Prompt_Edit_Box : Edit_Box_Type;
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
    IDCANCEL : Dialog_Button_Type;    --  Closes parent window after click
    IDCANCEL_permanent : Button_Type;  --  Doesn't close parent window after click
  end record; -- String_Prompt_Type

  --  Dialog at resource line 398

  --  Pre-Create operation to switch off default styles, or
  --  add ones that are not in usual GWindows Create parameters.
  --
  procedure On_Pre_Create (Window    : in out String_Prompt_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                            dwExStyle : in out Interfaces.C.unsigned);

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out String_Prompt_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "String_Prompt Title";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out String_Prompt_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );

  package Version_info is
    Authors : constant String := "Gautier de Montmollin";
    FileDescription : constant String := "LEA - a Lightweight Editor for Ada - Free, MIT license";
    FileVersion : constant String := "0.90";
    LegalCopyright : constant String := "Copyright © Gautier de Montmollin 2017 .. 2024";
    ProductName : constant String := "LEA";
    Translation : constant := 1033;
  end Version_info;

  --------------------------------------------------
  --  Defined resource symbols --> Ada constants  --
  --------------------------------------------------

  --  NB: only items with a defined symbol get a constant here
  --  These constants are needed for getting button and menu feedbacks.

  IDC_STATIC                      : constant :=     -1;
  Menu_MDI_Main                   : constant :=    102;
  Menu_MDI_Child                  : constant :=    104;
  LEA_Icon                        : constant :=    114;
  Toolbar_BMP                     : constant :=    123;
  Folders_BMP                     : constant :=    124;
  Menu_Fake                       : constant :=    126;
  Backup_none_button              : constant :=  40000;
  IDM_Go_to_line                  : constant :=  40001;
  IDM_Open_Project                : constant :=  40002;
  IDM_Previous_bookmark           : constant :=  40003;
  IDM_Toggle_bookmark             : constant :=  40004;
  IDM_Copy                        : constant :=  40005;
  IDM_Cut                         : constant :=  40006;
  IDM_Find_Next                   : constant :=  40007;
  IDM_Paste                       : constant :=  40008;
  IDM_Duplicate                   : constant :=  40009;
  IDM_New_File                    : constant :=  40011;
  IDM_Build_and_run               : constant :=  40012;
  IDM_Build                       : constant :=  40013;
  IDM_Compile_single              : constant :=  40014;
  IDM_ABOUT                       : constant :=  40015;
  IDM_Open_File                   : constant :=  40016;
  IDM_HAC_Mode                    : constant :=  40017;
  IDM_GNAT_Mode                   : constant :=  40018;
  IDM_Studio_view                 : constant :=  40019;
  IDM_Notepad_view                : constant :=  40020;
  IDM_Copy_Messages               : constant :=  40021;
  Replace_all_button              : constant :=  40021;
  Close_search_box                : constant :=  40022;
  IDM_Undo                        : constant :=  40023;
  Version_label                   : constant :=  40024;
  Whole_word                      : constant :=  40025;
  Backup_bak_button               : constant :=  40026;
  GNAT_URL                        : constant :=  40028;
  IDM_Save_All                    : constant :=  40029;
  IDM_Unselect_all                : constant :=  40030;
  GNAT_Version                    : constant :=  40031;
  IDM_Save_File                   : constant :=  40032;
  Right_margin_edit_box           : constant :=  40033;
  GNAVI_URL                       : constant :=  40034;
  Group_color_theme               : constant :=  40035;
  IDM_Redo                        : constant :=  40036;
  IDM_TEST_ARCHIVE                : constant :=  40038;
  ResEdit_URL                     : constant :=  40039;
  IDM_Save_As                     : constant :=  40040;
  IDM_QUIT                        : constant :=  40041;
  IDM_Close                       : constant :=  40042;
  IDM_RECOMPRESS_ARCHIVE          : constant :=  40043;
  IDM_EXTRACT                     : constant :=  40044;
  IDM_General_options             : constant :=  40045;
  IDM_Find                        : constant :=  40046;
  IDM_MRU_2                       : constant :=  40049;
  IDM_MRU_3                       : constant :=  40050;
  IDM_MRU_4                       : constant :=  40051;
  IDM_MRU_5                       : constant :=  40052;
  IDM_MRU_6                       : constant :=  40053;
  IDM_MRU_7                       : constant :=  40054;
  IDM_MRU_8                       : constant :=  40055;
  IDM_MRU_9                       : constant :=  40056;
  IDM_WINDOW_CASCADE              : constant :=  40057;
  IDM_WINDOW_TILE_HORIZONTAL      : constant :=  40058;
  IDM_WINDOW_TILE_VERTICAL        : constant :=  40059;
  IDM_WINDOW_CLOSE_ALL            : constant :=  40060;
  IDM_UPDATE_ARCHIVE              : constant :=  40061;
  IDM_Quick_Help                  : constant :=  40062;
  IDM_Web                         : constant :=  40063;
  IDM_Select_all                  : constant :=  40064;
  IDM_MRU_1                       : constant :=  40065;
  IDM_Indent                      : constant :=  40066;
  IDM_Unindent                    : constant :=  40067;
  IDM_Comment                     : constant :=  40068;
  IDM_Uncomment                   : constant :=  40069;
  IDM_Show_special_symbols        : constant :=  40070;
  IDM_Find_Previous               : constant :=  40071;
  ZipAda_URL                      : constant :=  40073;
  ZipAda_Version                  : constant :=  40074;
  HAC_Version                     : constant :=  40075;
  Tab_width_edit_box              : constant :=  40076;
  HAC_URL                         : constant :=  40077;
  IDM_Next_bookmark               : constant :=  40078;
  Model_find_box                  : constant :=  40079;
  Model_replace_box               : constant :=  40080;
  Line_value_box                  : constant :=  40081;
  Match_case                      : constant :=  40082;
  Copyright_label                 : constant :=  40083;
  Group_Backup                    : constant :=  40084;
  Find_next_button                : constant :=  40085;
  Find_previous_button            : constant :=  40086;
  LEA_URL                         : constant :=  40087;
  Find_all_button                 : constant :=  40088;
  Replace_and_find_next_button    : constant :=  40089;
  Color_theme_list_box            : constant :=  40090;
  Indentation_edit_box            : constant :=  40091;
  Ada_file_extension_edit_box     : constant :=  40092;
  String_Prompt_Edit_Box          : constant :=  40093;
  String_Prompt_Label             : constant :=  40094;
  Group_VM_Inter                  : constant :=  40095;
  Group_Stack                     : constant :=  40096;
  Stack_Bar                       : constant :=  40098;
  Stop_VM_Button                  : constant :=  40099;
  IDM_Ada_Sample                  : constant :=  40100;
  Zipped_file_box                 : constant :=  40101;
  Topic_box                       : constant :=  40103;
  Label_HAC_topic                 : constant :=  40104;
  Label_HAC_sample                : constant :=  40105;
  IDM_Show_indentation_lines      : constant :=  40106;
  IDM_Open_Containing_Folder      : constant :=  40107;
  Auto_Insert_Check_Box           : constant :=  40108;
  Select_All_Button               : constant :=  40109;
  Unselect_All_Button             : constant :=  40110;
  Changed_Files_List              : constant :=  40111;
  IDM_Go_to_memorized_Declaration : constant :=  40112;
  IDM_Go_to_memorized_Body        : constant :=  40113;
  IDM_Go_to_other_File            : constant :=  40114;
  Smart_Editor_Check_Box          : constant :=  40115;
  Credits_Box                     : constant :=  40116;
  Credits_Button                  : constant :=  40117;

  --  ** Some helper utilities (spec).

  procedure Dlg_to_Scn
    (xd, yd, wd, hd :  in Integer;
     xs, ys, ws, hs : out Integer);

  procedure Use_GUI_Font (Window : in out GWindows.Base.Base_Window_Type'Class);

  function Num_resource (id : Natural) return GString;  --  Just turn 123 into "#123".

  --  Last line of resource script file: 506

end LEA_Resource_GUI;
