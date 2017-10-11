---------------------------------------------------------------------------
-- GUI contents of resource script file: LEA.rc
-- Transcription time: 2017/10/11  17:39:34
-- GWenerator project file: lea.gwen
--
-- Translated by the RC2GW or by the GWenerator tool.
-- URL: http://sf.net/projects/gnavi
--
-- This file contains only automatically generated code. Do not edit this.
-- Rework the resource script instead, and re-run the translator.
-- RC Grammar version: >= 19-May-2016
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

package LEA_Resource_GUI is

  type Menu_MDI_Child_Type is tagged record
    Main: Menu_Type; -- Root of the whole menu tree
    Popup_0001: Menu_Type;  -- level 1; title: "&File"
    Popup_0002: Menu_Type;  -- level 2; title: "&Recent"
    Popup_0003: Menu_Type;  -- level 1; title: "&Edit"
    Popup_0004: Menu_Type;  -- level 1; title: "&Tools"
    Popup_0005: Menu_Type;  -- level 1; title: "&View"
    Popup_0006: Menu_Type;  -- level 1; title: "&Options"
    Popup_0007: Menu_Type;  -- level 1; title: "&Window"
    Popup_0008: Menu_Type;  -- level 1; title: "&Help"
  end record;  --  Menu_MDI_Child_Type

  -- Menu at line 90
  procedure Create_Full_Menu
     (Menu        : in out Menu_MDI_Child_Type);

  type Menu_MDI_Main_Type is tagged record
    Main: Menu_Type; -- Root of the whole menu tree
    Popup_0001: Menu_Type;  -- level 1; title: "&File"
    Popup_0002: Menu_Type;  -- level 2; title: "&Recent"
    Popup_0003: Menu_Type;  -- level 1; title: "&Options"
    Popup_0004: Menu_Type;  -- level 1; title: "&Window"
    Popup_0005: Menu_Type;  -- level 1; title: "&Help"
  end record;  --  Menu_MDI_Main_Type

  -- Menu at line 135
  procedure Create_Full_Menu
     (Menu        : in out Menu_MDI_Main_Type);

  type About_box_Type is new Window_Type with record

    Static_0001: Icon_Type;
    -- Label: IDC_STATIC
    Copyright_label: Label_Type;
    -- Label: IDC_STATIC
    -- Label: IDC_STATIC
    AZip_URL: Label_Type;
    -- Label: IDC_STATIC
    Version_label: Label_Type;
    Static_0006: Group_Box_Type;
    GNAT_URL: Label_Type;
    GNAT_Version: Label_Type;
    GNAVI_URL: Label_Type;
    ResEdit_URL: Label_Type;
    IDOK: Default_Dialog_Button_Type;    -- closes parent window after click
    IDOK_permanent: Default_Button_Type; -- doesn't close parent window after click
  end record; -- About_box_Type

  -- Dialog at resource line 162

  -- Pre-Create operation to switch off default styles
  -- or add ones that are not in usual GWindows Create parameters
  --
  procedure On_Pre_Create (Window    : in out About_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
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
      Is_Dynamic  : in     Boolean := False);

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out About_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionally resize Window as designed
     );

  type Option_box_Type is new Window_Type with record

    -- Label: 0
    Indentation_edit_box: Edit_Box_Type;
    -- Label: 0
    Right_margin_edit_box: Edit_Box_Type;
    Group_color_theme: Group_Box_Type;
    Color_theme_list_box: Drop_Down_Combo_Box_Type;
    Group_Backup: Group_Box_Type;
    Backup_bak_button: Radio_Button_Type;
    Backup_none_button: Radio_Button_Type;
    IDCANCEL: Dialog_Button_Type;    -- closes parent window after click
    IDCANCEL_permanent: Button_Type; -- doesn't close parent window after click
    IDOK: Default_Dialog_Button_Type;    -- closes parent window after click
    IDOK_permanent: Default_Button_Type; -- doesn't close parent window after click
  end record; -- Option_box_Type

  -- Dialog at resource line 183

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
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
      Is_Dynamic  : in     Boolean := False);

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Option_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionally resize Window as designed
     );

  package Version_info is
    Authors: constant String:= "Gautier de Montmollin";
    FileDescription: constant String:= "LEA - a Lightweight Editor for Ada - Free, MIT license";
    FileVersion: constant String:= "0.0";
    LegalCopyright: constant String:= "Copyright © Gautier de Montmollin 2017 .. 2017";
    ProductName: constant String:= "LEA";
    Translation: constant:= 1033;
  end Version_info;

  ------------------------------------------------
  -- Defined resource symbols --> Ada constants --
  ------------------------------------------------

  -- NB: only items with a defined symbol get a constant here
  -- These constants are needed for getting button and menu feedbacks.

  IDC_STATIC                : constant:=     -1;
  Menu_MDI_Main             : constant:=    102;
  Menu_MDI_Child            : constant:=    104;
  LEA_Doc_Icon              : constant:=    112;
  LEA_Icon                  : constant:=    114;
  Toolbar_BMP               : constant:=    123;
  Folders_BMP               : constant:=    124;
  Binoculars_Icon           : constant:=    132;
  Backup_none_button        : constant:=  40000;
  Copyright_label           : constant:=  40000;
  Group_Backup              : constant:=  40000;
  IDM_New_File              : constant:=  40000;
  AZip_URL                  : constant:=  40001;
  IDM_Open_File             : constant:=  40001;
  Indentation_edit_box      : constant:=  40001;
  Color_theme_list_box      : constant:=  40002;
  IDM_Undo                  : constant:=  40002;
  Version_label             : constant:=  40002;
  Backup_bak_button         : constant:=  40003;
  GNAT_URL                  : constant:=  40003;
  IDM_Save_All              : constant:=  40003;
  IDM_Unselect_all          : constant:=  40003;
  GNAT_Version              : constant:=  40004;
  IDM_Save_File             : constant:=  40004;
  Right_margin_edit_box     : constant:=  40004;
  GNAVI_URL                 : constant:=  40005;
  Group_color_theme         : constant:=  40005;
  IDM_Redo                  : constant:=  40005;
  IDM_ABOUT                 : constant:=  40005;
  IDM_TEST_ARCHIVE          : constant:=  40006;
  ResEdit_URL               : constant:=  40006;
  IDM_Save_As               : constant:=  40007;
  IDM_QUIT                  : constant:=  40008;
  IDM_Close                 : constant:=  40009;
  IDM_RECOMPRESS_ARCHIVE    : constant:=  40009;
  IDM_EXTRACT               : constant:=  40010;
  IDM_General_options       : constant:=  40010;
  IDM_FIND_IN_ARCHIVE       : constant:=  40011;
  IDM_FLAT_VIEW             : constant:=  40012;
  IDM_TREE_VIEW             : constant:=  40013;
  IDM_MRU_2                 : constant:=  40014;
  IDM_MRU_3                 : constant:=  40015;
  IDM_MRU_4                 : constant:=  40016;
  IDM_MRU_5                 : constant:=  40017;
  IDM_MRU_6                 : constant:=  40018;
  IDM_MRU_7                 : constant:=  40019;
  IDM_MRU_8                 : constant:=  40020;
  IDM_MRU_9                 : constant:=  40021;
  IDM_WINDOW_CASCADE        : constant:=  40022;
  IDM_WINDOW_TILE_HORIZONTAL: constant:=  40023;
  IDM_WINDOW_TILE_VERTICAL  : constant:=  40024;
  IDM_WINDOW_CLOSE_ALL      : constant:=  40025;
  IDM_ADD_FILES             : constant:=  40027;
  IDM_UPDATE_ARCHIVE        : constant:=  40028;
  IDM_Properties            : constant:=  40030;
  IDM_Quick_Help            : constant:=  40031;
  IDM_Web                   : constant:=  40032;
  IDM_Select_all            : constant:=  40033;
  IDM_MRU_1                 : constant:=  40034;

  -- ** Some helper utilities (spec).

  procedure Dlg_to_Scn(
    xd,yd,wd,hd:  in Integer;
    xs,ys,ws,hs: out Integer);

  procedure Use_GUI_Font(Window: in out GWindows.Base.Base_Window_Type'Class);

  function Num_resource(id: Natural) return GString;  --  Just turn 123 into "#123".

  -- Last line of resource script file: 275

end LEA_Resource_GUI;
