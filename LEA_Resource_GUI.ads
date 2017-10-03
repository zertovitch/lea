---------------------------------------------------------------------------
-- GUI contents of resource script file: LEA.rc
-- Transcription time: 2017/10/03  16:12:58
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
    Popup_0006: Menu_Type;  -- level 1; title: "&Window"
    Popup_0007: Menu_Type;  -- level 1; title: "&Help"
  end record;  --  Menu_MDI_Child_Type

  -- Menu at line 81
  procedure Create_Full_Menu
     (Menu        : in out Menu_MDI_Child_Type);

  type Menu_MDI_Main_Type is tagged record
    Main: Menu_Type; -- Root of the whole menu tree
    Popup_0001: Menu_Type;  -- level 1; title: "&File"
    Popup_0002: Menu_Type;  -- level 2; title: "&Recent"
    Popup_0003: Menu_Type;  -- level 1; title: "&Window"
    Popup_0004: Menu_Type;  -- level 1; title: "&Help"
  end record;  --  Menu_MDI_Main_Type

  -- Menu at line 122
  procedure Create_Full_Menu
     (Menu        : in out Menu_MDI_Main_Type);

  package Version_info is
    Authors: constant String:= "Gautier de Montmollin";
    FileDescription: constant String:= "LEA - a Lightweight Editor for Ada - Free, MIT license";
    FileVersion: constant String:= "0.0";
    LegalCopyright: constant String:= "Copyright � Gautier de Montmollin 2017 .. 2017";
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
  About_box                 : constant:=    107;
  Progress_box              : constant:=    109;
  Find_box                  : constant:=    111;
  AZip_Doc_Icon             : constant:=    112;
  AZip_Icon                 : constant:=    114;
  File_exists_box           : constant:=    118;
  Credits_box               : constant:=    121;
  Toolbar_BMP               : constant:=    123;
  Folders_BMP               : constant:=    124;
  Wait_refresh_box          : constant:=    125;
  Key_Icon                  : constant:=    131;
  Binoculars_Icon           : constant:=    132;
  Password_decryption_box   : constant:=    133;
  Password_encryption_box   : constant:=    135;
  Plus_icon                 : constant:=    137;
  Drop_files_box            : constant:=    140;
  Properties_box            : constant:=    142;
  Quick_help_box            : constant:=    143;
  Archive_Progress          : constant:=   1000;
  Conflict_simple_name      : constant:=   1000;
  Encrypted_entry           : constant:=   1000;
  GNAT_URL                  : constant:=   1000;
  Entry_operation_name      : constant:=   1001;
  GNAT_Version              : constant:=   1001;
  Conflict_location         : constant:=   1002;
  File_Progress             : constant:=   1002;
  GNAVI_URL                 : constant:=   1002;
  Name_to_be_searched       : constant:=   1002;
  Show_password_box         : constant:=   1002;
  Cancel_button             : constant:=   1003;
  Version_label             : constant:=   1003;
  Content_to_be_searched    : constant:=   1004;
  ResEdit_URL               : constant:=   1004;
  Credits_button            : constant:=   1005;
  Entry_name                : constant:=   1005;
  Password_edit             : constant:=   1005;
  Overwrite_Yes             : constant:=   1006;
  Comment_2                 : constant:=   1007;
  Comment_1                 : constant:=   1008;
  Overwrite_No              : constant:=   1008;
  Comp_size                 : constant:=  40000;
  Confirm_Password_Label    : constant:=  40000;
  Copyright_label           : constant:=  40000;
  IDM_NEW_ARCHIVE           : constant:=  40000;
  New_archive_msg           : constant:=  40000;
  Confirm_Icon              : constant:=  40001;
  Drop_archive_name         : constant:=  40001;
  IDM_OPEN_ARCHIVE          : constant:=  40001;
  Uncomp_size               : constant:=  40001;
  Comp_ratio                : constant:=  40002;
  Encrypt_check_box         : constant:=  40002;
  IDM_MRU_1                 : constant:=  40002;
  IDM_Unselect_all          : constant:=  40002;
  Password_confirm_edit     : constant:=  40002;
  IDM_ABOUT                 : constant:=  40003;
  Stats_list                : constant:=  40003;
  IDM_TEST_ARCHIVE          : constant:=  40004;
  Numb_entries              : constant:=  40004;
  IDM_QUIT                  : constant:=  40005;
  IDM_RECOMPRESS_ARCHIVE    : constant:=  40006;
  IDM_EXTRACT               : constant:=  40007;
  IDM_FIND_IN_ARCHIVE       : constant:=  40008;
  IDM_COMPARE_ARCHIVES      : constant:=  40009;
  IDM_FLAT_VIEW             : constant:=  40010;
  IDM_TREE_VIEW             : constant:=  40011;
  IDM_MRU_2                 : constant:=  40012;
  IDM_MRU_3                 : constant:=  40013;
  IDM_MRU_4                 : constant:=  40014;
  IDM_MRU_5                 : constant:=  40015;
  IDM_MRU_6                 : constant:=  40016;
  IDM_MRU_7                 : constant:=  40017;
  IDM_MRU_8                 : constant:=  40018;
  IDM_MRU_9                 : constant:=  40019;
  IDM_WINDOW_CASCADE        : constant:=  40020;
  IDM_WINDOW_TILE_HORIZONTAL: constant:=  40021;
  IDM_WINDOW_TILE_VERTICAL  : constant:=  40022;
  IDM_WINDOW_CLOSE_ALL      : constant:=  40023;
  IDM_MERGE_ARCHIVES        : constant:=  40024;
  AZip_URL                  : constant:=  40025;
  IDM_Touch_Time_Stamps     : constant:=  40025;
  IDM_Delete_selected       : constant:=  40026;
  IDM_ADD_FILES             : constant:=  40027;
  IDM_UPDATE_ARCHIVE        : constant:=  40028;
  IDM_CLOSE_ARCHIVE         : constant:=  40029;
  IDM_Encrypt_Archive       : constant:=  40030;
  IDM_Add_Files_Encryption  : constant:=  40031;
  IDM_Properties            : constant:=  40032;
  Overwrite_All             : constant:=  40032;
  IDM_Add_Folder            : constant:=  40033;
  IDM_Add_Folder_Encryption : constant:=  40034;
  Overwrite_None            : constant:=  40034;
  IDM_Quick_Help            : constant:=  40035;
  IDM_Web                   : constant:=  40036;
  Overwrite_Rename          : constant:=  40036;
  IDM_SAVE_ARCHIVE_AS       : constant:=  40037;
  IDM_Select_all            : constant:=  40038;
  ZipAda_Version            : constant:=  40039;
  ZipAda_URL                : constant:=  40040;

  -- ** Some helper utilities (spec).

  procedure Dlg_to_Scn(
    xd,yd,wd,hd:  in Integer;
    xs,ys,ws,hs: out Integer);

  procedure Use_GUI_Font(Window: in out GWindows.Base.Base_Window_Type'Class);

  function Num_resource(id: Natural) return GString;  --  Just turn 123 into "#123".

  -- Last line of resource script file: 216

end LEA_Resource_GUI;
