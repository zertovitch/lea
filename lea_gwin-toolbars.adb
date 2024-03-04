with LEA_Resource_GUI,
     LEA_Common.Color_Themes;

with GWindows.Base,
     GWindows.Menus,
     GWindows.Colors;

with GWin_Util;

with Interfaces.C;

package body LEA_GWin.Toolbars is

  procedure Init_Main_Tool_Bar
    (tb     : in out Office_Applications.Classic_Main_Tool_Bar_Type'Class;
     parent : in out LEA_GWin.MDI_Main.MDI_Main_Type)
  is
    use LEA_Resource_GUI;
    use LEA_Common.Color_Themes;
    sep_width : constant := 8;
    Fake_Menu : Menu_MDI_Child_Type;
    --
    procedure Add_Button_with_Tip
      (Image_Index : in Natural;
       Command_ID  : in Integer)
    is
      use GWindows.Menus;
      --  The tool tip's text is a copy of the menu's text.
      label : constant GString :=
        GWin_Util.Menu_Entry_Title_to_Toolbar_Label (Text (Fake_Menu.Main, Command, Command_ID));
    begin
      tb.Add_String (label);
      tb.Add_Button (Image_Index, Command_ID, tb.String_Count);
      tb.String_Count := tb.String_Count + 1;
    end Add_Button_with_Tip;

    st : Interfaces.C.unsigned;

    TBSTYLE_TOOLTIPS : constant := 16#0100#;
    TBSTYLE_FLAT     : constant := 16#0800#;
    TBSTYLE_LIST     : constant := 16#1000#;
    TBSTYLE_EX_MIXEDBUTTONS : constant := 8;

    use type Interfaces.C.unsigned;

  begin
    tb.Create (parent, 0, 0, 0, 40);
    tb.Dock (GWindows.Base.At_Top);

    tb.Images.Create (Num_resource (Toolbar_BMP), 32);
    tb.Set_Image_List (tb.Images);
    st := tb.Get_Style;
    tb.Set_Style (TBSTYLE_FLAT or TBSTYLE_TOOLTIPS or TBSTYLE_LIST or st);
    tb.Set_Extended_Style (TBSTYLE_EX_MIXEDBUTTONS);

    Create_Full_Menu (Fake_Menu);
    Add_Button_with_Tip  (0, IDM_New_File);
    Add_Button_with_Tip  (1, IDM_Open_File);
    Add_Button_with_Tip  (2, IDM_Save_File);
    Add_Button_with_Tip  (4, IDM_Save_All);
    tb.Add_Separator (sep_width);
    Add_Button_with_Tip (13, IDM_Cut);
    Add_Button_with_Tip (14, IDM_Copy);
    Add_Button_with_Tip (15, IDM_Paste);
    tb.Add_Separator (sep_width);
    Add_Button_with_Tip  (5, IDM_Undo);
    Add_Button_with_Tip  (6, IDM_Redo);
    tb.Add_Separator (sep_width);
    Add_Button_with_Tip  (9, IDM_Indent);
    Add_Button_with_Tip  (8, IDM_Unindent);
    tb.Add_Separator (sep_width);
    Add_Button_with_Tip (10, IDM_Comment);
    Add_Button_with_Tip (11, IDM_Uncomment);
    tb.Add_Separator (sep_width);
    Add_Button_with_Tip  (3, IDM_Find);
    tb.Add_Separator (sep_width);
    Add_Button_with_Tip (17, IDM_Build_and_run);
    tb.Add_Separator (sep_width);
    Add_Button_with_Tip (12, IDM_Show_special_symbols);
    Add_Button_with_Tip (16, IDM_Show_indentation_lines);
    tb.Background_Color (Color_Convert (Theme_Color (background)));
  end Init_Main_Tool_Bar;

  procedure Update_Theme
    (tb : in out Office_Applications.Classic_Main_Tool_Bar_Type'Class)
  is
    use LEA_Common.Color_Themes;
  begin
    tb.Background_Color (Color_Convert (Theme_Color (background)));
    tb.Redraw (Erase      => True,
               Redraw_Now => True);
  end Update_Theme;

end LEA_GWin.Toolbars;
