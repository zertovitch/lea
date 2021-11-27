with LEA_Resource_GUI;                 use LEA_Resource_GUI;

with GWindows.Base;
with GWindows.Menus;                    use GWindows.Menus;

with Interfaces.C;

package body LEA_GWin.Toolbars is

  use GWindows.Image_Lists, Interfaces.C;

  --  Filter & and \t
  --  Not having TTS_NO_PREFIX in tool_tip creation would do it as well.
  function Filter(s: GString) return GString is
    use type GString_Unbounded;
    u: GString_Unbounded;
  begin
    for i in s'Range loop
      case s(i) is
        when GCharacter'Val(0) =>
          null;
        when '&' =>
          if i < s'Last and then s(i+1)= '&' then
            u:= u & "&&&";  --  "&&" is translated as "&&&" in order to be displayed as a '&' !...
          else
            null;  --  Skip
          end if;
        when GCharacter'Val(9) => -- Tab
          exit;
        when '\' =>
          exit when i < s'Last and then s(i+1)= 't';
        when others =>
          u:= u & s(i);
      end case;
    end loop;
    return To_GString_From_Unbounded(u);
  end Filter;

  sep_width: constant:= 8;

  function Num_resource(id: Natural) return GString is
    img: constant String:= Integer'Image(id);
  begin
    return To_GString_From_String('#' & img(img'First+1..img'Last));
  end Num_resource;

  procedure Init_Main_toolbar (
    tb    : in out GWindows.Common_Controls.Ex_Tb.Ex_Toolbar_Control_Type'Class;
    il    : in out GWindows.Image_Lists.Image_List_Type;
    parent: in out LEA_GWin.MDI_Main.MDI_Main_Type
  )
  is
    Fake_Menu : Menu_MDI_Child_Type;
    --
    procedure Add_Button_with_Tip
      (Image_Index : in     Natural;
       Command_ID  : in     Integer)
    is
      --  The tool tip's text is a copy of the menu's text.
      label : constant GString := Filter(Text(Fake_Menu.Main, Command, Command_ID));
    begin
      --  The display of tool tips using tb.Add_String does not work on Windows 10
      --  for amd64/x64 when LEA is built as 64 bit, but it works on the same PC
      --  when LEA is built as 32 bit.
      --  Workaround: use Ex_Toolbar_Control_Type and its Add_Button method with texts,
      --  which inserts tooltips "manually".
      --  See also remarks & tests below.
      tb.Add_Button (Image_Index, Command_ID, GWindows.Common_Controls.Ex_Tb.No_Style, label, label);
    end Add_Button_with_Tip;

    TBSTYLE_EX_MIXEDBUTTONS : constant:= 8;
  begin
    tb.CreateEx (parent, 0, 0, 0, 40);
    tb.Dock (GWindows.Base.At_Top);

    Create (il, Num_resource(Toolbar_BMP), 32);
    tb.Set_Image_List (il);
    tb.Set_Extended_Style (TBSTYLE_EX_MIXEDBUTTONS);

    Create_Full_Menu(Fake_Menu);
    Add_Button_with_Tip ( 0, IDM_New_File);
    Add_Button_with_Tip ( 1, IDM_Open_File);
    Add_Button_with_Tip ( 2, IDM_Save_File);
    Add_Button_with_Tip ( 4, IDM_Save_All);
    tb.Add_Separator(sep_width);
    Add_Button_with_Tip (13, IDM_Cut);
    Add_Button_with_Tip (14, IDM_Copy);
    Add_Button_with_Tip (15, IDM_Paste);
    tb.Add_Separator(sep_width);
    Add_Button_with_Tip ( 5, IDM_Undo);
    Add_Button_with_Tip ( 6, IDM_Redo);
    tb.Add_Separator(sep_width);
    Add_Button_with_Tip ( 9, IDM_Indent);
    Add_Button_with_Tip ( 8, IDM_Unindent);
    tb.Add_Separator(sep_width);
    Add_Button_with_Tip (10, IDM_Comment);
    Add_Button_with_Tip (11, IDM_Uncomment);
    tb.Add_Separator(sep_width);
    Add_Button_with_Tip ( 3, IDM_Find);
    tb.Add_Separator(sep_width);
    Add_Button_with_Tip (12, IDM_Show_special_symbols);
    --
    --
    --  Here a piece of code to check whether strings are correctly sent
    --  to Windows API. It works on Windows 10, lea.exe executable: x86 and amd64/x64.
    --  Issue being investigated: tooltips via TBSTYLE_EX_MIXEDBUTTONS
    --  are not displayed on Windows 10 when built as amd64/x64.
    --
    --  Workaround found: use GWindows.Common_Controls.Ex_Tb.Ex_Toolbar_Control_Type
    --
    --  declare
    --    check : GString (1 .. 100);
    --    len : Natural;
    --  begin
    --    for i in 0 .. 14 loop
    --      tb.Get_String (i, check, len);
    --      Ada.Text_IO.Put_Line (i'Image & " -> " & G2S (check (1 .. len)));
    --    end loop;
    --  end;
  end Init_Main_toolbar;

end LEA_GWin.Toolbars;
