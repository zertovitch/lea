with LEA_Common;

with LEA_GWin.MDI_Main,
     LEA_GWin.Repair;

with GWindows.Clipboard,
     GWindows.Colors,
     GWindows.Cursors,
     GWindows.Windows;

with Ada.Directories,
     Ada.Strings.Unbounded,
     Ada.Strings.Wide_Unbounded;

package body LEA_GWin.Messages is

  procedure Message_Line_Action (Control : in out Message_List_Type; real_click : Boolean) is
    pl : LEA_LV_Ex.Data_Access;
    use HAC_Sys.Defs, LEA_LV_Ex, LEA_GWin.MDI_Main, Ada.Strings.Unbounded;
    mm : MDI_Main_Access;
    --
    function Get_Full_Name return String is
    begin
      --  For a future version: use the same search path as the compiler.
      return Ada.Directories.Full_Name (To_String (pl.file_name));
    end Get_Full_Name;
  begin
    for i in 0 .. Control.Item_Count loop
      if Control.Is_Selected (i) then
        pl := Control.Item_Data (i);
        if pl /= null then
          if Ada.Directories.Exists (Get_Full_Name) then
            mm := MDI_Main_Access (Control.mdi_main_parent);
            mm.Open_Child_Window_And_Load
              (S2G (Get_Full_Name),
               pl.location.line - 1,  --  Scintilla's lines are 0-based
               pl.location.column_start - 1,
               pl.location.column_start - 1);
               --  ^ NB: we dont mark the word, otherwise it would be
               --        .column_stop .
            --  At this point focus is on the editor window.
            if pl.repair_kind /= none
              and then real_click
              and then Control.Point_To_Client (GWindows.Cursors.Get_Cursor_Position).X < 16
            then
              LEA_GWin.Repair.Do_Repair (mm.all, pl.all);
              if pl.repair_kind = none then
                --  Remove tool icon:
                Control.Set_Item (Control.Text (Item => i, SubItem => 0), i, Icon => 0);
              end if;
            end if;
          end if;
        end if;
        exit;  --  Found selected line, not worth to continue.
      end if;
    end loop;
  end Message_Line_Action;

  overriding procedure On_Create (Control : in out Message_List_Type) is
  begin
    Control.Font.Create_Font (App_default_font, 15);
    Control.Set_Font (Control.Font);
    Control.Set_Extended_Style (GWindows.Common_Controls.Full_Row_Select);
    Control.Insert_Column ("", 0, 40);
    Control.Insert_Column ("", 1, 35);
    Control.Insert_Column ("", 2, 10);
    Control.Dock (GWindows.Base.Fill);
  end On_Create;

  overriding procedure On_Click (Control : in out Message_List_Type) is
  begin
    Control.Message_Line_Action (True);
    --  Focus back on the message list (so the keyboard is also focused there)
    Control.Focus;
  end On_Click;

  overriding procedure On_Double_Click (Control : in out Message_List_Type) is
  begin
    Control.Message_Line_Action (True);
  end On_Double_Click;

  procedure Apply_Options (Control : in out Message_List_Type) is
    use GWindows.Colors, LEA_Common, LEA_Common.Color_Themes, MDI_Main;
    --
    mdi_root : MDI_Main_Type renames MDI_Main_Type (Control.mdi_main_parent.all);
    theme    : Color_Theme_Type renames mdi_root.opt.color_theme;
  begin
    Control.Text_Color (GWindows_Color_Theme (theme, messages_foreground));
    Control.Back_Color (GWindows_Color_Theme (theme, messages_background));
    Control.Control_Back_Color (GWindows_Color_Theme (theme, messages_control_background));
    mdi_root.Message_Panel.Background_Color (GWindows_Color_Theme (theme, messages_control_background));
  end Apply_Options;

  procedure Copy_Messages (Control : in out Message_List_Type) is
    cols : Natural := 0;
    res  : GString_Unbounded;
    --  We separate columns with Tabs - useful when pasting into a spreadsheet.
    HTab : constant GCharacter := GCharacter'Val (9);
    use Ada.Strings.Wide_Unbounded;
  begin
    loop
      declare
        cn : constant GString := Control.Column_Text (cols);
      begin
        exit when cn = "";
        if cols > 0 then
          res := res & HTab;
        end if;
        res := res & cn;
      end;
      cols := cols + 1;
    end loop;
    res := res & NL;
    for i in 0 .. Control.Item_Count - 1 loop
      for c in 0 .. cols - 1 loop
        if c > 0 then
          res := res & HTab;
        end if;
        res := res & Control.Text (i, c);
      end loop;
      res := res & NL;
    end loop;
    --
    --  Now, send the whole stuff to the clipboard.
    --
    GWindows.Clipboard.Clipboard_Text
      (GWindows.Windows.Window_Access (Control.mdi_main_parent).all, res);
  end Copy_Messages;

  procedure Redraw_Icons (Control : in out Message_List_Type) is
    use HAC_Sys.Defs, LEA_Common.Color_Themes, LEA_LV_Ex;
    has_dark_background : constant Boolean :=
      dark_backgrounded (MDI_Main.MDI_Main_Type (Control.mdi_main_parent.all).opt.color_theme);
  begin
    for i in 0 .. Control.Item_Count - 1 loop
      if Control.Item_Data (i) /= null
        and then Control.Item_Data (i).repair_kind /= none
      then
        Control.Set_Item (Control.Text (i, 0), i, Wrench_Icon (True, has_dark_background));
      end if;
    end loop;
  end Redraw_Icons;

  procedure Set_Column_Scroll_Left
    (Control : in out Message_List_Type;
     Text    : in     GString;
     Index   : in     Integer;
     Width   : in     Integer)
  is
  begin
    --  Call parent method with a small width to force
    --  a horizontal scroll to the left:
    LEA_LV_Ex.Ex_List_View_Control_Type (Control).Set_Column (Text, Index, 20);
    --  Call parent method with correct width:
    LEA_LV_Ex.Ex_List_View_Control_Type (Control).Set_Column (Text, Index, Width);
  end Set_Column_Scroll_Left;

  function Wrench_Icon (is_reparable, has_dark_background : Boolean) return Natural is
  begin
    return Boolean'Pos (is_reparable) * (2 - Boolean'Pos (has_dark_background));
  end Wrench_Icon;

end LEA_GWin.Messages;
