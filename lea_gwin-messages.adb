with LEA_GWin.MDI_Main;
with LEA_GWin.Repair;

with GWindows.Clipboard;
with GWindows.Cursors;
with GWindows.Windows;

with Ada.Strings.Wide_Unbounded;

package body LEA_GWin.Messages is

  procedure Message_line_action (Control : in out Message_List_Type; real_click : Boolean) is
    pl : LEA_LV_Ex.Data_Access;
    use HAC_Sys.Defs, LEA_LV_Ex, LEA_GWin.MDI_Main;
    mm : MDI_Main_Access;
  begin
    for i in 0 .. Control.Item_Count loop
      if Control.Is_Selected (i) then
        pl := Control.Item_Data (i);
        if pl /= null then
          mm := MDI_Main_Access (Control.mdi_main_parent);
          mm.Open_Child_Window_And_Load (pl.file, pl.line, pl.col_a, pl.col_a);
          --  At this point focus is on the editor window.
          if pl.kind /= none
            and then real_click
            and then Control.Point_To_Client (GWindows.Cursors.Get_Cursor_Position).X < 16
          then
            LEA_GWin.Repair.Do_Repair (mm.all, pl.all);
            if pl.kind = none then
              --  Remove tool icon:
              Control.Set_Item (Control.Text(Item => i, SubItem => 0), i, Icon => 0);
            end if;
          end if;
        end if;
        exit;  --  Found selected line, not worth to continue.
      end if;
    end loop;
  end Message_line_action;

  overriding procedure On_Click (Control : in out Message_List_Type) is
  begin
    Control.Message_line_action (True);
    --  Focus back on the message list (so the keyboard is also focused there)
    Control.Focus;
  end On_Click;

  overriding procedure On_Double_Click (Control : in out Message_List_Type) is
  begin
    Control.Message_line_action (True);
  end On_Double_Click;

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

end LEA_GWin.Messages;
