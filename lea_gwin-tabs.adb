with LEA_GWin.MDI_Main;

with GWindows.Cursors,
     GWindows.Scintilla;

package body LEA_GWin.Tabs is

  overriding procedure On_Change (Control : in out LEA_Tab_Bar_Type) is
    dummy : Boolean;
  begin
    MDI_Main.Focus_an_already_opened_window
      (MDI_Main.MDI_Main_Type (Control.MDI_Parent.all),
       Control.info (Control.Selected_Tab).ID,
       is_open => dummy);
  end On_Change;

  overriding procedure On_Middle_Click (Control : in out LEA_Tab_Bar_Type) is
    chosen_tab : Integer;
    window_to_be_closed : GWindows.Windows.MDI.Pointer_To_MDI_Child_Window_Class;
  begin
    --  Focus on the middle-button-clicked tab:
    chosen_tab :=
      Control.Item_At_Position
        (Control.Point_To_Client (GWindows.Cursors.Get_Cursor_Position));
    Control.Selected_Tab (chosen_tab);
    Control.On_Change;
    --  Close the corresponding editor (and subsequently, the tab itself):
    window_to_be_closed := Control.info (chosen_tab).Window;
    window_to_be_closed.Close;
  end On_Middle_Click;

  overriding procedure Delete_Tab (Control : in out LEA_Tab_Bar_Type; Where : in Integer) is
  begin
    if Where >= 0 then
      GWindows.Common_Controls.Tab_Control_Type (Control).Delete_Tab (Where);  --  Call parent method
      Control.info.Delete (Where);
    end if;
  end Delete_Tab;

  function Tab_Index (Control : in out LEA_Tab_Bar_Type; ID : ID_Type) return Integer is
  begin
    for index in 0 .. Control.Tab_Count - 1 loop
      if Equivalent (ID, Control.info (index).ID) then
        return index;
      end if;
    end loop;
    return -1;
  end Tab_Index;

end LEA_GWin.Tabs;
