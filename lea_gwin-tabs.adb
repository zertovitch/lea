with LEA_GWin.MDI_Child,
     LEA_GWin.MDI_Main;

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

  overriding procedure On_Message
     (Window       : in out LEA_Tab_Bar_Type;
      message      : in     Interfaces.C.unsigned;
      wParam       : in     GWindows.Types.Wparam;
      lParam       : in     GWindows.Types.Lparam;
      Return_Value : in out GWindows.Types.Lresult)
  is
    tab_under_pointer : Integer;
    WM_MOUSEMOVE : constant := 512;
    WM_NCHITTEST : constant := 132;
    WM_PAINT     : constant :=  15;
    WM_SETCURSOR : constant :=  32;
    use type Interfaces.C.unsigned;
  begin
    if message in WM_MOUSEMOVE | WM_NCHITTEST | WM_PAINT | WM_SETCURSOR then
      tab_under_pointer :=
        Window.Item_At_Position
          (Window.Point_To_Client (GWindows.Cursors.Get_Cursor_Position));
      if tab_under_pointer >= 0 and then tab_under_pointer /= Window.tip_index then
        Window.tip_index := tab_under_pointer;
        declare
          window_of_tab_under_pointer : MDI_Child.MDI_Child_Type
            renames MDI_Child.MDI_Child_Type (Window.info (tab_under_pointer).Window.all);
          fn : constant GString := GU2G (window_of_tab_under_pointer.ID.File_Name);
        begin
          if fn'Length = 0 then
            Window.tips.Add_Tool_Tip (Window, "Document without file");
          else
            Window.tips.Add_Tool_Tip (Window, "File: " & fn);
          end if;
        end;
      end if;
    end if;
    GWindows.Common_Controls.Tab_Control_Type (Window).On_Message
      (message, wParam, lParam, Return_Value);
  end On_Message;

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
      Control.tip_index := -1;
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
