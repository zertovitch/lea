with LEA_Common.Color_Themes;

with LEA_GWin.MDI_Child,
     LEA_GWin.MDI_Main;

with LEA_Resource_GUI;

with GWindows.Cursors,
     GWindows.Scintilla,
     GWindows.Drawing_Objects;

with Ada.Strings.Wide_Unbounded;

with Ada.Text_IO;

package body LEA_GWin.Tabs is

  use GWindows.Menus;

  function Item_under_Mouse_Cursor (Control : in out LEA_Tab_Bar_Type) return Integer is
    (Control.Item_At_Position
      (Control.Point_To_Client
        (GWindows.Cursors.Get_Cursor_Position)));

  procedure Refresh_Tool_Tip (Control : in out LEA_Tab_Bar_Type) is
    tab_under_pointer : constant Integer := Item_under_Mouse_Cursor (Control);
  begin
    if tab_under_pointer >= 0
      and then tab_under_pointer /= Control.tip_index
    then
      Control.tip_index := tab_under_pointer;
      Control.tips.Delete_Tool_Tip (Control);
      --  NB: A more obvious way to proceed would be to use Update_Tool_Tip
      --      after an initial call to Add_Tool_Tip, but that doesn't work
      --      as expected (at least, on Windows 10).
      declare
        window_of_tab_under_pointer : MDI_Child.MDI_Child_Type
          renames
            MDI_Child.MDI_Child_Type
              (Control.info (tab_under_pointer).Window.all);
        fn : constant GString :=
               GU2G (window_of_tab_under_pointer.ID.File_Name);
      begin
        Control.tips.Add_Tool_Tip
          (Control,
           (if fn'Length = 0 then "No file yet" else "File: " & fn));
      end;
    end if;
  end Refresh_Tool_Tip;

  overriding procedure On_Change (Control : in out LEA_Tab_Bar_Type) is
  begin
    Control.info (Control.Selected_Tab).Window.Focus;
    Control.tip_index := invalid_tip_index;
    Refresh_Tool_Tip (Control);  --  The effect of this is invisible...
  end On_Change;

  overriding procedure On_Create (Control : in out LEA_Tab_Bar_Type) is
    use LEA_Resource_GUI;
    use LEA_Common.Color_Themes;
  begin
    Append_Item (Control.context_menu, "Close", IDM_Close);
    Append_Item (Control.context_menu, "Open containing &folder", IDM_Open_Containing_Folder);
    Control.Background_Color (Color_Convert (Theme_Color (tab_bar_background)));
    Control.Tab_Colors (Background_Color                  => Color_Convert (Theme_Color (tab_bar_background)),
                        Background_Selected_Color         => Color_Convert (Theme_Color (tab_background_selected)),
                        Background_Hovered_Color          => Color_Convert (Theme_Color (tab_background_hovered)),
                        Background_Selected_Hovered_Color => Color_Convert (Theme_Color (tab_background_selected_hovered)),
                        Foreground_Color                  => Color_Convert (Theme_Color (tab_foreground)),
                        Foreground_Selected_Color         => Color_Convert (Theme_Color (tab_foreground_selected)),
                        Foreground_Hovered_Color          => Color_Convert (Theme_Color (tab_foreground_hovered)),
                        Foreground_Selected_Hovered_Color => Color_Convert (Theme_Color (tab_foreground_selected_hovered)),
                        Frame_Color                       => Color_Convert (Theme_Color (tab_frame)));
  end On_Create;

  overriding procedure On_Message
     (Window       : in out LEA_Tab_Bar_Type;
      message      : in     Interfaces.C.unsigned;
      wParam       : in     GWindows.Types.Wparam;
      lParam       : in     GWindows.Types.Lparam;
      Return_Value : in out GWindows.Types.Lresult)
  is
    WM_MOUSEMOVE : constant := 512;
    WM_NCHITTEST : constant := 132;
    WM_PAINT     : constant :=  15;
    WM_SETCURSOR : constant :=  32;
    use type Interfaces.C.unsigned;
  begin
    if message in WM_MOUSEMOVE | WM_NCHITTEST | WM_PAINT | WM_SETCURSOR then
      Refresh_Tool_Tip (Window);
    end if;
    GWindows.Common_Controls.Tab_Control_Type (Window).On_Message
      (message, wParam, lParam, Return_Value);
  end On_Message;

  overriding procedure On_Middle_Click (Control : in out LEA_Tab_Bar_Type) is
    chosen_tab : constant Integer := Item_under_Mouse_Cursor (Control);
    chosen_child_window : GWindows.Windows.MDI.Pointer_To_MDI_Child_Window_Class;
  begin
    if chosen_tab >= 0 then
      --  Focus on the middle-button-clicked tab:
      Control.Selected_Tab (chosen_tab);
      Control.On_Change;
      chosen_child_window := Control.info (chosen_tab).Window;
      --  Close the corresponding editor (and subsequently, the tab itself):
      chosen_child_window.Close;
    end if;
  end On_Middle_Click;

  overriding procedure On_Right_Click (Control : in out LEA_Tab_Bar_Type) is
    chosen_tab : constant Integer := Item_under_Mouse_Cursor (Control);
    chosen_child_window : GWindows.Windows.MDI.Pointer_To_MDI_Child_Window_Class;
    use Ada.Strings.Wide_Unbounded;
  begin
    if chosen_tab >= 0 then
      chosen_child_window := Control.info (chosen_tab).Window;
      State
        (Control.context_menu,
         Command,
         LEA_Resource_GUI.IDM_Open_Containing_Folder,
         bool_to_state (Length (Control.info (chosen_tab).ID.File_Name) > 0));
      Immediate_Popup_Menu (Control.context_menu, chosen_child_window.all);
    end if;
  end On_Right_Click;

  overriding procedure Delete_Tab (Control : in out LEA_Tab_Bar_Type; Where : in Integer) is
  begin
    if Where >= 0 then
      GWindows.Common_Controls.Tab_Control_Type (Control).Delete_Tab (Where);  --  Call parent method
      Control.info.Delete (Where);
      Control.tip_index := invalid_tip_index;
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

  procedure Update_Theme (Control : in out LEA_Tab_Bar_Type'Class) is
    use LEA_Common.Color_Themes;
  begin
    Control.Background_Color (Color_Convert (Theme_Color (tab_bar_background)));
    Control.Tab_Colors (Background_Color                  => Color_Convert (Theme_Color (tab_bar_background)),
                        Background_Selected_Color         => Color_Convert (Theme_Color (tab_background_selected)),
                        Background_Hovered_Color          => Color_Convert (Theme_Color (tab_background_hovered)),
                        Background_Selected_Hovered_Color => Color_Convert (Theme_Color (tab_background_selected_hovered)),
                        Foreground_Color                  => Color_Convert (Theme_Color (tab_foreground)),
                        Foreground_Selected_Color         => Color_Convert (Theme_Color (tab_foreground_selected)),
                        Foreground_Hovered_Color          => Color_Convert (Theme_Color (tab_foreground_hovered)),
                        Foreground_Selected_Hovered_Color => Color_Convert (Theme_Color (tab_foreground_selected_hovered)),
                        Frame_Color                       => Color_Convert (Theme_Color (tab_frame)));
    
    Control.Redraw (Erase      => True,
                    Redraw_Now => True);
  end Update_Theme;

end LEA_GWin.Tabs;
