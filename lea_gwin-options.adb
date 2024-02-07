with LEA_Common.User_options;

with LEA_GWin.MDI_Child;

with LEA_Resource_GUI;

with GWindows.Application,
     GWindows.Base,
     GWindows.Buttons,
     GWindows.Constants,
     GWindows.Message_Boxes;

with GWin_Util;

package body LEA_GWin.Options is

  procedure On_General_Options (main : in out LEA_GWin.MDI_Main.MDI_Main_Type) is
    use LEA_Resource_GUI, LEA_Common, LEA_Common.Color_Themes, LEA_Common.User_options;
    --
    box : Option_box_Type;
    candidate : Option_Pack_Type := main.opt;
    --
    procedure Set_Data is
      use GWin_Util;
    begin
      box.Indentation_edit_box.Text    (candidate.indentation'Wide_Image);
      box.Tab_width_edit_box.Text      (candidate.tab_width'Wide_Image);
      box.Right_margin_edit_box.Text   (candidate.right_margin'Wide_Image);
      box.Auto_Insert_Check_Box.State  (boolean_to_state (candidate.auto_insert));
      box.Smart_Editor_Check_Box.State (boolean_to_state (candidate.smart_editor));
      box.Ada_file_extension_edit_box.Text (GU2G (candidate.ada_files_filter));
      --
      box.Backup_none_button.State (boolean_to_state (candidate.backup = none));
      box.Backup_bak_button.State  (boolean_to_state (candidate.backup = bak));
      --  Fill the drop-down list.
      for t in Color_Theme_Type loop
        box.Color_theme_list_box.Add (Nice_Image (t));
      end loop;
      box.Color_theme_list_box.Text (Nice_Image (candidate.color_theme));
    end Set_Data;
    --
    procedure Get_Data (Window : in out GWindows.Base.Base_Window_Type'Class) is
      use GWindows.Buttons, GWindows.Message_Boxes;
    begin
      candidate.indentation      := Integer'Wide_Value (box.Indentation_edit_box.Text);
      candidate.tab_width        := Integer'Wide_Value (box.Tab_width_edit_box.Text);
      candidate.right_margin     := Integer'Wide_Value (box.Right_margin_edit_box.Text);
      candidate.auto_insert      := box.Auto_Insert_Check_Box.State  = Checked;
      candidate.smart_editor     := box.Smart_Editor_Check_Box.State = Checked;
      candidate.ada_files_filter := G2GU (box.Ada_file_extension_edit_box.Text);
      if box.Backup_none_button.State = Checked then
        candidate.backup := none;
      elsif box.Backup_bak_button.State = Checked then
        candidate.backup := bak;
      end if;
      candidate.color_theme := Nice_Value (box.Color_theme_list_box.Text);
    exception
      when others =>
        Message_Box
          (Window, "Invalid data", "Incomplete reading of your changes", OK_Box, Error_Icon);
    end Get_Data;
    --
    has_changes, icons_redrawing : Boolean;
    --
  begin
    box.Create_Full_Dialog (main);
    Set_Data;
    box.Center (main);
    box.Small_Icon ("Options_Icon");
    On_Destroy_Handler (box, Get_Data'Unrestricted_Access);
    case GWindows.Application.Show_Dialog (box, main) is
      when GWindows.Constants.IDOK =>
        has_changes := main.opt /= candidate;
        if has_changes then
          icons_redrawing := main.opt.color_theme /= candidate.color_theme;
          main.opt := candidate;
          Apply_Main_Options (main);
          if icons_redrawing then
            main.Message_Panel.Message_List.Redraw_Icons;
            --  Some details like the wrench are theme-dependent.
          end if;
        end if;
      when others   =>
        null;  --  Contains the IDCANCEL case
    end case;
  end On_General_Options;

  procedure Apply_Main_Options (main : in out LEA_GWin.MDI_Main.MDI_Main_Type) is
    --
    procedure Apply_Changes_to_Child (Window : GWindows.Base.Pointer_To_Base_Window_Class) is
      use LEA_GWin.MDI_Child;
    begin
      if Window.all in MDI_Child_Type'Class then
        MDI_Child_Type (Window.all).Apply_Options;
      end if;
    end Apply_Changes_to_Child;
    --
    use LEA_GWin.MDI_Main, LEA_Common.Color_Themes;
  begin
    Select_Theme (main.opt.color_theme);
    main.text_files_filters (main.text_files_filters'First).Filter := main.opt.ada_files_filter;
    main.Project_Panel.Apply_Options;
    main.Message_Panel.Apply_Options;
    main.Update_Common_Menus;
    MDI_Client_Window (main).Enumerate_Children (Apply_Changes_to_Child'Unrestricted_Access);
  end Apply_Main_Options;

end LEA_GWin.Options;
