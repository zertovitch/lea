with LEA_Common;                        use LEA_Common;
with LEA_Common.User_options;           use LEA_Common.User_options;
with LEA_GWin.MDI_Child;                use LEA_GWin.MDI_Child;

with LEA_Resource_GUI;                  use LEA_Resource_GUI;

with GWindows;
with GWindows.Application;              use GWindows.Application;
with GWindows.Base;                     use GWindows.Base;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

with GWin_Util;                         use GWin_Util;

package body LEA_GWin.Options is

  procedure On_General_Options (main : in out MDI_Main_Type) is
    --
    box: Option_box_Type;
    candidate: Option_Pack_Type:= main.opt;
    --
    procedure Set_Data is
    begin
      box.Indentation_edit_box.Text        (Integer'Wide_Image(candidate.indentation));
      box.Tab_width_edit_box.Text          (Integer'Wide_Image(candidate.tab_width));
      box.Right_margin_edit_box.Text       (Integer'Wide_Image(candidate.right_margin));
      box.Ada_file_extension_edit_box.Text (GU2G (candidate.ada_files_filter));
      box.Backup_none_button.State(boolean_to_state(candidate.backup = none));
      box.Backup_bak_button.State(boolean_to_state(candidate.backup = bak));
      --  Fill the drop-down list.
      for t in Color_Theme_Type loop
        box.Color_theme_list_box.Add(Nice_Image(t) );
      end loop;
      box.Color_theme_list_box.Text(Nice_Image(candidate.color_theme));
    end Set_Data;
    --
    procedure Get_Data (Window : in out GWindows.Base.Base_Window_Type'Class) is
    begin
      candidate.indentation      := Integer'Wide_Value(box.Indentation_edit_box.Text);
      candidate.tab_width        := Integer'Wide_Value(box.Tab_width_edit_box.Text);
      candidate.right_margin     := Integer'Wide_Value(box.Right_margin_edit_box.Text);
      candidate.ada_files_filter := G2GU (box.Ada_file_extension_edit_box.Text);
      if box.Backup_none_button.State = Checked then
        candidate.backup := none;
      elsif box.Backup_bak_button.State = Checked then
        candidate.backup := bak;
      end if;
      candidate.color_theme:= Nice_Value(box.Color_theme_list_box.Text);
    exception
      when others =>
        Message_Box(Window, "Invalid data", "Incomplete reading of your changes", OK_Box, Error_Icon);
    end Get_Data;
    --
    has_changes: Boolean;
    --
  begin
    box.Create_Full_Dialog(main);
    Set_Data;
    box.Center(main);
    box.Small_Icon ("Options_Icon");
    On_Destroy_Handler (box, Get_Data'Unrestricted_Access);
    case Show_Dialog (box, main) is
      when IDOK     =>
        has_changes := main.opt /= candidate;
        if has_changes then
          main.opt:= candidate;
          Apply_Main_Options (main);
        end if;
      when others   =>
        null;  --  Contains the IDCANCEL case
    end case;
  end On_General_Options;

  procedure Apply_Main_Options (main : in out MDI_Main_Type) is
    procedure Apply_changes_to_child(Window : GWindows.Base.Pointer_To_Base_Window_Class) is
    begin
      if Window.all in MDI_Child_Type'Class then
        MDI_Child_Type(Window.all).Editor.Apply_options;
      end if;
    end Apply_changes_to_child;
  begin
    Text_files_filters (Text_files_filters'First).Filter := main.opt.ada_files_filter;
    Enumerate_Children(MDI_Client_Window (main).all, Apply_changes_to_child'Unrestricted_Access);
  end Apply_Main_Options;

end LEA_GWin.Options;
