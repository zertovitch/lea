with LEA_Common;                        use LEA_Common;
with LEA_Common.User_options;           use LEA_Common.User_options;

with LEA_Resource_GUI;                  use LEA_Resource_GUI;

with GWindows;
with GWindows.Application;              use GWindows.Application;
with GWindows.Base;
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
       box.Indentation_edit_box.Text(Integer'Wide_Image(candidate.indentation));
       box.Right_margin_edit_box.Text(Integer'Wide_Image(candidate.right_margin));
       box.Backup_none_button.State(boolean_to_state(candidate.backup = none));
       box.Backup_bak_button.State(boolean_to_state(candidate.backup = bak));
       for t in Color_Theme_Type loop
         box.Color_theme_list_box.Add(Color_Theme_Type'Wide_Image(t) );
       end loop;
       box.Color_theme_list_box.Text(Color_Theme_Type'Wide_Image(candidate.color_theme));
       --  !!  Find a way to have Color_theme_list_box read-only...
     end Set_Data;
     --
     procedure Get_Data (Window : in out GWindows.Base.Base_Window_Type'Class) is
     begin
       candidate.indentation:= Integer'Wide_Value(box.Indentation_edit_box.Text);
       candidate.right_margin:= Integer'Wide_Value(box.Right_margin_edit_box.Text);
       if box.Backup_none_button.State = Checked then
         candidate.backup := none;
       elsif box.Backup_bak_button.State = Checked then
         candidate.backup := bak;
       end if;
       candidate.color_theme:= Color_Theme_Type'Wide_Value(box.Color_theme_list_box.Text);
     exception
       when others =>
         Message_Box(
           Window,
           "Invalid data", "Incomplete reading of your changes",
           OK_Box, Error_Icon);
     end Get_Data;
     --
  begin
    box.Create_Full_Dialog(main);
    Set_Data;
    box.Center(main);
    box.Small_Icon ("Options_Icon");
    On_Destroy_Handler (box, Get_Data'Unrestricted_Access);
    case Show_Dialog (box, main) is
      when IDOK     =>
        main.opt:= candidate;
      when others   =>
        null;  --  Contains the IDCANCEL case
    end case;
  end;

end;
