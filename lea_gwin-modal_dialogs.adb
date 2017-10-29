--  Small modal dialogs

with LEA_Resource_GUI;                  use LEA_Resource_GUI;

with GWindows.Application;              use GWindows.Application;
with GWindows.Base;                     use GWindows.Base;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

package body LEA_GWin.Modal_dialogs is

  procedure Do_go_to_line (MDI_Child: in out MDI_Child_Type) is
    box : Go_to_line_box_Type;
    new_line: Integer := 0;
    --
    procedure Get_Data (Window : in out GWindows.Base.Base_Window_Type'Class) is
    begin
      new_line:= Integer'Wide_Value(box.Line_value_box.Text);
    exception
      when others =>
        Message_Box(Window, "Invalid data", "Line number is invalid", OK_Box, Error_Icon);
        new_line := 0;
    end Get_Data;
    --
  begin
    box.Create_Full_Dialog(MDI_Child);
    box.Center(MDI_Child);
    box.Line_value_box.Text (Integer'Wide_Image (MDI_Child.Editor.Get_current_line + 1));
    box.Line_value_box.Set_Selection (0, 10);
    box.Line_value_box.Focus;
    On_Destroy_Handler (box, Get_Data'Unrestricted_Access);
    case Show_Dialog (box, MDI_Child) is
      when IDOK     =>
        if new_line > 0 then
          MDI_Child.Editor.Set_current_line (new_line - 1);
        end if;
      when others   =>
        null;  --  Contains the IDCANCEL case
    end case;
  end;

end LEA_GWin.Modal_dialogs;
