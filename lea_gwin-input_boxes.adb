with LEA_Resource_GUI;                  use LEA_Resource_GUI;

with GWindows;
with GWindows.Application;              use GWindows.Application;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

package body LEA_GWin.Input_Boxes is

  function String_Input (
    Parent  : in out GWindows.Base.Base_Window_Type'Class;
    Message :        GString
  )
  return GString is
    --
    box : String_Prompt_Type;
    str : GString_Unbounded;
    --
    procedure Set_Data is
    begin
      box.String_Prompt_Label.Text (Message);
    end Set_Data;
    --
    procedure Get_Data (Window : in out GWindows.Base.Base_Window_Type'Class) is
    pragma Unreferenced (Window);
    begin
      str := G2GU (box.String_Prompt_Edit_Box.Text);
    end Get_Data;
    --
  begin
    box.Create_Full_Dialog (Parent);
    Set_Data;
    box.Center (Parent);
    box.String_Prompt_Edit_Box.Focus;
    box.Text ("Get / Get_Immediate / Get_Line");
    On_Destroy_Handler (box, Get_Data'Unrestricted_Access);
    case Show_Dialog (box, Parent) is
      when IDOK     =>
        return GU2G (str);
      when others =>
        return "";
    end case;
  end String_Input;

  procedure Skip_Line (
    Parent  : in  GWindows.Base.Base_Window_Type'Class;
    Message : in  GWindows.GString;
    Result  : out GWindows.Message_Boxes.Message_Box_Result
  )
  is
  begin
    Result := Message_Box (Parent, "Skip_Line", Message, GWindows.Message_Boxes.OK_Cancel_Box);
  end Skip_Line;

end LEA_GWin.Input_Boxes;
