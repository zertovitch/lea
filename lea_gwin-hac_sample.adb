with LEA_Resource_GUI;

with GWindows.Application,
     GWindows.Base,
     GWindows.Constants;

package body LEA_GWin.HAC_Sample is

  procedure Browse_and_Get (Main_Window : in out MDI_Main.MDI_Main_Type) is
    use LEA_Resource_GUI;
    box : HAC_example_box_Type;
    --
    procedure Set_Data is
    begin
      null;
    end;
    --
    procedure Get_Data (Window : in out GWindows.Base.Base_Window_Type'Class) is
    begin
      null;
    end;
    --
    use GWindows.Application, GWindows.Constants;
  begin
    box.Create_Full_Dialog (Main_Window);
    Set_Data;
    box.Center;
    On_Destroy_Handler (box, Get_Data'Unrestricted_Access);
    case Show_Dialog (box, Main_Window) is
      when IDOK   =>
        null;
      when others =>
        null;  --  Contains the IDCANCEL case
    end case;
  end;

end LEA_GWin.HAC_Sample;
