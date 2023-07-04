with GWindows.Base,
     GWindows.Common_Controls,
     GWindows.Windows.MDI,
     GWindows.Types;

with Ada.Containers.Vectors;
with Interfaces.C;

package LEA_GWin.Tabs is

  type Tab_Info_Type is record
    ID     : ID_Type;
    Window : GWindows.Windows.MDI.Pointer_To_MDI_Child_Window_Class;
  end record;

  package Tab_Info_Vectors is new Ada.Containers.Vectors (Natural, Tab_Info_Type);

  invalid_tip_index : constant := -1;

  type LEA_Tab_Bar_Type is
    new GWindows.Common_Controls.Tab_Control_Type with
      record
        --  Access to the containing main window:
        MDI_Parent : GWindows.Windows.MDI.Pointer_To_MDI_Main_Window_Class;
        --  Info corresponding to tabs:
        info       : Tab_Info_Vectors.Vector;
        tips       : GWindows.Common_Controls.Tool_Tip_Type;
        tip_index  : Integer := invalid_tip_index;
      end record;

  overriding procedure On_Change (Control : in out LEA_Tab_Bar_Type);

  overriding procedure On_Message
     (Window       : in out LEA_Tab_Bar_Type;
      message      : in     Interfaces.C.unsigned;
      wParam       : in     GWindows.Types.Wparam;
      lParam       : in     GWindows.Types.Lparam;
      Return_Value : in out GWindows.Types.Lresult);

  overriding procedure On_Middle_Click (Control : in out LEA_Tab_Bar_Type);

  overriding procedure Delete_Tab (Control : in out LEA_Tab_Bar_Type; Where : in Integer);

  function Tab_Index (Control : in out LEA_Tab_Bar_Type; ID : ID_Type) return Integer;

end LEA_GWin.Tabs;
