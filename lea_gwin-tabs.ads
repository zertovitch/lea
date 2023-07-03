with GWindows.Common_Controls,
     GWindows.Windows.MDI;

with Ada.Containers.Vectors;

package LEA_GWin.Tabs is

  type Tab_Info_Type is record
    ID     : ID_Type;
    Window : GWindows.Windows.MDI.Pointer_To_MDI_Child_Window_Class;
  end record;

  package Tab_Info_Vectors is new Ada.Containers.Vectors (Natural, Tab_Info_Type);

  type LEA_Tab_Bar_Type is
    new GWindows.Common_Controls.Tab_Control_Type with
      record
        --  Access to the containing main window:
        MDI_Parent : GWindows.Windows.MDI.Pointer_To_MDI_Main_Window_Class;
        --  Info corresponding to tabs:
        info       : Tab_Info_Vectors.Vector;
      end record;

  overriding procedure On_Change (Control : in out LEA_Tab_Bar_Type);

  overriding procedure On_Middle_Click (Control : in out LEA_Tab_Bar_Type);

  overriding procedure Delete_Tab (Control : in out LEA_Tab_Bar_Type; Where : in Integer);

  function Tab_Index (Control : in out LEA_Tab_Bar_Type; ID : ID_Type) return Integer;

end LEA_GWin.Tabs;
