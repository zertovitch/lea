with HAC_Sys.Defs;

with GWindows.Base;
with GWindows.Common_Controls.Ex_List_View;
with GWindows.Drawing_Objects;

package LEA_GWin.Messages is

  package LEA_LV_Ex is new GWindows.Common_Controls.Ex_List_View (HAC_Sys.Defs.Diagnostic_Kit);

  type Message_List_Type is new LEA_LV_Ex.Ex_List_View_Control_Type with record
    mdi_main_parent : GWindows.Base.Pointer_To_Base_Window_Class;
    Font            : GWindows.Drawing_Objects.Font_Type;
  end record;

  overriding procedure On_Create (Control : in out Message_List_Type);
  overriding procedure On_Click (Control : in out Message_List_Type);
  overriding procedure On_Double_Click (Control : in out Message_List_Type);

  ---------------------------------------------------------
  --  Methods introduced in the Message_List_Type class  --
  ---------------------------------------------------------

  procedure Apply_Options (Control : in out Message_List_Type);
  procedure Copy_Messages (Control : in out Message_List_Type);
  procedure Message_line_action (Control : in out Message_List_Type; real_click : Boolean);
  procedure Redraw_Icons (Control : in out Message_List_Type);

  --  Width of broadest message column in pixels.
  --  Ideally, a function using something like mdi parent's Client_Area_Width
  large_message_width : constant := 1000;

  function Wrench_Icon (is_reparable, has_dark_background : Boolean) return Natural;

end LEA_GWin.Messages;
