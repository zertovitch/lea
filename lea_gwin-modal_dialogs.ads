--  Small modal dialogs

with LEA_GWin.MDI_Child,
     LEA_GWin.MDI_Main;

package LEA_GWin.Modal_Dialogs is

  procedure Do_Go_to_Line (Child_Window : in out MDI_Child.MDI_Child_Type);

  procedure Show_About_Box (Main_Window : in out MDI_Main.MDI_Main_Type);

  procedure Browse_and_Get_Code_Sample (Main_Window : in out MDI_Main.MDI_Main_Type);

end LEA_GWin.Modal_Dialogs;
