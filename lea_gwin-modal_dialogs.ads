--  Small modal dialogs

with LEA_GWin.MDI_Child;                use LEA_GWin.MDI_Child;
with LEA_GWin.MDI_Main;                 use LEA_GWin.MDI_Main;

package LEA_GWin.Modal_dialogs is

  procedure Do_go_to_line (MDI_Child: in out MDI_Child_Type);

  procedure Do_about (MDI_Main : in out MDI_Main_Type);

end LEA_GWin.Modal_dialogs;
