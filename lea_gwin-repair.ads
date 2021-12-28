with LEA_GWin.Messages,
     LEA_GWin.MDI_Main;

with HAC_Sys.Defs;

package LEA_GWin.Repair is

  procedure Do_Repair (
    MDI_Main : in out LEA_GWin.MDI_Main.MDI_Main_Type;
    repair   : in out HAC_Sys.Defs.Diagnostic_Kit
  );

end LEA_GWin.Repair;
