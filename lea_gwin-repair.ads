with LEA_GWin.Messages;
with LEA_GWin.MDI_Main;

package LEA_GWin.Repair is

  procedure Do_Repair (
    MDI_Main : in out LEA_GWin.MDI_Main.MDI_Main_Type;
    repair   : in out LEA_GWin.Messages.Editor_repair_information
  );

end LEA_GWin.Repair;
