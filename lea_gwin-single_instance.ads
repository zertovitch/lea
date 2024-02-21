--
--  Manages a single instance of the editor.
--

with GWindows;

with LEA_GWin.MDI_Main;

package LEA_GWin.Single_Instance is

  LEA_Class_Name : constant GWindows.GString := GWindows.GStrings.To_GString_From_String ("LEA_Editor_Class_Name");

  procedure Manage_Single_Instance
    (Top_Window     : in     LEA_GWin.MDI_Main.MDI_Main_Access;
     Exit_Requested :    out Boolean);

end LEA_GWin.Single_Instance;
