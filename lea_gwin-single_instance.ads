--
--  Manages a single instance of the editor.
--

with GWindows;

with LEA_GWin.MDI_Main;

package LEA_GWin.Single_Instance is

  procedure Manage_Single_Instance
    (Top_Window                : in     LEA_GWin.MDI_Main.MDI_Main_Access;
     Application_Class_Name    : in     GString;
     Application_Instance_Name : in     GString;
     Exit_Requested            :    out Boolean);

  --  Exit_Requested =
  --
  --     False : no other instance is running, or another
  --             instance is running, but its main window
  --             cannot be found by the system.
  --
  --     True : another instance is running, was found by
  --            the system and we have passed to it the command-line
  --            arguments of our instance, which can stop safely.

end LEA_GWin.Single_Instance;
