with LEA_GWin.MDI_Main;

with GWindows.Common_Controls,
     GWindows.Image_Lists;

with Office_Applications;

package LEA_GWin.Toolbars is

  --  ** Main tool bar (add / remove / ...) at top left of the main window:

  procedure Init_Main_Tool_Bar
    (tb     : in out Office_Applications.Classic_Main_Tool_Bar_Type'Class;
     parent : in out LEA_GWin.MDI_Main.MDI_Main_Type);

  procedure Update_Theme
    (tb : in out Office_Applications.Classic_Main_Tool_Bar_Type'Class);

end LEA_GWin.Toolbars;
