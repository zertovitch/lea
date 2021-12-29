with LEA_GWin.MDI_Main;

with GWindows.Base;

package body LEA_GWin.Sliding_Panels is

  overriding procedure On_Bar_Moved (Splitter : in out LEA_splitter) is
  begin
    LEA_GWin.MDI_Main.MDI_Main_Type (Splitter.MDI_Main.all).Memorize_Splitters;
    --  Call parent method:
    GWin_Util.Splitter_with_dashes (Splitter).On_Bar_Moved;
  end On_Bar_Moved;

  overriding procedure On_Create (Window : in out Project_Panel_Type) is
    use GWindows.Base;
  begin
    Window.Dock (At_Left);
    --  Create widgets within the panel:
    Window.Splitter.Create (Window, At_Right);
    Window.Project_Tree.Create (Window, 1,1,20,20, Lines_At_Root => False);
    Window.Project_Tree.Dock (Fill);
  end On_Create;

  overriding procedure On_Create (Window : in out Message_Panel_Type) is
    use GWindows.Base, GWindows.Common_Controls;
  begin
    Window.Dock (At_Bottom);
    --  Create widgets within the panel:
    Window.Splitter.Create (Window, At_Top, Height => 5);
    Window.Message_List.Create (Window, 1,1,20,20, View => Report_View);
  end On_Create;

end LEA_GWin.Sliding_Panels;
