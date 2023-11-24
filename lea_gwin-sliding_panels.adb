with LEA_GWin.MDI_Main,
     LEA_Common.Color_Themes;

with GWindows.Base;

package body LEA_GWin.Sliding_Panels is

  overriding procedure On_Bar_Moved (Splitter : in out LEA_splitter) is
  begin
    LEA_GWin.MDI_Main.MDI_Main_Type (Splitter.MDI_Main.all).Memorize_Splitters;
    --  Call parent method:
    GWindows.GControls.GSize_Bars.GSize_Bar_Type (Splitter).On_Bar_Moved;
  end On_Bar_Moved;

  ---------------------
  --  Project Panel  --
  ---------------------

  overriding procedure On_Create (Window : in out Project_Panel_Type) is
    use GWindows.Base;
  begin
    Window.Dock (At_Left);
    --  Create widgets within the panel:
    Window.Splitter.Create (Window, At_Right, Width => 5);
    Window.Project_Tree.Create (Window, 1, 1, 20, 20, Lines_At_Root => False);
    Window.Project_Tree.Dock (Fill);

    Window.Apply_Options;
    Window.Splitter.Set_Dashes (Dash_Height        => 30,
                                Dash_Width         => 1,
                                Spacing_Height     => 0,
                                Spacing_Width      => 1,
                                Number_Of_Dashes_V => 1,
                                Number_Of_Dashes_H => 2);
  end On_Create;

  procedure Apply_Options (Window : in out Project_Panel_Type) is
    use LEA_Common.Color_Themes;
  begin
    Window.Splitter.Background_Color (Color_Convert (Theme_Color (splitter_background)));
    Window.Splitter.Dashes_Color (Color_Convert (Theme_Color (splitter_dashes)));
  end Apply_Options;

  ---------------------
  --  Message Panel  --
  ---------------------

  overriding procedure On_Create (Window : in out Message_Panel_Type) is
    use GWindows.Base, GWindows.Common_Controls;
  begin
    Window.Dock (At_Bottom);
    --  Create widgets within the panel:
    Window.Splitter.Create (Window, At_Top, Height => 5);
    Window.Message_List.Create (Window, 1, 1, 20, 20, View => Report_View);

    Window.Apply_Options;
    Window.Splitter.Set_Dashes (Dash_Height        => 1,
                                Dash_Width         => 40,
                                Spacing_Height     => 1,
                                Spacing_Width      => 0,
                                Number_Of_Dashes_V => 2,
                                Number_Of_Dashes_H => 1);

  end On_Create;

  procedure Apply_Options (Window : in out Message_Panel_Type) is
    use LEA_Common.Color_Themes;
  begin
    Window.Splitter.Background_Color (Color_Convert (Theme_Color (splitter_background)));
    Window.Splitter.Dashes_Color (Color_Convert (Theme_Color (splitter_dashes)));
    Window.Message_List.Apply_Options;
    Window.Redraw;
  end Apply_Options;

  ------------------------
  --  Subprogram Panel  --
  ------------------------

  --  Nothing to be done yet

end LEA_GWin.Sliding_Panels;
