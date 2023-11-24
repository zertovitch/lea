with LEA_GWin.Messages;

with GWindows.Common_Controls,
     GWindows.Panels,
     GWindows.Windows.MDI,
     GWindows.GControls.GSize_Bars;

package LEA_GWin.Sliding_Panels is

  type LEA_splitter is new GWindows.GControls.GSize_Bars.GSize_Bar_Type with record
    MDI_Main : GWindows.Windows.MDI.Pointer_To_MDI_Main_Window_Class;
  end record;

  overriding procedure On_Bar_Moved (Splitter : in out LEA_splitter);

  ---------------------
  --  Project Panel  --
  ---------------------

  type Project_Panel_Type is new GWindows.Panels.Panel_Type with record
    Project_Tree : GWindows.Common_Controls.Tree_View_Control_Type;
    Splitter     : LEA_splitter;
  end record;

  overriding procedure On_Create (Window : in out Project_Panel_Type);

  procedure Apply_Options (Window : in out Project_Panel_Type);

  ---------------------
  --  Message Panel  --
  ---------------------

  type Message_Panel_Type is new GWindows.Panels.Panel_Type with record
    Message_List : LEA_GWin.Messages.Message_List_Type;
    Splitter     : LEA_splitter;
  end record;

  overriding procedure On_Create (Window : in out Message_Panel_Type);

  procedure Apply_Options (Window : in out Message_Panel_Type);

  ------------------------
  --  Subprogram Panel  --
  ------------------------

  type Subprogram_Panel_Type is new GWindows.Panels.Panel_Type with record
    Subprogram_Tree : GWindows.Common_Controls.Tree_View_Control_Type;
    Splitter        : LEA_splitter;
  end record;

end LEA_GWin.Sliding_Panels;
