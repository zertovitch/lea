with LEA_GWin.Editor,
     LEA_GWin.Messages,
     LEA_GWin.Search_box,
     LEA_GWin.Sliding_Panels;

with LEA_Resource_GUI;

with LEA_Common.User_options;

with GWindows.Common_Controls;
with GWindows.Drawing;
with GWindows.Image_Lists;
with GWindows.Taskbar;
with GWindows.Types;
with GWindows.Windows.MDI;

with GWin_Util;

with Interfaces.C;

package LEA_GWin.MDI_Main is

  type MDI_Toolbar_Type is
    new GWindows.Common_Controls.Toolbar_Control_Type with null record;

  --  Handle clicks on toolbar:
  overriding procedure On_Button_Select (Control : in out MDI_Toolbar_Type;
                                         Item    : in     Integer);

  type MDI_Main_Type;
  type MDI_Main_Access is access all MDI_Main_Type;

  type MDI_Tab_Bar_Type is
    new GWindows.Common_Controls.Tab_Control_Type with
      record
        MDI_Parent : MDI_Main_Access;  --  -> access to the containing window
        ID         : ID_Vectors.Vector;
      end record;

  overriding procedure On_Change (Control : in out MDI_Tab_Bar_Type);

  function Tab_Index (Control : in out MDI_Tab_Bar_Type; ID : ID_Type) return Integer;

  type IDM_MRU_List is array(LEA_Common.User_options.MRU_List'Range) of Natural;

  type MDI_Main_Type is
    new GWindows.Windows.MDI.MDI_Main_Window_Type with
      record
        Project_File_Name      : GString_Unbounded;
        Project_Short_Name     : GString_Unbounded;
        Success_in_enumerated_close: Boolean;
        --  MRU (Most recently used) files names:
        --  Menu ID's stored into a handy array
        IDM_MRU                : IDM_MRU_List;
        Tool_Bar               : MDI_Toolbar_Type;
        Toolbar_Images         : GWindows.Image_Lists.Image_List_Type;
        Folders_Images         : GWindows.Image_Lists.Image_List_Type;
        --
        Tab_Bar                : MDI_Tab_Bar_Type;
        --
        Project_Panel          : Sliding_Panels.Project_Panel_Type;
        Message_Panel          : Sliding_Panels.Message_Panel_Type;
        --
        Menu                   : LEA_Resource_GUI.Menu_MDI_Main_Type;
        --  record_dimensions      : Boolean:= False; -- in On_Move, On_Size
        User_maximize_restore  : Boolean:= True;
        --  ^ Detect user-triggered max/restore commands
        record_dimensions      : Boolean:= False; -- in On_Move, On_Size
        --  Options of a "model" child window.
        opt                    : LEA_Common.User_options.Option_Pack_Type;
        --
        Task_bar_gadget_ok     : Boolean := False;  --  Coloring of taskbar icon (Windows 7+)
        Task_bar_gadget        : GWindows.Taskbar.Taskbar_List;
        --
        Search_box             : LEA_GWin.Search_box.LEA_search_box_type;
        --
        is_closing             : Boolean:= False;  --  True only during and after On_Close
        --  Direct input stream from an editor window:
        current_editor_stream  : aliased Editor.Editor_Stream_Type;
        build_successful       : Boolean := False;
        close_this_search_box  : Boolean := False;
        pragma Volatile (close_this_search_box);
        --
        text_files_filters     : GWindows.Common_Dialogs.Filter_Array
                                   (Initial_text_files_filters'Range):=
                                      Initial_text_files_filters;
      end record;

  overriding procedure On_Create (Window : in out MDI_Main_Type);
  --  Handles setting up icons, menus, etc.

  procedure On_File_New (Window : in out MDI_Main_Type; extra_first_doc: Boolean);
  --  File|New event

  procedure On_Move (Window : in out MDI_Main_Type;
                     Left   : in     Integer;
                     Top    : in     Integer);

  overriding procedure On_Size (Window : in out MDI_Main_Type;
                                Width  : in     Integer;
                                Height : in     Integer);

  overriding procedure On_File_Drop (Window     : in out MDI_Main_Type;
                                     File_Names : in     GWindows.Windows.Array_Of_File_Names);

  overriding procedure On_Erase_Background
     (Window : in out MDI_Main_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type) is null;

  procedure Redraw_all (Window: in out MDI_Main_Type);

  procedure Open_Child_Window_And_Load
    (Window       : in out MDI_Main_Type;
     File_Name    :        GString;
     Line         :        Integer := -1;
     Col_a, Col_z :        Integer := -1);

  overriding procedure On_Menu_Select
    (Window : in out MDI_Main_Type;
     Item   : in     Integer);

  overriding
  procedure On_Message (Window       : in out MDI_Main_Type;
                        message      : in     Interfaces.C.unsigned;
                        wParam       : in     GWindows.Types.Wparam;
                        lParam       : in     GWindows.Types.Lparam;
                        Return_Value : in out GWindows.Types.Lresult);

  overriding procedure On_Close (
        Window    : in out MDI_Main_Type;
        Can_Close :    out Boolean        );

  procedure Update_Common_Menus(
    Window         : in out MDI_Main_Type;
    top_entry_name :        GString := "";
    top_entry_line :        Integer := -1    --  When unknown, -1; otherwise: last visited line
  );

  procedure Update_Title (Window : in out MDI_Main_Type);

  procedure Perform_Search (Window : MDI_Main_Type; action : LEA_Common.Search_action);

  --  One or more splitter position may have changed, either directly or via a window
  --  resize. Time is to record the proportions.
  procedure Memorize_Splitters (Window : in out MDI_Main_Type);

end LEA_GWin.MDI_Main;
