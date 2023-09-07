with LEA_GWin.Editor,
     LEA_GWin.MDI_Main,
     LEA_GWin.Sliding_Panels;

with LEA_Resource_GUI;

with LEA_Common;

with HAC_Sys.Builder;

with Office_Applications;

with GWindows.Common_Controls;
with GWindows.Drawing;
with GWindows.Packing_Boxes;
with GWindows.Panels;
with GWindows.Types;
with GWindows.Windows.MDI;

with Ada.Numerics.Float_Random;

package LEA_GWin.MDI_Child is

  type MDI_Child_Type;
  type MDI_Child_Access is access all MDI_Child_Type;

  type MDI_Child_Status_bar_part is (directory_info, task_message);

  type MDI_Child_Status_Bar_Type is
    new GWindows.Common_Controls.Status_Bar_Type with null record;

  --  Clicks on some some parts of the status bar have effects (like "Go to line")
  overriding procedure On_Click (Bar : in out MDI_Child_Status_Bar_Type);

  type MDI_Child_Tree_View_Control_Type is new GWindows.Common_Controls.Tree_View_Control_Type with null record;
  --  overriding procedure On_Selection_Change (Control : in out MDI_Child_Tree_View_Control_Type);
  --  overriding procedure On_Focus (Control : in out MDI_Child_Tree_View_Control_Type);

  type MDI_Child_Packing_Box_Type is new GWindows.Packing_Boxes.Packing_Box_Type with null record;

  overriding procedure On_Erase_Background
     (Window : in out MDI_Child_Packing_Box_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type) is null;

  overriding procedure On_Paint
     (Window : in out MDI_Child_Packing_Box_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type) is null;

  type MDI_Child_Panel_Type is new GWindows.Panels.Panel_Type with null record;
  overriding procedure On_Erase_Background
     (Window : in out MDI_Child_Panel_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type) is null;
  overriding procedure On_Paint
     (Window : in out MDI_Child_Panel_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type) is null;

  type MDI_Child_Type is
    new Office_Applications.Classic_Document_Window_Type with
      record
        ID                : ID_Type;
        --  Window title = ID.Short_Name & {""|" *"}
        mdi_root          : LEA_GWin.MDI_Main.MDI_Main_Access; -- -> access to the containing window
        menu              : LEA_Resource_GUI.Menu_MDI_Child_Type;
        --  Tree_Bar_and_List: MDI_Child_Packing_Box_Type;
        Editor            : LEA_GWin.Editor.LEA_Scintilla_Type;
        context_menu      : GWindows.Menus.Menu_Type := GWindows.Menus.Null_Menu;
        Subprogram_Panel  : Sliding_Panels.Subprogram_Panel_Type;
        selected_path     : GString_Unbounded := Null_GString_Unbounded;
        --  opt              : Option_Pack_Type;  --  No per-child-window option in this app
        Status_Bar        : MDI_Child_Status_Bar_Type;
        name_search       : GString_Unbounded;
        content_search    : GString_Unbounded;
        current_password  : GString_Unbounded;
        temp_name_gen     : Ada.Numerics.Float_Random.Generator;
        any_path_in_zip   : Boolean;
        extract_dir       : GString_Unbounded;
        refreshing_list   : Boolean := False;
        is_closing        : Boolean := False;  --  True only during and after On_Close
        last_op_comment_1 : GString_Unbounded;
        last_op_comment_2 : GString_Unbounded;
        save_all_hint     : Boolean;  --  Used during Update_Information only.
      end record;

  overriding procedure On_Create (Window : in out MDI_Child_Type);

  procedure Create_LEA_MDI_Child
    (Window : in out MDI_Child_Type;
     Parent : in out MDI_Main.MDI_Main_Type;
     ID     : in     ID_Type);

  procedure Finish_subwindow_opening (Window : in out MDI_Child_Type);

  procedure On_Save (Window : in out MDI_Child_Type);

  overriding function Is_Document_Modified (Window : in MDI_Child_Type) return Boolean;

  procedure On_Save_As (Window : in out MDI_Child_Type);

  procedure On_Save_All (Window : in out MDI_Child_Type);

  overriding procedure On_File_Drop (
    Window     : in out MDI_Child_Type;
    File_Names : in     GWindows.Windows.Array_Of_File_Names
  );

  overriding procedure On_Size (
    Window : in out MDI_Child_Type;
    Width  : in     Integer;
    Height : in     Integer
  );

  overriding procedure On_Erase_Background
     (Window : in out MDI_Child_Type;
      Canvas    : in out GWindows.Drawing.Canvas_Type;
      Area      : in     GWindows.Types.Rectangle_Type) is null;

  overriding procedure On_Paint
     (Window : in out MDI_Child_Type;
      Canvas    : in out GWindows.Drawing.Canvas_Type;
      Area      : in     GWindows.Types.Rectangle_Type) is null;

  overriding procedure On_Menu_Select (
    Window : in out MDI_Child_Type;
    Item      : in     Integer
  );

  overriding procedure On_Focus (Window : in out MDI_Child_Type);

  overriding procedure On_Context_Menu
    (Window : in out MDI_Child_Type;
     X      : in     Integer;
     Y      : in     Integer);

  overriding procedure On_Close (
    Window    : in out MDI_Child_Type;
    Can_Close :    out Boolean
  );

  type Update_need is
    (first_display,   -- first display ever
     status_bar,      -- status bar, and topics listed below
     toolbar_and_menu -- update enable/disable of toolbar items and menu items
    );

  procedure Update_Information
    (Window : in out MDI_Child_Type;
     need   :        Update_need);

  --  This will update File menu of parent, itself, and all brothers and sisters
  procedure Update_Common_Menus (Window : MDI_Child_Type;
    top_entry_name : GString := "";
    top_entry_line : Natural := 0    --  When unknown, 0; otherwise: last visited line
  );

  procedure Show_Search_Box (Window : in out MDI_Child_Type);

  function Best_Name (Window : MDI_Child_Type) return GString;
  procedure Switch_Current_Directory (Window : MDI_Child_Type);

  --  Compile / Build actions

  procedure Build         (Window : in out MDI_Child_Type);
  procedure Build_as_Main (Window : in out MDI_Child_Type);
  procedure Build_and_run (Window : in out MDI_Child_Type);

end LEA_GWin.MDI_Child;
