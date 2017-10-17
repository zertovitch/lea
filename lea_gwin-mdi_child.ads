with LEA_Common;

with LEA_GWin.Editor;
with LEA_GWin.MDI_Main;                 use LEA_GWin.MDI_Main;

with LEA_Resource_GUI;                  use LEA_Resource_GUI;

with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.Common_Controls.Ex_List_View;
with GWindows.Drawing;
with GWindows.GControls.GSize_Bars;
with GWindows.Packing_Boxes;
with GWindows.Panels;
with GWindows.Static_Controls;
with GWindows.Types;
with GWindows.Windows.MDI;
with GWindows.Windows;                  use GWindows.Windows;

with Ada.Numerics.Float_Random;

package LEA_GWin.MDI_Child is

  type MDI_Child_Type;
  type MDI_Child_Access is access all MDI_Child_Type;

  type MDI_Child_Status_bar_part is ( directory_info, task_message );

  type MDI_Child_Status_Bar_Type is
    new GWindows.Common_Controls.Status_Bar_Type with null record;

  type LV_Payload is record
    index_before_sorting: Integer;
  end record;

  package LEA_LV_Ex is new GWindows.Common_Controls.Ex_List_View(LV_Payload);

  type MDI_Child_Tree_View_Control_Type is new Tree_View_Control_Type with null record;
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

  type MDI_Child_GSize_Bar_Type is new GWindows.GControls.GSize_Bars.GSize_Bar_Type with null record;
  overriding procedure On_Bar_Moved (Window : in out MDI_Child_GSize_Bar_Type);

  type MDI_Child_Type is
    new GWindows.Windows.MDI.MDI_Child_Window_Type with
      record
        File_Name        : GString_Unbounded;
        Short_Name       : GString_Unbounded;
        -- ^ Window title = Short_Name & {""|" *"}
        Parent           : MDI_Main_Access; -- -> access to the containing window
        Extra_first_doc  : Boolean:= False;
        -- ^ new file closed if kept virgin when opening another one (like blank Excel sheet).
        Menu             : Menu_MDI_Child_Type;
        Tree_Bar_and_List: MDI_Child_Packing_Box_Type;
        Bar_and_List     : MDI_Child_Panel_Type;
        Editor           : LEA_GWin.Editor.LEA_Scintilla_Type;
        Splitter         : MDI_Child_GSize_Bar_Type;
        Splitter_dashes  : GWindows.Static_Controls.Label_Type;
        Folder_Tree      : aliased MDI_Child_Tree_View_Control_Type;
        selected_path    : GString_Unbounded:= Null_GString_Unbounded;
        --  opt              : Option_Pack_Type;  --  No per-child-window option in this app
        Status_Bar       : MDI_Child_Status_Bar_Type;
        name_search      : GString_Unbounded;
        content_search   : GString_Unbounded;
        current_password : GString_Unbounded;
        temp_name_gen    : Ada.Numerics.Float_Random.Generator;
        any_path_in_zip  : Boolean;
        extract_dir      : GString_Unbounded;
        last_sort_col    : Integer:= -1; -- -1 if none
        last_sort_direc  : LEA_LV_Ex.Sort_Direction_Type;
        refreshing_list  : Boolean:= False;
        is_closing       : Boolean:= False;  --  True only during and after On_Close
        last_op_comment_1: GString_Unbounded;
        last_op_comment_2: GString_Unbounded;
        save_all_hint    : Boolean;  --  Used during Update_display only.
      end record;

  overriding procedure On_Create (Window : in out MDI_Child_Type);

  procedure On_Save (Window : in out MDI_Child_Type);
  --  This would be abstract in a 'generic' Office framework.

  function Is_file_saved (Window : in MDI_Child_Type) return Boolean;
  --  This would be abstract in a 'generic' Office framework.

  procedure On_Save_As (Window : in out MDI_Child_Type);

  procedure On_Save_All (Window : in out MDI_Child_Type);

  overriding procedure On_File_Drop (
    Window     : in out MDI_Child_Type;
    File_Names : in     Array_Of_File_Names
  );

  overriding procedure On_Size (
    Window : in out MDI_Child_Type;
    Width  : in     Integer;
    Height : in     Integer
  );

  overriding procedure On_Erase_Background
     (Window : in out MDI_Child_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type) is null;
  overriding procedure On_Paint
     (Window : in out MDI_Child_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type) is null;

  overriding procedure On_Menu_Select (
    Window : in out MDI_Child_Type;
    Item   : in     Integer
  );

  overriding procedure On_Focus (Window : in out MDI_Child_Type);

  overriding procedure On_Close (
    Window    : in out MDI_Child_Type;
    Can_Close :    out Boolean
  );

  type Update_need is
    (first_display,   -- first display ever
     status_bar,      -- status bar, and topics listed below
     toolbar_and_menu -- update enable/disable of toolbar items and menu items
    );

  procedure Update_display (
    Window : in out MDI_Child_Type;
    need   :        Update_need
  );

  procedure Update_Common_Menus(Window : MDI_Child_Type;
                                top_entry : GString:= "" );

  procedure Show_Search_Box (MDI_Child : in out MDI_Child_Type);

  procedure Perform_Search (MDI_Child : in out MDI_Child_Type; action : LEA_Common.Search_action);

end LEA_GWin.MDI_Child;
