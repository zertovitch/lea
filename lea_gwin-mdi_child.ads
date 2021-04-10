with LEA_Common;                        use LEA_Common;

with LEA_GWin.Editor;
with LEA_GWin.MDI_Main;                 use LEA_GWin.MDI_Main;

with LEA_Resource_GUI;                  use LEA_Resource_GUI;

with HAC_Sys.Builder;

with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.Drawing;
with GWindows.Packing_Boxes;
with GWindows.Panels;
with GWindows.Types;
with GWindows.Windows.MDI;
with GWindows.Windows;                  use GWindows.Windows;

with Ada.Numerics.Float_Random;

package LEA_GWin.MDI_Child is

  type MDI_Child_Type;
  type MDI_Child_Access is access all MDI_Child_Type;

  type MDI_Child_Status_bar_part is (directory_info, task_message);

  type MDI_Child_Status_Bar_Type is
    new GWindows.Common_Controls.Status_Bar_Type with null record;

  --  Clicks on some some parts of the status bar have effects (like "Go to line")
  overriding procedure On_Click (Bar : in out MDI_Child_Status_Bar_Type);

  type MDI_Child_Tree_View_Control_Type is new Tree_View_Control_Type with null record;
  --  overriding procedure On_Selection_Change (Control : in out MDI_Child_Tree_View_Control_Type);
  --  overriding procedure On_Focus (Control : in out MDI_Child_Tree_View_Control_Type);

  type MDI_Child_Packing_Box_Type is new GWindows.Packing_Boxes.Packing_Box_Type with null record;
  overriding procedure On_Erase_Background
     (MDI_Child : in out MDI_Child_Packing_Box_Type;
      Canvas    : in out GWindows.Drawing.Canvas_Type;
      Area      : in     GWindows.Types.Rectangle_Type) is null;
  overriding procedure On_Paint
     (MDI_Child : in out MDI_Child_Packing_Box_Type;
      Canvas    : in out GWindows.Drawing.Canvas_Type;
      Area      : in     GWindows.Types.Rectangle_Type) is null;

  type MDI_Child_Panel_Type is new GWindows.Panels.Panel_Type with null record;
  overriding procedure On_Erase_Background
     (MDI_Child : in out MDI_Child_Panel_Type;
      Canvas    : in out GWindows.Drawing.Canvas_Type;
      Area      : in     GWindows.Types.Rectangle_Type) is null;
  overriding procedure On_Paint
     (MDI_Child : in out MDI_Child_Panel_Type;
      Canvas    : in out GWindows.Drawing.Canvas_Type;
      Area      : in     GWindows.Types.Rectangle_Type) is null;

  type Subprogram_Panel_Type is new GWindows.Panels.Panel_Type with record
    Subprogram_Tree : GWindows.Common_Controls.Tree_View_Control_Type;
    Splitter        : LEA_splitter;
  end record;

  type MDI_Child_Type is
    new GWindows.Windows.MDI.MDI_Child_Window_Type with
      record
        File_Name        : GString_Unbounded;
        --  Window title = Short_Name & {""|" *"}
        Short_Name       : GString_Unbounded;
        MDI_Parent       : MDI_Main_Access; -- -> access to the containing window
        --  new file closed if kept virgin when opening another one (like blank Excel sheet).
        Extra_first_doc  : Boolean := False;
        Menu             : Menu_MDI_Child_Type;
        --  Tree_Bar_and_List: MDI_Child_Packing_Box_Type;
        Document_kind    : Document_kind_type := editable_text;
        Editor           : LEA_GWin.Editor.LEA_Scintilla_Type;
        Subprogram_Panel : Subprogram_Panel_Type;
        selected_path    : GString_Unbounded := Null_GString_Unbounded;
        --  opt              : Option_Pack_Type;  --  No per-child-window option in this app
        Status_Bar       : MDI_Child_Status_Bar_Type;
        name_search      : GString_Unbounded;
        content_search   : GString_Unbounded;
        current_password : GString_Unbounded;
        temp_name_gen    : Ada.Numerics.Float_Random.Generator;
        any_path_in_zip  : Boolean;
        extract_dir      : GString_Unbounded;
        refreshing_list  : Boolean := False;
        is_closing       : Boolean := False;  --  True only during and after On_Close
        last_op_comment_1: GString_Unbounded;
        last_op_comment_2: GString_Unbounded;
        save_all_hint    : Boolean;  --  Used during Update_display only.
        --
        BD               : HAC_Sys.Builder.Build_Data;
      end record;

  overriding procedure On_Create (MDI_Child : in out MDI_Child_Type);

  procedure Finish_subwindow_opening (MDI_Child : in out MDI_Child_Type);

  procedure On_Save (MDI_Child : in out MDI_Child_Type);
  --  This would be abstract in a 'generic' Office framework.

  function Is_file_saved (MDI_Child : in MDI_Child_Type) return Boolean;
  --  This would be abstract in a 'generic' Office framework.

  procedure On_Save_As (MDI_Child : in out MDI_Child_Type);

  procedure On_Save_All (MDI_Child : in out MDI_Child_Type);

  overriding procedure On_File_Drop (
    MDI_Child  : in out MDI_Child_Type;
    File_Names : in     Array_Of_File_Names
  );

  overriding procedure On_Size (
    MDI_Child : in out MDI_Child_Type;
    Width  : in     Integer;
    Height : in     Integer
  );

  overriding procedure On_Erase_Background
     (MDI_Child : in out MDI_Child_Type;
      Canvas    : in out GWindows.Drawing.Canvas_Type;
      Area      : in     GWindows.Types.Rectangle_Type) is null;
  overriding procedure On_Paint
     (MDI_Child : in out MDI_Child_Type;
      Canvas    : in out GWindows.Drawing.Canvas_Type;
      Area      : in     GWindows.Types.Rectangle_Type) is null;

  overriding procedure On_Menu_Select (
    MDI_Child : in out MDI_Child_Type;
    Item      : in     Integer
  );

  overriding procedure On_Focus (MDI_Child : in out MDI_Child_Type);

  overriding procedure On_Close (
    MDI_Child    : in out MDI_Child_Type;
    Can_Close :    out Boolean
  );

  type Update_need is
    (first_display,   -- first display ever
     status_bar,      -- status bar, and topics listed below
     toolbar_and_menu -- update enable/disable of toolbar items and menu items
    );

  procedure Update_display (
    MDI_Child : in out MDI_Child_Type;
    need      :        Update_need
  );

  --  This will update File menu of parent, itself, and all brothers and sisters
  procedure Update_Common_Menus (MDI_Child : MDI_Child_Type;
    top_entry_name : GString := "";
    top_entry_line : Natural := 0    --  When unknown, 0; otherwise: last visited line
  );

  procedure Show_Search_Box (MDI_Child : in out MDI_Child_Type);

  --  Compile / Build actions

  procedure Check_syntax   (MDI_Child : in out MDI_Child_Type);
  procedure Build_as_Main (MDI_Child : in out MDI_Child_Type);
  procedure Build          (MDI_Child : in out MDI_Child_Type);
  procedure Build_and_run  (MDI_Child : in out MDI_Child_Type);

end LEA_GWin.MDI_Child;
