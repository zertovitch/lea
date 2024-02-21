with LEA_GWin.Editor.Streaming,
     LEA_GWin.Tabs,
     LEA_GWin.Messages,
     LEA_GWin.Search_Box,
     LEA_GWin.Sliding_Panels;

with LEA_Resource_GUI;

with LEA_Common.User_options;

with HAC_Sys.Builder,
     HAC_Sys.Targets.HAC_Virtual_Machine,
     HAC_Sys.Targets.Semantics;

with Office_Applications;

with GWindows.Common_Controls,
     GWindows.Drawing,
     GWindows.Image_Lists,
     GWindows.Scintilla,
     GWindows.Taskbar,
     GWindows.Types,
     GWindows.Windows.MDI;

with GWin_Util;

with Ada.Containers.Vectors;

with Interfaces.C;

package LEA_GWin.MDI_Main is

  subtype Pair is Integer range 1 .. 2;

  type Declaration_Point_Pair is
    array (Pair) of HAC_Sys.Targets.Semantics.Declaration_Point;

  type MDI_Main_Type;
  type MDI_Main_Access is access all MDI_Main_Type;

  arg_no_start_line : constant := -1;

  type MDI_Main_Type is
    new Office_Applications.Classic_Main_Window_Type with
      record
        Project_File_Name           : GString_Unbounded;
        Project_Short_Name          : GString_Unbounded;
        Success_in_enumerated_close : Boolean;
        Folders_Images              : GWindows.Image_Lists.Image_List_Type;
        --
        Tab_Bar                     : Tabs.LEA_Tab_Bar_Type;
        --
        Project_Panel               : Sliding_Panels.Project_Panel_Type;
        Message_Panel               : Sliding_Panels.Message_Panel_Type;
        --
        Menu                        : LEA_Resource_GUI.Menu_MDI_Main_Type;
        --  record_dimensions : Boolean:= False; -- in On_Move, On_Size
        User_maximize_restore       : Boolean := True;
        --  ^ Detect user-triggered max/restore commands
        record_dimensions           : Boolean := False; -- in On_Move, On_Size
        --  Options of a "model" child window.
        opt                         : LEA_Common.User_options.Option_Pack_Type;
        --
        Task_bar_gadget_ok          : Boolean := False;  --  Coloring of taskbar icon (Windows 7+)
        Task_bar_gadget             : GWindows.Taskbar.Taskbar_List;
        --
        search_box                  : LEA_GWin.Search_Box.LEA_Search_Box_Type;
        build_successful            : Boolean := False;
        close_this_search_box       : Boolean := False;
        pragma Volatile (close_this_search_box);
        --
        text_files_filters          : GWindows.Common_Dialogs.Filter_Array
                                        (Initial_text_files_filters'Range) :=
                                           Initial_text_files_filters;
        --  HAC Build, activated manually:
        BD                          : HAC_Sys.Builder.Build_Data;
        --  HAC Semantics analysis (will be moved to a daemon):
        BD_sem                      : HAC_Sys.Builder.Build_Data;
        sem_machine                 : HAC_Sys.Targets.Semantics.Semantics_Machine_Reference :=
                                        new HAC_Sys.Targets.Semantics.Machine;
        memo_declaration            : Declaration_Point_Pair;
        memo_other_file             : GString_Unbounded;
        lea_file_cat                : aliased LEA_GWin.Editor.Streaming.LEA_File_Catalogue;
        --
        next_arg_start_line         : Integer := arg_no_start_line;
      end record;

  overriding procedure On_Create (Window : in out MDI_Main_Type);
  --  Handles setting up icons, menus, etc.

  procedure On_File_New (Window : in out MDI_Main_Type; extra_first_doc : Boolean);
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

  procedure Redraw_all (Window : in out MDI_Main_Type);

  procedure Open_Child_Window_And_Load
    (Window       : in out MDI_Main_Type;
     File_Name    :        GString;
     Line         :        Integer := -1;
     Col_a, Col_z :        Integer := -1);

  procedure Process_Argument
    (Window   : in out MDI_Main_Type;
     Position : in     Positive;
     Arg      : in     String);

  overriding procedure On_Menu_Select
    (Window : in out MDI_Main_Type;
     Item   : in     Integer);

  overriding
  procedure On_Message (Window       : in out MDI_Main_Type;
                        message      : in     Interfaces.C.unsigned;
                        wParam       : in     GWindows.Types.Wparam;
                        lParam       : in     GWindows.Types.Lparam;
                        Return_Value : in out GWindows.Types.Lresult);

  overriding procedure On_Close
    (Window    : in out MDI_Main_Type;
     Can_Close :    out Boolean);

  procedure Focus_an_already_opened_window
    (Window       : in out MDI_Main_Type;
     ID           :        ID_Type;
     Line         :        Integer                     := GWindows.Scintilla.INVALID_POSITION;
     Col_a, Col_z :        GWindows.Scintilla.Position := GWindows.Scintilla.INVALID_POSITION;
     is_open      :    out Boolean);

  procedure Go_to_memorized_Declaration (Window : in out MDI_Main_Type; number : Pair);

  procedure Update_Common_Menus
    (Window         : in out MDI_Main_Type;
     top_entry_name :        GString := "";
     top_entry_line :        Integer := -1);    --  When unknown, -1; otherwise: last visited line

  procedure Update_Title (Window : in out MDI_Main_Type);

  procedure Perform_Search (Window : MDI_Main_Type; action : LEA_Common.Search_action);

  --  One or more splitter position may have changed, either directly or via a window
  --  resize. Time is to record the proportions.
  procedure Memorize_Splitters (Window : in out MDI_Main_Type);

end LEA_GWin.MDI_Main;
