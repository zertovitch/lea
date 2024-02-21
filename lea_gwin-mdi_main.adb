with LEA_Common.Syntax;

with LEA_GWin.Embedded_Texts,
     LEA_GWin.MDI_Child,
     LEA_GWin.Modal_Dialogs,
     LEA_GWin.Options,
     LEA_GWin.Persistence,
     LEA_GWin.Toolbars;

with HAT;

with GWindows.Application,
     GWindows.Base,
     GWindows.Common_Dialogs,
     GWindows.Constants,
     GWindows.Cursors,
     GWindows.Menus,
     GWindows.Message_Boxes,
     GWindows.Registry,
     GWindows.Timers;

with Ada.Command_Line,
     Ada.Directories,
     Ada.Strings.Fixed,
     Ada.Text_IO,
     Ada.Unchecked_Deallocation,
     Ada.Wide_Characters.Handling;
with HAC_Sys.Librarian;

package body LEA_GWin.MDI_Main is

  use type Scintilla.Position;
  use LEA_Common, LEA_GWin.MDI_Child;
  use GWindows.Base, GWindows.Menus;

  procedure Focus_an_already_opened_window
    (Window       : in out MDI_Main_Type;
     ID           :        ID_Type;
     Line         :        Integer            := GWindows.Scintilla.INVALID_POSITION;
     Col_a, Col_z :        Scintilla.Position := GWindows.Scintilla.INVALID_POSITION;
     is_open      :    out Boolean)
  is
    procedure Identify (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
    begin
      if Any_Window /= null and then Any_Window.all in MDI_Child_Type'Class then
        declare
          pw : MDI_Child_Type renames MDI_Child_Type (Any_Window.all);
          new_pos_a, new_pos_z : GWindows.Scintilla.Position;
        begin
          if Equivalent (pw.ID, ID) then
            is_open := True;
            pw.Set_Foreground_Window;
            pw.Focus;  --  Focus on document already open in our app.
            --  Scintilla lines are 0-based
            if Line > -1 then
              pw.editor.Set_Current_Line (Line);
            end if;
            if Col_a > -1 then
              new_pos_a := pw.editor.Get_Current_Pos + Col_a;
              new_pos_z := pw.editor.Get_Current_Pos + Col_z;
              pw.editor.Set_Sel (new_pos_a, new_pos_z);
            end if;
          end if;
        end;
      end if;
    end Identify;

  begin
    is_open := False;
    Enumerate_Children
      (MDI_Client_Window (Window).all,
       Identify'Unrestricted_Access);
  end Focus_an_already_opened_window;

  procedure Go_to_memorized_Declaration (Window : in out MDI_Main_Type; number : Pair) is
    decl : Declaration_Point_Pair renames Window.memo_declaration;
  begin
    if not decl (1).is_built_in then
      Window.Open_Child_Window_And_Load
        (S2G (HAT.To_String (decl (number).file_name)),
         decl (number).line - 1,  --  Scintilla's lines are 0-based
         decl (number).column - 1,
         decl (number).column - 1);
    end if;
  end Go_to_memorized_Declaration;

  procedure Redraw_Child (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
  is
  begin
    if Any_Window /= null and then Any_Window.all in MDI_Child_Type'Class then
      --  !! some content refresh, dbl buffering
      Any_Window.Redraw;
    end if;
  end Redraw_Child;

  procedure Redraw_all (Window : in out MDI_Main_Type) is
  begin
    Window.Redraw;
    --  Redraw(Window.Tool_bar);
    Enumerate_Children (MDI_Client_Window (Window).all, Redraw_Child'Access);
  end Redraw_all;

  procedure Open_Child_Window_And_Load
    (Window       : in out MDI_Main_Type;
     File_Name,
     File_Title   :        GString;
     Line         :        Integer            := -1;
     Col_a, Col_z :        Scintilla.Position := -1)
  is
    is_open, file_loaded : Boolean;
    mru_line : Integer := -1;
    new_pos_a, new_pos_z : GWindows.Scintilla.Position;
    New_ID : ID_Type;
    New_Window : MDI_Child_Access;
    full_name : constant GString := S2G (Ada.Directories.Full_Name (G2S (File_Name)));
    use GWindows.Message_Boxes;
    use type GString_Unbounded;
  begin
    New_ID := (G2GU (full_name), G2GU (File_Title));
    Focus_an_already_opened_window (Window, New_ID, Line, Col_a, Col_z, is_open);
    if is_open then
      return;        -- nothing to do, document already in a window
    end if;
    New_Window := new MDI_Child_Type;
    --  We do here like Excel or Word: close the unused blank window
    Window.Close_Initial_Document;
    --
    Window.User_maximize_restore := False;
    New_Window.Create_LEA_MDI_Child (Window, New_ID);
    declare
      upper_name : GString := File_Name;
    begin
      New_Window.editor.Load_Text;
      file_loaded := True;
      To_Upper (upper_name);
      for m of Window.MRU.Item loop
        if Ada.Wide_Characters.Handling.To_Upper (GU2G (m.Name)) = upper_name then
          mru_line := m.Line;
          exit;
        end if;
      end loop;
      --  This will put MRU item on top.
      Update_Common_Menus (Window, File_Name, mru_line);
    exception
      when Ada.Text_IO.Name_Error =>
        file_loaded := False;
    end;
    New_Window.Finish_subwindow_opening;
    New_Window.editor.syntax_kind :=
      LEA_Common.Syntax.Guess_syntax (
        GU2G (New_Window.ID.File_Name),
        GU2G (Window.opt.ada_files_filter)
      );
    New_Window.editor.Set_Scintilla_Syntax;
    New_Window.editor.Focus;
    --  NB: Scintilla lines are 0-based
    if Line > -1 then
      New_Window.editor.Set_Current_Line (Line);
    elsif mru_line > -1 then
      --  Set cursor position to memorized line number
      New_Window.editor.Set_Current_Line (mru_line);
    end if;
    if Col_a > -1 then
      new_pos_a := New_Window.editor.Get_Current_Pos + Col_a;
      new_pos_z := New_Window.editor.Get_Current_Pos + Col_z;
      New_Window.editor.Set_Sel (new_pos_a, new_pos_z);
    end if;
    if file_loaded then
      New_Window.Set_Foreground_Window;
    else
      Message_Box
        (Window, "Error", "File " & File_Name & " not found", Icon => Exclamation_Icon);
      --  Remove tab (would be done by On_Close if ID.File_Name was
      --  not erased before closing the subwindow).
      Window.Tab_Bar.Delete_Tab (Window.Tab_Bar.Tab_Index (New_Window.ID));
      --  Prevent MRU name addition:
      New_Window.ID.File_Name := Null_GString_Unbounded;
      New_Window.Close;
    end if;
  end Open_Child_Window_And_Load;

  procedure Open_Child_Window_And_Load
    (Window       : in out MDI_Main_Type;
     File_Name    :        GString;
     Line         :        Integer := -1;
     Col_a, Col_z :        Integer := -1)
  is
  begin
    Open_Child_Window_And_Load
      (Window,
       File_Name,
       Office_Applications.Shorten_File_Name (File_Name, 50),
       Line,
       Scintilla.Position (Col_a),
       Scintilla.Position (Col_z));
  end Open_Child_Window_And_Load;

  procedure Process_Argument
    (Window   : in out MDI_Main_Type;
     Position : in     Positive;
     Arg      : in     String)
  is
  begin
    if Position = 1 then
      Window.next_arg_start_line := arg_no_start_line;
    end if;
    if Arg (Arg'First) = '+' then  --  Emacs syntax: +linenum filename
      Window.next_arg_start_line := 0;
      for j in Arg'First + 1 .. Arg'Last loop
        if Arg (j) in '0' .. '9' then
          Window.next_arg_start_line :=
            Window.next_arg_start_line * 10 +
              (Character'Pos (Arg (j)) - Character'Pos ('0'));
        else
          --  Invalid number after '+'.
          Window.next_arg_start_line := arg_no_start_line;
          exit;
        end if;
      end loop;
    else
      Window.Open_Child_Window_And_Load
        (LEA_Common.To_UTF_16 (Arg),
         Window.next_arg_start_line - 1);  --  NB: Scintilla lines are 0-based
      Window.next_arg_start_line := arg_no_start_line;
    end if;
  end Process_Argument;

  --  Switch between Notepad and Studio views
  --
  procedure Change_View (
        MDI_Main  : in out MDI_Main_Type;
        new_view  :        View_Mode_Type;
        force     :        Boolean
  )
  is
    old_view : constant View_Mode_Type := MDI_Main.opt.view_mode;
    --  mem_sel_path: constant GString_Unbounded:= MDI_Child.selected_path;
    --  sel_node: Tree_Item_Node;
  begin
    if old_view = new_view and not force then
      return;
    end if;
    MDI_Main.opt.view_mode := new_view;
    case new_view is
      when Notepad =>
        if old_view /= Notepad then
          --  Remember tree portion before hiding for user
          --  persistence and for next time we toggle back to Studio view.
          MDI_Main.Memorize_Splitters;
        end if;
        MDI_Main.Project_Panel.Width (0);
        MDI_Main.Project_Panel.Hide;
      when Studio =>
        MDI_Main.Project_Panel.Show;
    end case;
    --  Call to On_Size for having splitters adjusted
    MDI_Main.On_Size (MDI_Main.Width, MDI_Main.Height);
    --  (needed??) Update_display(MDI_Child, status_bar);
    case new_view is
      when Notepad =>
        null;
      when Studio =>
        null;
          --  (tree) MDI_Child.Folder_Tree.Select_Item(sel_node);
          --  (tree) Update_display(MDI_Child, node_selected); -- !! update done twice, once for remapping folders
          --  (tree) MDI_Child.Folder_Tree.Expand(sel_node);
          --  (tree) MDI_Child.Folder_Tree.Focus;
    end case;
  end Change_View;

  --  Switch between HAC and real Ada toolsets
  --
  procedure Change_Mode (
    MDI_Main  : in out MDI_Main_Type;
    new_mode  :        Toolset_mode_type
  )
  is
  begin
    MDI_Main.opt.toolset := new_mode;
    MDI_Main.Update_Common_Menus;
  end Change_Mode;

  search_box_timer_id : constant := 1;
  file_synch_timer_id : constant := 2;

  ---------------
  -- On_Create --
  ---------------

  procedure On_Create (Window : in out MDI_Main_Type) is
    use GWindows.Common_Controls, Ada.Command_Line;
    --
    --  Replace LEA default values by system-dependent ones (here those of GWindows)
    --
    procedure Replace_default (x : in out Integer) is
    begin
      if x = LEA_Common.User_options.use_default then
        x := GWindows.Constants.Use_Default;
      end if;
    end Replace_default;
    --
    start_line : Integer := -1;
    use GWindows.Application, GWindows.Taskbar, GWindows.Image_Lists, LEA_Resource_GUI;
  begin
    Persistence.Blockwise_IO.Load (Window.opt);
    for m in Window.MRU.Item'Range loop
      Window.MRU.Item (m) :=
        (Name => Window.opt.mru (m).name,
         Line => Window.opt.mru (m).line);
    end loop;
    --
    Replace_default (Window.opt.win_left);
    Replace_default (Window.opt.win_width);
    Replace_default (Window.opt.win_top);
    Replace_default (Window.opt.win_height);

    Small_Icon (Window, "LEA_Icon_Small");
    Large_Icon (Window, "AAA_Main_Icon");

    --  ** Menus and accelerators:
    --
    LEA_Resource_GUI.Create_Full_Menu (Window.Menu);
    MDI_Menu (Window, Window.Menu.Main, Window_Menu => 5);
    Accelerator_Table (Window, "Main_Menu");
    Window.MRU.ID_Menu :=
      (IDM_MRU_1,       IDM_MRU_2,       IDM_MRU_3,       IDM_MRU_4,
       IDM_MRU_5,       IDM_MRU_6,       IDM_MRU_7,       IDM_MRU_8,
       IDM_MRU_9
      );

    --  ** Other resources
    Window.Folders_Images.Create
      (Num_resource (Folders_BMP), 16, Color_Option => Copy_From_Resource);

    --  ** Main tool bar (New / Open / Save / ...) at top left of the main window:
    LEA_GWin.Toolbars.Init_Main_Tool_Bar (Window.Tool_Bar, Window);
    --  ** Main's tab bar:
    Window.Tab_Bar.MDI_Parent := Window'Unrestricted_Access;
    Window.Tab_Bar.Create (Window, 0, 30, 10, 25);
    Window.Tab_Bar.Dock (GWindows.Base.At_Top);
    GWin_Util.Use_GUI_Font (Window.Tab_Bar);
    --  Tool Tips for the Tab bar:
    Window.Tab_Bar.tips.Create (Window);
    Window.Tab_Bar.Set_Tool_Tips (Window.Tab_Bar.tips);
    GWin_Util.Use_GUI_Font (Window.Tab_Bar.tips);
    Window.Tab_Bar.tips.Set_Durations
      (Initial  => 0.2,
       Reshow   => 0.1,
       Til_Hide => 5.0);

    --  ** Sizeable panels. For a sketch, see the "Layout" sheet in lea_work.xls.
    --
    --    1) Left panel, with project or file tree:
    --
    Window.Project_Panel.Splitter.MDI_Main := Window'Unrestricted_Access;
    Window.Project_Panel.Create (Window, 1, 1, 20, 20);
    --
    --    2) Bottom panel, with messages:
    --
    Window.Message_Panel.Splitter.MDI_Main := Window'Unrestricted_Access;
    Window.Message_Panel.Message_List.mdi_main_parent := Window'Unrestricted_Access;
    Window.Message_Panel.Create (Window, 1, 1, 20, 80);
    Window.Message_Panel.Message_List.Set_Image_List (Small, Window.Folders_Images);

    --  ** Resize according to options:

    if Screen_Visibility ((Window.opt.win_left, Window.opt.win_top)) = Good then
      Window.Left (Window.opt.win_left);
      Window.Top  (Window.opt.win_top);
    end if;
    Window.Size (
      Integer'Max (640, Window.opt.win_width),
      Integer'Max (400, Window.opt.win_height)
    );
    Window.Zoom (Window.opt.MDI_main_maximized);

    Change_View (Window, Window.opt.view_mode, force => True);

    Window.Dock_Children;
    LEA_GWin.Options.Apply_Main_Options (Window);
    Window.Show;

    if Argument_Count = 0 then
      On_File_New (Window, extra_first_doc => True);
      --  ^ The MS Office-like first, empty document
    end if;
    for i in 1 .. Argument_Count loop
      Window.Process_Argument (i, Argument (i));
    end loop;
    --  Dropping files on the MDI background will trigger opening a document:
    Window.Accept_File_Drag_And_Drop;
    Window.record_dimensions := True;
    --
    begin
      Window.Task_bar_gadget.Set_Progress_State (Window, No_Progress);
      Window.Task_bar_gadget_ok := True;
    exception
      when Taskbar_Interface_Not_Supported =>
        Window.Task_bar_gadget_ok := False;
    end;
    Window.search_box.Create_as_Search_Box (Window);
    GWindows.Timers.Set_Timer (Window, search_box_timer_id, 100);
    GWindows.Timers.Set_Timer (Window, file_synch_timer_id, 250);
    --  Now we instruct the librarians of both HAC compilers
    --  to use our special LEA-flavoured file catalogue:
    Window.lea_file_cat.mdi_parent := Window'Unchecked_Access;
    HAC_Sys.Librarian.Set_Source_Access (Window.BD.LD, Window.lea_file_cat'Unchecked_Access);
    HAC_Sys.Librarian.Set_Source_Access (Window.BD_sem.LD, Window.lea_file_cat'Unchecked_Access);
  end On_Create;

  function Is_Minimized (MDI_Main : GWindows.Base.Base_Window_Type'Class)
    return Boolean
  is
  begin
    return GWindows.Base.Left (MDI_Main) <= -32000;
  end Is_Minimized;

  procedure On_Move (Window : in out MDI_Main_Type;
                     Left   : in     Integer;
                     Top    : in     Integer) is
  begin
    if Window.record_dimensions and
       not (Zoom (Window) or Is_Minimized (Window))
    then
      --  ^ Avoids recording dimensions before restoring them
      --   from previous session.
      Window.opt.win_left := Left;
      Window.opt.win_top  := Top;
      --  Will remember position if moved, maximized and closed
    end if;
  end On_Move;

  procedure On_Size (Window : in out MDI_Main_Type;
                     Width  : in     Integer;
                     Height : in     Integer)
  is
    w   : constant Natural := Window.Client_Area_Width;
    tbh : constant Natural := Window.Tool_Bar.Height;
    h   : constant Natural := Integer'Max (2, Window.Client_Area_Height - tbh);
    tree_w : constant Integer := Integer (Window.opt.project_tree_portion * Float (w));
    list_h : constant Integer := Integer (Window.opt.message_list_portion * Float (h));
    use GWindows.Types;
  begin
    --  Resize project tree and message list panels using the recorded proportions
    --  This operation is reciprocal to Memorize_Splitters.
    --
    --  Adapt project tree size:
    case Window.opt.view_mode is
      when Notepad =>
        --  Do nothing about project tree splitter: the panel is invisible and not used
        null;
      when Studio =>
        Window.Project_Panel.Location (Rectangle_Type'(0, 0, tree_w, h));
    end case;
    Window.Message_Panel.Location (Rectangle_Type'(0, h + tbh - list_h, w, h + tbh));
    --  Call Dock_Children for the finishing touch...
    Window.Dock_Children;
    if Window.record_dimensions and not (Window.Zoom or Is_Minimized (Window)) then
      --  ^ Avoids recording dimensions before restoring them
      --   from previous session.
      Window.opt.win_width := Width;
      Window.opt.win_height := Height;
      --  Will remember position if sized, maximized and closed
    end if;
  end On_Size;

  -----------------
  -- On_File_New --
  -----------------

  New_MDI_window_counter : Natural := 0;

  procedure On_File_New (Window : in out MDI_Main_Type; extra_first_doc : Boolean) is

    function Suffix return GWindows.GString is
    begin
      if New_MDI_window_counter = 0 then
        return "";
      else
        return Integer'Wide_Image (New_MDI_window_counter + 1);
      end if;
    end Suffix;

    New_Window : constant MDI_Child_Access := new MDI_Child_Type;
    Untitled_N : constant GString := "Untitled" & Suffix;

  begin
    New_Window.Extra_First_Doc := extra_first_doc;
    Window.User_maximize_restore := False;
    New_Window.Create_LEA_MDI_Child
      (Window,
       (File_Name  => Null_GString_Unbounded,  --  No file until first "Save".
        Short_Name => G2GU (Untitled_N)));

    --  Transfer user-defined default options:
    --  New_Window.xxx.Opt:= Gen_Opt.Options_For_New;
    --  Refresh_size_dependent_parameters(
    --  New_Window.Draw_Control.Picture,
    --  objects => True
    --  );

    New_MDI_window_counter := New_MDI_window_counter + 1;

    --  This is just to set the MRUs in the new window's menu:
    Window.Update_Common_Menus;
    --
    New_Window.Finish_subwindow_opening;
    New_Window.editor.Focus;
  end On_File_New;

  ------------------
  -- On_File_Open --
  ------------------

  procedure On_File_Open (MDI_Main : in out MDI_Main_Type) is
    File_Title : GString_Unbounded;
    Success    : Boolean;
    use GWindows.Windows;
    File_Names : Array_Of_File_Names_Access;
    procedure Dispose is new Ada.Unchecked_Deallocation
      (Array_Of_File_Names,
       Array_Of_File_Names_Access);
  begin
    GWindows.Common_Dialogs.Open_Files
      (MDI_Main,
       "Open file(s)",
       File_Names,
       MDI_Main.text_files_filters,
       ".ad*",
       File_Title,
       Success);

    if Success then
      for File_Name of File_Names.all loop
        Open_Child_Window_And_Load (MDI_Main, GU2G (File_Name));
      end loop;
      Dispose (File_Names);
    end if;
  end On_File_Open;

  procedure On_File_Drop (Window     : in out MDI_Main_Type;
                          File_Names : in     GWindows.Windows.Array_Of_File_Names) is
  begin
    Window.Focus;
    for File_Name of File_Names loop
      Open_Child_Window_And_Load (Window, GU2G (File_Name));
    end loop;
  end On_File_Drop;

  ----------------------
  -- My_MDI_Close_All --
  ----------------------

  procedure My_MDI_Close_All (MDI_Main : in out MDI_Main_Type) is
    procedure My_Close_Win (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    --  Enumeration call back to close MDI child windows
    is
    begin
      if Any_Window /= null
        and then Any_Window.all in MDI_Child_Type'Class
        and then MDI_Main.Success_in_enumerated_close
      then  --  No [cancel] button was selected up to now.
        GWindows.Base.Close (Any_Window.all);
      end if;
    end My_Close_Win;
  begin
    MDI_Main.Success_in_enumerated_close := True;
    GWindows.Base.Enumerate_Children (MDI_Client_Window (MDI_Main).all,
                                      My_Close_Win'Unrestricted_Access);
  end My_MDI_Close_All;

  --------------------
  -- On_Menu_Select --
  --------------------

  overriding procedure On_Menu_Select
    (Window : in out MDI_Main_Type;
     Item   : in     Integer)
  is
    procedure Call_Parent_Method is
    begin
      GWindows.Windows.Window_Type (Window).On_Menu_Select (Item);
    end Call_Parent_Method;
    use LEA_Resource_GUI;
  begin
    case Item is
      when IDM_New_File =>
        On_File_New (Window, extra_first_doc => False);
      when IDM_Open_File =>
        On_File_Open (Window);
      when IDM_Web =>
        GWin_Util.Start (LEA_web_page);
      when IDM_QUIT  =>
        Close (Window);
      when IDM_Close =>
        if Window.Count_MDI_Children = 0 then
          Close (Window);  --  Ctrl-W when no subwindow is open.
        else
          Call_Parent_Method;
        end if;
      when IDM_Copy_Messages =>
        Window.Message_Panel.Message_List.Copy_Messages;
      when IDM_WINDOW_CASCADE   =>
        MDI_Cascade (Window);
      when IDM_WINDOW_TILE_HORIZONTAL =>
        MDI_Tile_Horizontal (Window);
      when IDM_WINDOW_TILE_VERTICAL =>
        MDI_Tile_Vertical (Window);
      when IDM_WINDOW_CLOSE_ALL =>
        My_MDI_Close_All (Window);
      when IDM_General_options =>
        Options.On_General_Options (Window);
      when IDM_ABOUT =>
        Modal_Dialogs.Show_About_Box (Window);
      when IDM_Quick_Help =>
        Embedded_Texts.Show_Help (Window);
      when IDM_Ada_Sample =>
        Modal_Dialogs.Browse_and_Get_Code_Sample (Window);
      when IDM_Notepad_view =>
        Change_View (Window, Notepad, force => False);
      when IDM_Studio_view =>
        Change_View (Window, Studio, force => False);
      when IDM_HAC_Mode =>
        Change_Mode (Window, HAC_mode);
      when IDM_GNAT_Mode =>
        Change_Mode (Window, GNAT_mode);
      when IDM_Go_to_memorized_Declaration =>
        Window.Go_to_memorized_Declaration (1);
      when IDM_Go_to_memorized_Body =>
        Window.Go_to_memorized_Declaration (2);
      when IDM_Go_to_other_File =>
        Window.Open_Child_Window_And_Load (GU2G (Window.memo_other_file));
      when others =>
        --  We have perhaps a MRU (most recently used) file entry.
        for i_mru in Window.MRU.ID_Menu'Range loop
          if Item = Window.MRU.ID_Menu (i_mru) then
            Open_Child_Window_And_Load
              (Window,
               GU2G (Window.MRU.Item (i_mru).Name));
            exit;
          end if;
        end loop;
        Call_Parent_Method;
    end case;
  end On_Menu_Select;

  procedure On_Message (Window       : in out MDI_Main_Type;
                        message      : in     Interfaces.C.unsigned;
                        wParam       : in     GWindows.Types.Wparam;
                        lParam       : in     GWindows.Types.Lparam;
                        Return_Value : in out GWindows.Types.Lresult)
  is
    use Interfaces.C;
  begin
    case message is
      when GWindows.Timers.WM_TIMER =>
        case wParam is
        when search_box_timer_id =>
          if Window.close_this_search_box then
            Window.close_this_search_box := False;
            if Window.search_box.Visible then
              Window.Set_Foreground_Window;
              Window.Focus;
              Window.search_box.Hide;
            end if;
          end if;
        when file_synch_timer_id =>
          null;
        when others =>
          null;
        end case;
      when others =>
        null;
    end case;
    --  Call parent method
    GWindows.Windows.MDI.MDI_Main_Window_Type (Window).On_Message (
      message,
      wParam,
      lParam,
      Return_Value
    );
  end On_Message;

  -------------

  overriding procedure On_Close
    (Window    : in out MDI_Main_Type;
     Can_Close :    out Boolean)
  is
  begin
    Window.opt.MDI_main_maximized := Zoom (Window);
    if not (Window.opt.MDI_main_maximized or Is_Minimized (Window)) then
      Window.opt.win_left   := Left (Window);
      Window.opt.win_top    := Top (Window);
      Window.opt.win_width  := Width (Window);
      Window.opt.win_height := Height (Window);
    end if;

    My_MDI_Close_All (Window);
    --  ^ Don't forget to save unsaved files !
    --  Operation can be cancelled by user for one unsaved picture.
    Can_Close := Window.Success_in_enumerated_close;
    --
    if Can_Close then
      for m in Window.MRU.Item'Range loop
        Window.opt.mru (m) :=
          (name => Window.MRU.Item (m).Name,
           line => Window.MRU.Item (m).Line);
      end loop;
      Persistence.Blockwise_IO.Save (Window.opt);
      --
      --  !! Trick to remove a strange crash on Destroy_Children
      --  !! on certain Windows platforms - 29-Jun-2012
      GWindows.Base.On_Exception_Handler (Handler => null);
      --
      GWindows.Timers.Kill_Timer (Window, search_box_timer_id);
      GWindows.Timers.Kill_Timer (Window, file_synch_timer_id);
    end if;
  end On_Close;

  --  Menus of MDI main *and* all children need to have their "View" menu up-to-date.
  --
  procedure Update_View_Menu (m : Menu_Type; o : LEA_Common.User_options.Option_Pack_Type) is
    use LEA_Resource_GUI;
  begin
    case o.view_mode is
      when Notepad =>
        Check (m, Command, IDM_Notepad_view, True);
        Check (m, Command, IDM_Studio_view, False);
      when Studio =>
        Check (m, Command, IDM_Notepad_view, False);
        Check (m, Command, IDM_Studio_view, True);
    end case;
    case o.toolset is
      when HAC_mode =>
        Check (m, Command, IDM_HAC_Mode, True);
        Check (m, Command, IDM_GNAT_Mode, False);
      when GNAT_mode =>
        Check (m, Command, IDM_HAC_Mode, False);
        Check (m, Command, IDM_GNAT_Mode, True);
    end case;
    Check (m, Command, IDM_Show_special_symbols, not (o.show_special = none));
    Check (m, Command, IDM_Show_indentation_lines, o.show_indent);
  end Update_View_Menu;

  procedure Update_Common_Menus_Child (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
  is
    use Office_Applications;
  begin
    if Any_Window /= null and then Any_Window.all in MDI_Child_Type'Class then
      declare
        cw : MDI_Child_Type renames MDI_Child_Type (Any_Window.all);
      begin
        Update_MRU_Menu (cw.mdi_root.MRU, cw.menu.Popup_0001);
        Update_View_Menu (cw.menu.Main, cw.mdi_root.opt);
        --  Update_Toolbar_Menu(cw.View_menu, cw.MDI_Root.Floating_toolbars);
      end;
    end if;
  end Update_Common_Menus_Child;

  procedure Update_Common_Menus
    (Window         : in out MDI_Main_Type;
     top_entry_name :        GString := "";
     top_entry_line :        Integer := -1)    --  When unknown, -1; otherwise: last visited line
  is
    use Office_Applications;
  begin
    if top_entry_name /= "" then
      Add_MRU (Window.MRU, top_entry_name, top_entry_line);
    end if;
    Update_MRU_Menu (Window.MRU, Window.Menu.Popup_0001);
    Update_View_Menu (Window.Menu.Main, Window.opt);
    --  Update_Toolbar_Menu(Window.View_menu, Window.Floating_toolbars);
    GWindows.Base.Enumerate_Children
      (MDI_Client_Window (Window).all,
       Update_Common_Menus_Child'Access);
  end Update_Common_Menus;

  procedure Update_Title (Window : in out MDI_Main_Type) is
    use type GString_Unbounded;
  begin
    if Window.Project_File_Name = "" then
      Window.Text ("LEA - [Projectless]");
    else
      Window.Text ("LEA - [" & GU2G (Window.Project_Short_Name) & ']');
    end if;
  end Update_Title;

  procedure Perform_Search (Window : MDI_Main_Type; action : LEA_Common.Search_action) is
    procedure Search_on_focused_editor (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class) is
    begin
      if Any_Window /= null
        and then Any_Window.all in MDI_Child_Type'Class
        and then Window.Focus = Any_Window
      then
        MDI_Child_Type (Any_Window.all).editor.Search (action);
      end if;
    end Search_on_focused_editor;
  begin
    Enumerate_Children
      (MDI_Client_Window (Window).all,
       Search_on_focused_editor'Unrestricted_Access);
  end Perform_Search;

  --  The operation reciprocal to Memorize_Splitters is done in On_Size.
  --
  procedure Memorize_Splitters (Window : in out MDI_Main_Type) is
    p : Float;
  begin
    case Window.opt.view_mode is
      when Notepad =>
        --  Do nothing about project tree splitter: the panel is invisible and not used
        null;
      when Studio =>
        Window.opt.project_tree_portion :=
          Float (Window.Project_Panel.Width) /
          Float (Window.Client_Area_Width);
    end case;
    p :=
      Float (Window.Message_Panel.Height) /
      Float (Window.Client_Area_Height - Window.Tool_Bar.Height);
    p := Float'Max (0.1, p);  --  Avoid complete disappearance
    p := Float'Min (0.9, p);  --  Avoid eating up whole window
    Window.opt.message_list_portion := p;
    --
    --  NB: the splitter for subprogram tree is part of child window and
    --  memorized at child level.
  end Memorize_Splitters;

end LEA_GWin.MDI_Main;
