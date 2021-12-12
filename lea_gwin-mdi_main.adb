with LEA_Common.Syntax;

with LEA_GWin.Help;
with LEA_GWin.MDI_Child;
with LEA_GWin.Modal_Dialogs;
with LEA_GWin.Options;
with LEA_GWin.Toolbars;

with GWindows.Application;
with GWindows.Base;
with GWindows.Common_Dialogs;
with GWindows.Constants;
with GWindows.Menus;
with GWindows.Message_Boxes;
with GWindows.Registry;
with GWindows.Scintilla;

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Windows_Timers;

package body LEA_GWin.MDI_Main is

  use type GString_Unbounded, Scintilla.Position;
  use LEA_Common, LEA_GWin.MDI_Child;
  use GWindows.Base, GWindows.Menus;

  procedure Focus_an_already_opened_window(
    MDI_Main     :     MDI_Main_Type;
    File_Name    :     GString_Unbounded;
    Line         :     Integer            := -1;
    Col_a, Col_z :     Scintilla.Position := -1;
    is_open      : out Boolean )
  is
    procedure Identify (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
    begin
      if Any_Window /= null and then Any_Window.all in MDI_Child_Type'Class then
        declare
          pw: MDI_Child_Type renames MDI_Child_Type(Any_Window.all);
          new_pos_a, new_pos_z : GWindows.Scintilla.Position;
        begin
          if pw.File_Name = File_Name then
            is_open:= True;
            pw.Focus;  --  Focus on document already open in our app.
            --  Scintilla lines are 0-based
            if Line > -1 then
              pw.Editor.Set_current_line (Line);
            end if;
            if Col_a > -1 then
              new_pos_a := pw.Editor.Get_Current_Pos + Col_a;
              new_pos_z := pw.Editor.Get_Current_Pos + Col_z;
              pw.Editor.Set_Sel (new_pos_a, new_pos_z);
            end if;
          end if;
        end;
      end if;
    end Identify;

  begin
    is_open:= False;
    Enumerate_Children(
      MDI_Client_Window (MDI_Main).all,
      Identify'Unrestricted_Access
    );
  end Focus_an_already_opened_window;

  procedure Redraw_Child (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
  is
  begin
    if Any_Window /= null and then Any_Window.all in MDI_Child_Type'Class then
      --  !! some content refresh, dbl buffering
      Any_Window.Redraw;
    end if;
  end Redraw_Child;

  procedure Redraw_all (MDI_Main: in out MDI_Main_Type) is
  begin
    MDI_Main.Redraw;
    --  Redraw(MDI_Main.Tool_bar);
    Enumerate_Children(MDI_Client_Window (MDI_Main).all, Redraw_Child'Access);
  end Redraw_all;

  procedure Close_extra_first_child (MDI_Main: in out MDI_Main_Type) is
    --
    procedure Close_extra_first_document (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
    begin
      if Any_Window /= null and then Any_Window.all in MDI_Child_Type'Class then
        declare
          w: MDI_Child_Type renames MDI_Child_Type(Any_Window.all);
        begin
          if w.Extra_first_doc and Is_file_saved (w) then
            --  This situation happens only if the blank startup editor is at its initial state;
            --  the text is either untouched, or with all modifications undone.
            Any_Window.Close;
          end if;
        end;
      end if;
    end Close_extra_first_document;
    --
  begin
    Enumerate_Children (
      MDI_Client_Window (MDI_Main).all,
      Close_extra_first_document'Unrestricted_Access
    );
  end Close_extra_first_child;

  procedure Open_Child_Window_And_Load (
    MDI_Main     : in out MDI_Main_Type;
    File_Name,
    File_Title   :        GWindows.GString_Unbounded;
    Line         :        Integer            := -1;
    Col_a, Col_z :        Scintilla.Position := -1
  )
  is
    is_open, file_loaded : Boolean;
    mru_line : Integer := -1;
    new_pos_a, new_pos_z : GWindows.Scintilla.Position;
    New_Window : MDI_Child_Access;
    use GWindows.Message_Boxes;
  begin
    Focus_an_already_opened_window ( MDI_Main, File_Name, Line, Col_a, Col_z, is_open );
    if is_open then
      return;        -- nothing to do, document already in a window
    end if;
    New_Window := new MDI_Child_Type;
    --  We do here like Excel or Word: close the unused blank window
    Close_extra_first_child (MDI_Main);
    --
    MDI_Main.User_maximize_restore:= False;
    New_Window.File_Name:= File_Name;
    Create_MDI_Child (New_Window.all,
      MDI_Main,
      GU2G (File_Title),
      Is_Dynamic => True
    );
    New_Window.Short_Name:= File_Title;
    MDI_Active_Window (MDI_Main, New_Window.all);
    begin
      New_Window.Editor.Load_text;
      file_loaded := True;
      for m of MDI_Main.opt.mru loop
        if m.name = New_Window.File_Name then
          mru_line := m.line;  --  This will put MRU item on top.
          exit;
        end if;
      end loop;
      Update_Common_Menus (MDI_Main, GU2G(New_Window.File_Name), mru_line);
    exception
      when Ada.Text_IO.Name_Error =>
        file_loaded := False;
    end;
    New_Window.Finish_subwindow_opening;
    New_Window.Editor.syntax_kind :=
      LEA_Common.Syntax.Guess_syntax (
        GU2G (New_Window.File_Name),
        GU2G (MDI_Main.opt.ada_files_filter)
      );
    New_Window.Editor.Set_Scintilla_Syntax;
    New_Window.Editor.Focus;
    --  NB: Scintilla lines are 0-based
    if Line > -1 then
      New_Window.Editor.Set_current_line (Line);
    elsif mru_line > -1 then
      --  Set cursor position to memorized line number
      New_Window.Editor.Set_current_line (mru_line);
    end if;
    if Col_a > -1 then
      new_pos_a := New_Window.Editor.Get_Current_Pos + Col_a;
      new_pos_z := New_Window.Editor.Get_Current_Pos + Col_z;
      New_Window.Editor.Set_Sel (new_pos_a, new_pos_z);
    end if;
    if not file_loaded then
      Message_Box (
        MDI_Main,
        "Error",
        "File " & GU2G (File_Name) & " not found",
        Icon => Exclamation_Icon
      );
      --  Prevent MRU name addition:
      New_Window.File_Name := Null_GString_Unbounded;
      New_Window.Close;
    end if;
  end Open_Child_Window_And_Load;

  procedure On_Button_Select (
        Control : in out MDI_Toolbar_Type;
        Item    : in     Integer           ) is
    Parent : constant MDI_Main_Access := MDI_Main_Access (Controlling_Parent (Control));
  begin
    On_Menu_Select (Parent.all, Item);
  end On_Button_Select;

  overriding procedure On_Bar_Moved (Splitter : in out LEA_splitter) is
  begin
    Splitter.MDI_Main.Memorize_Splitters;
    --  Call parent method:
    GWin_Util.Splitter_with_dashes (Splitter).On_Bar_Moved;
  end On_Bar_Moved;

  function Shorten_file_name( s: GString ) return GString is
    max: constant:= 33;
    beg: constant:= 6;
  begin
    if s'Length < max then
      return s;
    else
      return
        s(s'First .. s'First + beg-1) &       -- beg
        "..." &                               -- 3
        s(s'Last - max + beg + 1 .. s'Last);  -- max - beg - 3
    end if;
  end Shorten_file_name;

  procedure Open_Child_Window_And_Load (
    MDI_Main     : in out MDI_Main_Type;
    File_Name    :        GWindows.GString_Unbounded;
    Line         :        Integer := -1;
    Col_a, Col_z :        Integer := -1
  )
  is
  begin
    Open_Child_Window_And_Load(
      MDI_Main,
      File_Name,
      G2GU(Shorten_file_name(GU2G(File_Name))),
      Line,
      Scintilla.Position (Col_a),
      Scintilla.Position (Col_z)
    );
  end Open_Child_Window_And_Load;

  function Valid_Left_Top (Left, Top : Integer)
    return Boolean
  is
    use GWindows.Application;
  begin
    return Left in -320 .. Desktop_Width  - 30 and
           Top  in -320 .. Desktop_Height - 80;
  end Valid_Left_Top;

  -----------------
  -- Persistence --
  -----------------

  kname: constant GString:= "Software\LEA";

  function Read_key(topic: Wide_String) return Wide_String is
    use GWindows.Registry;
  begin
    return Get_Value(kname, topic, HKEY_CURRENT_USER);
  end Read_key;

  procedure Write_key(topic: Wide_String; value: Wide_String) is
    use GWindows.Registry;
  begin
    Register( kname, topic, value, HKEY_CURRENT_USER );
  end Write_key;

  package Windows_persistence is new
    LEA_Common.User_options.Persistence(Read_key, Write_key);

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
    MDI_Main.opt.view_mode:= new_view;
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
    MDI_Main.opt.toolset:= new_mode;
    MDI_Main.Update_Common_Menus;
  end Change_Mode;

  timer_id: constant:= 1;

  ---------------
  -- On_Create --
  ---------------

  procedure On_Create ( MDI_Main : in out MDI_Main_Type ) is
    use GWindows.Common_Controls, Ada.Command_Line;
    --
    --  Replace LEA default values by system-dependent ones (here those of GWindows)
    --
    procedure Replace_default(x: in out Integer) is
    begin
      if x = LEA_Common.User_options.use_default then
        x:= GWindows.Constants.Use_Default;
      end if;
    end Replace_default;
    --
    start_line : Integer := -1;
    use GWindows.Taskbar, LEA_Resource_GUI;
  begin
    Windows_persistence.Load (MDI_Main.opt);  --  Load options from the registry
    LEA_GWin.Options.Apply_Main_Options (MDI_Main);
    --
    Replace_default(MDI_Main.opt.win_left);
    Replace_default(MDI_Main.opt.win_width);
    Replace_default(MDI_Main.opt.win_top);
    Replace_default(MDI_Main.opt.win_height);

    Small_Icon (MDI_Main, "LEA_Icon_Small");
    Large_Icon (MDI_Main, "AAA_Main_Icon");

    --  ** Menus and accelerators:
    --
    LEA_Resource_GUI.Create_Full_Menu(MDI_Main.Menu);
    MDI_Menu (MDI_Main, MDI_Main.Menu.Main, Window_Menu => 5);
    Accelerator_Table (MDI_Main, "Main_Menu");
    MDI_Main.IDM_MRU:=
      (IDM_MRU_1,       IDM_MRU_2,       IDM_MRU_3,       IDM_MRU_4,
       IDM_MRU_5,       IDM_MRU_6,       IDM_MRU_7,       IDM_MRU_8,
       IDM_MRU_9
      );

    --  ** Other resources
    MDI_Main.Folders_Images.Create (Num_resource(Folders_BMP), 16);

    --  ** Main tool bar (New / Open / Save / ...) at top left of the main window:
    LEA_GWin.Toolbars.Init_Main_toolbar(MDI_Main.Tool_Bar, MDI_Main.Toolbar_Images, MDI_Main);

    --  ** Sizeable panels. For a sketch, see the "Layout" sheet in lea_work.xls.
    --
    --    1) Left panel, with project or file tree:
    --
    MDI_Main.Project_Panel.Create (MDI_Main, 1,1,20,20);
    MDI_Main.Project_Panel.Dock (At_Left);
    MDI_Main.Project_Panel.Splitter.Create (MDI_Main.Project_Panel, At_Right);
    MDI_Main.Project_Panel.Splitter.MDI_Main := MDI_Main'Unrestricted_Access;
    MDI_Main.Project_Panel.Project_Tree.Create (MDI_Main.Project_Panel, 1,1,20,20, Lines_At_Root => False);
    MDI_Main.Project_Panel.Project_Tree.Dock (Fill);
    --
    --    2) Bottom panel, with messages:
    --
    MDI_Main.Message_Panel.Create (MDI_Main, 1,1,20,80);
    MDI_Main.Message_Panel.Dock (At_Bottom);
    MDI_Main.Message_Panel.Splitter.Create (MDI_Main.Message_Panel, At_Top, Height => 5);
    MDI_Main.Message_Panel.Splitter.MDI_Main := MDI_Main'Unrestricted_Access;
    MDI_Main.Message_Panel.Message_List.Font.Create_Font (App_default_font, 15);
    MDI_Main.Message_Panel.Message_List.Create (MDI_Main.Message_Panel, 1,1,20,20, View => Report_View);
    MDI_Main.Message_Panel.Message_List.mdi_main_parent := MDI_Main'Unrestricted_Access;
    MDI_Main.Message_Panel.Message_List.Set_Extended_Style (Full_Row_Select);
    MDI_Main.Message_Panel.Message_List.Set_Font (MDI_Main.Message_Panel.Message_List.Font);
    MDI_Main.Message_Panel.Message_List.Insert_Column ("", 0, 40);
    MDI_Main.Message_Panel.Message_List.Insert_Column ("", 1, 35);
    MDI_Main.Message_Panel.Message_List.Insert_Column ("", 2, 1000);
    MDI_Main.Message_Panel.Message_List.Set_Image_List (Small, MDI_Main.Folders_Images);
    MDI_Main.Message_Panel.Message_List.Dock (Fill);

    --  ** Resize according to options:

    if Valid_Left_Top(MDI_Main.opt.win_left, MDI_Main.opt.win_top) then
      Left(MDI_Main, MDI_Main.opt.win_left);
      Top( MDI_Main, MDI_Main.opt.win_top);
    end if;
    Size(MDI_Main,
      Integer'Max(640, MDI_Main.opt.win_width),
      Integer'Max(400, MDI_Main.opt.win_height)
    );
    Zoom(MDI_Main,MDI_Main.opt.MDI_main_maximized);

    Change_View (MDI_Main, MDI_Main.opt.view_mode, force => True);

    MDI_Main.Dock_Children;
    MDI_Main.Show;

    if Argument_Count=0 then
      On_File_New (MDI_Main, extra_first_doc => True);
      --  ^ The MS Office-like first, empty document
    end if;
    --  !! This works on 1st instance only:
    for i in 1 .. Argument_Count loop
      declare
        a : constant String := Argument (i);
      begin
        if a (a'First) = '+' then  --  Emacs +linenum
          start_line := 0;
          for j in a'First + 1 .. a'Last loop
            if a(j) in '0' .. '9' then
              start_line := start_line * 10 + (Character'Pos(a(j)) - Character'Pos('0'));
            else
              start_line := -1;  -- Invalid number
              exit;
            end if;
          end loop;
        else
          Open_Child_Window_And_Load(
            MDI_Main,
            G2GU(To_UTF_16(a)),
            start_line - 1  --  NB: Scintilla lines are 0-based
          );
          start_line := -1;
        end if;
      end;
    end loop;
    --  Dropping files on the MDI background will trigger opening a document:
    MDI_Main.Accept_File_Drag_And_Drop;
    MDI_Main.record_dimensions:= True;
    --
    begin
      MDI_Main.Task_bar_gadget.Set_Progress_State (MDI_Main, No_Progress);
      MDI_Main.Task_bar_gadget_ok := True;
    exception
      when Taskbar_Interface_Not_Supported =>
        MDI_Main.Task_bar_gadget_ok := False;
    end;
    MDI_Main.Search_box.Create_as_search_box(MDI_Main);
    Windows_Timers.Set_Timer(MDI_Main, timer_id, 100);
  end On_Create;

  function Minimized(MDI_Main: GWindows.Base.Base_Window_Type'Class)
    return Boolean
  is
  begin
    return GWindows.Base.Left(MDI_Main) <= -32000;
  end Minimized;

  procedure On_Move (MDI_Main : in out MDI_Main_Type;
                     Left     : in     Integer;
                     Top      : in     Integer) is
  begin
    if MDI_Main.record_dimensions and
       not (Zoom(MDI_Main) or Minimized(MDI_Main))
    then
      --  ^ Avoids recording dimensions before restoring them
      --   from previous session.
      MDI_Main.opt.win_left  := Left;
      MDI_Main.opt.win_top   := Top;
      --  Will remember position if moved, maximized and closed
    end if;
  end On_Move;

  procedure On_Size (MDI_Main : in out MDI_Main_Type;
                     Width    : in     Integer;
                     Height   : in     Integer)
  is
    w   : constant Natural := MDI_Main.Client_Area_Width;
    tbh : constant Natural := MDI_Main.Tool_Bar.Height;
    h   : constant Natural := Integer'Max(2, MDI_Main.Client_Area_Height - tbh);
    tree_w : constant Integer := Integer (MDI_Main.opt.project_tree_portion * Float(w));
    list_h : constant Integer := Integer (MDI_Main.opt.message_list_portion * Float(h));
    use GWindows.Types;
  begin
    --  Resize project tree and message list panels using the recorded proportions
    --  This operation is reciprocal to Memorize_Splitters.
    --
    --  Adapt project tree size:
    case MDI_Main.opt.view_mode is
      when Notepad =>
        --  Do nothing about project tree splitter: the panel is invisible and not used
        null;
      when Studio =>
        MDI_Main.Project_Panel.Location (Rectangle_Type'(0, 0, tree_w, h));
    end case;
    MDI_Main.Message_Panel.Location (Rectangle_Type'(0, h + tbh - list_h, w, h + tbh));
    --  Call Dock_Children for the finishing touch...
    MDI_Main.Dock_Children;
    if MDI_Main.record_dimensions and not (MDI_Main.Zoom or Minimized (MDI_Main)) then
      --  ^ Avoids recording dimensions before restoring them
      --   from previous session.
      MDI_Main.opt.win_width := Width;
      MDI_Main.opt.win_height:= Height;
      --  Will remember position if sized, maximized and closed
    end if;
  end On_Size;

  -----------------
  -- On_File_New --
  -----------------

  New_MDI_window_counter : Natural := 0;

  procedure On_File_New (
    MDI_Main        : in out MDI_Main_Type;
    extra_first_doc : Boolean;
    New_Window      : in     MDI_Child_Access
  )
  is

    function Suffix return GWindows.GString is
    begin
      if New_MDI_window_counter = 0 then
        return "";
      else
        return Integer'Wide_Image(New_MDI_window_counter + 1);
      end if;
    end Suffix;

    File_Title: constant GString:= "Untitled" & Suffix;

  begin
    New_Window.Extra_first_doc:= extra_first_doc;
    MDI_Main.User_maximize_restore:= False;
    Create_MDI_Child (New_Window.all, MDI_Main, File_Title, Is_Dynamic => True);
    New_Window.Short_Name:= G2GU(File_Title);
    MDI_Active_Window (MDI_Main, New_Window.all);

    --  Transfer user-defined default options:
    --  New_Window.xxx.Opt:= Gen_Opt.Options_For_New;
    --  Refresh_size_dependent_parameters(
    --  New_Window.Draw_Control.Picture,
    --  objects => True
    --  );

    New_MDI_window_counter := New_MDI_window_counter + 1;

    --  This is just to set the MRUs in the new window's menu:
    MDI_Main.Update_Common_Menus;
    --
    New_Window.Finish_subwindow_opening;
    New_Window.Editor.Focus;
  end On_File_New;

  procedure On_File_New (MDI_Main : in out MDI_Main_Type; extra_first_doc: Boolean) is
    New_Window : constant MDI_Child_Access := new MDI_Child_Type;
  begin
    On_File_New(MDI_Main, extra_first_doc, New_Window);
  end On_File_New;

  ------------------
  -- On_File_Open --
  ------------------

  procedure On_File_Open (MDI_Main : in out MDI_Main_Type) is
    File_Title : GString_Unbounded;
    Success    : Boolean;
    use GWindows.Windows;
    File_Names: Array_Of_File_Names_Access;
    procedure Dispose is new Ada.Unchecked_Deallocation(
      Array_Of_File_Names,
      Array_Of_File_Names_Access
    );
  begin
    GWindows.Common_Dialogs.Open_Files (
      MDI_Main,
      "Open file(s)",
      File_Names,
      MDI_Main.text_files_filters,
      ".ad*",
      File_Title,
      Success
    );
    if Success then
      for File_Name of File_Names.all loop
        Open_Child_Window_And_Load( MDI_Main, File_Name );
      end loop;
      Dispose(File_Names);
    end if;
  end On_File_Open;

  procedure On_File_Drop (MDI_Main   : in out MDI_Main_Type;
                          File_Names : in     GWindows.Windows.Array_Of_File_Names) is
  begin
    MDI_Main.Focus;
    for File_Name of File_Names loop
      Open_Child_Window_And_Load ( MDI_Main, File_Name );
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
    MDI_Main.Success_in_enumerated_close:= True;
    GWindows.Base.Enumerate_Children (MDI_Client_Window (MDI_Main).all,
                                      My_Close_Win'Unrestricted_Access);
  end My_MDI_Close_All;

  --------------------
  -- On_Menu_Select --
  --------------------

  procedure On_Menu_Select (
        MDI_Main : in out MDI_Main_Type;
        Item     : in     Integer        )
  is
    procedure Call_Parent_Method is
    begin
      GWindows.Windows.Window_Type (MDI_Main).On_Menu_Select (Item);
    end Call_Parent_Method;
    use LEA_Resource_GUI;
  begin
    case Item is
      when IDM_New_File=>
        On_File_New (MDI_Main, extra_first_doc => False);
      when IDM_Open_File =>
        On_File_Open (MDI_Main);
      when IDM_Web =>
        GWin_Util.Start(LEA_web_page);
      when IDM_QUIT  =>
        Close (MDI_Main);
      when IDM_Close =>
        if MDI_Main.Count_MDI_Children = 0 then
          Close (MDI_Main);  --  Ctrl-W when no subwindow is open.
        else
          Call_Parent_Method;
        end if;
      when IDM_Copy_Messages =>
        MDI_Main.Message_Panel.Message_List.Copy_Messages;
      when IDM_WINDOW_CASCADE   =>
        MDI_Cascade (MDI_Main);
      when IDM_WINDOW_TILE_HORIZONTAL =>
        MDI_Tile_Horizontal (MDI_Main);
      when IDM_WINDOW_TILE_VERTICAL =>
        MDI_Tile_Vertical (MDI_Main);
      when IDM_WINDOW_CLOSE_ALL =>
        My_MDI_Close_All(MDI_Main);
      when IDM_General_options =>
        Options.On_General_Options(MDI_Main);
      when IDM_ABOUT =>
        Modal_Dialogs.Show_About_Box (MDI_Main);
      when IDM_Quick_Help =>
        Help.Show_help (MDI_Main);
      when IDM_Ada_Sample =>
        Modal_Dialogs.Browse_and_Get_Code_Sample (MDI_Main);
      when IDM_Notepad_view =>
        Change_View (MDI_Main, Notepad, force => False);
      when IDM_Studio_view =>
        Change_View (MDI_Main, Studio, force => False);
      when IDM_HAC_Mode =>
        Change_Mode (MDI_Main, HAC_mode);
      when IDM_GNAT_Mode =>
        Change_Mode (MDI_Main, GNAT_mode);
      when others =>
        --  We have perhaps a MRU (most rectly used) file entry.
        for i_mru in MDI_Main.IDM_MRU'Range loop
          if Item = MDI_Main.IDM_MRU (i_mru) then
            Open_Child_Window_And_Load(
              MDI_Main,
              MDI_Main.opt.mru ( i_mru ).name
            );
            exit;
          end if;
        end loop;
        Call_Parent_Method;
    end case;
  end On_Menu_Select;

  procedure On_Message (MDI_Main : in out MDI_Main_Type;
                        message      : in     Interfaces.C.unsigned;
                        wParam       : in     GWindows.Types.Wparam;
                        lParam       : in     GWindows.Types.Lparam;
                        Return_Value : in out GWindows.Types.Lresult)
  is
    use Interfaces.C;

  begin
    if message = Windows_Timers.WM_TIMER then
      if MDI_Main.close_this_search_box then
        MDI_Main.close_this_search_box := False;
        if MDI_Main.Search_box.Visible then
          MDI_Main.Set_Foreground_Window;
          MDI_Main.Focus;
          MDI_Main.Search_box.Hide;
        end if;
      end if;
    end if;
    --  Call parent method
    GWindows.Windows.MDI.MDI_Main_Window_Type (MDI_Main).On_Message (
      message,
      wParam,
      lParam,
      Return_Value
    );
  end On_Message;

  -------------

  procedure On_Close (
        MDI_Main  : in out MDI_Main_Type;
        Can_Close :    out Boolean        ) is
  begin
    MDI_Main.opt.MDI_main_maximized:= Zoom(MDI_Main);
    if not (MDI_Main.opt.MDI_main_maximized or Minimized(MDI_Main)) then
      MDI_Main.opt.win_left  := Left(MDI_Main);
      MDI_Main.opt.win_top   := Top(MDI_Main);
      MDI_Main.opt.win_width := Width(MDI_Main);
      MDI_Main.opt.win_height:= Height(MDI_Main);
    end if;

    --  TC.GWin.Options.Save;

    My_MDI_Close_All(MDI_Main);
    --  ^ Don't forget to save unsaved files !
    --  Operation can be cancelled by user for one unsaved picture.
    Can_Close:= MDI_Main.Success_in_enumerated_close;
    --
    if Can_Close then
      Windows_persistence.Save(MDI_Main.opt);
      --  !! Trick to remove a strange crash on Destroy_Children
      --  !! on certain Windows platforms - 29-Jun-2012
      GWindows.Base.On_Exception_Handler (Handler => null);
      --
      Windows_Timers.Kill_Timer(MDI_Main, timer_id);
      MDI_Main.is_closing := True;
    end if;
  end On_Close;

  -------------
  -- Add_MRU --
  -------------

  procedure Add_MRU (MDI_Main: in out MDI_Main_Type; name: GString; line: Integer) is
    x: Integer:= MDI_Main.opt.mru'First-1;
    up_name: GString:= name;
    mem_line: Natural := 0;
  begin
    To_Upper(up_name);

    --  Search for name in the list.
    for m in MDI_Main.opt.mru'Range loop
      declare
        up_mru_m: GString:= GU2G(MDI_Main.opt.mru(m).name);
      begin
        To_Upper(up_mru_m);
        if up_mru_m = up_name then -- case insensitive comparison (Jan-2007)
          x:= m;
          mem_line := MDI_Main.opt.mru(m).line;
          exit;
        end if;
      end;
    end loop;

    --  Does item's name exist in list ?
    if x /= 0 then
      --  Roll up entries after the item, erasing it.
      for i in x .. MDI_Main.opt.mru'Last-1 loop
        MDI_Main.opt.mru(i):= MDI_Main.opt.mru(i+1);
      end loop;
      MDI_Main.opt.mru(MDI_Main.opt.mru'Last).name:= Null_GString_Unbounded;
    end if;

    --  Roll down the full list
    for i in reverse MDI_Main.opt.mru'First .. MDI_Main.opt.mru'Last-1 loop
      MDI_Main.opt.mru(i+1):= MDI_Main.opt.mru(i);
    end loop;

    if line > -1 then
      mem_line := line;
    end if;
    --  At least now, name will exist in the list
    MDI_Main.opt.mru(MDI_Main.opt.mru'First):= (G2GU(name), mem_line);

  end Add_MRU;

  procedure Update_MRU_Menu (MDI_Main: in out MDI_Main_Type; m: in Menu_Type) is
  begin
    for i in reverse MDI_Main.opt.mru'Range loop
      Text(
        m, Command, MDI_Main.IDM_MRU(i),
         '&' &
         S2G(Ada.Strings.Fixed.Trim(Integer'Image(i),Ada.Strings.Left)) &
         ' ' &
         Shorten_file_name(GU2G(MDI_Main.opt.mru(i).name))
      );
    end loop;
  end Update_MRU_Menu;

  --  Menus of MDI main *and* all children need to have their "View" menu up-to-date.
  --
  procedure Update_View_Menu (m: Menu_Type; o: LEA_Common.User_options.Option_Pack_Type) is
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
  end Update_View_Menu;

  procedure Update_Common_Menus_Child (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
  is
  begin
    if Any_Window /= null and then Any_Window.all in MDI_Child_Type'Class then
      declare
        cw: MDI_Child_Type renames MDI_Child_Type (Any_Window.all);
      begin
        Update_MRU_Menu(cw.MDI_Parent.all, cw.Menu.Popup_0001);
        Update_View_Menu(cw.Menu.Main, cw.MDI_Parent.opt);
        --  Update_Toolbar_Menu(cw.View_menu, cw.MDI_Parent.Floating_toolbars);
      end;
    end if;
  end Update_Common_Menus_Child;

  procedure Update_Common_Menus(
    MDI_Main       : in out MDI_Main_Type;
    top_entry_name : GString := "";
    top_entry_line : Integer := -1    --  When unknown, -1; otherwise: last visited line
  )
  is
  begin
    if top_entry_name /= "" then
      Add_MRU (MDI_Main, top_entry_name, top_entry_line);
    end if;
    Update_MRU_Menu(MDI_Main, MDI_Main.Menu.Popup_0001);
    Update_View_Menu(MDI_Main.Menu.Main, MDI_Main.opt);
    --  Update_Toolbar_Menu(MDI_Main.View_menu, MDI_Main.Floating_toolbars);
    GWindows.Base.Enumerate_Children(
      MDI_Client_Window (MDI_Main).all,
      Update_Common_Menus_Child'Access
    );
  end Update_Common_Menus;

  procedure Update_Title (MDI_Main : in out MDI_Main_Type) is
  begin
    if MDI_Main.Project_File_Name = "" then
      MDI_Main.Text("LEA - [Projectless]");
    else
      MDI_Main.Text("LEA - [" & GU2G(MDI_Main.Project_Short_Name) & ']');
    end if;
  end Update_Title;

  procedure Perform_Search (MDI_Main : MDI_Main_Type; action : LEA_Common.Search_action) is
    procedure Search_on_focused_editor (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class) is
    begin
      if Any_Window /= null
        and then Any_Window.all in MDI_Child_Type'Class
        and then MDI_Main.Focus = Any_Window
      then
        MDI_Child_Type(Any_Window.all).Editor.Search(action);
      end if;
    end Search_on_focused_editor;
  begin
    Enumerate_Children(
      MDI_Client_Window (MDI_Main).all,
      Search_on_focused_editor'Unrestricted_Access
    );
  end Perform_Search;

  --  The operation reciprocal to Memorize_Splitters is done in On_Size.
  --
  procedure Memorize_Splitters (MDI_Main : in out MDI_Main_Type) is
    p : Float;
  begin
    case MDI_Main.opt.view_mode is
      when Notepad =>
        --  Do nothing about project tree splitter: the panel is invisible and not used
        null;
      when Studio =>
        MDI_Main.opt.project_tree_portion :=
          Float (MDI_Main.Project_Panel.Width) /
          Float (MDI_Main.Client_Area_Width);
    end case;
    p :=
      Float (MDI_Main.Message_Panel.Height) /
      Float (MDI_Main.Client_Area_Height - MDI_Main.Tool_Bar.Height);
    p := Float'Max (0.1, p);  --  Avoid complete disappearance
    p := Float'Min (0.9, p);  --  Avoid eating up whole window
    MDI_Main.opt.message_list_portion := p;
    --
    --  NB: the splitter for subprogram tree is part of child window and
    --  memorized at child level.
  end Memorize_Splitters;

end LEA_GWin.MDI_Main;
