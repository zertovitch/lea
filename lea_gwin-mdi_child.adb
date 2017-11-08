with LEA_Common;                        use LEA_Common;
with LEA_Common.User_options;           use LEA_Common.User_options;

with LEA_GWin.Modal_dialogs;            use LEA_GWin.Modal_dialogs;
with LEA_GWin.Search_box;

with GWindows.Base;                     use GWindows.Base;
with GWindows.Common_Dialogs;           use GWindows.Common_Dialogs;
with GWindows.Cursors;                  use GWindows.Cursors;
with GWindows.Menus;                    use GWindows.Menus;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.Scintilla;                use GWindows.Scintilla;

with Ada.Directories;
--  with Ada.Environment_Variables;         use Ada.Environment_Variables;
with Ada.Strings.Wide_Fixed;            use Ada.Strings, Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;

package body LEA_GWin.MDI_Child is

  package Status_bar_parts is
    general_info      : constant := 130;
    length_and_lines  : constant := 170 + general_info;
    line_and_col      : constant := 130 + length_and_lines;
    selection         : constant := 110 + line_and_col;
    eol_indicator     : constant := 130 + selection;
    ansi_unicode      : constant := 120 + eol_indicator;
    ins_ovr           : constant :=  30 + ansi_unicode;
  end;

  overriding procedure On_Click (Bar : in out MDI_Child_Status_Bar_Type) is
    x : Integer;
    parent : MDI_Child_Type renames MDI_Child_Type (Bar.Parent.all);
    use Status_bar_parts;
    frame_width: constant := 8;  --  A hack, guessing the window frame's width
  begin
    x := Get_Cursor_Position.X;
    --  NB: parent.Left is the absolute position of the MDI
    --  Child window, not relative to MDI main!
    x := x - parent.Left - Bar.Left - frame_width;
    if x in length_and_lines .. line_and_col then
      Do_go_to_line (parent);
    end if;
  end On_Click;

  function Folder_Focus(MDI_Child : in MDI_Child_Type) return Boolean is
  begin
    return
      MDI_Child.Parent.opt.view_mode = Studio and then
      MDI_Child.Focus = MDI_Child.Folder_Tree'Unrestricted_Access;
  end Folder_Focus;

  procedure Update_status_bar(MDI_Child : in out MDI_Child_Type) is
    pos, sel_a, sel_z: Scintilla.Position;
  begin
    if MDI_Child.File_Name = Null_GString_Unbounded then
      MDI_Child.Status_Bar.Text("No file", 0);
    end if;
    if Folder_Focus(MDI_Child) then
      MDI_Child.Status_Bar.Text("Folder selected", 0);
      return;
    else
      MDI_Child.Status_Bar.Text("Text file", 0);
    end if;
    MDI_Child.Status_Bar.Text(
      "Length:" & Integer'Wide_Image(MDI_Child.Editor.GetLength) &
      "     Lines:" & Integer'Wide_Image(MDI_Child.Editor.GetLineCount),
      1);
    pos   := MDI_Child.Editor.GetCurrentPos;
    sel_a := MDI_Child.Editor.GetSelectionStart;
    sel_z := MDI_Child.Editor.GetSelectionEnd;
    MDI_Child.Status_Bar.Text(
      "Line:"  & Integer'Wide_Image(1 + MDI_Child.Editor.LineFromPosition(pos)) &
      " Col:" & Integer'Wide_Image(1 + MDI_Child.Editor.GetColumn(pos)),
      2);
    MDI_Child.Status_Bar.Text("Sel:" & Integer'Wide_Image(sel_z - sel_a) &
      " (ln:" & Integer'Wide_Image(
         1 +
         MDI_Child.Editor.LineFromPosition(sel_z) -
         MDI_Child.Editor.LineFromPosition(sel_a)
      ) & ')',
      3);
    case MDI_Child.Editor.GetEOLMode is
      when SC_EOL_CR =>
        MDI_Child.Status_Bar.Text("EOL: Mac (CR)",        4);
      when SC_EOL_CRLF =>
        MDI_Child.Status_Bar.Text("EOL: Windows (CR LF)", 4);
      when SC_EOL_LF =>
        MDI_Child.Status_Bar.Text("EOL: Unix (LF)",       4);
      when others =>
        null;
    end case;
    if MDI_Child.Editor.GetOvertype then
      MDI_Child.Status_Bar.Text("OVR", 6);
    else
      MDI_Child.Status_Bar.Text("INS", 6);
    end if;
  end Update_status_bar;

  procedure Update_tool_bar(MDI_Child : in out MDI_Child_Type) is
    bar: MDI_Toolbar_Type renames MDI_Child.Parent.Tool_Bar;
  begin
    bar.Enabled(IDM_Undo, MDI_Child.Editor.CanUndo);
    bar.Enabled(IDM_Redo, MDI_Child.Editor.CanRedo);
    bar.Enabled(IDM_Save_File, MDI_Child.Editor.modified);
    bar.Enabled(IDM_Save_All, MDI_Child.save_all_hint);
    bar.Enabled(IDM_Indent, True);
    bar.Enabled(IDM_Unindent, True);
    bar.Enabled(IDM_Comment, True);
    bar.Enabled(IDM_Uncomment, True);
    bar.Enabled(IDM_Find, True);
    bar.Enabled(IDM_Show_special_symbols, True);
    if not MDI_Child.is_closing then
      null;  --  bar.Enabled(IDM_ADD_FILES, True);
    end if;
  end Update_tool_bar;

  procedure Update_menus(MDI_Child : in out MDI_Child_Type) is
    bool_to_state: constant array(Boolean) of State_Type := (Disabled, Enabled);
  begin
    State(MDI_Child.Menu.Main, Command, IDM_Undo, bool_to_state(MDI_Child.Editor.CanUndo));
    State(MDI_Child.Menu.Main, Command, IDM_Redo, bool_to_state(MDI_Child.Editor.CanRedo));
    State(MDI_Child.Menu.Main, Command, IDM_Save_File, bool_to_state(MDI_Child.Editor.modified));
    State(MDI_Child.Menu.Main, Command, IDM_Save_All, bool_to_state(MDI_Child.save_all_hint));
  end;

  procedure Update_display(
    MDI_Child : in out MDI_Child_Type;
    need   :        Update_need
  )
  is
  pragma Unreferenced (need);
    any_modified: Boolean:= False;
    --
    procedure Check_any_modified (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    --  Enumeration call back to check if any MDI child window has a modified document
    is
    begin
      if Any_Window.all in MDI_Child_Type'Class then
        any_modified := any_modified or MDI_Child_Type(Any_Window.all).Editor.modified;
      end if;
    end Check_any_modified;

  begin
    GWindows.Base.Enumerate_Children (MDI_Client_Window (MDI_Child.Parent.all).all,
                                      Check_any_modified'Unrestricted_Access);
    MDI_Child.save_all_hint := any_modified;
    if MDI_Child.Editor.modified then
      MDI_Child.Text("* " & GU2G(MDI_Child.Short_Name));
    else
      MDI_Child.Text(GU2G(MDI_Child.Short_Name));
    end if;
    Update_status_bar(MDI_Child);
    Update_tool_bar(MDI_Child);
    Update_menus(MDI_Child);
  end Update_display;

  procedure Memorize_splitter(MDI_Child: in out MDI_Child_Type) is
  begin
    case MDI_Child.Parent.opt.view_mode is
      when Notepad =>
        null; -- do nothing: the splitter is invisible and not used
      when Studio =>
        MDI_Child.Parent.opt.tree_portion:=
          Float(MDI_Child.Folder_Tree.Width) / Float(MDI_Child.Client_Area_Width);
    end case;
  end Memorize_splitter;

  overriding procedure On_Bar_Moved (MDI_Child : in out MDI_Child_GSize_Bar_Type) is
  begin
    Memorize_splitter(MDI_Child_Type(MDI_Child.Parent.Parent.Parent.all));
    GWindows.GControls.GSize_Bars.GSize_Bar_Type(MDI_Child).On_Bar_Moved;
  end On_Bar_Moved;

  procedure Change_View (
        MDI_Child : in out MDI_Child_Type;
        new_view  :        View_Mode_Type;
        force     :        Boolean
  )
  is
    --  mem_sel_path: constant GString_Unbounded:= MDI_Child.selected_path;
    --  sel_node: Tree_Item_Node;
  begin
    if MDI_Child.Parent.opt.view_mode = new_view and not force then
      return;
    end if;
    MDI_Child.Parent.opt.view_mode:= new_view;
    case new_view is
      when Notepad =>
        if not force then
          Memorize_splitter(MDI_Child);
          -- Remember tree portion for user persistence or for next time we toggle back to tree view.
        end if;
        MDI_Child.Splitter.Hide;
        MDI_Child.Folder_Tree.Hide;
      when Studio =>
        MDI_Child.Splitter.Show;
        MDI_Child.Folder_Tree.Show;
    end case;
    MDI_Child.On_Size(MDI_Child.Width, MDI_Child.Height);
    Update_display(MDI_Child, status_bar);
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

  ---------------
  -- On_Create --
  ---------------

  procedure On_Create (MDI_Child : in out MDI_Child_Type) is
    use GWindows.Packing_Boxes;
  begin
    MDI_Child.Small_Icon("LEA_Doc_Icon_Name");

    --  Filial feelings:
    MDI_Child.Parent:= MDI_Main_Access(Controlling_Parent(MDI_Child));
    --  No per-child-window option in this app
    --
    --  --  We copy options to child level:
    --  MDI_Child.opt:= MDI_Child.Parent.opt;

    MDI_Child.Tree_Bar_and_List.Create(MDI_Child, Direction => Horizontal);
    MDI_Child.Tree_Bar_and_List.Dock(At_Top);

    MDI_Child.Folder_Tree.Create(
      MDI_Child.Tree_Bar_and_List,
      1,1,20,20,
      Lines_At_Root => False
    );
    MDI_Child.Folder_Tree.Dock(Fill);

    -- Panel with split bar and list
    MDI_Child.Bar_and_List.Create(MDI_Child.Tree_Bar_and_List, 1,1,20,20);
    MDI_Child.Bar_and_List.Dock(At_Right);

    MDI_Child.Splitter.Create(MDI_Child.Bar_and_List, Fill);
    MDI_Child.Splitter.Dock(At_Left);
    MDI_Child.Splitter_dashes.Create(MDI_Child.Splitter,
      Alignment => GWindows.Static_Controls.Center,
      Text => 1000 * ". " -- A cheap grip design for the split bar...
    );
    MDI_Child.Splitter_dashes.Dock(Fill);
    MDI_Child.Splitter_dashes.Enabled(False); -- Just give a grey look...

    MDI_Child.Editor.mdi_parent:= MDI_Child'Unrestricted_Access;
    MDI_Child.Editor.Create(MDI_Child.Bar_and_List, 50,1,20,20);
    MDI_Child.Editor.Dock(Fill);

    MDI_Child.Status_Bar.Create(MDI_Child, "No file");
    MDI_Child.Status_Bar.Parts (
      (  0 => Status_bar_parts.general_info,      --  General info ("Ada file", ...)
         1 => Status_bar_parts.length_and_lines,  --  Length & lines
         2 => Status_bar_parts.line_and_col,      --  Line / Col
         3 => Status_bar_parts.selection,         --  Selection
         4 => Status_bar_parts.eol_indicator,     --  Unix / Windows / Mac EOLs
         5 => Status_bar_parts.ansi_unicode,      --  ANSI / Unicode
         6 => Status_bar_parts.ins_ovr            --  Ins / Ovr
       )
    );
    MDI_Child.Status_Bar.Dock(At_Bottom);

    MDI_Child.Dock_Children;
    if MDI_Child.Parent.opt.view_mode = Studio then
      Change_View(MDI_Child, Studio, force => True);
    end if;

    LEA_Resource_GUI.Create_Full_Menu(MDI_Child.Menu);
    --  The list of MDI open children will appear below
    --  the menu indicated with Window_Menu (should be the one with Cascade/Tile/...).
    MDI_Child.MDI_Menu(MDI_Child.Menu.Main, Window_Menu => 7);

    -- Maximize-demaximize (non-maximized case) to avoid invisible windows...
    declare
      memo_unmaximized_children: constant Boolean:=
        not MDI_Child.Parent.opt.MDI_childen_maximized;
    begin
      if memo_unmaximized_children then
        MDI_Child.Parent.Freeze;
        MDI_Child.Zoom;
      end if;
      On_Size(MDI_Child,Width(MDI_Child),Height(MDI_Child));
      if memo_unmaximized_children then
        MDI_Child.Parent.Thaw; -- Before Zoom, otherwise uncomplete draw.
        MDI_Child.Zoom(False);
        MDI_Child.Parent.Tool_Bar.Redraw;
      end if;
    end;
    MDI_Child.Update_display(first_display);
    MDI_Child.Accept_File_Drag_And_Drop;
    Ada.Numerics.Float_Random.Reset(MDI_Child.temp_name_gen);
  end On_Create;

  procedure Save (MDI_Child    : in out MDI_Child_Type;
                  File_Name : in     GWindows.GString)
  is
    written_name: GString_Unbounded:=
      To_GString_Unbounded(File_Name);
    temp_ext: constant GString:= ".$$$";
    backup_name: constant GString:= File_Name & ".bak";

    with_backup: constant Boolean:= MDI_Child.Parent.opt.backup = bak;

    --  save_error,
    backup_error_1, backup_error_2, backup_error_3: exception;

    use Ada.Directories;

  begin
    if with_backup then
      written_name:= written_name & temp_ext;
    end if;
    MDI_Child.Editor.Save_text(GU2G(written_name));
    if with_backup then
      --  If there was an exception at writing,
      --  the original file is untouched.
      --
      --  !!  !!  !! MESS with Ada.Directories, UTF, whatever -> use another tactic...
      --
      --  1/ delete old backup
      if File_Exists(To_UTF_8(backup_name)) then
        begin
          Delete_File(To_UTF_8(backup_name));
        exception
          when others =>
            raise backup_error_1;
        end;
      end if;
      --  2/ file -> backup
      if File_Exists(To_UTF_8(File_Name)) then
        begin
          Rename(
            To_UTF_8(File_Name),
            To_UTF_8(backup_name)
          );
        exception
          when others =>
            raise backup_error_2;
        end;
      end if;
      --  3/ new file -> file
      begin
        Rename(
          To_UTF_8(GU2G(written_name)),
          To_UTF_8(File_Name)
        );
      exception
        when others =>
          raise backup_error_3;
      end;
    end if;
    --  The eventual startup extra new window is now saved.
    --  So, in any case, we won't close it now on next window open.
    MDI_Child.Extra_first_doc:= False;
    Update_Common_Menus(MDI_Child, File_Name, MDI_Child.Editor.Get_current_line);
    MDI_Child.Editor.SetSavePoint;
    MDI_Child.Editor.modified:= False;
  exception
    when backup_error_1 =>
      Message_Box (MDI_Child, "Save", "Cannot delete old backup" & NL & "-> " & backup_name, OK_Box, Exclamation_Icon);
    when backup_error_2 =>
      Message_Box (MDI_Child, "Save", "Cannot rename old version to backup" & NL & "-> " & backup_name, OK_Box, Exclamation_Icon);
    when backup_error_3 =>
      Message_Box (MDI_Child, "Save", "Cannot rename new version to actual file" & NL & "-> " & File_Name, OK_Box, Exclamation_Icon);
    when others =>
      Message_Box (MDI_Child, "Save", "Cannot save" & NL & "-> " & File_Name, OK_Box, Exclamation_Icon);
  end Save;

  procedure On_Save (MDI_Child : in out MDI_Child_Type) is
    File_Name : constant GWindows.GString := GU2G (MDI_Child.File_Name);
  begin
    if File_Name = "" then
      MDI_Child.Focus;
      MDI_Child.On_Save_As;
    else
      Save (MDI_Child, File_Name);
    end if;
  end On_Save;

  function Is_file_saved (MDI_Child : in MDI_Child_Type) return Boolean is
  begin
    return not MDI_Child.Editor.modified;
  end Is_file_saved;

  ----------------
  -- On_Save_As --
  ----------------

  procedure On_Save_As (MDI_Child : in out MDI_Child_Type)
  is
    New_File_Name : GWindows.GString_Unbounded;
    File_Title    : GWindows.GString_Unbounded;
    Success       : Boolean;
  begin
    New_File_Name := MDI_Child.File_Name;
    Save_File (
      MDI_Child, "Save file as...", New_File_Name, Ada_files_filters,
      ".ada", File_Title,
      Success
    );
    if not Success then
      return;
    end if;
    if File_Exists(To_UTF_8(GU2G(New_File_Name))) then
      if Message_Box (
        MDI_Child,
        "Save as",
        "The file " & GU2G (New_File_Name) & " already exists. Replace ?",
        Yes_No_Box,
        Question_Icon
      ) = No
      then
        return;
      end if;
    end if;

    Save(MDI_Child, GU2G(New_File_Name));
    MDI_Child.File_Name := New_File_Name;
    MDI_Child.Text(GU2G(File_Title));
    MDI_Child.Short_Name:= File_Title;
    MDI_Child.Update_Common_Menus(GU2G(New_File_Name), MDI_Child.Editor.Get_current_line);
    MDI_Child.Editor.Set_syntax (Guess_syntax (GU2G (MDI_Child.File_Name)));
  end On_Save_As;

  procedure On_Save_All (MDI_Child : in out MDI_Child_Type) is
    --
    procedure Save_any_modified (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
    begin
      if Any_Window.all in MDI_Child_Type'Class then
        declare
          one_child: MDI_Child_Type renames MDI_Child_Type(Any_Window.all);
        begin
          if not one_child.Is_file_saved then
            one_child.On_Save;
          end if;
        end;
      end if;
    end Save_any_modified;
    --
  begin
    GWindows.Base.Enumerate_Children (MDI_Client_Window (MDI_Child.Parent.all).all,
                                      Save_any_modified'Unrestricted_Access);
  end On_Save_All;

  procedure On_File_Drop (MDI_Child     : in out MDI_Child_Type;
                          File_Names : in     Array_Of_File_Names)
  is
    parent    : MDI_Main_Access;
  begin
    MDI_Child.Focus;
    --  We save the parent access since this MDI_Child may be closed when
    --  i > File_Names'First if MDI_Child is a temporary MS-Office-like
    --  blank window - See procedure Close_extra_first_child.
    parent:= MDI_Child.Parent;
    for i in File_Names'Range loop
      Open_Child_Window_And_Load(parent.all, File_Names(i));
    end loop;
  end On_File_Drop;

  --  This will update File menu of parent, itself, and all brothers and sisters
  procedure Update_Common_Menus(MDI_Child : MDI_Child_Type;
    top_entry_name : GString := "";
    top_entry_line : Natural := 0    --  When unknown, 0; otherwise: last visited line
  )
  is
  begin
    Update_Common_Menus( MDI_Child.Parent.all, top_entry_name, top_entry_line );
  end Update_Common_Menus;

  procedure On_Size (MDI_Child : in out MDI_Child_Type;
                     Width  : in     Integer;
                     Height : in     Integer) is
    pragma Warnings (Off, Width);   -- only client area is considered
    pragma Warnings (Off, Height);  -- only client area is considered
    w: constant Natural:= MDI_Child.Client_Area_Width;
    h: constant Natural:= Integer'Max(2, MDI_Child.Client_Area_Height - MDI_Child.Status_Bar.Height);
    splitter_w: constant:= 4; -- between tree and list
    tree_w: constant Integer:= Integer(MDI_Child.Parent.opt.tree_portion * Float(w)) - splitter_w / 2;
    use GWindows.Types;
  begin
    if MDI_Child.Parent.User_maximize_restore then
      MDI_Child.Parent.opt.MDI_childen_maximized:= Zoom(MDI_Child);
    end if;
    MDI_Child.Tree_Bar_and_List.Location(Rectangle_Type'(0, 0, w, h));
    case MDI_Child.Parent.opt.view_mode is
      when Notepad =>
        MDI_Child.Folder_Tree.Location(Rectangle_Type'(0, 0, 1, h));
        MDI_Child.Bar_and_List.Location(Rectangle_Type'(0, 0, w, h));
        MDI_Child.Splitter.Location(Rectangle_Type'(0, 0, 1, h));
        -- !! Editor location MDI_Child.Directory_List.Location(Rectangle_Type'(0, 0, w, h));
      when Studio =>
        MDI_Child.Folder_Tree.Location(Rectangle_Type'(0, 0, tree_w, h));
        MDI_Child.Bar_and_List.Location(Rectangle_Type'(tree_w, 0, w, h));
        -- Splitter bar and directory list are inside the Bar_and_List panel
        MDI_Child.Splitter.Location(Rectangle_Type'(0, 0, splitter_w, h));
        -- !! Editor location .Location(Rectangle_Type'(splitter_w, 0, MDI_Child.Bar_and_List.Width, h));
    end case;
    Dock_Children (MDI_Child);
  end On_Size;

  procedure On_Menu_Select (
        MDI_Child : in out MDI_Child_Type;
        Item   : in     Integer        ) is
  begin
    case Item is
      when IDM_Save_File =>
        MDI_Child.On_Save;
      when IDM_Save_As =>
        MDI_Child.On_Save_As;
      when IDM_Save_All =>
        MDI_Child.On_Save_All;
      when IDM_Close =>
        MDI_Child.Close;
      when IDM_Undo =>
        MDI_Child.Editor.Undo;
        MDI_Child.Update_display(toolbar_and_menu);  --  Eventually disable Undo if no more available
      when IDM_Redo =>
        MDI_Child.Editor.Redo;
        MDI_Child.Update_display(toolbar_and_menu);  --  Eventually disable Redo if no more available
      when IDM_Cut =>           MDI_Child.Editor.Cut;
      when IDM_Copy =>          MDI_Child.Editor.Copy;
      when IDM_Paste =>         MDI_Child.Editor.Paste;
      when IDM_Select_all =>    MDI_Child.Editor.SelectAll;
      when IDM_Indent =>        MDI_Child.Editor.Tab;
      when IDM_Unindent =>      MDI_Child.Editor.BackTab;
      when IDM_Comment =>       MDI_Child.Editor.Selection_comment;
      when IDM_Uncomment =>     MDI_Child.Editor.Selection_uncomment;
      when IDM_Find =>          MDI_Child.Show_Search_Box;
      when IDM_Find_Next =>     MDI_Child.Editor.Search(find_next);
      when IDM_Find_Previous => MDI_Child.Editor.Search(find_previous);
      when IDM_Go_to_line =>    Do_go_to_line (MDI_Child);
      when IDM_Toggle_bookmark =>
        MDI_Child.Editor.Bookmark_toggle (MDI_Child.Editor.Get_current_line);
      when IDM_Next_bookmark =>
        MDI_Child.Editor.Bookmark_next;
      when IDM_Previous_bookmark =>
        MDI_Child.Editor.Bookmark_previous;
      when IDM_Show_special_symbols =>
        Toggle_show_special(MDI_Child.Parent.opt);
        MDI_Child.Editor.Apply_options;
      when IDM_Duplicate =>
        MDI_Child.Editor.Duplicate;
      when IDM_FLAT_VIEW =>
        Change_View(MDI_Child, Notepad, force => False);
      when IDM_TREE_VIEW =>
        Change_View(MDI_Child, Studio, force => False);
      when others =>
        On_Menu_Select (Window_Type (MDI_Child), Item);
    end case;
  end On_Menu_Select;

  overriding procedure On_Focus (MDI_Child : in out MDI_Child_Type) is
  begin
    Update_display(MDI_Child, toolbar_and_menu);
    MDI_Child.Editor.Focus;
  end On_Focus;

  overriding procedure On_Close (MDI_Child    : in out MDI_Child_Type;
                      Can_Close :    out Boolean) is
  begin
    Can_Close:= True;
    if Is_file_saved(MDI_Child) then
      MDI_Child.Update_Common_Menus(GU2G(MDI_Child.File_Name), MDI_Child.Editor.Get_current_line);
    else -- This happens only for documents that may stay in an unsaved state.
      loop
        case Message_Box
               (MDI_Child,
                "Close file", -- sheet, picture, ...
                "Do you want to save the changes you made to " &
                GU2G(MDI_Child.Short_Name) & "' ?",
                Yes_No_Cancel_Box,
                Question_Icon)
        is
          when Yes    => On_Save(MDI_Child);
                         exit when Is_file_saved(MDI_Child);
          when No     => exit;
          when Cancel => MDI_Child.Parent.Success_in_enumerated_close:= False;
                         Can_Close:= False;
                         exit;
          when others => null;
        end case;
      end loop;
    end if;
    if Can_Close then
      --  !! Empty the editor's memory if needed
      --
      --  No per-child-window option in this app
      --  -- Pass view mode and the tree width portion to parent,
      --  -- this will memorize choice of last closed window.
      --  MDI_Child.Parent.opt.view_mode:= MDI_Child.opt.view_mode;
      --
      Memorize_splitter(MDI_Child);
      --  MDI_Child.Parent.opt.tree_portion:= MDI_Child.opt.tree_portion;
      --
      --  For the case there is no more child window, disable toolbar items.
      --  This action is reversed as soon as another child window is focused.
      MDI_Child.Parent.Tool_Bar.Enabled(IDM_Save_File, False);
      MDI_Child.Parent.Tool_Bar.Enabled(IDM_Save_All, False);
      MDI_Child.Parent.Tool_Bar.Enabled(IDM_Undo, False);
      MDI_Child.Parent.Tool_Bar.Enabled(IDM_Redo, False);
      MDI_Child.Parent.Tool_Bar.Enabled(IDM_Comment, False);
      MDI_Child.Parent.Tool_Bar.Enabled(IDM_Uncomment, False);
      MDI_Child.Parent.Tool_Bar.Enabled(IDM_Indent, False);
      MDI_Child.Parent.Tool_Bar.Enabled(IDM_Unindent, False);
      MDI_Child.Parent.Tool_Bar.Enabled(IDM_Find, False);
      MDI_Child.Parent.Tool_Bar.Enabled(IDM_Show_special_symbols, False);
      MDI_Child.is_closing:= True;
    end if;
  end On_Close;

  procedure Show_Search_Box (MDI_Child : in out MDI_Child_Type) is
    sel_a, sel_z: Scintilla.Position;
  begin
    sel_a:= MDI_Child.Editor.GetSelectionStart;
    sel_z:= MDI_Child.Editor.GetSelectionEnd;
    if sel_z > sel_a then
      --  Goodie: put the selected text into the "find" box.
      MDI_Child.Parent.Search_box.Find_box.Text (MDI_Child.Editor.GetTextRange (sel_a, sel_z));
    end if;
    MDI_Child.Parent.Search_box.Show;
    MDI_Child.Parent.Search_box.Find_box.Focus;
  end Show_Search_Box;

end LEA_GWin.MDI_Child;
