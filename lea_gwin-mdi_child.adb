with LEA_Common;                        use LEA_Common;
with LEA_Common.User_options;           use LEA_Common.User_options;

with LEA_GWin.Search_box;

with GWindows.Base;                     use GWindows.Base;
with GWindows.Common_Dialogs;           use GWindows.Common_Dialogs;
with GWindows.Menus;                    use GWindows.Menus;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.Scintilla;                use GWindows.Scintilla;

with Ada.Directories;
--  with Ada.Environment_Variables;         use Ada.Environment_Variables;
with Ada.Strings.Wide_Fixed;            use Ada.Strings, Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;

package body LEA_GWin.MDI_Child is

  function Folder_Focus(Window : in MDI_Child_Type) return Boolean is
  begin
    return
      Window.Parent.opt.view_mode = Studio and then
      Window.Focus = Window.Folder_Tree'Unrestricted_Access;
  end Folder_Focus;

  procedure Update_status_bar(Window : in out MDI_Child_Type) is
    pos, sel_a, sel_z: Scintilla.Position;
  begin
    if Window.File_Name = Null_GString_Unbounded then
      Window.Status_Bar.Text("No file", 0);
    end if;
    if Folder_Focus(Window) then
      Window.Status_Bar.Text("Folder selected", 0);
      return;
    else
      Window.Status_Bar.Text("Text file", 0);
    end if;
    Window.Status_Bar.Text(
      "Length:" & Integer'Wide_Image(Window.Editor.GetLength) &
      "     Lines:" & Integer'Wide_Image(Window.Editor.GetLineCount),
      1);
    pos:= Window.Editor.GetCurrentPos;
    sel_a:= Window.Editor.GetSelectionStart;
    sel_z:= Window.Editor.GetSelectionEnd;
    Window.Status_Bar.Text(
      "Line:"  & Integer'Wide_Image(1 + Window.Editor.LineFromPosition(pos)) &
      " Col:" & Integer'Wide_Image(1 + Window.Editor.GetColumn(pos)),
      2);
    Window.Status_Bar.Text("Sel:" & Integer'Wide_Image(sel_z - sel_a) &
      " (ln:" & Integer'Wide_Image(
         1 +
         Window.Editor.LineFromPosition(sel_z) -
         Window.Editor.LineFromPosition(sel_a)
      ) & ')',
      3);
    case Window.Editor.GetEOLMode is
      when SC_EOL_CR =>
        Window.Status_Bar.Text("EOL: Mac (CR)",        4);
      when SC_EOL_CRLF =>
        Window.Status_Bar.Text("EOL: Windows (CR LF)", 4);
      when SC_EOL_LF =>
        Window.Status_Bar.Text("EOL: Unix (LF)",       4);
      when others =>
        null;
    end case;
    if Window.Editor.GetOvertype then
      Window.Status_Bar.Text("OVR", 6);
    else
      Window.Status_Bar.Text("INS", 6);
    end if;
  end Update_status_bar;

  procedure Update_tool_bar(Window : in out MDI_Child_Type) is
    bar: MDI_Toolbar_Type renames Window.Parent.Tool_Bar;
  begin
    bar.Enabled(IDM_Undo, Window.Editor.CanUndo);
    bar.Enabled(IDM_Redo, Window.Editor.CanRedo);
    bar.Enabled(IDM_Save_File, Window.Editor.modified);
    bar.Enabled(IDM_Save_All, Window.save_all_hint);
    bar.Enabled(IDM_Indent, True);
    bar.Enabled(IDM_Unindent, True);
    bar.Enabled(IDM_Comment, True);
    bar.Enabled(IDM_Uncomment, True);
    bar.Enabled(IDM_Find, True);
    bar.Enabled(IDM_Show_special_symbols, True);
    if not Window.is_closing then
      null;  --  bar.Enabled(IDM_ADD_FILES, True);
    end if;
  end Update_tool_bar;

  procedure Update_menus(Window : in out MDI_Child_Type) is
    bool_to_state: constant array(Boolean) of State_Type := (Disabled, Enabled);
  begin
    State(Window.Menu.Main, Command, IDM_Undo, bool_to_state(Window.Editor.CanUndo));
    State(Window.Menu.Main, Command, IDM_Redo, bool_to_state(Window.Editor.CanRedo));
    State(Window.Menu.Main, Command, IDM_Save_File, bool_to_state(Window.Editor.modified));
    State(Window.Menu.Main, Command, IDM_Save_All, bool_to_state(Window.save_all_hint));
  end;

  procedure Update_display(
    Window : in out MDI_Child_Type;
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
    GWindows.Base.Enumerate_Children (MDI_Client_Window (Window.Parent.all).all,
                                      Check_any_modified'Unrestricted_Access);
    Window.save_all_hint := any_modified;
    if Window.Editor.modified then
      Window.Text("* " & GU2G(Window.Short_Name));
    else
      Window.Text(GU2G(Window.Short_Name));
    end if;
    Update_status_bar(Window);
    Update_tool_bar(Window);
    Update_menus(Window);
  end Update_display;

  procedure Memorize_splitter(Window: in out MDI_Child_Type) is
  begin
    case Window.Parent.opt.view_mode is
      when Notepad =>
        null; -- do nothing: the splitter is invisible and not used
      when Studio =>
        Window.Parent.opt.tree_portion:=
          Float(Window.Folder_Tree.Width) / Float(Window.Client_Area_Width);
    end case;
  end Memorize_splitter;

  overriding procedure On_Bar_Moved (Window : in out MDI_Child_GSize_Bar_Type) is
  begin
    Memorize_splitter(MDI_Child_Type(Window.Parent.Parent.Parent.all));
    GWindows.GControls.GSize_Bars.GSize_Bar_Type(Window).On_Bar_Moved;
  end On_Bar_Moved;

  procedure Change_View (
        Window   : in out MDI_Child_Type;
        new_view :        View_Mode_Type;
        force    :        Boolean
  )
  is
    --  mem_sel_path: constant GString_Unbounded:= Window.selected_path;
    --  sel_node: Tree_Item_Node;
  begin
    if Window.Parent.opt.view_mode = new_view and not force then
      return;
    end if;
    Window.Parent.opt.view_mode:= new_view;
    case new_view is
      when Notepad =>
        if not force then
          Memorize_splitter(Window);
          -- Remember tree portion for user persistence or for next time we toggle back to tree view.
        end if;
        Window.Splitter.Hide;
        Window.Folder_Tree.Hide;
      when Studio =>
        Window.Splitter.Show;
        Window.Folder_Tree.Show;
    end case;
    Window.On_Size(Window.Width, Window.Height);
    Update_display(Window, status_bar);
    case new_view is
      when Notepad =>
        null;
      when Studio =>
        null;
          --  (tree) Window.Folder_Tree.Select_Item(sel_node);
          --  (tree) Update_display(Window, node_selected); -- !! update done twice, once for remapping folders
          --  (tree) Window.Folder_Tree.Expand(sel_node);
          --  (tree) Window.Folder_Tree.Focus;
    end case;
  end Change_View;

  ---------------
  -- On_Create --
  ---------------

  procedure On_Create (Window : in out MDI_Child_Type) is
    use GWindows.Packing_Boxes;
  begin
    Window.Small_Icon("LEA_Doc_Icon_Name");

    --  Filial feelings:
    Window.Parent:= MDI_Main_Access(Controlling_Parent(Window));
    --  No per-child-window option in this app
    --
    --  --  We copy options to child level:
    --  Window.opt:= Window.Parent.opt;

    Window.Tree_Bar_and_List.Create(Window, Direction => Horizontal);
    Window.Tree_Bar_and_List.Dock(At_Top);

    Window.Folder_Tree.Create(
      Window.Tree_Bar_and_List,
      1,1,20,20,
      Lines_At_Root => False
    );
    Window.Folder_Tree.Dock(Fill);

    -- Panel with split bar and list
    Window.Bar_and_List.Create(Window.Tree_Bar_and_List, 1,1,20,20);
    Window.Bar_and_List.Dock(At_Right);

    Window.Splitter.Create(Window.Bar_and_List, Fill);
    Window.Splitter.Dock(At_Left);
    Window.Splitter_dashes.Create(Window.Splitter,
      Alignment => GWindows.Static_Controls.Center,
      Text => 1000 * ". " -- A cheap grip design for the split bar...
    );
    Window.Splitter_dashes.Dock(Fill);
    Window.Splitter_dashes.Enabled(False); -- Just give a grey look...

    Window.Editor.mdi_parent:= Window'Unrestricted_Access;
    Window.Editor.Create(Window.Bar_and_List, 50,1,20,20);
    Window.Editor.Dock(Fill);

    Window.Status_Bar.Create(Window, "No file");
    Window.Status_Bar.Parts(
      (0 => 130,  --  General info ("Ada file", ...)
       1 => 300,  --  Length & lines
       2 => 430,  --  Line / Col
       3 => 540,  --  Selection
       4 => 670,  --  Unix / Windows / Mac EOLs
       5 => 790,  --  ANSI / Unicode
       6 => 820   --  Ins / Ovr
       )
    );
    Window.Status_Bar.Dock(At_Bottom);

    Window.Dock_Children;
    if Window.Parent.opt.view_mode = Studio then
      Change_View(Window, Studio, force => True);
    end if;

    LEA_Resource_GUI.Create_Full_Menu(Window.Menu);
    --  The list of MDI open children will appear below
    --  the menu indicated with Window_Menu (should be the one with Cascade/Tile/...).
    Window.MDI_Menu(Window.Menu.Main, Window_Menu => 6);

    -- Maximize-demaximize (non-maximized case) to avoid invisible windows...
    declare
      memo_unmaximized_children: constant Boolean:=
        not Window.Parent.opt.MDI_childen_maximized;
    begin
      if memo_unmaximized_children then
        Window.Parent.Freeze;
        Window.Zoom;
      end if;
      On_Size(Window,Width(Window),Height(Window));
      if memo_unmaximized_children then
        Window.Parent.Thaw; -- Before Zoom, otherwise uncomplete draw.
        Window.Zoom(False);
        Window.Parent.Tool_Bar.Redraw;
      end if;
    end;
    Window.Update_display(first_display);
    Window.Accept_File_Drag_And_Drop;
    Ada.Numerics.Float_Random.Reset(Window.temp_name_gen);
  end On_Create;

  procedure Save (Window    : in out MDI_Child_Type;
                  File_Name : in     GWindows.GString)
  is
    written_name: GString_Unbounded:=
      To_GString_Unbounded(File_Name);
    temp_ext: constant GString:= ".$$$";
    backup_name: constant GString:= File_Name & ".bak";

    with_backup: constant Boolean:= Window.Parent.opt.backup = bak;

    --  save_error,
    backup_error_1, backup_error_2, backup_error_3: exception;

    use Ada.Directories;

  begin
    if with_backup then
      written_name:= written_name & temp_ext;
    end if;
    Window.Editor.Save_text(GU2G(written_name));
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
    Window.Extra_first_doc:= False;
    Update_Common_Menus(Window, File_Name, Window.Editor.Get_current_line);
    Window.Editor.SetSavePoint;
    Window.Editor.modified:= False;
  exception
    when backup_error_1 =>
      Message_Box (Window, "Save", "Cannot delete old backup" & NL & "-> " & backup_name, OK_Box, Exclamation_Icon);
    when backup_error_2 =>
      Message_Box (Window, "Save", "Cannot rename old version to backup" & NL & "-> " & backup_name, OK_Box, Exclamation_Icon);
    when backup_error_3 =>
      Message_Box (Window, "Save", "Cannot rename new version to actual file" & NL & "-> " & File_Name, OK_Box, Exclamation_Icon);
    when others =>
      Message_Box (Window, "Save", "Cannot save" & NL & "-> " & File_Name, OK_Box, Exclamation_Icon);
  end Save;

  procedure On_Save (Window : in out MDI_Child_Type) is
    File_Name : constant GWindows.GString := To_GString_From_Unbounded (Window.File_Name);
  begin
    if File_Name = "" then
      On_Save_As (Window);
    else
      Save (Window, File_Name);
    end if;
  end On_Save;

  function Is_file_saved (Window : in MDI_Child_Type) return Boolean is
  begin
    return not Window.Editor.modified;
  end Is_file_saved;

  ----------------
  -- On_Save_As --
  ----------------

  procedure On_Save_As (Window : in out MDI_Child_Type)
  is
    New_File_Name : GWindows.GString_Unbounded;
    File_Title    : GWindows.GString_Unbounded;
    Success       : Boolean;
  begin
    New_File_Name := Window.File_Name;
    Save_File (
      Window, "Save file as...", New_File_Name, Ada_files_filters,
      ".ada", File_Title,
      Success
    );
    if not Success then
      return;
    end if;
    if File_Exists(To_UTF_8(GU2G(New_File_Name))) then
      if Message_Box (
        Window,
        "Save as",
        "The file " & GU2G (New_File_Name) & " already exists. Replace ?",
        Yes_No_Box,
        Question_Icon
      ) = No
      then
        return;
      end if;
    end if;

    Save(Window, GU2G(New_File_Name));
    Window.File_Name := New_File_Name;
    Window.Text(GU2G(File_Title));
    Window.Short_Name:= File_Title;
    Window.Update_Common_Menus(GU2G(New_File_Name), Window.Editor.Get_current_line);
    Window.Editor.Set_syntax (Guess_syntax (GU2G (Window.File_Name)));
  end On_Save_As;

  procedure On_Save_All (Window : in out MDI_Child_Type) is
    --
    procedure Save_any_modified (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
    begin
      if Any_Window.all in MDI_Child_Type'Class then
        declare
          one_child: MDI_Child_Type renames MDI_Child_Type(Any_Window.all);
        begin
          if not one_child.Is_file_saved then
            one_child.Save(GU2G (one_child.File_Name));
          end if;
        end;
      end if;
    end Save_any_modified;
    --
  begin
    GWindows.Base.Enumerate_Children (MDI_Client_Window (Window.Parent.all).all,
                                      Save_any_modified'Unrestricted_Access);
  end On_Save_All;

  procedure On_File_Drop (Window     : in out MDI_Child_Type;
                          File_Names : in     Array_Of_File_Names)
  is
    parent    : MDI_Main_Access;
  begin
    Window.Focus;
    --  We save the parent access since this Window may be closed when
    --  i > File_Names'First if Window is a temporary MS-Office-like
    --  blank window - See procedure Close_extra_first_child.
    parent:= Window.Parent;
    for i in File_Names'Range loop
      Open_Child_Window_And_Load(parent.all, File_Names(i));
    end loop;
  end On_File_Drop;

  --  This will update File menu of parent, itself, and all brothers and sisters
  procedure Update_Common_Menus(Window : MDI_Child_Type;
    top_entry_name : GString := "";
    top_entry_line : Natural := 0    --  When unknown, 0; otherwise: last visited line
  )
  is
  begin
    Update_Common_Menus( Window.Parent.all, top_entry_name, top_entry_line );
  end Update_Common_Menus;

  procedure On_Size (Window : in out MDI_Child_Type;
                     Width  : in     Integer;
                     Height : in     Integer) is
    pragma Warnings (Off, Width);   -- only client area is considered
    pragma Warnings (Off, Height);  -- only client area is considered
    w: constant Natural:= Window.Client_Area_Width;
    h: constant Natural:= Integer'Max(2, Window.Client_Area_Height - Window.Status_Bar.Height);
    splitter_w: constant:= 4; -- between tree and list
    tree_w: constant Integer:= Integer(Window.Parent.opt.tree_portion * Float(w)) - splitter_w / 2;
    use GWindows.Types;
  begin
    if Window.Parent.User_maximize_restore then
      Window.Parent.opt.MDI_childen_maximized:= Zoom(Window);
    end if;
    Window.Tree_Bar_and_List.Location(Rectangle_Type'(0, 0, w, h));
    case Window.Parent.opt.view_mode is
      when Notepad =>
        Window.Folder_Tree.Location(Rectangle_Type'(0, 0, 1, h));
        Window.Bar_and_List.Location(Rectangle_Type'(0, 0, w, h));
        Window.Splitter.Location(Rectangle_Type'(0, 0, 1, h));
        -- !! Editor location Window.Directory_List.Location(Rectangle_Type'(0, 0, w, h));
      when Studio =>
        Window.Folder_Tree.Location(Rectangle_Type'(0, 0, tree_w, h));
        Window.Bar_and_List.Location(Rectangle_Type'(tree_w, 0, w, h));
        -- Splitter bar and directory list are inside the Bar_and_List panel
        Window.Splitter.Location(Rectangle_Type'(0, 0, splitter_w, h));
        -- !! Editor location .Location(Rectangle_Type'(splitter_w, 0, Window.Bar_and_List.Width, h));
    end case;
    Dock_Children (Window);
  end On_Size;

  procedure On_Menu_Select (
        Window : in out MDI_Child_Type;
        Item   : in     Integer        ) is
  begin
    case Item is
      when IDM_Save_File =>
        Window.On_Save;
      when IDM_Save_As =>
        Window.On_Save_As;
      when IDM_Save_All =>
        Window.On_Save_All;
      when IDM_Close =>
        Window.Close;
      when IDM_Undo =>
        Window.Editor.Undo;
        Window.Update_display(toolbar_and_menu);  --  Eventually disable Undo if no more available
      when IDM_Redo =>
        Window.Editor.Redo;
        Window.Update_display(toolbar_and_menu);  --  Eventually disable Redo if no more available
      when IDM_Select_all =>
        Window.Editor.SelectAll;
      when IDM_Indent =>
        Window.Editor.Tab;
      when IDM_Unindent =>
        Window.Editor.BackTab;
      when IDM_Comment =>
        Window.Editor.Selection_comment;
      when IDM_Uncomment =>
        Window.Editor.Selection_uncomment;
      when IDM_Find =>
        Window.Show_Search_Box;
      when IDM_Find_Next =>
        Window.Editor.Search(find_next);
      when IDM_Show_special_symbols =>
        Toggle_show_special(Window.Parent.opt);
        Window.Editor.Apply_options;
      when IDM_Duplicate =>
        Window.Editor.Duplicate;
      when IDM_FLAT_VIEW =>
        Change_View(Window, Notepad, force => False);
      when IDM_TREE_VIEW =>
        Change_View(Window, Studio, force => False);
      when others =>
        On_Menu_Select (Window_Type (Window), Item);
    end case;
  end On_Menu_Select;

  overriding procedure On_Focus (Window : in out MDI_Child_Type) is
  begin
    Update_display(Window, toolbar_and_menu);
    Window.Editor.Focus;
  end On_Focus;

  overriding procedure On_Close (Window    : in out MDI_Child_Type;
                      Can_Close :    out Boolean) is
  begin
    Can_Close:= True;
    if Is_file_saved(Window) then
      Window.Update_Common_Menus(GU2G(Window.File_Name), Window.Editor.Get_current_line);
    else -- This happens only for documents that may stay in an unsaved state.
      loop
        case Message_Box
               (Window,
                "Close file", -- sheet, picture, ...
                "Do you want to save the changes you made to " &
                GU2G(Window.Short_Name) & "' ?",
                Yes_No_Cancel_Box,
                Question_Icon)
        is
          when Yes    => On_Save(Window);
                         exit when Is_file_saved(Window);
          when No     => exit;
          when Cancel => Window.Parent.Success_in_enumerated_close:= False;
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
      --  Window.Parent.opt.view_mode:= Window.opt.view_mode;
      --
      Memorize_splitter(Window);
      --  Window.Parent.opt.tree_portion:= Window.opt.tree_portion;
      --
      --  For the case there is no more child window, disable toolbar items.
      --  This action is reversed as soon as another child window is focused.
      Window.Parent.Tool_Bar.Enabled(IDM_Save_File, False);
      Window.Parent.Tool_Bar.Enabled(IDM_Save_All, False);
      Window.Parent.Tool_Bar.Enabled(IDM_Undo, False);
      Window.Parent.Tool_Bar.Enabled(IDM_Redo, False);
      Window.Parent.Tool_Bar.Enabled(IDM_Comment, False);
      Window.Parent.Tool_Bar.Enabled(IDM_Uncomment, False);
      Window.Parent.Tool_Bar.Enabled(IDM_Indent, False);
      Window.Parent.Tool_Bar.Enabled(IDM_Unindent, False);
      Window.Parent.Tool_Bar.Enabled(IDM_Find, False);
      Window.Parent.Tool_Bar.Enabled(IDM_Show_special_symbols, False);
      Window.is_closing:= True;
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
