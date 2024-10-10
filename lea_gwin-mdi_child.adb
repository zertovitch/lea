with LEA_Common.Syntax,
     LEA_Common.User_options,
     LEA_Common.Color_Themes;

with LEA_GWin.Messages,
     LEA_GWin.Modal_Dialogs,
     LEA_GWin.Options,
     LEA_GWin.Run_Windowed,
     LEA_GWin.Search_Box,
     LEA_GWin.Tabs;

with HAC_Sys.Co_Defs,
     HAC_Sys.Defs,
     HAC_Sys.Errors,
     HAC_Sys.Librarian,
     HAC_Sys.Targets.Semantics;

with GWindows.Base,
     GWindows.Common_Dialogs,
     GWindows.Cursors,
     GWindows.Menus,
     GWindows.Message_Boxes,
     GWindows.Scintilla;

with GWin_Util;

with Ada.Characters.Handling,
     Ada.Calendar,
     Ada.Directories,
     Ada.Exceptions,
     Ada.Strings.Unbounded,
     Ada.Strings.Wide_Fixed,
     Ada.Strings.Wide_Unbounded,
     Ada.Text_IO.Text_Streams;

package body LEA_GWin.MDI_Child is

  use LEA_Common, MDI_Main;
  use GWindows.Message_Boxes, GWindows.Scintilla;
  use Ada.Strings.Wide_Unbounded;

  Options : LEA_Common.User_options.Option_Pack_Type renames LEA_Common.User_options.Options;

  package Status_Bar_Parts is
    --  Values designate the x-value of the *right* side of each part.
    general_info      : constant := 130;
    length_and_lines  : constant := 170 + general_info;
    line_and_col      : constant := 130 + length_and_lines;
    selection         : constant := 110 + line_and_col;
    eol_indicator     : constant := 130 + selection;
    ansi_unicode      : constant := 120 + eol_indicator;
    ins_ovr           : constant :=  30 + ansi_unicode;
  end Status_Bar_Parts;

  overriding procedure On_Click (Bar : in out MDI_Child_Status_Bar_Type) is
    x : Integer;
    parent : MDI_Child_Type renames MDI_Child_Type (Bar.Parent.all);
    use Status_Bar_Parts;
    frame_width : constant := 8;  --  A hack, guessing the window frame's width
    bar_margin  : constant := 2;
  begin
    x := GWindows.Cursors.Get_Cursor_Position.X;
    --  NB: parent.Left is the absolute position of the MDI
    --  Child window, not relative to MDI main!
    x := x - parent.Left - Bar.Left - frame_width;
    if x in length_and_lines + bar_margin .. line_and_col - bar_margin then
      --  Click on the "Line & Column" part.
      Modal_Dialogs.Do_Go_to_Line (parent);
    elsif x in ansi_unicode + bar_margin .. ins_ovr - bar_margin then
      --  Click on INS or OVR to toggle it.
      parent.editor.Edit_Toggle_Overtype;
      parent.Update_Information (status_bar);
    end if;
  end On_Click;

  function Folder_Focus (MDI_Child : in MDI_Child_Type) return Boolean is
  begin
    return
      Options.view_mode = Studio; --  !!  and then
      --  !! Window.Focus = Window.Folder_Tree'Unrestricted_Access;
  end Folder_Focus;

  procedure Apply_Options (Window : in out MDI_Child_Type) is
  begin
    Window.Update_Information (first_display);
    Window.editor.Apply_Options;
  end Apply_Options;

  procedure Update_Information
    (Window : in out MDI_Child_Type;
     need   :        Update_need)
  is
    use GWindows.Common_Controls;
    use LEA_Common.Color_Themes;
    any_modified : Boolean := False;

    procedure Update_Menus is
      use LEA_Resource_GUI, GWindows.Menus;
      is_any_selection : constant Boolean :=
        Window.editor.Get_Selection_Start < Window.editor.Get_Selection_End;
    begin
      State (Window.menu.Main, Command, IDM_Cut,                    bool_to_state (is_any_selection));
      State (Window.menu.Main, Command, IDM_Copy,                   bool_to_state (is_any_selection));
      State (Window.menu.Main, Command, IDM_Paste,                  bool_to_state (Window.editor.Can_Paste));
      State (Window.menu.Main, Command, IDM_Undo,                   bool_to_state (Window.editor.Can_Undo));
      State (Window.menu.Main, Command, IDM_Redo,                   bool_to_state (Window.editor.Can_Redo));
      State (Window.menu.Main, Command, IDM_Open_Containing_Folder, bool_to_state (Length (Window.ID.file_name) > 0));
      State (Window.menu.Main, Command, IDM_Save_File,              bool_to_state (Window.editor.modified));
      State (Window.menu.Main, Command, IDM_Save_All,               bool_to_state (any_modified));
    end Update_Menus;

    procedure Update_Status_Bar is
      pos, sel_a, sel_z : Scintilla.Position;
    begin
      --  Part 0
      Window.Status_Bar.Part_Colors
        (Part             => 0,
         Background_Color => Color_Convert (Theme_Color (status_bar_background)),
         Text_Color       => Color_Convert (Theme_Color (status_bar_foreground)));
      if Window.ID.file_name = Null_GString_Unbounded then
        Window.Status_Bar.Text ("No file", 0, How => Owner_Drawn);
      end if;
      if False then -- Folder_Focus (Window) then  TODO: Manage Folder Focus
        Window.Status_Bar.Text ("Folder selected", 0, How => Owner_Drawn);
        return;
      else
        case Window.editor.document_kind is
          when editable_text =>
            Window.Status_Bar.Text (
              LEA_Common.Syntax.File_type_image (Window.editor.syntax_kind),
              0,
              How => Owner_Drawn
            );
          when help_main =>
            Window.Status_Bar.Text ("Help", 0, How => Owner_Drawn);
        end case;
      end if;

      --  Part 1
      Window.Status_Bar.Part_Colors
        (Part             => 1,
         Background_Color => Color_Convert (Theme_Color (status_bar_background)),
         Text_Color       => Color_Convert (Theme_Color (status_bar_foreground)));
      Window.Status_Bar.Text
        ("Length:" & Window.editor.Get_Length'Wide_Image &
         "     Lines:" & Window.editor.Get_Line_Count'Wide_Image,
         1,
         How => Owner_Drawn);

      --  Part 2
      Window.Status_Bar.Part_Colors
        (Part             => 2,
         Background_Color => Color_Convert (Theme_Color (status_bar_background)),
         Text_Color       => Color_Convert (Theme_Color (status_bar_foreground)));
      pos   := Window.editor.Get_Current_Pos;
      sel_a := Window.editor.Get_Selection_Start;
      sel_z := Window.editor.Get_Selection_End;
      Window.Status_Bar.Text
        ("Line:"  & Integer'Wide_Image (1 + Window.editor.Line_From_Position (pos)) &
         " Col:" & Integer'Wide_Image (1 + Window.editor.Get_Column (pos)),
         2,
         How => Owner_Drawn);

      --  Part 3
      Window.Status_Bar.Part_Colors
        (Part             => 3,
         Background_Color => Color_Convert (Theme_Color (status_bar_background)),
         Text_Color       => Color_Convert (Theme_Color (status_bar_foreground)));
      Window.Status_Bar.Text
        ("Sel:" & Scintilla.Position'Wide_Image (sel_z - sel_a) &
         " (ln:" & Integer'Wide_Image
           (1 +
            Window.editor.Line_From_Position (sel_z) -
            Window.editor.Line_From_Position (sel_a)) & ')',
         3,
         How => Owner_Drawn);

      --  Part 4
      Window.Status_Bar.Part_Colors
        (Part             => 4,
         Background_Color => Color_Convert (Theme_Color (status_bar_background)),
         Text_Color       => Color_Convert (Theme_Color (status_bar_foreground)));
      case Window.editor.Get_EOL_Mode is
        when SC_EOL_CR =>
          Window.Status_Bar.Text ("EOL: Mac (CR)",        4, How => Owner_Drawn);
        when SC_EOL_CRLF =>
          Window.Status_Bar.Text ("EOL: Windows (CR LF)", 4, How => Owner_Drawn);
        when SC_EOL_LF =>
          Window.Status_Bar.Text ("EOL: Unix (LF)",       4, How => Owner_Drawn);
        when others =>
          null;
      end case;

      --  Part 5
      Window.Status_Bar.Part_Colors
        (Part             => 5,
         Background_Color => Color_Convert (Theme_Color (status_bar_background)),
         Text_Color       => Color_Convert (Theme_Color (status_bar_foreground)));

      --  Part 6
      if Window.editor.Get_Overtype then
        Window.Status_Bar.Part_Colors
          (Part             => 6,
           Background_Color => Color_Convert (Theme_Color (status_bar_background)),
           Text_Color       => Color_Convert (Theme_Color (status_bar_foreground_highlighted)));
        Window.Status_Bar.Text ("OVR", 6, How => Owner_Drawn);
      else
        Window.Status_Bar.Part_Colors
          (Part             => 6,
           Background_Color => Color_Convert (Theme_Color (status_bar_background)),
           Text_Color       => Color_Convert (Theme_Color (status_bar_foreground)));
        Window.Status_Bar.Text ("INS", 6, How => Owner_Drawn);
      end if;

      --  Background
      if need = first_display then
        Window.Status_Bar.Background_Color (Color_Convert (Theme_Color (status_bar_background)),
                                            Update_Now => True);
      end if;

    end Update_Status_Bar;

    procedure Update_Tool_Bar is
      bar : Office_Applications.Classic_Main_Tool_Bar_Type
        renames Window.mdi_root.Tool_Bar;
      is_any_selection : constant Boolean :=
        Window.editor.Get_Selection_Start < Window.editor.Get_Selection_End;
      use LEA_Resource_GUI;
    begin
      bar.Enabled (IDM_Undo, Window.editor.Can_Undo);
      bar.Enabled (IDM_Redo, Window.editor.Can_Redo);
      bar.Enabled (IDM_Save_File, Window.editor.modified);
      bar.Enabled (IDM_Save_All, any_modified);
      bar.Enabled (IDM_Cut, is_any_selection);
      bar.Enabled (IDM_Copy, is_any_selection);
      bar.Enabled (IDM_Paste, Window.editor.Can_Paste);
      bar.Enabled (IDM_Indent, True);
      bar.Enabled (IDM_Unindent, True);
      bar.Enabled (IDM_Comment, True);
      bar.Enabled (IDM_Uncomment, True);
      bar.Enabled (IDM_Find, True);
      bar.Enabled (IDM_Build_and_run, True);
      bar.Enabled (IDM_Show_special_symbols, True);
      bar.Enabled (IDM_Show_indentation_lines, True);
      --  if not Window.is_closing then
      --    null;  --  bar.Enabled(IDM_ADD_FILES, True);
      --  end if;
    end Update_Tool_Bar;

    procedure Process_Siblings (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    --  Enumeration call back to check if any MDI child window has a modified document
    is
    begin
      if Any_Window.all in MDI_Child_Type'Class then
        declare
          sibling : MDI_Child_Type renames MDI_Child_Type (Any_Window.all);
          tab_bar : Tabs.LEA_Tab_Bar_Type renames sibling.mdi_root.tab_bar;
        begin
          any_modified := any_modified or sibling.editor.modified;
          --  Adapt title in the tab bar (code similar to that in On_Save_As):
          for index in 0 .. tab_bar.Tab_Count - 1 loop
            if tab_bar.info (index).ID = sibling.ID then
              tab_bar.Text
                (index,
                 (if sibling.editor.modified then "* " else "") &
                 Simple_Name (GU2G (sibling.ID.short_name)));
            end if;
          end loop;
        end;
      end if;
    end Process_Siblings;

  begin
    GWindows.Base.Enumerate_Children
      (MDI_Client_Window (Window.mdi_root.all).all,
       Process_Siblings'Unrestricted_Access);
    if Window.editor.modified then
      Window.Text ("* " & GU2G (Window.ID.short_name));
    else
      Window.Text (GU2G (Window.ID.short_name));
    end if;
    Update_Status_Bar;
    Update_Tool_Bar;
    Update_Menus;
  end Update_Information;

  ---------------
  -- On_Create --
  ---------------

  procedure On_Create (Window : in out MDI_Child_Type) is
    use GWindows.Base;
  begin
    Window.Small_Icon ("LEA_Doc_Icon_Name");

    --  Filial feelings:
    Window.mdi_root := MDI_Main_Access (Controlling_Parent (Window));
    --  No per-child-window option in this app
    --
    --  --  We copy options to child level:
    --  Window.opt:= Options;

    --  Window.Tree_Bar_and_List.Create(Window, Direction => Horizontal);
    --  Window.Tree_Bar_and_List.Dock(At_Top);
    --
    --    Right panel, with subprogram tree:
    --

    --
    --  *** This will be activated with the Subprogram Tree feature ***
    --
    --  Window.Subprogram_Panel.Create (Window, 1,1,20,20);
    --  Window.Subprogram_Panel.Dock (At_Right);
    --  Window.Subprogram_Panel.Splitter.Create (Window.Subprogram_Panel, At_Left);
    --  Window.Subprogram_Panel.Splitter.MDI_Main := Window.MDI_Root;
    --  Window.Subprogram_Panel.Subprogram_Tree.Create (Window.Subprogram_Panel, 1,1,20,20, Lines_At_Root => False);
    --  Window.Subprogram_Panel.Subprogram_Tree.Dock (Fill);

    --  Status_Bar must be created before Editor
    Window.Status_Bar.Create (Window, "No file");
    Window.Status_Bar.Parts (
        (0 => Status_Bar_Parts.general_info,      --  General info ("Ada file", ...)
         1 => Status_Bar_Parts.length_and_lines,  --  Length & lines
         2 => Status_Bar_Parts.line_and_col,      --  Line / Col
         3 => Status_Bar_Parts.selection,         --  Selection
         4 => Status_Bar_Parts.eol_indicator,     --  Unix / Windows / Mac EOLs
         5 => Status_Bar_Parts.ansi_unicode,      --  ANSI / Unicode
         6 => Status_Bar_Parts.ins_ovr            --  Ins / Ovr (Insert vs. Overwrite mode)
       )
    );

    Window.editor.mdi_parent := Window'Unrestricted_Access;
    Window.editor.Create (Window, 50, 1, 20, 20);  --  Widget starts as a small square...
    Window.editor.Dock (Fill);                        --  ...expands into MDI child window.
    Window.editor.Set_EOL_Mode (SC_EOL_LF);  --  Windows 10's cmd and notepad accept LF EOL's.

    Window.Status_Bar.Dock (At_Bottom);
    Window.Dock_Children;

    LEA_Resource_GUI.Create_Full_Menu (Window.menu);
    --  The list of MDI open children will appear below
    --  the menu indicated with Window_Menu (should be the one with Cascade/Tile/...).
    Window.MDI_Menu (Window.menu.Main, Window_Menu => 7);

    --  Maximize-demaximize (non-maximized case) to avoid invisible windows...
    declare
      memo_unmaximized_children : constant Boolean :=
        not Options.MDI_childen_maximized;
    begin
      if memo_unmaximized_children then
        Window.mdi_root.Freeze;
        Window.Zoom;
      end if;
      On_Size (Window, Width (Window), Height (Window));
      if memo_unmaximized_children then
        Window.mdi_root.Thaw;  --  Before Zoom, otherwise drawinf is uncomplete.
        Window.Zoom (False);
        Window.mdi_root.Tool_Bar.Redraw;
      end if;
    end;
    Window.Update_Information (first_display);
    Window.Accept_File_Drag_And_Drop;
    Ada.Numerics.Float_Random.Reset (Window.temp_name_gen);
  end On_Create;

  procedure Create_LEA_MDI_Child
    (Window : in out MDI_Child_Type;
     Parent : in out MDI_Main.MDI_Main_Type;
     ID     : in     ID_Type)
  is
    procedure Append_Tab is
      title : constant GString := GU2G (Window.ID.short_name);
    begin
      Parent.tab_bar.Insert_Tab (Parent.tab_bar.Tab_Count, Simple_Name (title));
      Parent.tab_bar.Selected_Tab (Parent.tab_bar.Tab_Count - 1);
      Parent.tab_bar.info.Append ((Window.ID, Window'Unrestricted_Access));
    end Append_Tab;
  begin
    Window.ID := ID;
    Create_MDI_Child (Window, Parent, GU2G (ID.short_name), Is_Dynamic => True);
    MDI_Active_Window (Parent, Window);
    Append_Tab;
  end Create_LEA_MDI_Child;

  procedure Finish_subwindow_opening (Window : in out MDI_Child_Type) is
    MDI_Main : MDI_Main_Type renames Window.mdi_root.all;
  begin
    MDI_Main.User_maximize_restore := True;
    if Options.MDI_childen_maximized then
      Window.Zoom;
      MDI_Main.Redraw_all;
    end if;
    --  Show things in the main status bar - effective only after Thaw!
  end Finish_subwindow_opening;

  procedure Save (MDI_Child    : in out MDI_Child_Type;
                  File_Name : in     GWindows.GString)
  is
    written_name : GString_Unbounded :=
      To_GString_Unbounded (File_Name);
    temp_ext : constant GString := ".$$$";
    backup_name : constant GString := File_Name & ".bak";

    use LEA_Common.User_options;
    with_backup : constant Boolean := Options.backup = bak;

    backup_error_1, backup_error_2, backup_error_3 : exception;

    use Ada.Directories;

  begin
    if with_backup then
      written_name := written_name & temp_ext;
    end if;
    --
    MDI_Child.editor.Save_Text (GU2G (written_name));
    --
    if with_backup then
      --  If there was an exception at writing,
      --  the original file is untouched.
      --
      --  !! !! Special characters: MESS with Ada.Directories & UTF,
      --        whatever -> use another tactic...
      --
      --  1/ delete old backup
      if File_Exists (To_UTF_8 (backup_name)) then
        begin
          Delete_File (To_UTF_8 (backup_name));
        exception
          when others =>
            raise backup_error_1;
        end;
      end if;
      --  2/ file -> backup
      if File_Exists (To_UTF_8 (File_Name)) then
        begin
          Rename (To_UTF_8 (File_Name), To_UTF_8 (backup_name));
        exception
          when others =>
            raise backup_error_2;
        end;
      end if;
      --  3/ new file -> file
      begin
        Rename (To_UTF_8 (GU2G (written_name)), To_UTF_8 (File_Name));
      exception
        when others =>
          raise backup_error_3;
      end;
    end if;
    --  The possible startup extra new document is now saved as a file.
    --  So, in any case, we won't close it now on next window opening.
    MDI_Child.Extra_First_Doc := False;
    MDI_Child.Update_Common_Menus (File_Name, MDI_Child.editor.Get_Current_Line_Number);
    MDI_Child.editor.Set_Save_Point;
    MDI_Child.editor.modified := False;
    MDI_Child.editor.Set_Read_Only (False);
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

  procedure On_Save (Window : in out MDI_Child_Type) is
    File_Name : constant GWindows.GString := GU2G (Window.ID.file_name);
  begin
    if File_Name = "" or else Window.editor.Get_Read_Only then
      Window.Focus;
      if Window.editor.Get_Read_Only then
        Message_Box (Window, "Save", "This document is read-only" & NL & "You need to save it under another name");
      end if;
      Window.On_Save_As;
    else
      Save (Window, File_Name);
    end if;
  end On_Save;

  overriding function Is_Document_Modified (Window : in MDI_Child_Type) return Boolean is
  begin
    return Window.editor.modified;
  end Is_Document_Modified;

  ----------------
  -- On_Save_As --
  ----------------

  procedure On_Save_As (Window : in out MDI_Child_Type)
  is
    New_File_Name : GWindows.GString_Unbounded;
    File_Title    : GWindows.GString_Unbounded;
    Success       : Boolean;
    New_ID : ID_Type;
    tab_bar : Tabs.LEA_Tab_Bar_Type renames Window.mdi_root.tab_bar;
    use HAC_Sys.Defs;
    use type Alfa;
    main_unit : Alfa renames Window.mdi_root.BD.CD.main_unit_ident_with_case;
  begin
    if main_unit /= Empty_Alfa then
      --  Suggest the Ada main's name of last tentative build.
      New_File_Name :=
        G2GU (S2G (HAC_Sys.Librarian.GNAT_File_Naming (A2S (main_unit)))) &
        ".adb";
    elsif Window.ID.file_name = "" then
      --  No file yet for this window.
      --  Suggest the short window name (window title).
      New_File_Name := Window.ID.short_name;
    else
      --  Tentative name is current file name.
      New_File_Name := Window.ID.file_name;
    end if;
    GWindows.Common_Dialogs.Save_File (
      Window,
      "Save file as...",
      New_File_Name,
      Window.mdi_root.text_files_filters,
      ".ada",
      File_Title,
      Success
    );
    if not Success then
      return;
    end if;
    if File_Exists (To_UTF_8 (GU2G (New_File_Name))) then
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
    --  !!  Address what happens if another open document has the new file name!

    Save (Window, GU2G (New_File_Name));
    Window.Text (GU2G (File_Title));
    New_ID := (file_name => New_File_Name, short_name => File_Title);
    --  Change title in the tab bar.
    for index in 0 .. tab_bar.Tab_Count - 1 loop
      if tab_bar.info (index).ID = Window.ID then
        tab_bar.info (index).ID := New_ID;
        tab_bar.Text (index, Simple_Name (GU2G (New_File_Name)));
        exit;
      end if;
    end loop;
    Window.ID := New_ID;
    Window.Update_Common_Menus (GU2G (New_File_Name), Window.editor.Get_Current_Line_Number);
    Window.editor.syntax_kind :=
      LEA_Common.Syntax.Guess_syntax (
        GU2G (Window.ID.file_name),
        GU2G (Options.ada_files_filter)
      );
    Window.editor.Set_Scintilla_Syntax;
  end On_Save_As;

  procedure On_Save_All (Window : in out MDI_Child_Type) is
    --
    procedure Save_any_modified (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
    begin
      if Any_Window.all in MDI_Child_Type'Class then
        declare
          one_child : MDI_Child_Type renames MDI_Child_Type (Any_Window.all);
        begin
          if one_child.Is_Document_Modified then
            one_child.On_Save;
          end if;
        end;
      end if;
    end Save_any_modified;
    --
  begin
    GWindows.Base.Enumerate_Children (MDI_Client_Window (Window.mdi_root.all).all,
                                      Save_any_modified'Unrestricted_Access);
  end On_Save_All;

  procedure On_File_Drop (Window     : in out MDI_Child_Type;
                          File_Names : in     GWindows.Windows.Array_Of_File_Names)
  is
    parent : MDI_Main_Access;
  begin
    Window.Focus;
    --  We save the parent access because this Window may be already closed
    --  since the second iteration of the loop below if Window is was a temporary
    --  MS-Office-like blank window - See procedure Close_extra_first_child.
    parent := Window.mdi_root;
    for File_Name of File_Names loop
      Open_Child_Window_And_Load (parent.all, GU2G (File_Name));
    end loop;
  end On_File_Drop;

  --  This will update File menu of parent, itself, and all brothers and sisters
  procedure Update_Common_Menus
    (Window : MDI_Child_Type;
     top_entry_name : GString := "";
     top_entry_line : Natural := 0)    --  When unknown, 0; otherwise: last visited line
  is
  begin
    Update_Common_Menus (Window.mdi_root.all, top_entry_name, top_entry_line);
  end Update_Common_Menus;

  procedure On_Size (Window : in out MDI_Child_Type;
                     Width  : in     Integer;
                     Height : in     Integer)
  is
  begin
    if Window.mdi_root.User_maximize_restore then
      Options.MDI_childen_maximized := Window.Zoom;
    end if;
    Window.Dock_Children;
  end On_Size;

  function Best_Name (Window : MDI_Child_Type) return GString is
  begin
    if Length (Window.ID.file_name) = 0 then  --  For example, an unsaved template.
      return GU2G (Window.ID.short_name);
    else
      return GU2G (Window.ID.file_name);
    end if;
  end Best_Name;

  procedure Switch_Current_Directory (Window : MDI_Child_Type) is
    use Ada.Directories;
  begin
    --  We switch the current directory in order to compile other units that
    --  may reside in the same directory as main.
    --  To do: support project files with source paths.
    Set_Directory (Containing_Directory (G2S (GU2G (Window.ID.file_name))));
  exception
    when Ada.Directories.Name_Error => null;  --  Could be a sample, an unsaved file, ...
  end Switch_Current_Directory;

  procedure Build_as_Main (Window : in out MDI_Child_Type) is
    use HAC_Sys.Defs, Messages;
    MDI_Main : MDI_Main_Type renames Window.mdi_root.all;
    ml : Message_List_Type renames MDI_Main.Message_Panel.Message_List;
    message_count, err_count, remark_count : Natural := 0;
    --
    procedure LEA_HAC_Build_Error_Feedback (kit : Diagnostic_Kit) is
      use Ada.Characters.Handling, Ada.Strings, Ada.Strings.Wide_Fixed,
        LEA_Common.Color_Themes;
      msg_up : String :=
        HAC_Sys.Errors.Diagnostic_Prefix (kit.diagnostic_kind) & To_String (kit.message);
      has_dark_background : constant Boolean := Theme_Dark_Backgrounded;
    begin
      msg_up (msg_up'First) := To_Upper (msg_up (msg_up'First));
      ml.Insert_Item
        (Trim (kit.location.line'Wide_Image, Left),
         message_count,
         Icon => Wrench_Icon (kit.repair_kind /= none, has_dark_background));
      --  Here we set a payload in order to get the source file and position
      --  when selecting a row in the error / warnings message list.
      ml.Item_Data
        (message_count,
         new Diagnostic_Kit'(kit));  --  Copy `kit` into a new heap allocated object.
      ml.Set_Sub_Item (S2G (msg_up), message_count, 1);
      message_count := message_count + 1;
      case kit.diagnostic_kind is
        when error       => err_count    := err_count    + 1;
        when Remark_Type => remark_count := remark_count + 1;
        when style       => null;
      end case;
    end LEA_HAC_Build_Error_Feedback;
    --
    procedure LEA_HAC_Build_Feedback (message : String) is
    begin
      ml.Insert_Item ("    ", message_count);
      ml.Set_Sub_Item (S2G (message), message_count, 1);
      message_count := message_count + 1;
    end LEA_HAC_Build_Feedback;
    --
    use HAC_Sys.Builder,
        Ada.Calendar, Ada.Directories, Ada.Strings, Ada.Strings.Wide_Fixed, Ada.Text_IO;
    f : Ada.Text_IO.File_Type;
    file_name  : constant String := G2S (GU2G (Window.ID.file_name));
    short_name : constant String := G2S (GU2G (Window.ID.short_name));
    shebang_offset : Natural;
    t1, t2 : Time;
    compiler_bug : Boolean := False;
    --
  begin
    case Options.toolset is
      when HAC_mode =>
        --  We connect this window's input stream to this window's editor.
        Window.current_editor_stream.Reset
          (Window.editor, shebang_offset);
        Set_Main_Source_Stream
          (Window.mdi_root.BD,
           Window.current_editor_stream'Unchecked_Access,
           G2S (Window.Best_Name),
           shebang_offset);
        Window.Switch_Current_Directory;
        --
        ml.Clear;
        --  Hiding of column 2 is on purpose before setting width for 0 and 1.
        ml.Set_Column ("", 2, 0);
        ml.Set_Column ("Line",     0, 60);
        ml.Set_Column_Scroll_Left ("Message",  1, 800);
        Set_Message_Feedbacks
          (Window.mdi_root.BD,
           (pipe         => LEA_HAC_Build_Error_Feedback'Unrestricted_Access,
            progress     => LEA_HAC_Build_Feedback'Unrestricted_Access,
            detail_level => 1));
        t1 := Clock;
        begin
          Build_Main (Window.mdi_root.BD);
        exception
          when e : others =>
            ml.Insert_Item ("", message_count);
            ml.Set_Sub_Item
              ("*** Crash in HAC - streaming or compiler bug: " &
               S2G (Ada.Exceptions.Exception_Message (e)),
               message_count, 1);
            message_count := message_count + 1;
            compiler_bug := True;
        end;
        t2 := Clock;
        Set_Message_Feedbacks (Window.mdi_root.BD, HAC_Sys.Co_Defs.default_trace);
        if compiler_bug then
          MDI_Main.build_successful := False;
        else
          --  Here we have a single-unit build, from the current child window:
          MDI_Main.build_successful := Build_Successful (Window.mdi_root.BD);
          if err_count = 0 then
            ml.Insert_Item ("", message_count);
            ml.Set_Sub_Item
              ("Build finished in" &
               Duration'Wide_Image (t2 - t1) &
               " seconds." &
               Window.mdi_root.BD.Total_Compiled_Lines'Wide_Image &
               " lines compiled in total. No error," &
               (case remark_count is
                  when 0 => " no note or warning",
                  when 1 => " 1 note or warning",
                  when others => remark_count'Wide_Image & " notes or warnings"),
               message_count, 1);
          else
            --  Jump on first error
            for Index in 0 .. ml.Item_Count - 1 loop
              if Trim (ml.Text (Index, 0), Both) /= "" then
                ml.Selected (Index, True);
                ml.Message_Line_Action (real_click => False);
                exit;
              end if;
            end loop;
          end if;
        end if;
      when GNAT_mode =>
        null;
    end case;
  end Build_as_Main;

  procedure Build (Window : in out MDI_Child_Type) is
  begin
    case Options.toolset is
      when HAC_mode =>
        case Options.view_mode is
          when Notepad =>
            Window.Build_as_Main;
          when Studio =>
            null;
            --  !!  In project/studio mode, we will build
            --      project's main, from editors or files
        end case;
      when GNAT_mode =>
        null;
    end case;
  end Build;

  procedure Build_and_run (Window : in out MDI_Child_Type) is
  begin
    Window.Build;
    if Window.mdi_root.build_successful then
      Run_Windowed (Window);
    end if;
  end Build_and_run;

  procedure On_Menu_Select (
        Window : in out MDI_Child_Type;
        Item      : in     Integer
  )
  is
    use LEA_Resource_GUI;
  begin
    if Window.editor.document_kind /= editable_text then
      --  Call parent method
      GWindows.Windows.Window_Type (Window).On_Menu_Select (Item);
      return;
    end if;
    --
    case Item is
      when IDM_Open_Containing_Folder =>
        if Window.ID.file_name /= "" then
          GWin_Util.Start (Ada.Directories.Containing_Directory (G2S (GU2G (Window.ID.file_name))));
        end if;
      when IDM_Save_File =>
        Window.On_Save;
      when IDM_Save_As =>
        Window.On_Save_As;
      when IDM_Save_All =>
        Window.On_Save_All;
      when IDM_Close =>
        Window.Close;
      when IDM_Undo =>
        Window.editor.Undo;
        Window.Update_Information (toolbar_and_menu);  --  Possibly disable Undo if no more available
      when IDM_Redo =>
        Window.editor.Redo;
        Window.Update_Information (toolbar_and_menu);  --  Possibly disable Redo if no more available
      when IDM_Cut =>           Window.editor.Cut;
      when IDM_Copy =>          Window.editor.Copy;
      when IDM_Paste =>         Window.editor.Paste;
      when IDM_Select_all =>    Window.editor.Select_All;
      when IDM_Indent =>
        Window.editor.Set_Tab_Width (Options.indentation);
        Window.editor.Tab;
        Window.editor.Set_Tab_Width (Options.tab_width);
      when IDM_Unindent =>
        Window.editor.Set_Tab_Width (Options.indentation);
        Window.editor.Back_Tab;
        Window.editor.Set_Tab_Width (Options.tab_width);
      when IDM_Comment =>       Window.editor.Selection_Comment;
      when IDM_Uncomment =>     Window.editor.Selection_Uncomment;
      when IDM_Find =>          Window.Show_Search_Box;
      when IDM_Find_Next =>
        --  If F3 is pressed or "Find next" menu entry is selected
        --  while search box is focused, we need to update the drop-down list(s).
        Window.mdi_root.search_box.Update_Drop_Downs;
        Window.editor.Search (find_next);
      when IDM_Find_Previous =>
        Window.mdi_root.search_box.Update_Drop_Downs;
        Window.editor.Search (find_previous);
      when IDM_Go_to_line =>
        Modal_Dialogs.Do_Go_to_Line (Window);
      when IDM_Toggle_bookmark =>
        Window.editor.Bookmark_Toggle (Window.editor.Get_Current_Line_Number);
      when IDM_Next_bookmark =>
        Window.editor.Bookmark_Next;
      when IDM_Previous_bookmark =>
        Window.editor.Bookmark_Previous;
      --  Compile / Build actions
      when IDM_Compile_single =>  Window.Build_as_Main;
      when IDM_Build          =>  Window.Build;
      when IDM_Build_and_run  =>  Window.Build_and_run;
      --
      when IDM_Show_special_symbols =>
        LEA_Common.User_options.Toggle_show_special (Options);
        LEA_GWin.Options.Apply_Main_Options (Window.mdi_root.all);
      when IDM_Show_indentation_lines =>
        Options.show_indent := not Options.show_indent;
        LEA_GWin.Options.Apply_Main_Options (Window.mdi_root.all);
      when IDM_Duplicate =>
        Window.editor.Duplicate;
      when others =>
        --  Call parent method
        GWindows.Windows.Window_Type (Window).On_Menu_Select (Item);
    end case;
  end On_Menu_Select;

  overriding procedure On_Focus (Window : in out MDI_Child_Type) is
    tab_bar : Tabs.LEA_Tab_Bar_Type renames Window.mdi_root.tab_bar;
    tab_index : Integer;
  begin
    Update_Information (Window, toolbar_and_menu);
    Window.editor.Focus;
    tab_index := tab_bar.Tab_Index (Window.ID);
    if tab_index >= 0 then
      tab_bar.Selected_Tab (tab_index);
    end if;
    Window.editor.Semantics;
  end On_Focus;

  overriding procedure On_Context_Menu
    (Window : in out MDI_Child_Type;
     X      : in     Integer;
     Y      : in     Integer)
  is
    main : constant MDI_Main_Access := Window.mdi_root;
    is_any_selection : constant Boolean :=
      Window.editor.Get_Selection_Start < Window.editor.Get_Selection_End;
    can_paste : constant Boolean := Window.editor.Can_Paste;
    need_separator : Boolean;
    ed_point : GWindows.Types.Point_Type;
    pos : Position;
    use GWindows.Menus, HAC_Sys.Co_Defs, LEA_Resource_GUI;
    --
    procedure Add_Entries_for_Go_to_Declaration is
      decl : Declaration_Point_Pair;
      declarations : Natural;
    begin
      Window.editor.Find_HAC_Declarations (pos, decl (1), decl (2), declarations);
      if declarations > 0 and then not decl (1).is_built_in then
        if need_separator then
          Append_Separator (Window.context_menu);
          need_separator := False;
        end if;
        if not decl (1).self_reference then
          Append_Item
            (Window.context_menu,
             "Go to item declaration",
             IDM_Go_to_memorized_Declaration);
        end if;
        if declarations = 2 and then not decl (2).self_reference then
          Append_Item
            (Window.context_menu,
             "Go to item " &
             (if main.BD_sem.CD.id_table (decl (1).id_index).entity in
                 declared_number_or_enum_item | constant_object | type_mark
              then
                --  Incomplete & complete declarations of record, constant, etc.
                "full declaration"
              else
                --  Spec & body for subprogram, package, task, etc.
                "body"),
             IDM_Go_to_memorized_Body);
        end if;
        main.memo_declaration := decl;
      end if;
    end Add_Entries_for_Go_to_Declaration;
    --
    procedure Add_Entries_for_Spec_Body_Swap is
      fn_curr : constant String := G2S (GU2G (Window.ID.file_name));
      seems_spec : constant Boolean :=
        fn_curr'Length > 0 and then fn_curr (fn_curr'Last) = 's';
      fn_other : constant String :=
        (if seems_spec then
           main.BD_sem.LD.cat.Full_Body_Source_Name (fn_curr)
         else
           main.BD_sem.LD.cat.Full_Spec_Source_Name (fn_curr));
    begin
      if fn_curr'Length > 0 then
        if main.BD_sem.LD.cat.Exists (fn_other) then
          if need_separator then
            Append_Separator (Window.context_menu);
            need_separator := False;
          end if;
          Append_Item
            (Window.context_menu,
             (if seems_spec then
                "Jump to body file"
              else
                "Jump to specification file"),
             IDM_Go_to_other_File);
          main.memo_other_file := G2GU (S2G (fn_other));
        end if;
      end if;
    end Add_Entries_for_Spec_Body_Swap;
    --
  begin
    Window.context_menu := Create_Popup;
    if is_any_selection then
      Append_Item (Window.context_menu, "Cut",    IDM_Cut);
      Append_Item (Window.context_menu, "Copy",   IDM_Copy);
    end if;
    if can_paste then
      Append_Item (Window.context_menu, "Paste",  IDM_Paste);
    end if;
    --
    need_separator := is_any_selection or can_paste;
    if Options.smart_editor then
      case Options.toolset is
        when HAC_mode =>
          Add_Entries_for_Spec_Body_Swap;
          if X >= 0 and then Y >= 0 then
            ed_point := Window.editor.Point_To_Client ((X, Y));
            pos :=
              Window.editor.Position_From_Point_Close (ed_point.X, ed_point.Y);
          else
            --  Invalid mouse coordinates. Likely, the menu key was used.
            pos := Window.editor.Get_Current_Pos;
          end if;
          Add_Entries_for_Go_to_Declaration;
        when GNAT_mode =>
          null;
      end case;
    end if;
    --
    if Count (Window.context_menu) = 0 then
      Append_Item (Window.context_menu, "(empty)", 1);
      State (Window.context_menu, GWindows.Menus.Position, 1, Disabled);
    end if;
    Window.editor.ongoing_context_menu := True;
    Window.editor.Call_Tip_Cancel;
    Immediate_Popup_Menu (Window.context_menu, main.all);
    Destroy_Menu (Window.context_menu);
    Window.editor.ongoing_context_menu := False;
  end On_Context_Menu;

  overriding procedure On_Close (Window    : in out MDI_Child_Type;
                                 Can_Close :    out Boolean)
  is
    use LEA_Resource_GUI;
    bar : Office_Applications.Classic_Main_Tool_Bar_Type
      renames Window.mdi_root.Tool_Bar;
    tab_bar : Tabs.LEA_Tab_Bar_Type renames Window.mdi_root.tab_bar;
  begin
    Can_Close := True;
    if Window.Is_Document_Modified then
      --  This happens only for documents that may stay in an unsaved state.
      loop
        case Message_Box
               (Window,
                "Close file", -- sheet, picture, ...
                "Do you want to save the changes you made to """ &
                GU2G (Window.ID.short_name) & """ ?",
                Yes_No_Cancel_Box,
                Question_Icon)
        is
          when Yes =>
            Window.On_Save;
            exit when not Window.Is_Document_Modified;
          when No =>
            exit;
          when Cancel =>
            Window.mdi_root.Success_in_enumerated_close := False;
            Can_Close := False;
            exit;
          when others => null;
        end case;
        --  We continue in the loop as long as "Yes" was chosen
        --  and the saving was unsuccessful.
      end loop;
    else
      --  We can safely close this document.
      Window.Update_Common_Menus
        (GU2G (Window.ID.file_name), Window.editor.Get_Current_Line_Number);
    end if;
    if Can_Close then
      --  !! Empty the editor's memory if needed
      --
      --  No per-child-window option in this app
      --  -- Pass view mode and the tree width portion to parent,
      --  -- this will memorize choice of last closed window.
      --  Options.view_mode:= Window.opt.view_mode;
      --
      --  !!  Memorize_splitter(Window);
      --  Options.tree_portion:= Window.opt.tree_portion;
      --
      --  For the case there is no more child window, disable toolbar items.
      --  This action is reversed as soon as another child window is focused.
      bar.Enabled (IDM_Save_File, False);
      bar.Enabled (IDM_Save_All, False);
      bar.Enabled (IDM_Undo, False);
      bar.Enabled (IDM_Redo, False);
      bar.Enabled (IDM_Cut, False);
      bar.Enabled (IDM_Copy, False);
      bar.Enabled (IDM_Paste, False);
      bar.Enabled (IDM_Comment, False);
      bar.Enabled (IDM_Uncomment, False);
      bar.Enabled (IDM_Indent, False);
      bar.Enabled (IDM_Unindent, False);
      bar.Enabled (IDM_Find, False);
      bar.Enabled (IDM_Build_and_run, False);
      bar.Enabled (IDM_Show_special_symbols, False);
      bar.Enabled (IDM_Show_indentation_lines, False);
      tab_bar.Delete_Tab (tab_bar.Tab_Index (Window.ID));
      Window.is_closing := True;
    end if;
  end On_Close;

  procedure Show_Search_Box (Window : in out MDI_Child_Type) is
    sel_a, sel_z : Scintilla.Position;
    procedure Feed_Text_Boxes (new_text : GString) is
    begin
      Window.mdi_root.search_box.Update_Drop_Downs;
      Window.mdi_root.search_box.Find_box.Text    (new_text);
      Window.mdi_root.search_box.Replace_box.Text (new_text);
      Window.mdi_root.search_box.Update_Drop_Downs;
    end Feed_Text_Boxes;
  begin
    sel_a := Window.editor.Get_Selection_Start;
    sel_z := Window.editor.Get_Selection_End;
    if sel_z > sel_a then
      --  Goodie: put the selected text into the "find" box and also
      --  the "replace" box.
      --  Two advantages of putting the same text
      --  in the "replace" box are:
      --    - often, one wants a variation of the searched text as replacement
      --    - an accidental replacement won't change the text.
      --  Note that we borrowed this behaviour from GNAT Studio.
      Feed_Text_Boxes (Window.editor.Get_Text_Range (sel_a, sel_z));
    end if;
    Window.mdi_root.search_box.Show;
    Window.mdi_root.search_box.Find_box.Focus;
  end Show_Search_Box;

end LEA_GWin.MDI_Child;
