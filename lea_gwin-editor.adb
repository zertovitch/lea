--  LEA_GWin.Editor is derived from: gnavi\gwindows\samples\scintilla

with LEA_GWin.MDI_Child,
     LEA_GWin.MDI_Main,
     LEA_GWin.Messages;

with LEA_Common.User_options;

with HAC_Sys.Defs;

with GWindows.Colors,
     GWindows.Message_Boxes;

with Ada.Wide_Characters.Handling,
     Ada.Integer_Wide_Text_IO,
     Ada.Streams.Stream_IO,
     Ada.Strings.Wide_Fixed,
     Ada.Strings.Unbounded;

package body LEA_GWin.Editor is

  use LEA_Common;
  use MDI_Main, MDI_Child;
  use Ada.Strings, Ada.Strings.Wide_Fixed;
  use GWindows.Message_Boxes;

  overriding
  procedure On_Change (Editor : in out LEA_Scintilla_Type) is
    --  parent: MDI_Child_Type renames MDI_Child_Type(Editor.MDI_Root.all);
  begin
    --  NB: Status bar display and other changes (menus / icons) is done @ On_Update_UI
    --      Here, it causes a flood of updates on multiline edit.
    null;  --  parent.Update_Information(toolbar_and_menu);
  end On_Change;

  function Matching (c : GCharacter) return GCharacter is
  begin
    case c is
      when '(' => return ')';
      when '"' => return '"';
      when others => return ' ';
    end case;
  end Matching;

  overriding
  procedure On_Character_Added
    (Editor      : in out LEA_Scintilla_Type;
     Special_Key : in     GWindows.Windows.Special_Key_Type;
     Value       : in     GWindows.GCharacter)
  is
  pragma Unreferenced (Special_Key);
    Cur_Pos  : constant Position   := Get_Current_Pos (Editor);
    Line     : constant Integer    := Line_From_Position (Editor, Cur_Pos);
    Prev_Ind : constant Integer    := Get_Line_Indentation (Editor, Line - 1);
    New_Ind  :          Integer;
    Pos      :          Position;
    CR       : constant GCharacter := GCharacter'Val (13);
    LF       : constant GCharacter := GCharacter'Val (10);
    opt : LEA_Common.User_options.Option_Pack_Type
            renames MDI_Child_Type (Editor.mdi_parent.all).MDI_Root.opt;
    use LEA_Common.Syntax;
  begin
    --  This works on Windows (CR, LF) and Unix (LF); we ignore the old Macs (CR).
    case Value is
      when LF =>
        if Line > 0 then
          New_Ind := Prev_Ind;  --  We mimic previous line's indentation.
          if Editor.syntax_kind = Ada_syntax then
            --  Look for extra indentation when the line ends with some specific keywords.
            Pos := Cur_Pos - 1;
            if Editor.Get_Text_Range (Pos - 1, Pos) = (1 => CR) then
              --  Skip the CR in Windows' CR & LF line end.
              --  Reminder: Scintilla's Pos is the cursor's position, *between* characters. So,
              --  (Pos - 1, Pos) wraps *one* character, not *two* like for a slice (Pos - 1 .. Pos).
              Pos := Pos - 1;
            end if;
            if Editor.Get_Text_Range (Pos - 5, Pos) = "begin"
              or else Editor.Get_Text_Range (Pos - 6, Pos) = "record"
              or else Editor.Get_Text_Range (Pos - 1, Pos) = "("
            then
              --  On a "Return" keypress right after "begin", "record" or "(",
              --  we add an extra indentation.
              New_Ind := New_Ind + MDI_Child_Type (Editor.mdi_parent.all).MDI_Root.opt.indentation;
            end if;
          end if;
          if New_Ind > 0 then
            Editor.Add_Text (New_Ind * ' ');
          end if;
        end if;
      when '(' | '"' =>
        if opt.auto_insert then
          if Cur_Pos = Editor.Get_Text_Length
            or else Editor.Get_Text_Range (Cur_Pos, Cur_Pos + 1) /= (1 => Value)
          then
            Editor.Add_Text ((1 => Matching (Value)));
            Editor.Go_To_Pos (Cur_Pos);
          end if;
        end if;
      when others =>
        null;
    end case;
  end On_Character_Added;

  margin_leftmost         : constant := 0;
  margin_for_line_numbers : constant := 1;
  margin_for_bookmarks    : constant := 2;

  marker_for_bookmarks : constant := 0;

  overriding
  procedure On_Create (Editor : in out LEA_Scintilla_Type) is
    use GWindows.Colors;
  begin
    --  Set up editor
    Editor.Set_EOL_Mode (SC_EOL_CRLF);
    Editor.Set_Use_Tabs (False);  --  New Tab keystrokes use space only (Tab character euthanasia).
    Editor.Set_Edge_Mode (EDGE_LINE);
    --
    --  Multi-line edit
    Editor.Set_Multiple_Selection;
    Editor.Set_Mouse_Selection_Rectangular;
    Editor.Set_Additional_Selection_Typing;
    Editor.Set_Virtual_Space_Options (SCVS_RECTANGULARSELECTION);
    --

    Editor.Set_Scintilla_Syntax;

    Editor.Apply_options;

    Editor.Set_Margin_Width_N (margin_for_line_numbers, 50);
    Editor.Set_Margin_Type_N (margin_for_line_numbers, SC_MARGIN_NUMBER);
    Editor.Set_Margin_Width_N (margin_for_bookmarks, 20);
    Editor.Set_Margin_Type_N (margin_for_bookmarks, SC_MARGIN_SYMBOL);
    Editor.Set_Margin_Sensitive_N (margin_for_bookmarks, True);
    Editor.Set_Margin_Mask_N (margin_leftmost,         0);
    Editor.Set_Margin_Mask_N (margin_for_line_numbers, 0);
    Editor.Set_Margin_Mask_N (margin_for_bookmarks,    2 ** marker_for_bookmarks);
    Editor.Marker_Define (marker_for_bookmarks, SC_MARK_BOOKMARK);
    Editor.Marker_Set_Fore (marker_for_bookmarks, Blue);
    Editor.Marker_Set_Back (marker_for_bookmarks, Light_Blue);
    Editor.Focus;
  end On_Create;

  overriding
  procedure On_Margin_Click (Editor  : in out LEA_Scintilla_Type;
                             Pos     : in     Position;
                             Margin  : in     Integer)
  is
    line : constant Integer := Editor.Line_From_Position (Pos);
  begin
    if Margin = margin_for_bookmarks then
      Editor.Bookmark_toggle (line);
    end if;
  end On_Margin_Click;

  overriding
  procedure On_Message
    (Editor       : in out LEA_Scintilla_Type;
     message      : in     Interfaces.C.unsigned;
     wParam       : in     GWindows.Types.Wparam;
     lParam       : in     GWindows.Types.Lparam;
     Return_Value : in out GWindows.Types.Lresult)
  is
    WM_KEYDOWN                 : constant := 256;
    WM_LBUTTONDOWN             : constant := 513;
    WM_RBUTTONDOWN             : constant := 516;
    status_refresh_needed: Boolean:= False;
    parent: MDI_Child_Type renames MDI_Child_Type(Editor.mdi_parent.all);
  begin
    case message is
      when WM_KEYDOWN | WM_LBUTTONDOWN | WM_RBUTTONDOWN =>
        status_refresh_needed := True;  --  Likely, cursor has moved; refresh Line / Col indicator
      when others =>
        null;
    end case;
    --  Call parent method.
    Scintilla_Type(Editor).On_Message(message, wParam, lParam, Return_Value);
    --
    if status_refresh_needed then
      parent.Update_Information (status_bar);
    end if;
  end On_Message;

  overriding
  procedure On_Save_Point_Reached (Editor : in out LEA_Scintilla_Type) is
    parent: MDI_Child_Type renames MDI_Child_Type(Editor.mdi_parent.all);
  begin
    --  We have had enough Undo's to make the document unmodified again.
    Editor.modified:= False;
    parent.Update_Information (toolbar_and_menu);
  end On_Save_Point_Reached;

  overriding
  procedure On_Save_Point_Left (Editor : in out LEA_Scintilla_Type) is
    parent: MDI_Child_Type renames MDI_Child_Type(Editor.mdi_parent.all);
  begin
    --  Either new changes after last saved state, or Undo's from last saved state.
    Editor.modified:= True;
    parent.Update_Information (toolbar_and_menu);
  end On_Save_Point_Left;

  word_highlighting_indicator_index: constant := 0;

  procedure Highlight_word
    (Editor   : in out LEA_Scintilla_Type;
     word     :        GString;
     is_whole :        Boolean
    )
  is
    line : constant Integer := Editor.Get_current_line;
    --  Performance: we scope the highlighting to 'around' lines around current one.
    around : constant := 200;
    line_a : constant Integer := Integer'Max (line - around, 1);
    line_z : constant Integer := Integer'Min (line + around,
      Editor.Line_From_Position (Editor.Get_Length));
    pos_a :          Position := Editor.Position_From_Line (line_a);
    pos_z : constant Position := Editor.Position_From_Line (line_z);
    pos, found_a, found_z : Position;
    sel_a, sel_z : Position;
    flags : Integer := SCFIND_MATCHCASE;
  begin
    Editor.Indicator_Clear_Range (0, Editor.Get_Length);
    if word = "" then
      return;
    end if;
    sel_a:= Editor.Get_Selection_Start;
    sel_z:= Editor.Get_Selection_End;
    Editor.Indic_Set_Style (word_highlighting_indicator_index, INDIC_ROUNDBOX);
    if is_whole then
      flags := flags + SCFIND_WHOLEWORD;
    end if;
    Editor.Set_Search_Flags(flags);
    while pos_a < pos_z loop
      Editor.Set_Target_Start (pos_a);
      Editor.Set_Target_End (pos_z);
      pos := Editor.Search_In_Target(word);
      exit when pos < 0;
      --  Mark the found word
      found_a := Editor.Get_Target_Start;
      found_z := Editor.Get_Target_End;
      if found_a >= sel_a and then found_z <= sel_z then
        null;  --  We don't want highlighting within selection.
      else
        Editor.Indicator_Fill_Range (found_a, found_z - found_a);
      end if;
      --  Restrict search area for next word
      pos_a := found_z;
    end loop;
  end Highlight_word;

  overriding
  procedure On_Update_UI (Editor : in out LEA_Scintilla_Type) is
    parent: MDI_Child_Type renames MDI_Child_Type(Editor.mdi_parent.all);
    pos : constant Position := Editor.Get_Current_Pos;
    p1, p2 : Position := INVALID_POSITION;
    sel_a, sel_z : Position;
    lin_a, lin_z: Integer;
    --
    function Is_parenthesis (s: GString) return Boolean is (s="(" or else s=")");
    is_whole : Boolean;
    --
    function Get_character (pos : Position) return GCharacter is
      s : constant GString := Editor.Get_Text_Range (pos, pos + 1);
    begin
      return s (s'First);
    end Get_character;
    --
    function Is_ident_char (c: GCharacter) return Boolean is
      (c in 'a'..'z' or c in 'A'..'Z' or c in '0'..'9' or c = '_');
  begin
    --  NB: On_Position_Changed is deprecated and inactive in SciLexer v.3.5.6
    if Editor.pos_last_update_UI = pos then  --  Any change ?
      return;
    end if;
    Editor.pos_last_update_UI := pos;
    parent.Update_Information (status_bar);
    --  Highlight instances of selected word
    sel_a:= Editor.Get_Selection_Start;
    sel_z:= Editor.Get_Selection_End;
    if sel_a /= Editor.sel_a_last_update_UI
      or else sel_z /= Editor.sel_z_last_update_UI
    then  --  Any change ?
      Editor.sel_a_last_update_UI := sel_a;
      Editor.sel_z_last_update_UI := sel_z;
      lin_a:= Editor.Line_From_Position (sel_a);
      lin_z:= Editor.Line_From_Position (sel_z);
      if sel_z > sel_a and then lin_a = lin_z then  --  We consider only a selection on one line
        --  If selection is a whole word, we highlight only whole words:
        is_whole :=
          (sel_a = 0 or else not Is_ident_char (Get_character (sel_a - 1)))
          and then
          (sel_z = Editor.Get_Length or else not Is_ident_char (Get_character (sel_z)));
        Highlight_word (Editor, Trim (Editor.Get_Text_Range (sel_a, sel_z), Both), is_whole);
      else
        Editor.Indicator_Clear_Range (0, Editor.Get_Length);
      end if;
    end if;
    --
    --  Parentheses matching
    --
    if pos > 0 and then Is_parenthesis (Editor.Get_Text_Range (pos - 1, pos)) then
      p1 := pos - 1;  --  Found on the left of the cursor
    elsif Is_parenthesis (Editor.Get_Text_Range (pos, pos + 1)) then
      p1 := pos;      --  Found at the cursor
    end if;
    if p1 = INVALID_POSITION then
      --  No parenthesis
      Editor.Brace_Highlight (INVALID_POSITION, INVALID_POSITION);
    else
      p2 := Editor.Brace_Match (p1);
      if p2 = INVALID_POSITION then
        --  Parenthesis unmatched
        Editor.Brace_Bad_Light (p1);
      else
        Editor.Brace_Highlight (p1, p2);
      end if;
    end if;
  end On_Update_UI;

  procedure Apply_options (Editor : in out LEA_Scintilla_Type) is
    use GWindows.Colors;
    --
    type Color_topic is (
      foreground, background,
      keyword, number, comment, string, character,
      error_foreground, error_background,
      caret,
      selection_foreground,
      selection_background,
      matched_parenthesis,
      unmatched_parenthesis,
      parenthesis_background,
      matched_word_highlight
    );
    --
    theme_color : constant array (Color_Theme_Type, Color_topic) of Color_Type :=
      (
        Default =>
          (foreground             => Black,
           background             => White,
           keyword                => Blue,
           number                 => Dark_Orange,
           comment                => Dark_Green,
           string                 => Dark_Gray,
           character              => Dark_Gray,
           error_foreground       => Black,
           error_background       => Pink,
           caret                  => Black,
           selection_foreground   => Black,
           selection_background   => Light_Gray,
           matched_parenthesis    => Dark_Green,
           unmatched_parenthesis  => Dark_Red,
           parenthesis_background => 16#F5E7CB#,
           matched_word_highlight => Dark_Green
          ),
        Dark_side   =>
          (foreground             => Light_Gray,
           background             => 16#242322#,
           keyword                => Dark_Orange,
           number                 => Red,
           comment                => 16#CF9F72#,
           string                 => Yellow,
           character              => Yellow,
           error_foreground       => White,
           error_background       => Dark_Red,
           caret                  => White,
           selection_foreground   => White,
           selection_background   => 16#D28022#,
           matched_parenthesis    => Green,
           unmatched_parenthesis  => Red,
           parenthesis_background => 16#505050#,
           matched_word_highlight => Green
          )
      );
    --
    parent    : MDI_Child_Type renames MDI_Child_Type(Editor.mdi_parent.all);
    mdi_root  : MDI_Main_Type renames parent.MDI_Root.all;
    theme     : Color_Theme_Type renames mdi_root.opt.color_theme;
    Edit_Zone : constant := SCE_ADA_DEFAULT;
  begin
    --  General style
    --  Font color of the line numbers in the left margin:
    Editor.Style_Set_Fore (STYLE_DEFAULT, Gray);
    Editor.Style_Set_Back (STYLE_DEFAULT, theme_color (theme, background));
    Editor.Style_Set_Size (STYLE_DEFAULT, App_default_font_size);
    Editor.Style_Set_Font (STYLE_DEFAULT, App_default_font);
    Editor.Set_Sel_Fore (True, theme_color (theme, selection_foreground));
    Editor.Set_Sel_Back (True, theme_color (theme, selection_background));
    Editor.Style_Clear_All;
    --  Font color of the editor zone (not related to Ada, and works
    --  only *after* Style_Clear_All, for some reason):
    Editor.Style_Set_Fore (SCE_ADA_DEFAULT, theme_color (theme, foreground));
    Editor.Set_Caret_Fore (theme_color (theme, caret));

    if Editor.document_kind /= editable_text then
      Editor.Set_Edge_Mode (EDGE_NONE);
      return;
    end if;

    Editor.Set_Tab_Width (mdi_root.opt.tab_width);
    Editor.Set_Edge_Column (mdi_root.opt.right_margin);

    --  Style: parentheses coloring
    --    For matched parentheses:
    Editor.Style_Set_Fore (STYLE_BRACELIGHT, theme_color (theme, matched_parenthesis));
    Editor.Style_Set_Back (STYLE_BRACELIGHT, theme_color (theme, parenthesis_background));
    --    For unmatched parentheses:
    Editor.Style_Set_Fore (STYLE_BRACEBAD, theme_color (theme, unmatched_parenthesis));
    Editor.Style_Set_Back (STYLE_BRACEBAD, theme_color (theme, parenthesis_background));

    --  Style: Ada-specific coloring
    Editor.Style_Set_Fore (SCE_ADA_DEFAULT, theme_color (theme, foreground));
    Editor.Style_Set_Back (SCE_ADA_DEFAULT, theme_color (theme, background));
    Editor.Style_Set_Size (SCE_ADA_DEFAULT, App_default_font_size);
    Editor.Style_Set_Font (SCE_ADA_DEFAULT, App_default_font);
    --
    Editor.Style_Set_Fore (SCE_ADA_COMMENTLINE, theme_color (theme, comment));
    Editor.Style_Set_Fore (SCE_ADA_NUMBER,      theme_color (theme, number));
    Editor.Style_Set_Fore (SCE_ADA_WORD,        theme_color (theme, keyword));
    Editor.Style_Set_Fore (SCE_ADA_STRING,      theme_color (theme, string));
    Editor.Style_Set_Fore (SCE_ADA_CHARACTER,   theme_color (theme, character));
    Editor.Style_Set_Fore (SCE_ADA_IDENTIFIER,  theme_color (theme, foreground));
    --
    --  Cases where the text is obviously wrong
    --  (unfinished character or string, illegal identifier)
    Editor.Style_Set_Fore (SCE_ADA_CHARACTEREOL, theme_color (theme, error_foreground));
    Editor.Style_Set_Back (SCE_ADA_CHARACTEREOL, theme_color (theme, error_background));
    Editor.Style_Set_Fore (SCE_ADA_STRINGEOL, theme_color (theme, error_foreground));
    Editor.Style_Set_Back (SCE_ADA_STRINGEOL, theme_color (theme, error_background));
    Editor.Style_Set_Fore (SCE_ADA_ILLEGAL, theme_color (theme, error_foreground));
    Editor.Style_Set_Back (SCE_ADA_ILLEGAL, theme_color (theme, error_background));

    Editor.Indic_Set_Fore (
      word_highlighting_indicator_index,
      theme_color (theme, matched_word_highlight)
    );

    case mdi_root.opt.show_special is
      when none =>
        Editor.Set_View_WS (SCWS_INVISIBLE);
        Editor.Set_View_EOL (False);
      when spaces =>
        Editor.Set_View_WS (SCWS_VISIBLEALWAYS);
        Editor.Set_View_EOL (False);
      when spaces_eols =>
        Editor.Set_View_WS (SCWS_VISIBLEALWAYS);
        Editor.Set_View_EOL (True);
    end case;
    Editor.Set_Indentation_Guides (mdi_root.opt.show_indent);

  end Apply_options;

  function Get_current_line (Editor : LEA_Scintilla_Type) return Integer is
  begin
    return Editor.Line_From_Position (Editor.Get_Current_Pos);
  end Get_current_line;

  procedure Set_current_line (Editor : in out LEA_Scintilla_Type; line: Integer) is
    shake: constant:= 10;
  begin
    --  Tactic to show the desired line closer to the middle of the window,
    --  avoiding top or bottom if possible.
    if line > shake then
      Editor.Go_To_Line (line - shake);  --  A bit too high
    end if;
    Editor.Go_To_Line (line + shake);    --  A bit too low
    Editor.Go_To_Line (line);            --  Finally, set the correct line
  end Set_current_line;

  --  If the last line of a selection is fully selected, the end of the selection's
  --  position is at the line *after* the selection and an operation like comment or
  --  uncomment will involve that extra line - a major annoyance!

  procedure Get_Reduced_Selection (Editor : LEA_Scintilla_Type; sel_a, sel_z : out Position) is
    sel_y : Position;
    lin_y, lin_z: Integer;
  begin
    sel_a:= Editor.Get_Selection_Start;
    sel_z:= Editor.Get_Selection_End;
    if sel_z > sel_a then
      sel_y := sel_z - 1;
      lin_y:= Editor.Line_From_Position(sel_y);
      lin_z:= Editor.Line_From_Position(sel_z);
      if lin_y < lin_z then
        sel_z := sel_y;
      end if;
    end if;
  end Get_Reduced_Selection;

  procedure Selection_comment (Editor : in out LEA_Scintilla_Type) is
    --
    blank_line_code : constant := -1;
    --
    procedure Get_visible_indentation
      (s: in GString; ind : out Integer; favorable : out Boolean) is
      use Ada.Wide_Characters.Handling;
    begin
      for i in s'Range loop
        case s (i) is
          when ' ' | GWindows.GCharacter'Val (8) =>
            null;  --  only white space
          when GWindows.GCharacter'Val (13) | GWindows.GCharacter'Val (10) =>
            ind := blank_line_code;
            favorable := False;
            return;
          when others =>
            ind := i - s'First;
            if s (i) = '-' and then i < s'Last and then s (i + 1) = '-' then
              --  A comment
              favorable := True;
            else
              if i + 4 <= s'Last and then To_Upper (s (i .. i + 4)) = "BEGIN" then
                --  Not a good idea to take the indentation from that line...
                favorable := False;
              else
                favorable := True;
              end if;
            end if;
            return;
        end case;
      end loop;
      ind := blank_line_code;
      favorable := False;
    end Get_visible_indentation;
    --
    procedure Get_visible_indentation
      (line: Integer; ind : out Integer; favorable : out Boolean)
    is
      pos, pos_next: Position;
    begin
      pos     := Editor.Position_From_Line (line);
      pos_next:= Editor.Position_From_Line (line+1);
      if pos = pos_next then
        ind := blank_line_code;  --  Empty document case
        favorable := False;
        return;
      end if;
      --  Analyse whole line:
      Get_visible_indentation
        (Editor.Get_Text_Range (pos, pos_next), ind, favorable);
    end Get_visible_indentation;
    --
    pos, sel_a, sel_z: Position;
    ind, ind_prev_line, ind_min, lin_a, lin_z: Integer;
    favorable : Boolean;
  begin
    Get_Reduced_Selection (Editor, sel_a, sel_z);
    lin_a:= Editor.Line_From_Position(sel_a);
    lin_z:= Editor.Line_From_Position(sel_z);
    --  Look for indentation *before* the selected block.
    ind_prev_line:= 0;
    for l in reverse 1 .. lin_a - 1 loop
      Get_visible_indentation (l, ind, favorable);
      if ind > blank_line_code then
        if favorable then
          ind_prev_line := ind;
        else
          --  Fall-back: indentation of first line of the block.
          Get_visible_indentation (lin_a, ind_prev_line, favorable);
        end if;
        exit;
      end if;
    end loop;
    --  Look for the block's minimal indentation (but ignore blank lines for that).
    ind_min:= Integer'Last;
    for l in lin_a .. lin_z loop
      Get_visible_indentation (l, ind, favorable);
      if ind = blank_line_code then
        null;  --  Ignore blank lines for minimal indentation calculation
      else
        ind_min := Integer'Min (ind_min, ind);
      end if;
    end loop;
    if ind_min = Integer'Last then
      ind_min := 0;
    end if;
    --  The whole commenting can be undone and redone in a single "Undo" / Redo":
    Editor.Begin_Undo_Action;
    --
    for l in lin_a .. lin_z loop
      --  1) First, remove leading blanks up to ind_min column.
      pos := Position'Min(
        Editor.Position_From_Line(l) + Position (ind_min),
        --  A blank line (ignored by ind_min) may have less than ind_min columns:
        Editor.Get_Line_Indent_Position (l)
      );
      Editor.Set_Current_Pos (pos);
      Editor.Del_Line_Left;
      --  2) Then, insert an indented "--  ", with a fixed indentation (ind_prev_line)
      --    which is using indentation of any non-blank line above the block.
      pos := Editor.Position_From_Line (l);
      Editor.Insert_Text (pos, ind_prev_line * ' ' & "--  ");
    end loop;
    --
    Editor.End_Undo_Action;
    --  Select the whole block of lines again.
    Editor.Set_Sel (Editor.Position_From_Line(lin_a), Editor.Position_From_Line(lin_z + 1));
  end Selection_comment;

  procedure Selection_uncomment (Editor : in out LEA_Scintilla_Type) is
    pos, sel_a, sel_z : Position;
    lin_a, lin_z : Integer;
  begin
    Get_Reduced_Selection (Editor, sel_a, sel_z);
    lin_a := Editor.Line_From_Position (sel_a);
    lin_z := Editor.Line_From_Position (sel_z);
    --  The whole uncommenting can be undone and redone in a single "Undo" / Redo":
    Editor.Begin_Undo_Action;
    for l in lin_a .. lin_z loop
      pos := Editor.Get_Line_Indent_Position (l);
      if Editor.Get_Text_Range (pos, pos + 4) = "--  " then
        Editor.Set_Sel (pos, pos + 4);
        Editor.Clear;
      elsif Editor.Get_Text_Range (pos, pos + 3) = "-- " then
        Editor.Set_Sel (pos, pos + 3);
        Editor.Clear;
      elsif Editor.Get_Text_Range (pos, pos + 2) = "--" then
        Editor.Set_Sel (pos, pos + 2);
        Editor.Clear;
      end if;
    end loop;
    Editor.End_Undo_Action;
    --  Select the whole block of lines again.
    Editor.Set_Sel (
      Editor.Position_From_Line (lin_a),
      Editor.Position_From_Line (lin_z + 1)
    );
  end Selection_uncomment;

  procedure Search (Editor : in out LEA_Scintilla_Type; action : LEA_Common.Search_action)
  is
    MDI_Child : MDI_Child_Type renames MDI_Child_Type (Editor.mdi_parent.all);
    MDI_Main  : MDI_Main_Type  renames MDI_Child.MDI_Root.all;
    find_str  : constant GString := MDI_Main.Search_box.Find_box.Text;
    repl_str  : constant GString := MDI_Main.Search_box.Replace_box.Text;
    --  replace_str : GString:= MDI_Main.Search_box.Replace_Box.Text;
    pos, sel_a, sel_z : Position;
    line, col, count  : Integer;
    ml : LEA_GWin.Messages.Message_List_Type renames MDI_Main.Message_Panel.Message_List;
    line_msg_col_width : constant := 70;
    col_msg_col_width  : constant := 40;
    function Right_aligned_line_number (line: Positive) return Wide_String is
      s : Wide_String := "12345";
    begin
      Ada.Integer_Wide_Text_IO.Put (s, line);
      return s;
    end Right_aligned_line_number;
    function Right_aligned_column_number (column: Positive) return Wide_String is
      s : Wide_String := "123";
    begin
      Ada.Integer_Wide_Text_IO.Put (s, column);
      return s;
    end Right_aligned_column_number;
    use LEA_GWin.Messages, Ada.Strings.Unbounded;
  begin
    if find_str = "" then  --  Probably a "find next" (F3) with no search string.
      MDI_Child.Show_Search_Box;
      return;
    end if;
    --  Remember selection
    sel_a := Editor.Get_Selection_Start;
    sel_z := Editor.Get_Selection_End;
    Editor.Set_Search_Flags (MDI_Main.Search_box.Compose_Scintilla_search_flags);
    case action is
      when find_next | find_previous =>
        for attempt in 1 .. 2 loop
          if action = find_next then
            Editor.Set_Target_Start (Position'Max (Editor.Get_Current_Pos, Editor.Get_Anchor));
            Editor.Set_Target_End (Editor.Get_Length);
          else  --  action = find_previous
            Editor.Set_Target_Start (Position'Min (Editor.Get_Current_Pos, Editor.Get_Anchor));
            Editor.Set_Target_End (0);
          end if;
          pos := Editor.Search_In_Target (find_str);
          if pos >= 0 then  --  Found
            --  First, we go off a few lines in order to have a good focus.
            Editor.Go_To_Pos (Editor.Get_Target_Start);
            if action = find_next then
              Editor.Go_To_Line (Editor.Get_current_line + 10);
            else
              Editor.Go_To_Line (Editor.Get_current_line - 5);
            end if;
            --  Now we select the found text.
            Editor.Set_Sel (Editor.Get_Target_Start, Editor.Get_Target_End);
            exit;
          elsif attempt = 1 then  --  Not found: wrap around and try again.
            if action = find_next then
              --  Make search from the top of document on 2nd attempt:
              Editor.Set_Sel (0, 0);
            else
              --  Make search from the bottom of document on 2nd attempt:
              Editor.Set_Sel (Editor.Get_Length , Editor.Get_Length);
            end if;
          else
            --  Not found, even *after* the wrap around: find_str is really nowhere!
            --  Restore initial selection
            Editor.Set_Sel (sel_a, sel_z);
            Message_Box (MDI_Child.MDI_Root.Search_box, "Search", "No occurrence found", OK_Box, Information_Icon);
            if MDI_Main.Search_box.Visible then
              MDI_Main.Search_box.Focus;
            end if;
          end if;
        end loop;
      when replace_and_find_next =>
        --  Selection must be identical to the text to be found.
        --  Otherwise, it is just a random, possibly manual, selection.
        --  But wait: we cannot just compare strings: the options (match case, ...) must
        --  must be taken into account as well.
        --  Solution: we search the string *within* the selection, which normally
        --  contains the string itself, from a previous "Find" operation.
        Editor.Set_Target_Start (sel_a);
        Editor.Set_Target_End (sel_z);
        pos := Editor.Search_In_Target (find_str);
        if pos >= 0 then  --  Found
          --  The replacement can be undone and redone in a single "Undo" / Redo":
          Editor.Begin_Undo_Action;
          --  Replace: Clear, then Insert.
          Editor.Clear;
          Editor.Insert_Text (sel_a, repl_str);
          --
          Editor.End_Undo_Action;
          --  Skip the text we just replaced, otherwise
          --  we find always the same text if the replacement
          --  text contains the search string.
          Editor.Set_Current_Pos (sel_a + repl_str'Length);
        end if;
        --  Find next - in any case.
        Editor.Search (action => find_next);
      when find_all =>
        ml.Clear;
        ml.Set_Column ("Line", 0, line_msg_col_width);
        ml.Set_Column ("Col",  1, col_msg_col_width);
        ml.Set_Column (
          "Searching for [" & find_str & ']', 2,
          large_message_width - line_msg_col_width - col_msg_col_width
        );
        --  Prepare a forward search in the entire document:
        Editor.Set_Target_Start (0);
        Editor.Set_Target_End (Editor.Get_Length);
        count := 0;
        loop
          pos := Editor.Search_In_Target (find_str);
          exit when pos < 0;
          line := Editor.Line_From_Position (pos);
          col  := Editor.Get_Column (pos);
          ml.Insert_Item (Right_aligned_line_number (line + 1), count);
          ml.Item_Data(
            count,
            new HAC_Sys.Defs.Diagnostic_Kit'(
              file_name   => To_Unbounded_String (G2S (GU2G (MDI_Child.ID.File_Name))),
              line        => line + 1,  --  Lines in Diagnostic_Kit are 1-based.
              column_a    => col,
              column_z    => col + find_str'Length,
              others      => <>
            )
          );
          ml.Set_Sub_Item (Right_aligned_column_number (col + 1), count, 1);
          ml.Set_Sub_Item (Editor.Get_Line (line), count, 2);
          count := count + 1;
          --  Reduce the search target:
          Editor.Set_Target_Start (Editor.Get_Target_End);
          Editor.Set_Target_End (Editor.Get_Length);
        end loop;
        ml.Set_Column_Scroll_Left
          ("Search for [" & find_str & "] (" &
           Trim (Integer'Wide_Image (count), Left) & " items)", 2,
           large_message_width - line_msg_col_width - col_msg_col_width);
      when replace_all =>
        ml.Clear;
        ml.Set_Column (
          "Replacing all [" & find_str & "] by [" & repl_str & ']', 0,
          large_message_width
        );
        ml.Set_Column ("", 1, 0);
        ml.Set_Column ("", 2, 0);
        --  Prepare a forward search in the entire document:
        Editor.Set_Target_Start (0);
        Editor.Set_Target_End (Editor.Get_Length);
        --  We decide that the "Replace All" replacement can be undone
        --  and redone in single "Undo" or Redo":
        Editor.Begin_Undo_Action;
        --
        count := 0;
        loop
          pos := Editor.Search_In_Target (find_str);
          exit when pos < 0;
          count := count + 1;
          --  Replace: Clear, then Insert.
          Editor.Set_Sel (Editor.Get_Target_Start, Editor.Get_Target_End);
          Editor.Clear;
          Editor.Insert_Text (pos, repl_str);
          --  Reduce the search target:
          Editor.Set_Target_Start (pos + repl_str'Length);
          Editor.Set_Target_End (Editor.Get_Length);
        end loop;
        --
        Editor.End_Undo_Action;
        ml.Set_Column (
          "Replaced all [" & find_str & "] by [" & repl_str & "] (" &
          Trim (Integer'Wide_Image (count), Left) & " items)", 0,
          large_message_width
        );
        Message_Box (
          MDI_Child.MDI_Root.Search_box,
          "Replace all",
          "Replaced all (" &
          Trim (Integer'Wide_Image (count), Left) &
          ") occurrences of" & NL & NL &
          "     [" & find_str & "]" & NL &
          "        by" & NL &
          "     [" & repl_str & "]." & NL & NL &
          "Operation can be undone in one ""Undo"".");
        if MDI_Main.Search_box.Visible then
          --  Without the following, some other application's window
          --  comes to the foreground (typically Explorer), a major annoyance!
          MDI_Main.Set_Foreground_Window;
          MDI_Main.Search_box.Focus;
        end if;
    end case;
  end Search;

  procedure Bookmark_next (Editor : in out LEA_Scintilla_Type) is
    line : constant Integer :=
      Editor.Marker_Next (Editor.Get_current_line + 1, 2 ** marker_for_bookmarks);
  begin
    if line >= 0 then
      Editor.Set_current_line (line);
    end if;
  end Bookmark_next;

  procedure Bookmark_previous (Editor : in out LEA_Scintilla_Type) is
    line : constant Integer :=
      Editor.Marker_Previous (Editor.Get_current_line - 1, 2 ** marker_for_bookmarks);
  begin
    if line >= 0 then
      Editor.Set_current_line (line);
    end if;
  end Bookmark_previous;

  type U32 is mod 2**32;

  procedure Bookmark_toggle (Editor : in out LEA_Scintilla_Type; line : Integer) is
    flags: U32;
    dummy : Integer;
  begin
    flags := U32 (Editor.Marker_Get (line));
    if (flags and 2 ** marker_for_bookmarks) = 0 then
      dummy := Editor.Marker_Add (line, marker_for_bookmarks);
    else
      Editor.Marker_Delete (line, marker_for_bookmarks);
    end if;
  end Bookmark_toggle;

  function EOL (Editor : LEA_Scintilla_Type) return GString is
  begin
    case Editor.Get_EOL_Mode is
      when SC_EOL_CRLF =>
        return GWindows.GCharacter'Val (13) & GWindows.GCharacter'Val (10);
      when SC_EOL_CR =>
        return (1 => GWindows.GCharacter'Val (13));
      when SC_EOL_LF =>
        return (1 => GWindows.GCharacter'Val (10));
      when others =>
        return "";
    end case;
  end EOL;

  procedure Duplicate (Editor : in out LEA_Scintilla_Type) is
    --  NB: the Duplicate feature is actually present in Scintilla
    --  v.3.5.6 and was "accidentally" reprogrammed in full here.
    --  At least, it gives the possibility to customize it...
    pos, sel_a, sel_z, line_start, next_line_start: Position;
    lin : Integer;
    selections : Positive;
  begin
    sel_a:= Editor.Get_Selection_Start;
    sel_z:= Editor.Get_Selection_End;
    pos  := Editor.Get_Current_Pos;
    if sel_a = sel_z then  --  No selection: we duplicate the current line
      lin := Editor.Line_From_Position(sel_a);
      line_start      := Editor.Position_From_Line(lin);
      next_line_start := Editor.Position_From_Line(lin+1);
      if line_start < next_line_start then
        if Editor.Line_From_Position(next_line_start) = lin then
          --  Special case: we are on last line. Actually, next_line_start is the
          --  end of current line - and of the whole document as well.
          --  We need to add an EOL first.
          Editor.Insert_Text (next_line_start,
            EOL(Editor) & Editor.Get_Text_Range (line_start, next_line_start));
        else
          Editor.Insert_Text (next_line_start,
                          Editor.Get_Text_Range (line_start, next_line_start));
        end if;
      end if;
    else  --  There is a selection (or selections): we duplicate it (them).
      selections := Editor.Get_Selections;
      declare
        sel_n_a, sel_n_z, caret_n : array (1..selections) of Position;
        length : Position;
      begin
        for n in 1 .. selections loop
          sel_n_a (n) := Editor.Get_Selection_N_Start (n);
          sel_n_z (n) := Editor.Get_Selection_N_End (n);
          caret_n (n) := Editor.Get_Selection_N_Caret (n);
        end loop;
        Editor.Begin_Undo_Action;
        for n in 1 .. selections loop
          --  Duplicate text at the end of the nth selection.
          length := sel_n_z (n) - sel_n_a (n);
          Editor.Insert_Text (sel_n_z (n), Editor.Get_Text_Range (sel_n_a (n), sel_n_z (n)));
          for nn in 1 .. selections loop
            --  All selections located after the current one will be shifted by the text insertion.
            if sel_n_a (nn) > sel_n_z (n) then
              sel_n_a (nn) := sel_n_a (nn) + length;
              sel_n_z (nn) := sel_n_z (nn) + length;
              caret_n (nn) := caret_n (nn) + length;
            end if;
          end loop;
        end loop;
        Editor.End_Undo_Action;
        if selections = 1 then
          --  Restore selection *and* cursor as before
          if pos = sel_a then
            Editor.Set_Sel (sel_z, sel_a);  --  Right to left: cursor at begin of selection
          else
            Editor.Set_Sel (sel_a, sel_z);  --  Left to right: cursor at end of selection
          end if;
        else
          --  Version for multiple selections (TBD: try removing special case above).
          --  NB: the parameters of Set_Selection are inverted compared to SetSel
          --  (a Scintilla oddity).
          if caret_n (1) = sel_n_a (1) then
            Editor.Set_Selection (sel_n_a (1), sel_n_z (1));
          else
            Editor.Set_Selection (sel_n_z (1), sel_n_a (1));
          end if;
          for n in 2 .. selections loop
            if caret_n (n) = sel_n_a (n) then
              Editor.Add_Selection (sel_n_a (n), sel_n_z (n));
            else
              Editor.Add_Selection (sel_n_z (n), sel_n_a (n));
            end if;
          end loop;
        end if;
      end;
    end if;
  end Duplicate;

  procedure Load_Text (Editor : in out LEA_Scintilla_Type; contents : String) is
    p : Character := ' ';
  begin
    Editor.Set_EOL_Mode (SC_EOL_CRLF);
    for c of contents loop
      if c = ASCII.LF then
        exit when p = ASCII.CR;             --  CR LF
        Editor.Set_EOL_Mode (SC_EOL_LF);    --  non-CR LF (Unix / Linux)
        exit;
      else
        if p = ASCII.CR then                --  CR non-LF (old Mac's)
          Editor.Set_EOL_Mode (SC_EOL_CR);
          exit;
        end if;
      end if;
      p:= c;
    end loop;
    Editor.Insert_Text(0, S2G(contents));  --  ASCII to Unicode (UTF-16) conversion
    Editor.Empty_Undo_Buffer;
    Editor.Set_Save_Point;
    Editor.modified:= False;
  end Load_Text;

  procedure Load_text (Editor : in out LEA_Scintilla_Type) is
    use Ada.Streams.Stream_IO;
    f : File_Type;
    parent : MDI_Child_Type renames MDI_Child_Type (Editor.mdi_parent.all);
  begin
    Open (f, In_File, To_UTF_8 (GU2G (parent.ID.File_Name)), Form_For_IO_Open_and_Create);
    declare
      l : constant Ada.Streams.Stream_IO.Count := Size (f);
      s : String (1 .. Integer (l));
    begin
      String'Read (Stream(f), s);
      Editor.Load_Text (contents => s);
    end;
    Close(f);
  end Load_text;

  procedure Save_text (Editor : in out LEA_Scintilla_Type; under: GString) is
    use Ada.Streams.Stream_IO;
    f : File_Type;
    --  s : aliased Editor_Stream_Type;
    --  c : Character;
  begin
    Create(f, Out_File, To_UTF_8 (under), Form_For_IO_Open_and_Create);
    if Editor.Get_Length > 0 then
      declare
        b : constant GString := Editor.Get_Text_Range (Min => 0, Max => Editor.Get_Length);
      begin
        String'Write (Stream(f), G2S(b));
      end;
    end if;
    Close(f);
    --  We do *not* change Editor.SetSavePoint and Editor.modified until
    --  all operations around backups are successful. This is managed by
    --  the parent window's method, MDI_Child_Type.Save.

    --  --
    --  --  Chunk of code for testing Editor_Stream_Type:
    --  --
    --  Create(f, Out_File, To_UTF_8(under) & "_STREAM_.txt", Form_For_IO_Open_and_Create);
    --  s.Reset (Editor);
    --  begin
    --    loop
    --      Character'Read (s'Access, c);
    --      Character'Write (Stream(f), c);
    --    end loop;
    --  exception
    --    when End_Error => null;
    --  end;
    --  Close(f);
  end Save_text;

  procedure Set_Scintilla_Syntax (Editor : in out LEA_Scintilla_Type) is
    use LEA_Common.Syntax;
  begin
    case Editor.syntax_kind is
      when Undefined =>
        Editor.Set_Lexer (SCLEX_NULL);
        Editor.Set_Key_Words (0, "");
      when Ada_syntax =>
        Editor.Set_Lexer (SCLEX_ADA);
        Editor.Set_Key_Words (0, Ada_keywords);
      when GPR_syntax =>
        Editor.Set_Lexer (SCLEX_ADA);
        Editor.Set_Key_Words (0, GPR_keywords);
        --  !! Issue: keyword'Attribute (e.g. project'Project_Dir)
        --     is not recognized by SCLEX_ADA.
    end case;
  end Set_Scintilla_Syntax;

  --------------------------------------------------------------
  --  Output of the editor's text is used as an input stream  --
  --------------------------------------------------------------

  procedure Reset
    (Stream         : in out Editor_Stream_Type;
     using          : in out LEA_Scintilla_Type'Class;
     shebang_offset :    out Natural)
  is
  begin
    Stream.index   := 0;
    Stream.editor  := using'Unchecked_Access;
    shebang_offset := 0;
    if using.Get_Length > 2 and then using.Get_Text_Range (0, 2) = "#!" then
      --  Start from the second line (Scintilla is 0-based):
      Stream.index := Ada.Streams.Stream_Element_Offset (using.Position_From_Line (1));
      shebang_offset := 1;
    end if;
  end Reset;

  overriding
  procedure Read
    (Stream : in out Editor_Stream_Type;
     Item   :    out Ada.Streams.Stream_Element_Array;
     Last   :    out Ada.Streams.Stream_Element_Offset)
  is
    use Ada.Streams;
    --
    procedure Copy_slice (amount: Ada.Streams.Stream_Element_Offset) is
      slice: constant String :=
        G2S (Stream.editor.Get_Text_Range(
          Position (Stream.index),
          Position (Stream.index + amount)));
      ei: Stream_Element_Offset := Item'First;
    begin
      for s of slice loop
        Item (ei) := Character'Pos(s);
        ei := ei + 1;
      end loop;
      Stream.index := Stream.index + amount;
    end Copy_slice;
  begin
    if Position (Stream.index) >= Stream.editor.Get_Length then
      --  Zero transfer -> Last:= Item'First - 1, see RM 13.13.1(8)
      --  No End_Error here, T'Read will raise it: RM 13.13.2(37)
      if Item'First > Stream_Element_Offset'First then
        Last:= Item'First - 1;
        return;
      else
        --  Well, we cannot return Item'First - 1...
        raise Constraint_Error; -- RM 13.13.1(11) requires this.
      end if;
    end if;
    if Item'Length = 0 then
      --  Nothing to be read actually.
      Last:= Item'Last;  --  Since Item'Length = 0, we have Item'Last < Item'First
      return;
    end if;
    --  From now on, we can assume Item'Length > 0.

    if Position (Stream.index + Item'Length) < Stream.editor.Get_Length then
      --  * Normal case: even after reading, the index will be in the range
      Last := Item'Last;
      Copy_slice (Item'Length);
      --  Now: Stream.index < Editor.GetLength,
      --  then at least one element is left to be read
    else
      --  * Special case: we exhaust the buffer
      Last := Item'First + Stream_Element_Offset (Stream.editor.Get_Length) - 1 - Stream.index;
      Copy_slice (Last - Item'First + 1);
      --  If Last < Item'Last, the T'Read attribute raises End_Error
      --  because of the incomplete reading.
    end if;
  end Read;

  overriding
  procedure Write
    (Stream : in out Editor_Stream_Type;
     Item   : in     Ada.Streams.Stream_Element_Array)
  is
    write_is_not_supported: exception;
  begin
    raise write_is_not_supported;
  end Write;

end LEA_GWin.Editor;
