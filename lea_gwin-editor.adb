--  LEA_GWin.Editor is derived from: gnavi\gwindows\samples\scintilla

with LEA_GWin.MDI_Child;                use LEA_GWin.MDI_Child;
with LEA_GWin.MDI_Main;                 use LEA_GWin.MDI_Main;

with GWindows.Colors;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Strings.Wide_Fixed;            use Ada.Strings, Ada.Strings.Wide_Fixed;

package body LEA_GWin.Editor is

  Ada_keywords : constant GWindows.GString :=
    "abort abs abstract accept access aliased all and array at begin body case " &
    "constant declare delay delta digits do else elsif end entry exception " &
    "exit for function generic goto if in interface is limited loop mod new not null of " &
    "or others out overriding package pragma private procedure protected raise range " &
    "record rem renames requeue return reverse select separate some subtype synchronized tagged " &
    "task terminate then type until use when while with xor";

  --  Other keyword sets in mind:
  --  - GNAT project files

  overriding
  procedure On_Change (Editor : in out LEA_Scintilla_Type) is
    --  parent: MDI_Child_Type renames MDI_Child_Type(Editor.mdi_parent.all);
  begin
    --  NB: Status bar display and other changes (menus / icons) is done @ On_Update_UI
    --      Here, it causes a flood of updates on multiline edit.
    null;  --  parent.Update_display(toolbar_and_menu);
  end On_Change;

  overriding
  procedure On_Character_Added
    (Editor      : in out LEA_Scintilla_Type;
     Special_Key : in     GWindows.Windows.Special_Key_Type;
     Value       : in     GWindows.GCharacter)
  is
  pragma Unreferenced (Special_Key);
    CurPos : constant Position := GetCurrentPos (Editor);
    Line     : constant Integer := LineFromPosition (Editor, CurPos);
    Prev_Ind : constant Integer := GetLineIndentation (Editor, Line - 1);
  begin
    --  This works on Windows (CR, LF) and Unix (LF); we ignore the old Macs (CR).
    if Value = GWindows.GCharacter'Val (10) and Line > 0 and Prev_Ind > 0 then
      Editor.AddText(Prev_Ind * " ");
    end if;
  end On_Character_Added;

  overriding
  procedure On_Create (Editor : in out LEA_Scintilla_Type) is
  begin
    --  Set up editor
    Editor.SetEOLMode (SC_EOL_CRLF);
    Editor.SetUseTabs (False);
    Editor.SetEdgeMode (EDGE_LINE);
    --
    --  Multi-line edit
    Editor.Set_Multiple_Selection;
    Editor.Set_Mouse_Selection_Rectangular;
    Editor.Set_Additional_Selection_Typing;
    Editor.Set_Virtual_Space_Options (SCVS_RECTANGULARSELECTION);
    --
    --  Editor.SetIndentationGuides (True);

    Editor.Set_syntax (Ada_syntax);

    Editor.Apply_options;

    Editor.SetMarginTypeN (1, SC_MARGIN_NUMBER);  --  Display line numbers
    Editor.SetMarginWidthN (1, 40);
    Editor.SetMarginWidthN (2, 10);

    Editor.Focus;
  end On_Create;

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
      parent.Update_display(status_bar);
    end if;
  end On_Message;

  overriding
  procedure On_Save_Point_Reached (Editor : in out LEA_Scintilla_Type) is
    parent: MDI_Child_Type renames MDI_Child_Type(Editor.mdi_parent.all);
  begin
    --  We have had enough Undo's to make the document unmodified again.
    Editor.modified:= False;
    parent.Update_display(toolbar_and_menu);
  end On_Save_Point_Reached;

  overriding
  procedure On_Save_Point_Left (Editor : in out LEA_Scintilla_Type) is
    parent: MDI_Child_Type renames MDI_Child_Type(Editor.mdi_parent.all);
  begin
    --  Either new changes after last saved state, or Undo's from last saved state.
    Editor.modified:= True;
    parent.Update_display(toolbar_and_menu);
  end On_Save_Point_Left;

  overriding
  procedure On_Update_UI (Editor : in out LEA_Scintilla_Type) is
    parent: MDI_Child_Type renames MDI_Child_Type(Editor.mdi_parent.all);
    pos : constant Position := Editor.GetCurrentPos;
  begin
    --  NB: On_Position_Changed is deprecated and inactive in SciLexer v.3.5.6
    if Editor.pos_last_update_UI /= pos then
      Editor.pos_last_update_UI := pos;
      parent.Update_display(status_bar);
    end if;
  end On_Update_UI;

  procedure Apply_options (Editor : in out LEA_Scintilla_Type) is
    use GWindows.Colors;
    --
    App_default_font      : constant GString := "Courier New";
    App_default_font_size : constant := 10;
    --
    type Color_topic is (
      foreground, background,
      keyword, number, comment, string, character,
      error_foreground, error_background,
      caret,
      selection_foreground,
      selection_background
    );
    --
    theme_color: constant array(Color_Theme_Type, Color_topic) of Color_Type :=
      (
        Default =>
          (foreground           => Black,
           background           => White,
           keyword              => Blue,
           number               => Dark_Orange,
           comment              => Dark_Green,
           string               => Dark_Gray,
           character            => Dark_Gray,
           error_foreground     => Black,
           error_background     => Pink,
           caret                => Black,
           selection_foreground => Black,
           selection_background => Light_Gray
          ),
        Dark_side   =>
          (foreground           => Light_Gray,
           background           => 16#242322#,
           keyword              => Dark_Orange,
           number               => Red,
           comment              => 16#CF9F72#,
           string               => Yellow,
           character            => Yellow,
           error_foreground     => White,
           error_background     => Dark_Red,
           caret                => White,
           selection_foreground => White,
           selection_background => 16#D28022#
          )
      );
    --
    parent   : MDI_Child_Type renames MDI_Child_Type(Editor.mdi_parent.all);
    mdi_root : MDI_Main_Type renames parent.Parent.all;
    theme    : Color_Theme_Type renames mdi_root.opt.color_theme;
  begin
    Editor.SetTabWidth (mdi_root.opt.indentation);
    Editor.SetEdgeColumn (mdi_root.opt.right_margin);

    Editor.StyleSetFore (STYLE_DEFAULT, Gray);  --  For the line numbers
    Editor.StyleSetBack (STYLE_DEFAULT, theme_color(theme, background));
    Editor.StyleSetSize (STYLE_DEFAULT, App_default_font_size);
    Editor.StyleSetFont (STYLE_DEFAULT, App_default_font);
    Editor.StyleClearAll;

    Editor.StyleSetFore (SCE_ADA_DEFAULT, theme_color(theme, foreground));
    Editor.SetSelFore (True, theme_color(theme, selection_foreground));
    Editor.StyleSetBack (SCE_ADA_DEFAULT, theme_color(theme, background));
    Editor.SetSelBack (True, theme_color(theme, selection_background));
    Editor.StyleSetSize (SCE_ADA_DEFAULT, App_default_font_size);
    Editor.StyleSetFont (SCE_ADA_DEFAULT, App_default_font);

    Editor.StyleSetFore (SCE_ADA_COMMENTLINE, theme_color(theme, comment));
    Editor.StyleSetFore (SCE_ADA_NUMBER,      theme_color(theme, number));
    Editor.StyleSetFore (SCE_ADA_WORD,        theme_color(theme, keyword));
    Editor.StyleSetFore (SCE_ADA_STRING,      theme_color(theme, string));
    Editor.StyleSetFore (SCE_ADA_CHARACTER,   theme_color(theme, character));
    Editor.StyleSetFore (SCE_ADA_IDENTIFIER,  theme_color(theme, foreground));

    --  Cases where the text is obviously wrong
    --  (unfinished character or string, illegal identifier)
    Editor.StyleSetFore (SCE_ADA_CHARACTEREOL, theme_color(theme, error_foreground));
    Editor.StyleSetBack (SCE_ADA_CHARACTEREOL, theme_color(theme, error_background));
    Editor.StyleSetFore (SCE_ADA_STRINGEOL, theme_color(theme, error_foreground));
    Editor.StyleSetBack (SCE_ADA_STRINGEOL, theme_color(theme, error_background));
    Editor.StyleSetFore (SCE_ADA_ILLEGAL, theme_color(theme, error_foreground));
    Editor.StyleSetBack (SCE_ADA_ILLEGAL, theme_color(theme, error_background));

    Editor.SetCaretFore (theme_color(theme, caret));

    case mdi_root.opt.show_special is
      when none =>
        Editor.SetViewWS(SCWS_INVISIBLE);
        Editor.SetViewEOL(False);
      when spaces =>
        Editor.SetViewWS(SCWS_VISIBLEALWAYS);
        Editor.SetViewEOL(False);
      when spaces_eols =>
        Editor.SetViewWS(SCWS_VISIBLEALWAYS);
        Editor.SetViewEOL(True);
    end case;

  end Apply_options;

  function Get_current_line (Editor : LEA_Scintilla_Type) return Integer is
  begin
    return Editor.LineFromPosition (Editor.GetCurrentPos);
  end Get_current_line;

  procedure Set_current_line (Editor : in out LEA_Scintilla_Type; line: Integer) is
    shake: constant:= 10;
  begin
    --  Tactic to show the desired line closer to the middle of the window,
    --  avoiding top or bottom if possible.
    if line > shake then
      Editor.GotoLine (line - shake);  --  A bit too high
    end if;
    Editor.GotoLine (line + shake);    --  A bit too low
    Editor.GotoLine (line);            --  Finally, set the correct line
  end Set_current_line;

  procedure Selection_comment (Editor : in out LEA_Scintilla_Type) is
    --
    blank_line_code: constant:= -1;
    --
    function Get_visible_indentation(s: GString) return Integer is
    begin
      for i in s'Range loop
        case s(i) is
          when ' ' | GWindows.GCharacter'Val (8) =>
            null;  --  only white space
          when GWindows.GCharacter'Val (13) | GWindows.GCharacter'Val (10) =>
            return blank_line_code;
          when others =>
            return i - s'First;
        end case;
      end loop;
      return blank_line_code;
    end Get_visible_indentation;
    --
    function Get_visible_indentation(line: Integer) return Integer is
      pos, pos_next: Scintilla.Position;
    begin
      pos     := Editor.PositionFromLine(line);
      pos_next:= Editor.PositionFromLine(line+1);
      if pos = pos_next then
        return blank_line_code;  --  Empty document case
      end if;
      return Get_visible_indentation(Editor.GetTextRange(pos, pos_next));  --  analyse whole line
    end Get_visible_indentation;
    --
    pos, sel_a, sel_z: Scintilla.Position;
    ind, ind_prev_line, ind_min, lin_a, lin_z: Integer;
  begin
    sel_a:= Editor.GetSelectionStart;
    sel_z:= Editor.GetSelectionEnd;
    lin_a:= Editor.LineFromPosition(sel_a);
    lin_z:= Editor.LineFromPosition(sel_z);
    --  Look for indentation *before* the selected block.
    ind_prev_line:= 0;
    for l in reverse 1 .. lin_a - 1 loop
      ind:= Get_visible_indentation(l);
      if ind > blank_line_code then
        ind_prev_line:= ind;
        exit;
      end if;
    end loop;
    --  Look for the block's minimal indentation (but ignore blank lines for that).
    ind_min:= Integer'Last;
    for l in lin_a .. lin_z loop
      ind:= Get_visible_indentation(l);
      if ind = blank_line_code then
        null;  --  Ignore blank lines for minimal indentation calculation
      else
        ind_min:= Integer'Min(ind_min, ind);
      end if;
    end loop;
    if ind_min = Integer'Last then
      ind_min := 0;
    end if;
    --  The whole commenting can be undone and redone in a single "Undo" / Redo":
    Editor.BeginUndoAction;
    for l in lin_a .. lin_z loop
      --  1) First, remove leading blanks up to ind_min column.
      pos:= Position'Min(
        Editor.PositionFromLine(l) + ind_min,
        --  A blank line (ignored by ind_min) may have less than ind_min columns:
        Editor.GetLineIndentPosition(l)
      );
      Editor.SetCurrentPos(pos);
      Editor.DelLineLeft;
      --  2) Then, insert an indented "--  ", with a fixed indentation (ind_prev_line)
      --    which is using indentation of any non-blank line above the block.
      pos:= Editor.PositionFromLine(l);
      Editor.InsertText(pos, ind_prev_line * ' ' & "--  ");
    end loop;
    Editor.EndUndoAction;
    --  Select the whole block again.
    Editor.SetSel(Editor.PositionFromLine(lin_a), Editor.PositionFromLine(lin_z + 1) - 1);
  end Selection_comment;

  procedure Selection_uncomment (Editor : in out LEA_Scintilla_Type) is
    pos, sel_a, sel_z: Scintilla.Position;
    lin_a, lin_z: Integer;
  begin
    sel_a:= Editor.GetSelectionStart;
    sel_z:= Editor.GetSelectionEnd;
    lin_a:= Editor.LineFromPosition(sel_a);
    lin_z:= Editor.LineFromPosition(sel_z);
    --  The whole uncommenting can be undone and redone in a single "Undo" / Redo":
    Editor.BeginUndoAction;
    for l in lin_a .. lin_z loop
      pos := Editor.GetLineIndentPosition(l);
      if Editor.GetTextRange(pos, pos + 4) = "--  " then
        Editor.SetSel(pos, pos + 4);
        Editor.Clear;
      elsif Editor.GetTextRange(pos, pos + 3) = "-- " then
        Editor.SetSel(pos, pos + 3);
        Editor.Clear;
      elsif Editor.GetTextRange(pos, pos + 2) = "--" then
        Editor.SetSel(pos, pos + 2);
        Editor.Clear;
      end if;
    end loop;
    Editor.EndUndoAction;
    --  Select the whole block again.
    Editor.SetSel(Editor.PositionFromLine(lin_a), Editor.PositionFromLine(lin_z + 1) - 1);
  end Selection_uncomment;

  procedure Search (Editor : in out LEA_Scintilla_Type; action : LEA_Common.Search_action)
  is
    MDI_Child : MDI_Child_Type renames MDI_Child_Type(Editor.mdi_parent.all);
    MDI_Main  : MDI_Main_Type renames MDI_Child.Parent.all;
    find_str    : constant GString:= MDI_Main.Search_box.Find_box.Text;
    --  replace_str : GString:= MDI_Main.Search_box.Replace_Box.Text;
    pos, sel_a, sel_z: GWindows.Scintilla.Position;
  begin
    if find_str = "" then  --  Probably a "find next" (F3) with no search string.
      MDI_Child.Show_Search_Box;
      return;
    end if;
    sel_a:= Editor.GetSelectionStart;
    sel_z:= Editor.GetSelectionEnd;
    Editor.SetSearchFlags(MDI_Main.Search_box.Compose_Scintilla_search_flags);
    case action is
      when find_next | find_previous =>
        for attempt in 1 .. 2 loop
          if action = find_next then
            Editor.SetTargetStart (Integer'Max (Editor.GetCurrentPos, Editor.GetAnchor));
            Editor.SetTargetEnd (Editor.GetLength);
          else
            Editor.SetTargetStart (Integer'Min (Editor.GetCurrentPos, Editor.GetAnchor));
            Editor.SetTargetEnd (0);
          end if;
          pos := Editor.SearchInTarget(find_str);
          if pos >= 0 then  --  Found
            Editor.SetSel (Editor.GetTargetStart, Editor.GetTargetEnd);
            exit;
          elsif attempt = 1 then  --  Not found: wrap around and try again.
            if action = find_next then
              Editor.SetSel (0, 0);  --  Will search the entire document from the top on 2nd attempt.
            else
              Editor.SetSel (Editor.GetLength , Editor.GetLength);  --  Same, but from the bottom.
            end if;
          else  --  Not found *after* the wrap around: find_str is really nowhere!
            Editor.SetSel (sel_a, sel_z);
            Message_Box (MDI_Child, "Search", "No occurrence found", OK_Box, Information_Icon);
          end if;
        end loop;
      when replace_and_find_next =>
        --  Selection must be identical to the text to be found.
        --  But wait: we cannot just compare strings: the options (match case, ...) must
        --  be taken into account as well. Solution: we do a search *within* the selection.
        Editor.SetTargetStart (sel_a);
        Editor.SetTargetEnd (sel_z);
        pos := Editor.SearchInTarget(find_str);
        if pos >= 0 then  --  Found
          --  The replacement can be undone and redone in a single "Undo" / Redo":
          Editor.BeginUndoAction;
          --  Replace: Clear, then Insert.
          Editor.Clear;
          Editor.InsertText (sel_a, MDI_Main.Search_box.Replace_box.Text);
          Editor.EndUndoAction;
        end if;
        --  Find next - anyway.
        Editor.Search (action => find_next);
      when find_all =>
        null;
      when replace_all =>
        null;
    end case;
  end Search;

  function EOL (Editor : LEA_Scintilla_Type) return GString is
  begin
    case Editor.GetEOLMode is
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
    pos, sel_a, sel_z, line_start, next_line_start: Scintilla.Position;
    lin : Integer;
  begin
    sel_a:= Editor.GetSelectionStart;
    sel_z:= Editor.GetSelectionEnd;
    pos := Editor.GetCurrentPos;
    if sel_a = sel_z then  --  No selection: we duplicate the current line
      lin := Editor.LineFromPosition(sel_a);
      line_start      := Editor.PositionFromLine(lin);
      next_line_start := Editor.PositionFromLine(lin+1);
      if line_start < next_line_start then
        if Editor.LineFromPosition(next_line_start) = lin then
          --  Special case: we are on last line. Actually, next_line_start is the
          --  end of current line - and of the whole document as well.
          --  We need to add an EOL first.
          Editor.InsertText(next_line_start, EOL(Editor) & Editor.GetTextRange(line_start, next_line_start));
        else
          Editor.InsertText(next_line_start, Editor.GetTextRange(line_start, next_line_start));
        end if;
      end if;
    else  --  There is a selection: we duplicate it.
      Editor.InsertText(sel_z, Editor.GetTextRange(sel_a, sel_z));
      --  Restore selection *and* cursor as before
      if pos = sel_a then
        Editor.SetSel(sel_z, sel_a);  --  Cursor at begin of selection
      else
        Editor.SetSel(sel_a, sel_z);  --  Cursor at end of selection
      end if;
    end if;
  end Duplicate;

  procedure Load_text (Editor : in out LEA_Scintilla_Type) is
    f: File_Type;
    parent: MDI_Child_Type renames MDI_Child_Type(Editor.mdi_parent.all);
  begin
    Open(f, In_File, To_UTF_8(GU2G(parent.File_Name)), Form_For_IO_Open_and_Create);
    declare
      l: constant Ada.Streams.Stream_IO.Count:= Size(f);
      s: String(1..Integer(l));
      p: Character:= ' ';
    begin
      String'Read(Stream(f), s);
      Editor.SetEOLMode (SC_EOL_CRLF);
      for c of s loop
        if c = ASCII.LF then
          exit when p = ASCII.CR;           --  CR LF
          Editor.SetEOLMode (SC_EOL_LF);    --  non-CR LF
          exit;
        else
          if p = ASCII.CR then              --  CR non-LF
            Editor.SetEOLMode (SC_EOL_CR);
            exit;
          end if;
        end if;
        p:= c;
      end loop;
      Editor.InsertText(0, S2G(s));  --  ASCII to Unicode (UTF-16) conversion
      Editor.EmptyUndoBuffer;
      Editor.SetSavePoint;
      Editor.modified:= False;
    end;
    Close(f);
  end Load_text;

  procedure Save_text (Editor : in out LEA_Scintilla_Type; under: GString) is
    f: File_Type;
  begin
    Create(f, Out_File, To_UTF_8(under), Form_For_IO_Open_and_Create);
    if Editor.GetLength > 0 then
      declare
        b: constant GString:= Editor.GetTextRange(Min => 0, Max => Editor.GetLength);
      begin
        String'Write(Stream(f), G2S(b));
      end;
    end if;
    Close(f);
    --  We do *not* change Editor.SetSavePoint and Editor.modified until
    --  all operations around backups are successful. This is managed by
    --  the parent window's method, MDI_Child_Type.Save.
  end Save_text;

  procedure Set_syntax (Editor : in out LEA_Scintilla_Type; syntax: Syntax_type) is
  begin
    case syntax is
      when Ada_syntax =>
        Editor.SetLexer (SCLEX_ADA);
        Editor.SetKeyWords (0, Ada_keywords);
      when Undefined =>
        Editor.SetLexer (SCLEX_NULL);
        Editor.SetKeyWords (0, "");
    end case;
  end Set_syntax;

end LEA_GWin.Editor;
