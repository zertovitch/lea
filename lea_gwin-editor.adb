--  LEA_GWin.Editor is derived from: gnavi\gwindows\samples\scintilla

with LEA_Common;                        use LEA_Common;
with LEA_GWin.MDI_Child;                use LEA_GWin.MDI_Child;
with LEA_GWin.MDI_Main;                 use LEA_GWin.MDI_Main;

with GWindows.Colors;

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
  procedure On_Change (Control : in out LEA_Scintilla_Type) is
    parent: MDI_Child_Type renames MDI_Child_Type(Control.mdi_parent.all);
  begin
    parent.Update_display(toolbar_and_menu);
  end On_Change;

  overriding
  procedure On_Character_Added
    (Control     : in out LEA_Scintilla_Type;
     Special_Key : in     GWindows.Windows.Special_Key_Type;
     Value       : in     GWindows.GCharacter)
  is
  pragma Unreferenced (Special_Key);
    CurPos : constant Position := GetCurrentPos (Control);
    Line     : constant Integer := LineFromPosition (Control, CurPos);
    Prev_Ind : constant Integer := GetLineIndentation (Control, Line - 1);
  begin
     if Value = GWindows.GCharacter'Val (13) and Line > 0 and Prev_Ind > 0 then
       Control.AddText(Prev_Ind * " ");
     end if;
  end On_Character_Added;

  overriding
  procedure On_Create (Window : in out LEA_Scintilla_Type) is
  begin
    --  Set up editor
    Window.SetEOLMode (SC_EOL_CRLF);
    Window.SetUseTabs (False);
    Window.SetEdgeMode (EDGE_LINE);
    --  Window.SetIndentationGuides (True);

    Window.SetLexer (SCLEX_ADA);
    Window.SetKeyWords (0, Ada_keywords);

    Window.Apply_options;

    Window.SetMarginTypeN (1, SC_MARGIN_NUMBER);  --  Display line numbers
    Window.SetMarginWidthN (1, 40);
    Window.SetMarginWidthN (2, 10);

    Window.Focus;
  end On_Create;

  overriding
  procedure On_Message
    (Window       : in out LEA_Scintilla_Type;
     message      : in     Interfaces.C.unsigned;
     wParam       : in     GWindows.Types.Wparam;
     lParam       : in     GWindows.Types.Lparam;
     Return_Value : in out GWindows.Types.Lresult)
  is
    WM_KEYDOWN                 : constant := 256;
    WM_LBUTTONDOWN             : constant := 513;
    WM_RBUTTONDOWN             : constant := 516;
    status_refresh_needed: Boolean:= False;
    parent: MDI_Child_Type renames MDI_Child_Type(Window.mdi_parent.all);
  begin
    case message is
      when WM_KEYDOWN | WM_LBUTTONDOWN | WM_RBUTTONDOWN =>
        status_refresh_needed := True;  --  Likely, cursor has moved; refresh Line / Col indicator
      when others =>
        null;
    end case;
    --  Call parent method.
    Scintilla_Type(Window).On_Message(message, wParam, lParam, Return_Value);
    --
    if status_refresh_needed then
      parent.Update_display(status_bar);
    end if;
  end On_Message;

  overriding
  procedure On_Position_Changed (Control : in out LEA_Scintilla_Type;
                                 Pos     : in     Position)
  is
  pragma Unreferenced (Pos);
    parent: MDI_Child_Type renames MDI_Child_Type(Control.mdi_parent.all);
  begin
    parent.Update_display(status_bar);
  end On_Position_Changed;

  overriding
  procedure On_Save_Point_Reached (Control : in out LEA_Scintilla_Type) is
    parent: MDI_Child_Type renames MDI_Child_Type(Control.mdi_parent.all);
  begin
    --  We have had enough Undo's to make the document unmodified again.
    Control.modified:= False;
    parent.Update_display(toolbar_and_menu);
  end On_Save_Point_Reached;

  overriding
  procedure On_Save_Point_Left (Control : in out LEA_Scintilla_Type) is
    parent: MDI_Child_Type renames MDI_Child_Type(Control.mdi_parent.all);
  begin
    --  Either new changes, or Undo's from the last saved state.
    Control.modified:= True;
    parent.Update_display(toolbar_and_menu);
  end On_Save_Point_Left;

  overriding
  procedure On_Update_UI (Control : in out LEA_Scintilla_Type) is
    parent: MDI_Child_Type renames MDI_Child_Type(Control.mdi_parent.all);
  begin
    parent.Update_display(status_bar);
  end;

  procedure Apply_options (Window : in out LEA_Scintilla_Type) is
      use GWindows.Colors;
      --
      App_default_font      : constant GString := "Courier New";
      App_default_font_size : constant := 10;
      --
      type Color_topic is (
        foreground, background,
        keyword, number, comment, string, character,
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
             caret                => White,
             selection_foreground => White,
             selection_background => 16#D28022#
            )
        );
      --
      parent   : MDI_Child_Type renames MDI_Child_Type(Window.mdi_parent.all);
      mdi_root : MDI_Main_Type renames parent.Parent.all;
      theme    : Color_Theme_Type renames mdi_root.opt.color_theme;
   begin
      Window.SetTabWidth (mdi_root.opt.indentation);
      Window.SetEdgeColumn (mdi_root.opt.right_margin);

      Window.StyleSetFore (STYLE_DEFAULT, Gray);  --  For the line numbers
      Window.StyleSetBack (STYLE_DEFAULT, theme_color(theme, background));
      Window.StyleSetSize (STYLE_DEFAULT, App_default_font_size);
      Window.StyleSetFont (STYLE_DEFAULT, App_default_font);
      Window.StyleClearAll;

      Window.StyleSetFore (SCE_ADA_DEFAULT, theme_color(theme, foreground));
      Window.SetSelFore (True, theme_color(theme, selection_foreground));
      Window.StyleSetBack (SCE_ADA_DEFAULT, theme_color(theme, background));
      Window.SetSelBack (True, theme_color(theme, selection_background));
      Window.StyleSetSize (SCE_ADA_DEFAULT, App_default_font_size);
      Window.StyleSetFont (SCE_ADA_DEFAULT, App_default_font);

      Window.StyleSetFore (SCE_ADA_COMMENTLINE, theme_color(theme, comment));
      Window.StyleSetFore (SCE_ADA_NUMBER,      theme_color(theme, number));
      Window.StyleSetFore (SCE_ADA_WORD,        theme_color(theme, keyword));
      Window.StyleSetFore (SCE_ADA_STRING,      theme_color(theme, string));
      Window.StyleSetFore (SCE_ADA_CHARACTER,   theme_color(theme, character));
      Window.StyleSetFore (SCE_ADA_IDENTIFIER,  theme_color(theme, foreground));

      --  Cases where the text is obviously wrong
      --  (unfinished character or string, illegal identifier)
      Window.StyleSetFore (SCE_ADA_CHARACTEREOL, White);
      Window.StyleSetBack (SCE_ADA_CHARACTEREOL, Dark_Red);
      Window.StyleSetFore (SCE_ADA_STRINGEOL, White);
      Window.StyleSetBack (SCE_ADA_STRINGEOL, Dark_Red);
      Window.StyleSetFore (SCE_ADA_ILLEGAL, White);
      Window.StyleSetBack (SCE_ADA_ILLEGAL, Dark_Red);

      Window.SetCaretFore (theme_color(theme, caret));
  end Apply_options;

  procedure Selection_comment (Editor : in out LEA_Scintilla_Type) is
    function Get_visible_indentation(s: GString) return Integer is
    begin
      for i in s'Range loop
        case s(i) is
          when ' ' | GWindows.GCharacter'Val (8) =>
            null;  --  only white space
          when GWindows.GCharacter'Val (13) | GWindows.GCharacter'Val (10) =>
            return 0;
          when others =>
            return i - s'First;
        end case;
      end loop;
      return 0;
    end Get_visible_indentation;
    --
    function Get_visible_indentation(line: Integer) return Integer is
      pos, pos_next: Scintilla.Position;
    begin
      pos     := Editor.PositionFromLine(line);
      pos_next:= Editor.PositionFromLine(line+1);
      if pos = pos_next then
        return 0;  --  Empty document case
      end if;
      return Get_visible_indentation(Editor.GetTextRange(pos, pos_next));  --  analyse whole line
    end Get_visible_indentation;
    --
    pos, sel_a, sel_z, lin_a, lin_z: Scintilla.Position;
    ind, ind_prev_line, ind_min: Integer;
  begin
    sel_a:= Editor.GetSelectionStart;
    sel_z:= Editor.GetSelectionEnd;
    lin_a:= Editor.LineFromPosition(sel_a);
    lin_z:= Editor.LineFromPosition(sel_z);
    --  Look for indentation *before* the selected block.
    ind_prev_line:= 0;
    for l in reverse 1 .. lin_a - 1 loop
      ind:= Get_visible_indentation(l);
      if ind > 0 then
        ind_prev_line:= ind;
        exit;
      end if;
    end loop;
    --  Look for the block's minimal indentation (but ignore blank lines for that).
    ind_min:= Integer'Last;
    for l in lin_a .. lin_z loop
      ind:= Get_visible_indentation(l);
      if ind = 0 then
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
      --  First, remove leading blanks up to ind_min column.
      pos:= Position'Min(
        Editor.PositionFromLine(l) + ind_min,
        --  A blank line (ignroed by ind_min) may have less than ind_min columns.
        Editor.GetLineIndentPosition(l)
      );
      Editor.SetCurrentPos(pos);
      Editor.DelLineLeft;
      --  Then, insert an indented "--  ", with a fixed indentation (ind_prev_line)
      --    which is using indentation of any non-blank line above the block.
      pos:= Editor.PositionFromLine(l);
      Editor.InsertText(pos, ind_prev_line * ' ' & "--  ");
    end loop;
    Editor.EndUndoAction;
    Editor.SetSel(Editor.PositionFromLine(lin_a), Editor.PositionFromLine(lin_z + 1) - 1);
  end Selection_comment;

  procedure Load_text (Window : in out LEA_Scintilla_Type) is
    f: File_Type;
    parent: MDI_Child_Type renames MDI_Child_Type(Window.mdi_parent.all);
  begin
    Open(f, In_File, To_UTF_8(GU2G(parent.File_Name)), Form_For_IO_Open_and_Create);
    declare
      l: constant Ada.Streams.Stream_IO.Count:= Size(f);
      s: String(1..Integer(l));
      p: Character:= ' ';
    begin
      String'Read(Stream(f), s);
      Window.SetEOLMode (SC_EOL_CRLF);
      for c of s loop
        if c = ASCII.LF then
          exit when p = ASCII.CR;           --  CR LF
          Window.SetEOLMode (SC_EOL_LF);    --  non-CR LF
          exit;
        else
          if p = ASCII.CR then              --  CR non-LF
            Window.SetEOLMode (SC_EOL_CR);
            exit;
          end if;
        end if;
        p:= c;
      end loop;
      Window.InsertText(0, S2G(s));  --  ASCII to Unicode (UTF-16) conversion
      Window.EmptyUndoBuffer;
      Window.SetSavePoint;
      Window.modified:= False;
    end;
    Close(f);
  end Load_text;

  procedure Save_text (Window : in out LEA_Scintilla_Type; under: GString) is
    f: File_Type;
  begin
    Create(f, Out_File, To_UTF_8(under), Form_For_IO_Open_and_Create);
    if Window.GetLength > 0 then
      declare
        b: constant GString:= Window.GetTextRange(Min => 0, Max => Window.GetLength);
      begin
        String'Write(Stream(f), G2S(b));
      end;
    end if;
    Close(f);
    --  We do *not* change Window.SetSavePoint and Window.modified until
    --  all operations around backups are successful. This is managed by
    --  the parent window's method, MDI_Child_Type.Save.
  end Save_text;

end LEA_GWin.Editor;
