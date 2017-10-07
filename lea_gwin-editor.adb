--  LEA_GWin.Editor is derived from: gnavi\gwindows\samples\scintilla

with LEA_Common;                        use LEA_Common;
with LEA_GWin.MDI_Child;                use LEA_GWin.MDI_Child;
with LEA_GWin.MDI_Main;                 use LEA_GWin.MDI_Main;

with GWindows.Colors;

with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;

package body LEA_GWin.Editor is

  Tab_Width : constant := 2;

  Key_Words : constant GWindows.GString :=
    "abort abs abstract accept access aliased all and array at begin body case " &
    "constant declare delay delta digits do else elsif end entry exception " &
    "exit for function generic goto if in interface is limited loop mod new not null of " &
    "or others out overriding package pragma private procedure protected raise range " &
    "record rem renames requeue return reverse select separate some subtype synchronized tagged " &
    "task terminate then type until use when while with xor";

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
  begin
     if
       Value = GWindows.GCharacter'Val (10)
       or
       Value = GWindows.GCharacter'Val (13)
     then
        declare
           Line     : constant Integer := LineFromPosition (Control, CurPos);
           Prev_Loc : constant Integer := GetLineIndentation (Control, Line - 1);
        begin
           if Line > 0 and Prev_Loc > 0 then
              SetLineIndentation (Control,
                                  Line,
                                  Prev_Loc - Tab_Width);
           end if;
        end;
     end if;
  end On_Character_Added;

  overriding
  procedure On_Create (Window : in out LEA_Scintilla_Type) is
      use GWindows, GWindows.Colors;
      --
      App_default_font      : constant GString := "Courier New";
      App_default_font_size : constant := 10;
      --
      type Color_topic is (
        foreground, background,
        keyword, number, comment, string, character,
        caret
      );
      --
      theme_color: constant array(Color_Theme_Type, Color_topic) of Color_Type :=
        (
          NPP_default =>
            (foreground => Black,
             background => White,
             keyword    => Dark_Blue,
             number     => Dark_Orange,
             comment    => Dark_Green,
             string     => Dark_Gray,
             character  => Dark_Gray,
             caret      => Black),
          Dark_side   =>
            (foreground => Light_Gray,
             background => Dark_Gray,
             keyword    => Dark_Orange,
             number     => Red,
             comment    => 16#CF9F72#,
             string     => Yellow,
             character  => Yellow,
             caret      => White)
        );
      --
      parent: MDI_Child_Type renames MDI_Child_Type(Window.mdi_parent.all);
   begin
      --  Set up editor
      Window.SetEOLMode (SC_EOL_CRLF);
      Window.SetTabWidth (Tab_Width);
      Window.SetUseTabs (False);
      Window.SetEdgeColumn (80);
      Window.SetEdgeMode (EDGE_LINE);
      --  Window.SetIndentationGuides (True);

      Window.SetLexer (SCLEX_ADA);
      Window.SetKeyWords (0, Key_Words);

      Window.StyleSetFore (STYLE_DEFAULT, Gray);  --  For the line numbers
      Window.StyleSetBack (STYLE_DEFAULT, theme_color(parent.opt.color_theme, background));
      Window.StyleSetSize (STYLE_DEFAULT, App_default_font_size);
      Window.StyleSetFont (STYLE_DEFAULT, App_default_font);
      Window.StyleClearAll;

      Window.StyleSetFore (SCE_ADA_DEFAULT, theme_color(parent.opt.color_theme, foreground));
      Window.StyleSetBack (SCE_ADA_DEFAULT, theme_color(parent.opt.color_theme, background));
      Window.StyleSetSize (SCE_ADA_DEFAULT, App_default_font_size);
      Window.StyleSetFont (SCE_ADA_DEFAULT, App_default_font);

      Window.StyleSetFore (SCE_ADA_COMMENTLINE, theme_color(parent.opt.color_theme, comment));
      Window.StyleSetFore (SCE_ADA_NUMBER,      theme_color(parent.opt.color_theme, number));
      Window.StyleSetFore (SCE_ADA_WORD,        theme_color(parent.opt.color_theme, keyword));
      Window.StyleSetFore (SCE_ADA_STRING,      theme_color(parent.opt.color_theme, string));
      Window.StyleSetFore (SCE_ADA_CHARACTER,   theme_color(parent.opt.color_theme, character));
      Window.StyleSetFore (SCE_ADA_IDENTIFIER,  theme_color(parent.opt.color_theme, foreground));

      --  Cases where the text is obviously wrong
      --  (unfinished character or string, illegal identifier)
      Window.StyleSetFore (SCE_ADA_CHARACTEREOL, White);
      Window.StyleSetBack (SCE_ADA_CHARACTEREOL, Dark_Red);
      Window.StyleSetFore (SCE_ADA_STRINGEOL, White);
      Window.StyleSetBack (SCE_ADA_STRINGEOL, Dark_Red);
      Window.StyleSetFore (SCE_ADA_ILLEGAL, White);
      Window.StyleSetBack (SCE_ADA_ILLEGAL, Dark_Red);

      Window.SetCaretFore (theme_color(parent.opt.color_theme, caret));

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
    parent: MDI_Child_Type renames MDI_Child_Type(Control.mdi_parent.all);
  begin
    --  !!  Doesn't seem to work ???
    parent.Update_display(status_bar);
  end;

  overriding
  procedure On_Save_Point_Reached (Control : in out LEA_Scintilla_Type) is
    parent: MDI_Child_Type renames MDI_Child_Type(Control.mdi_parent.all);
  begin
    --  We have had enough Undo's to make the document unmodified again.
    Control.modified:= False;
    parent.Update_display(toolbar_and_menu);
  end;

  overriding
  procedure On_Save_Point_Left (Control : in out LEA_Scintilla_Type) is
    parent: MDI_Child_Type renames MDI_Child_Type(Control.mdi_parent.all);
  begin
    --  Either new changes, or Undo's from the last saved state.
    Control.modified:= True;
    parent.Update_display(toolbar_and_menu);
  end;

  procedure Load_text (Window : in out LEA_Scintilla_Type) is
    f: File_Type;
    parent: MDI_Child_Type renames MDI_Child_Type(Window.mdi_parent.all);
  begin
    Open(f, In_File, G2S(GU2G(parent.File_Name)));
    declare
      l: constant Count:= Size(f);
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
      Window.InsertText(0, S2G(s));
      Window.EmptyUndoBuffer;
      Window.SetSavePoint;
      Window.modified:= False;
    end;
    Close(f);
  end Load_text;

end LEA_GWin.Editor;
