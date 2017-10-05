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
  procedure On_Character_Added
    (Control     : in out LEA_Scintilla_Type;
     Special_Key : in     GWindows.Windows.Special_Key_Type;
     Value       : in     GWindows.GCharacter)
  is
  pragma Unreferenced (Special_Key);
    parent: MDI_Child_Type renames MDI_Child_Type(Control.mdi_parent.all);
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
     parent.Update_display(toolbar_and_menu);
  end On_Character_Added;

  overriding
  procedure On_Create (Window : in out LEA_Scintilla_Type) is
      use GWindows, GWindows.Colors;
      --
      App_default_font      : constant GString := "Courier New";
      App_default_font_size : constant := 10;
      --
      type Color_topic is (foreground, background, keyword, number, comment, string, character);
      theme_color: constant array(Color_Theme_Type, Color_topic) of Color_Type :=
        (
          NPP_default =>
            (foreground => Black,
             background => White,
             keyword    => Dark_Blue,
             number     => Orange,
             comment    => Green,
             string     => Dark_Gray,
             character  => Dark_Gray),
          Dark_side   =>
            (foreground => Light_Gray,
             background => Dark_Gray,
             keyword    => Dark_Orange,
             number     => Red,
             comment    => 16#CF9F72#,
             string     => Yellow,
             character  => Yellow)
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

      Window.SetMarginTypeN (1, SC_MARGIN_NUMBER);  --  Display line numbers
      Window.SetMarginWidthN (1, 40);
      Window.SetMarginWidthN (2, 10);

      Window.Focus;
   end On_Create;

  procedure Load_text (Window : in out LEA_Scintilla_Type) is
    f: File_Type;
    parent: MDI_Child_Type renames MDI_Child_Type(Window.mdi_parent.all);
  begin
    Open(f, In_File, G2S(GU2G(parent.File_Name)));
    declare
      l: constant Count:= Size(f);
      s: String(1..Integer(l));
    begin
      String'Read(Stream(f), s);
      Window.AddText(S2G(s));
      Window.EmptyUndoBuffer;
    end;
    Close(f);
  end;

end LEA_GWin.Editor;
