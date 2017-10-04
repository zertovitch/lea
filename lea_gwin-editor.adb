--  Derived from: gnavi\gwindows\samples\scintilla

with GWindows.Base;
with GWindows.Colors;
with GWindows.Windows;

package body LEA_GWin.Editor is

  Tab_Width : constant := 2;

  Key_Words : constant GWindows.GString :=
    "abort abs abstract accept access aliased all and array at begin body case " &
    "constant declare delay delta digits do else elsif end entry exception " &
    "exit for function generic goto if in interface is limited loop mod new not null of " &
    "or others out overriding package pragma private procedure protected raise range " &
    "record rem renames requeue return reverse select separate some subtype synchronized tagged " &
    "task terminate then type until use when while with xor";

  procedure Do_Character_Added
    (Window      : in out GWindows.Base.Base_Window_Type'Class;
     Special_Key : in     GWindows.Windows.Special_Key_Type;
     Value       : in     GWindows.GCharacter)
  is
  pragma Unreferenced (Special_Key);
     CurPos : constant Position := GetCurrentPos (Scintilla_Type (Window));
  begin
     if
       Value = GWindows.GCharacter'Val (10)
       or
       Value = GWindows.GCharacter'Val (13)
     then
        declare
           Line     : constant Integer := LineFromPosition
             (Scintilla_Type (Window), CurPos);
           Prev_Loc : constant Integer := GetLineIndentation
             (Scintilla_Type (Window), Line - 1);
        begin
           if Line > 0 and Prev_Loc > 0 then
              SetLineIndentation (Scintilla_Type (Window),
                                  Line,
                                  Prev_Loc - Tab_Width);
           end if;
        end;
     end if;
  end Do_Character_Added;

  overriding
  procedure On_Create (Window : in out LEA_Scintilla_Type) is
      use GWindows, GWindows.Colors;
      --
      App_default_font      : constant GString := "Courier New";
      App_default_font_size : constant := 10;
      --
   begin
      Window.On_Character_Added_Handler (Do_Character_Added'Unrestricted_Access);

      --  Set up editor
      Window.SetEOLMode (SC_EOL_CRLF);
      Window.SetTabWidth (Tab_Width);
      Window.SetUseTabs (False);
      Window.SetEdgeColumn (80);
      Window.SetEdgeMode (EDGE_LINE);
      --  Window.SetIndentationGuides (True);

      Window.SetLexer (SCLEX_ADA);
      Window.SetKeyWords (0, Key_Words);

      Window.StyleSetFore (STYLE_DEFAULT, Black);
      Window.StyleSetBack (STYLE_DEFAULT, White);
      Window.StyleSetSize (STYLE_DEFAULT, App_default_font_size);
      Window.StyleSetFont (STYLE_DEFAULT, App_default_font);
      Window.StyleClearAll;

      Window.StyleSetFore (SCE_ADA_DEFAULT, Black);
      Window.StyleSetBack (SCE_ADA_DEFAULT, White);
      Window.StyleSetSize (SCE_ADA_DEFAULT, App_default_font_size);
      Window.StyleSetFont (SCE_ADA_DEFAULT, App_default_font);

      Window.StyleSetFore (SCE_ADA_COMMENTLINE, Red);
      Window.StyleSetFore (SCE_ADA_NUMBER,      Blue);
      Window.StyleSetFore (SCE_ADA_WORD,        Dark_Green);
      Window.StyleSetFore (SCE_ADA_STRING,      Dark_Red);
      Window.StyleSetFore (SCE_ADA_CHARACTER,   Blue);
      Window.StyleSetFore (SCE_ADA_IDENTIFIER,  Black);

      --  Cases where the text is obviously wrong
      --  (unfinished character or string, illegal identifier)
      Window.StyleSetFore (SCE_ADA_CHARACTEREOL, White);
      Window.StyleSetBack (SCE_ADA_CHARACTEREOL, Dark_Red);
      Window.StyleSetFore (SCE_ADA_STRINGEOL, White);
      Window.StyleSetBack (SCE_ADA_STRINGEOL, Dark_Red);
      Window.StyleSetFore (SCE_ADA_ILLEGAL, White);
      Window.StyleSetBack (SCE_ADA_ILLEGAL, Dark_Red);

      Window.SetMarginTypeN (1, SC_MARGIN_NUMBER);
      Window.SetMarginWidthN (1, 40);
      Window.SetMarginWidthN (2, 10);

      Window.Focus;
   end On_Create;

end LEA_GWin.Editor;
