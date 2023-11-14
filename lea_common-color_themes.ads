--  In this package, color themes and their colors
--  are defined. Add your own!

package LEA_Common.Color_Themes is

  type Color_Theme_Type is (Default, Dark_Side, Solarized_Light);

  type Color_Topic is
    (foreground, background,
     keyword, number, comment,
     string_literal, character_literal,
     error_foreground, error_background,
     caret,
     selection_foreground,
     selection_background,
     matched_parenthesis,
     unmatched_parenthesis,
     parenthesis_background,
     matched_word_highlight,
     messages_foreground,
     messages_background,
     messages_control_background,
     tool_tip_background,
     tool_tip_foreground_highlighted,
     caret_line_background,
     bookmark_foreground,
     bookmark_background,
     line_number_background);

  --  Color encoding: each value of Red, Green, Blue on 8 bits.
  --  Red * 16#1_00_00# + Green * 16#1_00# + Blue.

  type RGB_Type is range 0 .. 2**24 - 1;

  function Nice_Image (ct : Color_Theme_Type) return UTF_16_String;
  function Nice_Value (im : UTF_16_String) return Color_Theme_Type;

  --  Select the current Theme
  procedure Select_Theme (Theme : Color_Theme_Type);

  --  Return the currently selected Theme
  function  Current_Theme return Color_Theme_Type;

  --  Return the Topic color of the current Theme
  function  Theme_Color (Topic : Color_Topic) return RGB_Type;

  --  Return the Topic color of Theme
  function  Theme_Color (Theme : Color_Theme_Type;
                         Topic : Color_Topic) return RGB_Type;

  --  Return True when the current Theme has a dark background else False
  function  Theme_Dark_Backgrounded return Boolean;

  --  Return True when Theme has a dark background else False
  function  Theme_Dark_Backgrounded (Theme : Color_Theme_Type) return Boolean;

end LEA_Common.Color_Themes;
