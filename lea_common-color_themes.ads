--  In this package, color themes and their colors
--  are defined. Add your own!

package LEA_Common.Color_Themes is

  type Color_Theme_Type is (Default, Dark_Side, Solarized_Light);

  dark_backgrounded : constant array (Color_Theme_Type) of Boolean :=
    (Dark_Side => True, others => False);

  function Nice_Image (ct : Color_Theme_Type) return UTF_16_String;
  function Nice_Value (im : UTF_16_String) return Color_Theme_Type;

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
     messages_control_background);

  type RGB_Type is range 0 .. 2**24;

  White       : constant RGB_Type := 16#FFFFFF#;
  Black       : constant RGB_Type := 16#000000#;
  Silver      : constant RGB_Type := 16#E0E0E0#;
  Light_Gray  : constant RGB_Type := 16#C0C0C0#;
  Gray        : constant RGB_Type := 16#808080#;
  Dark_Gray   : constant RGB_Type := 16#404040#;
  Red         : constant RGB_Type := 16#FF0000#;
  Dark_Red    : constant RGB_Type := 16#800000#;
  Green       : constant RGB_Type := 16#00FF00#;
  Dark_Green  : constant RGB_Type := 16#008000#;
  Light_Blue  : constant RGB_Type := 16#88DDFF#;
  Blue        : constant RGB_Type := 16#0000FF#;
  Dark_Blue   : constant RGB_Type := 16#000080#;
  Yellow      : constant RGB_Type := 16#FFFF00#;
  Magenta     : constant RGB_Type := 16#FF00FF#;
  Cyan        : constant RGB_Type := 16#00FFFF#;
  Pink        : constant RGB_Type := 16#FFAFAF#;
  Orange      : constant RGB_Type := 16#FFC800#;
  Dark_Orange : constant RGB_Type := 16#F08D24#;

  package Solarized is
    --  https://ethanschoonover.com/solarized/#the-values
    --  https://en.wikipedia.org/wiki/Solarized

    --  Base tones, dark to light:
    base02 : constant RGB_Type := 16#073642#;
    base01 : constant RGB_Type := 16#586e75#;
    base00 : constant RGB_Type := 16#657b83#;
    base0  : constant RGB_Type := 16#839496#;
    base1  : constant RGB_Type := 16#93a1a1#;
    base2  : constant RGB_Type := 16#eee8d5#;
    base3  : constant RGB_Type := 16#fdf6e3#;

    --  Colours:
    orange  : constant RGB_Type := 16#cb4b16#;
    red     : constant RGB_Type := 16#dc322f#;
    magenta : constant RGB_Type := 16#d33682#;
    blue    : constant RGB_Type := 16#268bd2#;
    cyan    : constant RGB_Type := 16#2aa198#;
    green   : constant RGB_Type := 16#859900#;

  end Solarized;

  theme_color : constant array (Color_Theme_Type, Color_Topic) of RGB_Type :=
    (Default =>
       (foreground                  => Black,
        background                  => White,
        keyword                     => Blue,
        number                      => Dark_Orange,
        comment                     => Dark_Green,
        string_literal              => Dark_Gray,
        character_literal           => Dark_Gray,
        error_foreground            => Black,
        error_background            => Pink,
        caret                       => Black,
        selection_foreground        => Black,
        selection_background        => Light_Gray,
        matched_parenthesis         => Dark_Green,
        unmatched_parenthesis       => Dark_Red,
        parenthesis_background      => 16#cbe7f5#,
        matched_word_highlight      => Dark_Green,
        messages_foreground         => Black,
        messages_background         => White,
        messages_control_background => White),

     Dark_Side =>
       (foreground                  => Light_Gray,
        background                  => 16#222324#,
        keyword                     => Dark_Orange,
        number                      => Red,
        comment                     => 16#729fcf#,
        string_literal              => Yellow,
        character_literal           => Yellow,
        error_foreground            => White,
        error_background            => Dark_Red,
        caret                       => White,
        selection_foreground        => White,
        selection_background        => 16#2280d2#,
        matched_parenthesis         => Green,
        unmatched_parenthesis       => Red,
        parenthesis_background      => 16#505050#,
        matched_word_highlight      => Green,
        messages_foreground         => Light_Gray,
        messages_background         => 16#161718#,
        messages_control_background => 16#121314#),

     Solarized_Light =>
       (foreground                  => Solarized.base01,
        background                  => Solarized.base3,
        keyword                     => Solarized.green,
        number                      => Solarized.magenta,
        comment                     => Solarized.base1,
        string_literal              => Solarized.cyan,
        character_literal           => Solarized.blue,
        error_foreground            => Solarized.base3,
        error_background            => Solarized.orange,
        caret                       => Black,
        selection_foreground        => Solarized.base3,
        selection_background        => Solarized.base00,
        matched_parenthesis         => Solarized.green,
        unmatched_parenthesis       => Solarized.red,
        parenthesis_background      => Solarized.base2,
        matched_word_highlight      => Solarized.green,
        messages_foreground         => Solarized.base02,
        messages_background         => Solarized.base3,
        messages_control_background => Solarized.base2));

end LEA_Common.Color_Themes;
