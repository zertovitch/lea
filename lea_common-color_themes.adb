package body LEA_Common.Color_Themes is

  Selected_Theme : Color_Theme_Type := Default;

  White           : constant RGB_Type := 16#FFFFFF#;
  Black           : constant RGB_Type := 16#000000#;
--  Silver          : constant RGB_Type := 16#E0E0E0#;
  Very_Light_Gray : constant RGB_Type := 16#F8F8F8#;
  Light_Gray      : constant RGB_Type := 16#C0C0C0#;
  Gray            : constant RGB_Type := 16#808080#;
  Dark_Gray       : constant RGB_Type := 16#404040#;
  Very_Dark_Gray  : constant RGB_Type := 16#101010#;
  Red             : constant RGB_Type := 16#FF0000#;
  Dark_Red        : constant RGB_Type := 16#800000#;
  Green           : constant RGB_Type := 16#00FF00#;
  Dark_Green      : constant RGB_Type := 16#008000#;
  Light_Blue      : constant RGB_Type := 16#88DDFF#;
  Blue            : constant RGB_Type := 16#0000FF#;
  Dark_Blue       : constant RGB_Type := 16#000080#;
  Yellow          : constant RGB_Type := 16#FFFF00#;
--  Magenta         : constant RGB_Type := 16#FF00FF#;
--  Cyan            : constant RGB_Type := 16#00FFFF#;
  Pink            : constant RGB_Type := 16#FFAFAF#;
  Light_Pink      : constant RGB_Type := 16#FFF2F2#;
  Orange          : constant RGB_Type := 16#FFC800#;
  Dark_Orange     : constant RGB_Type := 16#F08D24#;

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

  theme_color_array : constant array (Color_Theme_Type, Color_Topic) of RGB_Type :=
    (Default =>
       (foreground                        => Very_Dark_Gray,
        background                        => White,
        keyword                           => Blue,
        number                            => Dark_Orange,
        comment                           => Dark_Green,
        string_literal                    => Dark_Gray,
        character_literal                 => Dark_Gray,
        error_foreground                  => Red,
        error_background                  => Light_Pink,
        caret                             => Black,
        selection_foreground              => Black,
        selection_background              => Light_Gray,
        matched_parenthesis               => Dark_Green,
        unmatched_parenthesis             => Dark_Red,
        parenthesis_background            => 16#cbe7f5#,
        matched_word_highlight            => Dark_Green,
        messages_foreground               => Black,
        messages_background               => White,
        messages_control_background       => White,
        tool_tip_background               => Very_Light_Gray,
        tool_tip_foreground_highlighted   => Dark_Blue,
        caret_line_background             => 16#f0f0ff#,
        bookmark_foreground               => Blue,
        bookmark_background               => Light_Blue,
        line_number_foreground            => Light_Gray,
        line_number_background            => Very_Light_Gray,
        status_bar_foreground             => 16#202020#,
        status_bar_foreground_highlighted => Dark_Orange,
        status_bar_background             => White,
        splitter_background               => Light_Gray,
        splitter_dashes                   => Gray,
        tab_bar_background                => White,
        tab_background                    => 16#F8F8F8#,
        tab_background_selected           => 16#F0F0FF#,
        tab_background_hovered            => 16#F4F4FC#,
        tab_background_selected_hovered   => 16#D8D8EF#,
        tab_foreground                    => 16#848484#,
        tab_foreground_selected           => Very_Dark_Gray,
        tab_foreground_hovered            => 16#4A4A4A#,
        tab_foreground_selected_hovered   => Black,
        tab_frame                         => Black),

     Dark_Side =>
       (foreground                        => Light_Gray,
        background                        => 16#222324#,
        keyword                           => Dark_Orange,
        number                            => Red,
        comment                           => 16#729fcf#,
        string_literal                    => Yellow,
        character_literal                 => Yellow,
        error_foreground                  => Pink,
        error_background                  => Dark_Red,
        caret                             => White,
        selection_foreground              => White,
        selection_background              => 16#2280d2#,
        matched_parenthesis               => Green,
        unmatched_parenthesis             => Red,
        parenthesis_background            => 16#505050#,
        matched_word_highlight            => Green,
        messages_foreground               => Light_Gray,
        messages_background               => 16#161718#,
        messages_control_background       => 16#121314#,
        tool_tip_background               => Dark_Gray,
        tool_tip_foreground_highlighted   => Light_Blue,
        caret_line_background             => 16#402020#,
        bookmark_foreground               => 16#c06060#,
        bookmark_background               => 16#c06060#,
        line_number_foreground            => Gray,
        line_number_background            => 16#383334#,
        status_bar_foreground             => Light_Gray,
        status_bar_foreground_highlighted => Orange,
        status_bar_background             => 16#161718#,
        splitter_background               => Dark_Gray,
        splitter_dashes                   => Gray,
        tab_bar_background                => 16#222324#,
        tab_background                    => 16#383334#,
        tab_background_selected           => 16#402020#,
        tab_background_hovered            => 16#3C292A#,
        tab_background_selected_hovered   => 16#503030#,
        tab_foreground                    => 16#7C7C7C#,
        tab_foreground_selected           => 16#F8F8F8#,
        tab_foreground_hovered            => 16#BABABA#,
        tab_foreground_selected_hovered   => 16#FCFCFC#,
        tab_frame                         => 16#A09C90#),

     Solarized_Light =>
       (foreground                        => Solarized.base01,
        background                        => Solarized.base3,
        keyword                           => Solarized.green,
        number                            => Solarized.magenta,
        comment                           => Solarized.base1,
        string_literal                    => Solarized.cyan,
        character_literal                 => Solarized.blue,
        error_foreground                  => Solarized.orange,
        error_background                  => Solarized.base2,
        caret                             => Black,
        selection_foreground              => Solarized.base3,
        selection_background              => Solarized.base00,
        matched_parenthesis               => Solarized.green,
        unmatched_parenthesis             => Solarized.red,
        parenthesis_background            => Solarized.base2,
        matched_word_highlight            => Orange,  --  Scintilla blends this with background.
        messages_foreground               => Solarized.base02,
        messages_background               => Solarized.base3,
        messages_control_background       => Solarized.base2,
        tool_tip_background               => 16#fdf3e0#,  --  base2, a bit darker
        tool_tip_foreground_highlighted   => Dark_Blue,
        caret_line_background             => 16#ffe8d8#,
        bookmark_foreground               => 16#ffc8c8#,
        bookmark_background               => 16#ffc8c8#,
        line_number_foreground            => Solarized.base1,
        line_number_background            => 16#f6ecdc#,
        status_bar_foreground             => Solarized.base02,
        status_bar_foreground_highlighted => Dark_Orange,
        status_bar_background             => Solarized.base3,
        splitter_background               => Solarized.base2,
        splitter_dashes                   => Solarized.base0,
        tab_bar_background                => Solarized.base3,
        tab_background                    => 16#F6ECDC#,
        tab_background_selected           => 16#FFE8D8#,
        tab_background_hovered            => 16#FAEADA#,
        tab_background_selected_hovered   => 16#FFEFDF#,
        tab_foreground                    => 16#ABB6BA#,
        tab_foreground_selected           => 16#586E75#,
        tab_foreground_hovered            => 16#819297#,
        tab_foreground_selected_hovered   => 16#485E65#,
        tab_frame                         => 16#D4A6C2#));

  --  *************************************************************************
  function Nice_Image (ct : Color_Theme_Type) return UTF_16_String is
  begin
    case ct is
      when Default         => return "Default theme";
      when Dark_Side       => return "Dark Side";
      when Solarized_Light => return "Solarized Light";
    end case;
  end Nice_Image;

  --  *************************************************************************
  function Nice_Value (im : UTF_16_String) return Color_Theme_Type is
  begin
    for ct in Color_Theme_Type loop
      if im = Nice_Image (ct) then
        return ct;
      end if;
    end loop;
    return Default;
  end Nice_Value;

  --  *************************************************************************
  procedure Select_Theme (Theme : Color_Theme_Type) is
  begin
    Selected_Theme := Theme;
  end Select_Theme;

  --  *************************************************************************
  function Current_Theme return Color_Theme_Type is
  begin
    return Selected_Theme;
  end Current_Theme;

  --  *************************************************************************
  function Theme_Color (Topic : Color_Topic) return RGB_Type is
  begin
    return theme_color_array (Selected_Theme, Topic);
  end Theme_Color;

  --  *************************************************************************
  function Theme_Color (Theme : Color_Theme_Type;
                        Topic : Color_Topic) return RGB_Type is
  begin
    return theme_color_array (Theme, Topic);
  end Theme_Color;

  --  *************************************************************************
  function Theme_Dark_Backgrounded return Boolean is
  begin
    return Selected_Theme = Dark_Side;
  end Theme_Dark_Backgrounded;

  --  *************************************************************************
  function Theme_Dark_Backgrounded (Theme : Color_Theme_Type) return Boolean is
  begin
    return Theme = Dark_Side;
  end Theme_Dark_Backgrounded;

end LEA_Common.Color_Themes;
