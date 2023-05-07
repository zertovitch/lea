--------------------------------------------------------------------
--  This package contains elements for LEA applications that are  --
--  common to all GUI systems / toolkits.                         --
--------------------------------------------------------------------

with HAT;

with Ada.Strings.UTF_Encoding,
     Ada.Strings.Wide_Unbounded;

package LEA_Common is

  LEA_web_page : constant String := "http://l-e-a.sf.net/";

  -------------
  -- Strings --
  -------------

  --  Internal format for LEA: UTF-16
  subtype UTF_16_String is Ada.Strings.UTF_Encoding.UTF_16_Wide_String;
  subtype UTF_16_Unbounded_String is Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

  --  Format for file names on Open / Create operations: UTF-8
  --  See RM A.8.2: File Management
  --  Example: "encoding=8bits", "encoding=utf8"
  Form_For_IO_Open_and_Create : constant String := "encoding=utf8";

  subtype UTF_8_String is Ada.Strings.UTF_Encoding.UTF_8_String;
  function File_Exists (s : UTF_8_String) return Boolean;

  --  Conversions UTF-8 <-> UTF-16
  function To_UTF_16 (s : UTF_8_String) return UTF_16_String;
  function To_UTF_8 (s : UTF_16_String) return UTF_8_String;

  function To_String (V : HAT.VString) return String renames HAT.VStr_Pkg.To_String;

  --------------------------------
  --  Some useful enumerations  --
  --------------------------------

  type View_Mode_Type is (Notepad, Studio);

  package Color_Themes is  --  To do: move to a child package

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
    Red         : constant RGB_Type := 16#00FF00#;
    Dark_Red    : constant RGB_Type := 16#008000#;
    Green       : constant RGB_Type := 16#FF0000#;
    Dark_Green  : constant RGB_Type := 16#800000#;
    Light_Blue  : constant RGB_Type := 16#DD88FF#;
    Blue        : constant RGB_Type := 16#0000FF#;
    Dark_Blue   : constant RGB_Type := 16#000080#;
    Yellow      : constant RGB_Type := 16#FFFF00#;
    Magenta     : constant RGB_Type := 16#00FFFF#;
    Cyan        : constant RGB_Type := 16#FF00FF#;
    Pink        : constant RGB_Type := 16#AFFFAF#;
    Orange      : constant RGB_Type := 16#C8FF00#;
    Dark_Orange : constant RGB_Type := 16#8DF024#;

    --  https://ethanschoonover.com/solarized/
    --
    --  Base tones, dark to light:
    solarized_base02 : constant RGB_Type := 16#073642#;
    solarized_base00 : constant RGB_Type := 16#657b83#;
    solarized_base1  : constant RGB_Type := 16#93a1a1#;
    solarized_base2  : constant RGB_Type := 16#eee8d5#;
    solarized_base3  : constant RGB_Type := 16#fdf6e3#;
    --  Colours:
    solarized_orange : constant RGB_Type := 16#cb4b16#;
    solarized_red    : constant RGB_Type := 16#dc322f#;
    solarized_cyan   : constant RGB_Type := 16#2aa198#;
    solarized_green  : constant RGB_Type := 16#859900#;

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
           parenthesis_background      => 16#F5E7CB#,
           matched_word_highlight      => Dark_Green,
           messages_foreground         => Black,
           messages_background         => White,
           messages_control_background => White),

        Dark_Side =>
          (foreground                  => Light_Gray,
           background                  => 16#242322#,
           keyword                     => Dark_Orange,
           number                      => Red,
           comment                     => 16#CF9F72#,
           string_literal              => Yellow,
           character_literal           => Yellow,
           error_foreground            => White,
           error_background            => Dark_Red,
           caret                       => White,
           selection_foreground        => White,
           selection_background        => 16#D28022#,
           matched_parenthesis         => Green,
           unmatched_parenthesis       => Red,
           parenthesis_background      => 16#505050#,
           matched_word_highlight      => Green,
           messages_foreground         => Light_Gray,
           messages_background         => 16#181716#,
           messages_control_background => 16#141312#),

        Solarized_Light =>
          (foreground                  => solarized_base00,
           background                  => solarized_base3,
           keyword                     => solarized_orange,
           number                      => solarized_red,
           comment                     => solarized_base1,
           string_literal              => solarized_cyan,
           character_literal           => solarized_green,
           error_foreground            => solarized_base3,
           error_background            => solarized_orange,
           caret                       => Black,
           selection_foreground        => solarized_base00,
           selection_background        => solarized_base2,
           matched_parenthesis         => solarized_green,
           unmatched_parenthesis       => solarized_red,
           parenthesis_background      => solarized_base2,
           matched_word_highlight      => solarized_green,
           messages_foreground         => solarized_base02,
           messages_background         => solarized_base3,
           messages_control_background => solarized_base2));

  end Color_Themes;

  type Search_action is (find_next, find_previous, replace_and_find_next, find_all, replace_all);

  type Show_special_symbol_mode is (none, spaces, spaces_eols);

  type Toolset_mode_type is (HAC_mode, GNAT_mode);

  type Document_kind_type is (
    editable_text,
    help_main       --  There is only at most one of this.
  );

end LEA_Common;
