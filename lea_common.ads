with HAC_Pack;

with Ada.Strings.UTF_Encoding,
     Ada.Strings.Wide_Unbounded;

-----------------------------------------------------------------
-- LEA elements that are common to all GUI systems / toolkits. --
-----------------------------------------------------------------

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

  function To_String (V : HAC_Pack.VString) return String renames HAC_Pack.VStr_Pkg.To_String;

  --------------------------------
  --  Some useful enumerations  --
  --------------------------------

  type View_Mode_Type is (Notepad, Studio);

  type Color_Theme_Type is (Default, Dark_side);

  function Nice_Image (ct : Color_Theme_Type) return UTF_16_String;
  function Nice_Value (im : UTF_16_String) return Color_Theme_Type;

  type Search_action is (find_next, find_previous, replace_and_find_next, find_all, replace_all);

  type Show_special_symbol_mode is (none, spaces, spaces_eols);

  type Toolset_mode_type is (HAC_mode, GNAT_mode);

  type Document_kind_type is (
    editable_text,
    help_main       --  There is only at most one of this.
  );

end LEA_Common;
