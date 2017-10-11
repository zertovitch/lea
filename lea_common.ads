with Ada.Strings.UTF_Encoding;
with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;

-----------------------------------------------------------------
-- LEA elements that are common to all GUI systems / toolkits. --
-----------------------------------------------------------------

package LEA_Common is

  type View_Mode_Type is (Notepad, Studio);

  type Color_Theme_Type is (Default, Dark_side);

  -------------
  -- Strings --
  -------------

  --  Internal format for LEA: UTF-16
  subtype UTF_16_String is Ada.Strings.UTF_Encoding.UTF_16_Wide_String;
  subtype UTF_16_Unbounded_String is Unbounded_Wide_String;

  --  Format for file names on Open / Create operations: UTF-8
  --  See RM A.8.2: File Management
  --  Example: "encoding=8bits", "encoding=utf8"
  Form_For_IO_Open_and_Create: constant String:= "encoding=utf8";

  subtype UTF_8_String is Ada.Strings.UTF_Encoding.UTF_8_String;
  function File_Exists(s: UTF_8_String) return Boolean;

  --  Conversions UTF-8 <-> UTF-16
  function To_UTF_16(s: UTF_8_String) return UTF_16_String;
  function To_UTF_8(s: UTF_16_String) return UTF_8_String;

end LEA_Common;
