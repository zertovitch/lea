with Ada.Wide_Characters.Handling;      use Ada.Wide_Characters.Handling;
with Ada.Strings;                       use Ada.Strings;
with Ada.Strings.Wide_Fixed;            use Ada.Strings.Wide_Fixed;
with Ada.Wide_Text_IO;

with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package body LEA_Common is

  function To_UTF_16(s: UTF_8_String) return UTF_16_String
  is
  begin
    return Ada.Strings.UTF_Encoding.Conversions.Convert(s);
  end To_UTF_16;

  function To_UTF_8(s: UTF_16_String) return UTF_8_String is
  begin
    return Ada.Strings.UTF_Encoding.Conversions.Convert(s);
  end To_UTF_8;

end LEA_Common;
