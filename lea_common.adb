with Ada.Streams.Stream_IO,
     Ada.Strings.Fixed,
     Ada.Strings.UTF_Encoding.Conversions;

package body LEA_Common is

  function To_UTF_16 (s : UTF_8_String) return UTF_16_String
  is
  begin
    return Ada.Strings.UTF_Encoding.Conversions.Convert (s);
  end To_UTF_16;

  function To_UTF_8 (s : UTF_16_String) return UTF_8_String is
  begin
    return Ada.Strings.UTF_Encoding.Conversions.Convert (s);
  end To_UTF_8;

  function File_Exists (s : UTF_8_String) return Boolean is
    use Ada.Streams.Stream_IO, Ada.Strings.Fixed;
    f : File_Type;
  begin
    if Index (s, "*") > 0 then
      return False;
    end if;
    Open (f, In_File, s, Form_For_IO_Open_and_Create);
    Close (f);
    return True;
  exception
    when Name_Error =>
      return False;  --  The file doesn't exist
    when Use_Error =>
      return True;   --  The file exists and is already opened
  end File_Exists;

end LEA_Common;
