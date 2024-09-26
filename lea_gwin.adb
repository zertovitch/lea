with Ada.Text_IO;

package body LEA_GWin is

  function Equivalent (Id_1, Id_2 : ID_Type) return Boolean is
    F1 : GString := GU2G (Id_1.file_name);
    F2 : GString := GU2G (Id_2.file_name);
    S1 : GString := GU2G (Id_1.short_name);
    S2 : GString := GU2G (Id_2.short_name);
    trace : constant Boolean := False;
    result : Boolean;
    use Ada.Text_IO;
  begin
    if trace then
      Put_Line ("F1 = [" & G2S (F1) & ']');
      Put_Line ("F2 = [" & G2S (F2) & ']');
      Put_Line ("S1 = [" & G2S (S1) & ']');
      Put_Line ("S2 = [" & G2S (S2) & ']');
    end if;
    if F1 = "" or else F2 = "" then
      --  The "or" condition lets a chance
      --  for the case where an assumed file is searched,
      --  like from message list after a compilation by HAC
      --  (LEA_GWin.Messages, Message_Line_Action) but
      --  an actual file doesn't exist, like a new file or
      --  a sample program from the samples collection.
      To_Upper (S1);
      To_Upper (S2);
      result := S1 = S2;
    else
      To_Upper (F1);
      To_Upper (F2);
      result := F1 = F2;
    end if;
    if trace then
      Put_Line ("Equivalent: " & result'Image);
    end if;
    return result;
  end Equivalent;

  function Color_Convert
    (rgb : LEA_Common.Color_Themes.RGB_Type) return GWindows.Colors.Color_Type is
    use type LEA_Common.Color_Themes.RGB_Type;
    use GWindows.Colors;
  begin
    return
      To_Color
        (Red_Value   => Color_Range (rgb / 2**16),
         Green_Value => Color_Range ((rgb / 2**8) mod (2**8)),
         Blue_Value  => Color_Range (rgb mod (2**8)));
  end Color_Convert;

--   function GWindows_Color_Theme
--     (theme : LEA_Common.Color_Themes.Color_Theme_Type;
--      topic : LEA_Common.Color_Themes.Color_Topic) return GWindows.Colors.Color_Type
--   is
--   begin
--     return Color_Convert (LEA_Common.Color_Themes.theme_color (theme, topic));
--   end GWindows_Color_Theme;

  function Simple_Name (path : GString) return GString is
    start : Natural := path'First;
  begin
    for i in reverse path'Range loop
      if path (i) = '\' then
        start := i + 1;
        exit;
      end if;
    end loop;
    return path (start .. path'Last);
  end Simple_Name;

end LEA_GWin;
