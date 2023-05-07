package body LEA_Common.Color_Themes is

  function Nice_Image (ct : Color_Theme_Type) return UTF_16_String is
  begin
    case ct is
      when Default         => return "Default theme";
      when Dark_Side       => return "Dark Side";
      when Solarized_Light => return "Solarized Light";
    end case;
  end Nice_Image;

  function Nice_Value (im : UTF_16_String) return Color_Theme_Type is
  begin
    for ct in Color_Theme_Type loop
      if im = Nice_Image (ct) then
        return ct;
      end if;
    end loop;
    return Default;
  end Nice_Value;

end LEA_Common.Color_Themes;
