with Ada.Wide_Characters.Handling;

package body LEA_Common.Syntax is

  function Guess_syntax (file_name, custom_filter : UTF_16_String) return Syntax_type is
    use Ada.Wide_Characters.Handling;
    u_name : constant UTF_16_String := To_Upper (file_name);
    u_filt : constant UTF_16_String := To_Upper (custom_filter);
  begin
    --  Go through the custom filter, like: "*.ADS;*.ADB;*.A"
    for s1 in u_filt'Range loop
      if u_filt (s1) = '*' then  --  Wildcard spotted.
        for s2 in s1 + 1 .. u_filt'Last loop
          --  Now we are with s2, for instance, in the substring: ".ADB;*.A"
          if s2 = u_filt'Last or else u_filt (s2 + 1) = ';' then
            declare
              ext : constant UTF_16_String := u_filt (s1 + 1 .. s2);  --  Ex: ".ADB"
            begin
              if u_name'Length >= ext'Length
                 and then u_name (u_name'Last - ext'Length + 1 .. u_name'Last) = ext
              then
                return Ada_syntax;
              end if;
            end;
          end if;
        end loop;
      end if;
    end loop;
    --  Hard-coded guesses.
    if u_name'Length > 3 then
      if u_name (u_name'Last - 3 .. u_name'Last - 1) = ".AD" then
        return Ada_syntax;
      elsif u_name (u_name'Last - 3 .. u_name'Last) = ".GPR" then
        return GPR_syntax;
      end if;
    end if;
    return Undefined;
  end Guess_syntax;

  function File_type_image (syn : Syntax_type) return UTF_16_String is
  begin
    case syn is
      when Undefined  => return "Text file";
      when Ada_syntax => return "Ada file";
      when GPR_syntax => return "GNAT project file";
    end case;
  end File_type_image;

end LEA_Common.Syntax;
