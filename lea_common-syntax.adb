package body LEA_Common.Syntax is

  function Guess_syntax (file_name : UTF_16_String) return Syntax_type is
  begin
    if file_name'Length > 3 then
      if file_name(file_name'Last-3 .. file_name'Last-1) = ".ad" then
        return Ada_syntax;
      elsif file_name(file_name'Last-3 .. file_name'Last) = ".gpr" then
        return GPR_syntax;
      end if;
    end if;
    return Undefined;
  end Guess_syntax;

  function File_type_image (syn: Syntax_type) return UTF_16_String is
  begin
    case syn is
      when Undefined  => return "Text file";
      when Ada_syntax => return "Ada file";
      when GPR_syntax => return "GNAT project file";
    end case;
  end File_type_image;

end LEA_Common.Syntax;
