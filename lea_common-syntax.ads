package LEA_Common.Syntax is

  type Syntax_type is (Undefined, Ada_syntax);

  Ada_keywords : constant UTF_16_String :=
    "abort abs abstract accept access aliased all and array at begin body case " &
    "constant declare delay delta digits do else elsif end entry exception " &
    "exit for function generic goto if in interface is limited loop mod new not null of " &
    "or others out overriding package pragma private procedure protected raise range " &
    "record rem renames requeue return reverse select separate some subtype synchronized tagged " &
    "task terminate then type until use when while with xor";

  --  Other keyword sets in mind:
  --  - GNAT project files

  function Guess_syntax (file_name : UTF_16_String) return Syntax_type;

  function File_type_image (syn: Syntax_type) return UTF_16_String;

end LEA_Common.Syntax;
