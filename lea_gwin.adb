package body LEA_GWin is

  function Equivalent (Id_1, Id_2 : ID_Type) return Boolean is
    use type GString_Unbounded;
  begin
    if Id_1.File_Name = "" or else Id_2.File_Name = "" then
      return Id_1.Short_Name = Id_2.Short_Name;
    else
      return Id_1.File_Name = Id_2.File_Name;
    end if;
  end Equivalent;

end LEA_GWin;
