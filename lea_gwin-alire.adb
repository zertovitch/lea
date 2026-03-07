with LEA_GWin.Messages.IO_Pipe,
     LEA_GWin.Shell;

with Ada.Directories;

package body LEA_GWin.Alire is

   procedure Alr_Command (window : in out MDI_Child.MDI_Child_Type; command : String) is
     fn   : constant String := G2S (GU2G (window.ID.file_name));
     toml : constant String := Detect_Alire_TOML (fn);
     ml   : Messages.Message_List_Type renames window.mdi_root.Message_Panel.Message_List;
     res  : Integer;
   begin
     Messages.IO_Pipe.Set_Current_IO_Pipe (ml);
     ml.Set_Column ("", 0, 800);
     Messages.IO_Pipe.Change_Header ("Alire command: " & command);
     ml.Clear;
     if toml = "" then
       Messages.IO_Pipe.Put_Console ("Alire Manifest (alire.toml) not found");
       Messages.IO_Pipe.New_Line_Console;
     else
       declare
         use Ada.Directories;
         old_dir : constant String := Current_Directory;
         new_dir : constant String := Containing_Directory (toml);
       begin
         Set_Directory (new_dir);
         Shell.Execute_to_LEA_Console (command, res);
         Set_Directory (old_dir);
       end;
     end if;
   end Alr_Command;

   ---------------
   -- Alr_Build --
   ---------------

   procedure Alr_Build (window : in out MDI_Child.MDI_Child_Type) is
   begin
     Alr_Command (window, "alr build");
   end Alr_Build;

   -------------
   -- Alr_Run --
   -------------

   procedure Alr_Run (window : in out MDI_Child.MDI_Child_Type) is
   begin
     Alr_Command (window, "alr run");
   end Alr_Run;

   -----------------------
   -- Detect_Alire_TOML --
   -----------------------

   function Detect_Alire_TOML (file_name : String) return String is
     sep_pos : Natural := 0;
   begin

      for i in file_name'Range loop
        if file_name (i) in '\' | '/' then
          sep_pos := i;
        end if;
      end loop;

      if sep_pos = 0 then
        return "";
      else
        declare
          candidate : constant String := file_name (file_name'First .. sep_pos) & "alire.toml";
        begin
          if Ada.Directories.Exists (candidate) then
            return candidate;
          else
            --  Not found, search in upper directory, if any.
            return Detect_Alire_TOML (file_name (file_name'First .. sep_pos - 1));
          end if;
        end;
      end if;

   end Detect_Alire_TOML;

end LEA_GWin.Alire;
