with LEA_Common.Syntax;

with LEA_GWin.MDI_Child;

with HAT;

with Zip, UnZip.Streams, Zip_Streams;

with GWindows.Base,
     GWindows.Message_Boxes;

with Ada.Command_Line;

package body LEA_GWin.Embedded_Texts is

  procedure Show_embedded (
    Main_Window : in out MDI_Main.MDI_Main_Type;
    File_Name   :        String;
    Short_Name  :        String;
    Is_Help     :        Boolean
  )
  is
    use LEA_Common, LEA_Common.Syntax, LEA_GWin.MDI_Child,
        HAT,
        GWindows.Message_Boxes, Zip_Streams;
    lea_exe : constant String := Ada.Command_Line.Command_Name;
    zi : Zip.Zip_Info;
    mem_stream_unpacked : aliased Memory_Zipstream;
    unpacked : HAT.VString;
    already_open : Boolean := False;
    New_ID : constant ID_Type :=
      (file_name  => Null_GString_Unbounded,  --  No file until first "Save".
       short_name => G2GU (S2G (Short_Name)));
    --
    procedure Check_Duplicate_Embedded_Doc
      (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
    begin
      if already_open then
        --  Duplicate already found.
        return;
      end if;
      if Any_Window.all in MDI_Child_Type'Class then
        declare
          one_child : MDI_Child_Type renames MDI_Child_Type (Any_Window.all);
        begin
          if Is_Help then
            already_open := one_child.editor.document_kind = help_main;
          else
            already_open := Equivalent (one_child.ID, New_ID);
          end if;
          if already_open then
            one_child.Focus;
          end if;
        end;
      end if;
    end Check_Duplicate_Embedded_Doc;
    --
    New_Window : MDI_Child_Access;
  begin
    --  We want only one copy of the help of any sample document displayed.
    GWindows.Base.Enumerate_Children
      (Main_Window.MDI_Client_Window.all,
       Check_Duplicate_Embedded_Doc'Unrestricted_Access);
    if already_open then
      return;
    end if;
    Zip.Load (zi, lea_exe);
    UnZip.Streams.Extract (mem_stream_unpacked, zi, File_Name);
    Get (mem_stream_unpacked, unpacked);
    if not Is_Help then
      unpacked :=
        "--  **************************************************************" & ASCII.LF &
        "--  **  This sample is not saved anywhere on your file system.  **" & ASCII.LF &
        "--  **  Don't forget to save it, especially if you modify it!   **" & ASCII.LF &
        "--  **************************************************************" & ASCII.LF &
        ASCII.LF &
        unpacked;
    end if;
    declare
      unpacked_str : constant String := HAT.To_String (unpacked);  --  visible to dbg
    begin
      New_Window := new MDI_Child_Type;
      if Is_Help then
        New_Window.editor.document_kind := help_main;
      else
        Main_Window.Close_Initial_Document;
      end if;
      Main_Window.User_maximize_restore := False;
      New_Window.Create_LEA_MDI_Child (Main_Window, New_ID);
      New_Window.editor.Load_Text (contents => unpacked_str);
      if Is_Help then
        New_Window.editor.Set_Read_Only (True);
      else
        New_Window.editor.syntax_kind := Ada_syntax;
        New_Window.editor.Set_Scintilla_Syntax;
      end if;
    end;
    --  This is just to set the MRUs in the new window's menu:
    Main_Window.Update_Common_Menus;
    --
    New_Window.Finish_subwindow_opening;
    New_Window.editor.Focus;
  exception
    when Zip.Archive_corrupted =>
      Message_Box (Main_Window, "Embedded file", "Could not unpack file from " & S2G (lea_exe));
  end Show_embedded;

  procedure Show_Help (Main_Window : in out MDI_Main.MDI_Main_Type) is
  begin
    Show_embedded (Main_Window, "lea_help.txt", "Help", Is_Help => True);
  end Show_Help;

  procedure Show_Sample (Main_Window : in out MDI_Main.MDI_Main_Type; Dir, Sample_Name : String) is
  begin
    Show_embedded
      (Main_Window, "hac_samples/" & Dir & '/' & Sample_Name, Sample_Name, Is_Help => False);
  end Show_Sample;

end LEA_GWin.Embedded_Texts;
