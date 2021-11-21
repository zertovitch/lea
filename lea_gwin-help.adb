with LEA_Common.Syntax;
with LEA_GWin.MDI_Child;

with HAL;

with Zip, UnZip.Streams, Zip_Streams;

with GWindows.Base;
with GWindows.Message_Boxes;

with Ada.Command_Line;

package body LEA_GWin.Help is

  procedure Show_embedded (
    Main_Window : in out MDI_Main.MDI_Main_Type;
    File_Name   :        String;
    Short_Name  :        String;
    Is_Help     :        Boolean
  )
  is
    use LEA_Common, LEA_Common.Syntax, LEA_GWin.MDI_Child,
        HAL,
        GWindows.Message_Boxes, Zip_Streams;
    lea_exe : constant String := Ada.Command_Line.Command_Name;
    zi : Zip.Zip_info;
    mem_stream_unpacked : aliased Memory_Zipstream;
    unpacked : HAL.VString;
    already_open: Boolean := False;
    --
    procedure Check_help_doc (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
    begin
      if Any_Window.all in MDI_Child_Type'Class then
        declare
          one_child: MDI_Child_Type renames MDI_Child_Type(Any_Window.all);
        begin
          if one_child.Document_kind = help_main then
            one_child.Focus;
            already_open := True;
          end if;
        end;
      end if;
    end Check_help_doc;
    --
    New_Window : MDI_Child_Access;
  begin
    if Is_Help then  --  We want only one copy of the help file displayed
      GWindows.Base.Enumerate_Children (Main_Window.MDI_Client_Window.all,
                                        Check_help_doc'Unrestricted_Access);
      if already_open then
        return;
      end if;
    end if;
    Zip.Load (zi, lea_exe);
    UnZip.Streams.Extract (mem_stream_unpacked, zi, File_Name);
    Get (mem_stream_unpacked, unpacked);
    if not Is_Help then
      unpacked :=
        "--  This sample is not saved anywhere on your file system." & ASCII.LF &
        "--  Don't forget to save it if you modify it!" & ASCII.LF &
        "--" & ASCII.LF &
        unpacked;
    end if;
    declare
      unpacked_str: constant String := To_String (unpacked);  --  visible to dbg
    begin
      New_Window := new MDI_Child_Type;
      if Is_Help then
        New_Window.Document_kind := help_main;
      end if;
      New_Window.Short_Name:= G2GU (S2G (Short_Name));
      Main_Window.User_maximize_restore:= False;
      Create_MDI_Child (New_Window.all,
        Main_Window,
        GU2G (New_Window.Short_Name),
        Is_Dynamic => True
      );
      Main_Window.MDI_Active_Window (New_Window.all);
      New_Window.Editor.Load_text (contents => unpacked_str);
      if Is_Help then
        New_Window.Editor.Set_Read_Only (True);
      else
        New_Window.Editor.syntax_kind := Ada_syntax;
        New_Window.Editor.Set_Scintilla_Syntax;
      end if;
    end;
    --  This is just to set the MRUs in the new window's menu:
    Main_Window.Update_Common_Menus;
    --
    New_Window.Finish_subwindow_opening;
    New_Window.Editor.Focus;
  exception
    when Zip.Archive_corrupted =>
      Message_Box (Main_Window, "Embedded file", "Could not unpack file from " & S2G (lea_exe));
  end Show_embedded;

  procedure Show_help (Main_Window : in out MDI_Main.MDI_Main_Type) is
  begin
    Show_embedded (Main_Window, "lea_help.txt", "Help", Is_Help => True);
  end Show_help;

  procedure Show_sample (Main_Window : in out MDI_Main.MDI_Main_Type; Dir, File_Name : String) is
  begin
    Show_embedded (Main_Window, "hac_samples/" & Dir & '/' & File_Name, File_Name, Is_Help => False);
  end Show_sample;

end LEA_GWin.Help;
