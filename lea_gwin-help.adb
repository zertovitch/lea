with LEA_Common;                        use LEA_Common;
with LEA_GWin.MDI_Child;                use LEA_GWin.MDI_Child;

with Zip;
with UnZip.Streams;

with Zip_Streams;                       use Zip_Streams;

with GWindows.Base;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package body LEA_GWin.Help is

  procedure Show_help (MDI_Main : in out MDI_Main_Type) is
    lea_exe: constant String := Command_Name;
    zi: Zip.Zip_info;
    mem_stream_unpacked : aliased Memory_Zipstream;
    unpacked : Unbounded_String;
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
    GWindows.Base.Enumerate_Children (MDI_Client_Window (MDI_Main).all,
                                      Check_help_doc'Unrestricted_Access);
    if already_open then
      return;
    end if;
    Zip.Load (zi, lea_exe);
    UnZip.Streams.Extract (mem_stream_unpacked, zi, "lea_help.txt");
    Get (mem_stream_unpacked, unpacked);
    declare
      unpacked_str: constant String := To_String (unpacked);  --  visible to dbg
    begin
      New_Window := new MDI_Child_Type;
      New_Window.Document_kind := help_main;
      New_Window.Short_Name:= G2GU("Help");
      MDI_Main.User_maximize_restore:= False;
      Create_MDI_Child (New_Window.all,
        MDI_Main,
        GU2G (New_Window.Short_Name),
        Is_Dynamic => True
      );
      MDI_Active_Window (MDI_Main, New_Window.all);
      New_Window.Editor.Load_text (contents => unpacked_str);
      New_Window.Editor.SetReadOnly (True);
    end;
    --  This is just to set the MRUs in the new window's menu:
    MDI_Main.Update_Common_Menus;
    --
    New_Window.Finish_subwindow_opening;
    New_Window.Editor.Focus;
  exception
    when Zip.Archive_corrupted =>
      Message_Box (MDI_Main, "Help", "Cannot unpack help file");
  end Show_help;

end LEA_GWin.Help;
