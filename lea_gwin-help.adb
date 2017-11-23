--  This package deals with unpacking and displaying
--  help. Currently help is a text file; could be a HTML document
--  (would need temp storage).

with LEA_Common;                        use LEA_Common;
with LEA_GWin.MDI_Child;                use LEA_GWin.MDI_Child;

with Zip;
with UnZip.Streams;

with Zip_Streams;                       use Zip_Streams;

with GWindows.Base;

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
    New_Window := new MDI_Child_Type;
    New_Window.Short_Name:= G2GU("Help");
    Create_MDI_Child (New_Window.all,
      MDI_Main,
      GU2G (New_Window.Short_Name),
      Is_Dynamic => True
    );
    MDI_Active_Window (MDI_Main, New_Window.all);
    New_Window.Editor.SetReadOnly (True);
    New_Window.Editor.Load_text (contents => To_String (unpacked));
    --  !!  MDI_Main.Finish_subwindow_opening (New_Window.all);
    New_Window.Editor.Focus;
  end Show_help;

end LEA_GWin.Help;
