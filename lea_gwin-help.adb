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
  begin
    GWindows.Base.Enumerate_Children (MDI_Client_Window (MDI_Main).all,
                                      Check_help_doc'Unrestricted_Access);
    if already_open then
      return;
    end if;
    Zip.Load (zi, lea_exe);
    UnZip.Streams.Extract (mem_stream_unpacked, zi, "lea_help.txt");
    Get (mem_stream_unpacked, unpacked);
    --  !! here : convert to editor content
  end;

end LEA_GWin.Help;
