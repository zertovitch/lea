with LEA_GWin.MDI_Child;

with Ada.Directories;
with GWindows.GStrings;
with HAT;

package body LEA_GWin.Editor.Streaming is

  --------------------------
  --  Editor_Stream_Type  --
  --------------------------

  procedure Reset
    (Stream         : in out Editor_Stream_Type;
     using          : in out LEA_Scintilla_Type'Class;
     shebang_offset :    out Natural)
  is
  begin
    Stream.index   := 0;
    Stream.editor  := using'Unchecked_Access;
    shebang_offset := 0;
    if using.Get_Length > 2 and then using.Get_Text_Range (0, 2) = "#!" then
      --  Start from the second line (Scintilla is 0-based):
      Stream.index := Ada.Streams.Stream_Element_Offset (using.Position_From_Line (1));
      shebang_offset := 1;
    end if;
  end Reset;

  overriding procedure Read
    (Stream : in out Editor_Stream_Type;
     Item   :    out Ada.Streams.Stream_Element_Array;
     Last   :    out Ada.Streams.Stream_Element_Offset)
  is
    use Ada.Streams;

    procedure Copy_slice (amount : Ada.Streams.Stream_Element_Offset) is
      slice : constant String :=
        G2S (Stream.editor.Get_Text_Range
          (Position (Stream.index),
           Position (Stream.index + amount)));
      ei : Stream_Element_Offset := Item'First;
    begin
      for s of slice loop
        Item (ei) := Character'Pos (s);
        ei := ei + 1;
      end loop;
      Stream.index := Stream.index + amount;
    end Copy_slice;

  begin
    if Position (Stream.index) >= Stream.editor.Get_Length then
      --  Zero transfer -> Last:= Item'First - 1, see RM 13.13.1(8)
      --  No End_Error here, T'Read will raise it: RM 13.13.2(37)
      if Item'First > Stream_Element_Offset'First then
        Last := Item'First - 1;
        return;
      else
        --  Well, we cannot return Item'First - 1...
        raise Constraint_Error; -- RM 13.13.1(11) requires this.
      end if;
    end if;
    if Item'Length = 0 then
      --  Nothing to be read actually.
      Last := Item'Last;  --  Since Item'Length = 0, we have Item'Last < Item'First
      return;
    end if;
    --  From now on, we can assume Item'Length > 0.

    if Position (Stream.index + Item'Length) < Stream.editor.Get_Length then
      --  * Normal case: even after reading, the index will be in the range
      Last := Item'Last;
      Copy_slice (Item'Length);
      --  Now: Stream.index < Editor.GetLength,
      --  then at least one element is left to be read
    else
      --  * Special case: we exhaust the buffer
      Last := Item'First + Stream_Element_Offset (Stream.editor.Get_Length) - 1 - Stream.index;
      Copy_slice (Last - Item'First + 1);
      --  If Last < Item'Last, the T'Read attribute raises End_Error
      --  because of the incomplete reading.
    end if;
  end Read;

  overriding procedure Write
    (Stream : in out Editor_Stream_Type;
     Item   : in     Ada.Streams.Stream_Element_Array)
  is
    write_is_not_supported : exception;
  begin
    raise write_is_not_supported;
  end Write;

  ----------------------
  --  File_Catalogue  --
  ----------------------

  overriding function Exists (cat : LEA_File_Catalogue; name : String) return Boolean is
  begin
    return cat.Full_Source_Name (name) /= "";
  end Exists;

  overriding function Full_Source_Name (cat : LEA_File_Catalogue; name : String) return String is
    --  !!  To do: add pathes. See HAC_Pkg.
  begin
    declare
      full_physical_name : constant String := Ada.Directories.Full_Name (name);
    begin
      if HAC_Sys.Files.Default.File_Catalogue (cat).Exists (full_physical_name) then
        return full_physical_name;
      end if;
    end;
    return "";
  exception
    when others =>
      return "";
  end Full_Source_Name;

  overriding function Is_Open (cat : LEA_File_Catalogue; name : String) return Boolean is
  begin
    return HAC_Sys.Files.Default.File_Catalogue (cat).Is_Open (cat.Full_Source_Name (name));
  end Is_Open;

  overriding procedure Source_Open
    (cat         : in out LEA_File_Catalogue;
     name        : in     String;
     stream      :    out HAC_Sys.Files.Root_Stream_Class_Access)
  is
    full_name : constant String := cat.Full_Source_Name (name);
    full_name_upper_GString : GString := S2G (full_name);
    editor_found : Boolean := False;
    trace : constant Boolean := False;
    use GWindows.Base;

    procedure Stream_Fronting (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
      use MDI_Child;
    begin
      if (not editor_found)
        and then Any_Window /= null
        and then Any_Window.all in MDI_Child_Type'Class
      then
        declare
          cw : MDI_Child_Type renames MDI_Child_Type (Any_Window.all);
          fn : GString := GU2G (cw.ID.File_Name);
          shebang_offset : Natural;
        begin
          To_Upper (fn);
          if trace then
            HAT.Put ("\--> window: " & G2S (fn));
          end if;
          if fn = full_name_upper_GString then
            if trace then
              HAT.Put (" - match!");
            end if;
            editor_found := True;
            cw.current_editor_stream.Reset (cw.editor, shebang_offset);
            --  Here we ignore the file's stream because we prefer to
            --  substitute the editor's stream.
            --  That way, all open editor windows are used for a build
            --  including all possible text modifications.
            stream := cw.current_editor_stream'Unchecked_Access;
          end if;
        end;
        if trace then
          HAT.New_Line;
        end if;
      end if;
    end Stream_Fronting;

  begin
    if full_name = "" then
      raise Ada.Directories.Name_Error;
    end if;
    HAC_Sys.Files.Default.File_Catalogue (cat).Source_Open (full_name, stream);
    --  We open the "physical" stream even if it is not actually used.
    --  That way, it is locked.

    To_Upper (full_name_upper_GString);
    if trace then
      HAT.Put_Line
        ("Source_Open: " & G2S (full_name_upper_GString) & " - checking open windows:");
    end if;
    Enumerate_Children
      (MDI_Client_Window (cat.mdi_parent.all).all,
       Stream_Fronting'Unrestricted_Access);
    if trace then
      HAT.New_Line;
    end if;
  end Source_Open;

  overriding procedure Skip_Shebang
    (cat            : in out LEA_File_Catalogue;
     name           : in     String;
     shebang_offset :    out Natural) is
  begin
    HAC_Sys.Files.Default.File_Catalogue (cat).Skip_Shebang
      (cat.Full_Source_Name (name),
       shebang_offset);
  end Skip_Shebang;

  overriding procedure Close (cat : in out LEA_File_Catalogue; name : String) is
  begin
    HAC_Sys.Files.Default.File_Catalogue (cat).Close (cat.Full_Source_Name (name));
  end Close;

end LEA_GWin.Editor.Streaming;
