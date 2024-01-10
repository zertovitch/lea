package body LEA_GWin.Editor.Streaming is

  --------------------------------------------------------------
  --  Output of the editor's text is used as an input stream  --
  --------------------------------------------------------------

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

end LEA_GWin.Editor.Streaming;
