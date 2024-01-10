package LEA_GWin.Editor.Streaming is

  ------------------------------------------------------
  --  Output of the editor's text as an input stream  --
  ------------------------------------------------------

  type Editor_Stream_Type is new Ada.Streams.Root_Stream_Type with record
    index  : Ada.Streams.Stream_Element_Offset := 0;
    editor : access LEA_Scintilla_Type'Class;
  end record;

  procedure Reset
    (Stream         : in out Editor_Stream_Type;
     using          : in out LEA_Scintilla_Type'Class;
     shebang_offset :    out Natural);

  --  Only input streams are supported. Basically we
  --  pump the Scintilla editor buffer contents as an input
  --  stream. One application is to use this stream for
  --  the HAC compiler.
  --
  overriding procedure Read
    (Stream : in out Editor_Stream_Type;
     Item   :    out Ada.Streams.Stream_Element_Array;
     Last   :    out Ada.Streams.Stream_Element_Offset);

  --  Output streams are not supported.
  --  But we need to define a bogus `Write` procedure to
  --  solve the abstract ancestor.
  --
  overriding procedure Write
    (Stream : in out Editor_Stream_Type;
     Item   : in     Ada.Streams.Stream_Element_Array);

end LEA_GWin.Editor.Streaming;
