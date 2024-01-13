with HAC_Sys.Files.Default;

package LEA_GWin.Editor.Streaming is

  ------------------------------------------------------
  --  Editor_Stream_Type                              --
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

  -----------------------------------------------------------------
  --  File_Catalogue                                             --
  -----------------------------------------------------------------
  --  Extension of the file system with the contents of          --
  --  the open editor widgets.                                   --
  --  Idea: when there is a file *and* and open editor,          --
  --  we will prefer the editor.                                 --
  --    * Principal advantage is to take possible modifications  --
  --        of the text compared to the file into account.       --
  --    * A minor advantage could be a faster compilation        --
  --        if the file is stored on a slow drive.               --
  -----------------------------------------------------------------

  type LEA_File_Catalogue is
    limited new HAC_Sys.Files.Default.File_Catalogue with
  record
    mdi_parent : GWindows.Base.Pointer_To_Base_Window_Class;
  end record;
    --  We enrich the default file system with searching of
    --  files through pathes.

  overriding function Exists (cat : LEA_File_Catalogue; name : String) return Boolean;

  overriding function Full_Source_Name (cat : LEA_File_Catalogue; name : String) return String;

  overriding function Is_Open (cat : LEA_File_Catalogue; name : String) return Boolean;

  overriding procedure Source_Open
    (cat    : in out LEA_File_Catalogue;
     name   : in     String;
     stream :    out HAC_Sys.Files.Root_Stream_Class_Access);

  overriding procedure Skip_Shebang
    (cat            : in out LEA_File_Catalogue;
     name           : in     String;
     shebang_offset :    out Natural);

  overriding procedure Close (cat : in out LEA_File_Catalogue; name : String);

end LEA_GWin.Editor.Streaming;
