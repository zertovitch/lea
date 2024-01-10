with LEA_Common.Syntax;

with HAC_Sys.Targets.Semantics;

with GWindows.Base,
     GWindows.Scintilla,
     GWindows.Types,
     GWindows.Windows;

with Ada.Streams;

with Interfaces.C;

package LEA_GWin.Editor is

  use GWindows.Scintilla;

  type LEA_Scintilla_Type is new Scintilla_Type with record
    --  Direct access to the window owning the editor widget.
    --  This is needed to reach the options.
    mdi_parent               : GWindows.Base.Pointer_To_Base_Window_Class;
    document_kind            : LEA_Common.Document_kind_type
                                       := LEA_Common.editable_text;
    --  Is the doc modified from load or last save ?
    modified                 : Boolean     := False;
    pos_last_update_UI       : Position    := INVALID_POSITION;
    sel_a_last_update_UI     : Position    := INVALID_POSITION;
    sel_z_last_update_UI     : Position    := INVALID_POSITION;
    previous_selection_count : Positive    := 1;
    syntax_kind              : LEA_Common.Syntax.Syntax_type := LEA_Common.Syntax.Undefined;
  end record;

  overriding procedure On_Change (Editor : in out LEA_Scintilla_Type);

  overriding procedure On_Character_Added
    (Editor      : in out LEA_Scintilla_Type;
     Special_Key : in     GWindows.Windows.Special_Key_Type;
     Value       : in     GWindows.GCharacter);

  overriding procedure On_Create (Editor : in out LEA_Scintilla_Type);

  overriding procedure On_Dwell_Start
    (Editor : in out LEA_Scintilla_Type;
     Pos    : in     Position);

  overriding procedure On_Dwell_End
    (Editor : in out LEA_Scintilla_Type;
     Pos    : in     Position);

  overriding procedure On_Margin_Click
    (Editor  : in out LEA_Scintilla_Type;
     Pos     : in     Position;
     Margin  : in     Integer);

  overriding procedure On_Message
    (Editor       : in out LEA_Scintilla_Type;
     message      : in     Interfaces.C.unsigned;
     wParam       : in     GWindows.Types.Wparam;
     lParam       : in     GWindows.Types.Lparam;
     Return_Value : in out GWindows.Types.Lresult);

  overriding procedure On_Modified
    (Editor              : in out LEA_Scintilla_Type;
     Pos                 : in     Position;
     Modification_Type   : in     Interfaces.Unsigned_32;
     Text                : in     GString;
     Lines_Added         : in     Integer;
     Line                : in     Integer;
     Fold_Level_Now      : in     Integer;
     Fold_Level_Previous : in     Integer);

  overriding procedure On_Save_Point_Reached
    (Editor : in out LEA_Scintilla_Type);

  overriding procedure On_Save_Point_Left
    (Editor : in out LEA_Scintilla_Type);

  overriding procedure On_Update_UI (Editor : in out LEA_Scintilla_Type);

  ----------------------------------------------------------
  --  Methods introduced in the LEA_Scintilla_Type class  --
  ----------------------------------------------------------

  procedure Apply_Options (Editor : in out LEA_Scintilla_Type);
  procedure Set_Current_Line (Editor : in out LEA_Scintilla_Type; line : Integer);

  --  Comment / uncomment
  procedure Selection_Comment (Editor : in out LEA_Scintilla_Type);
  procedure Selection_Uncomment (Editor : in out LEA_Scintilla_Type);

  --  Search & replace actions
  procedure Search (Editor : in out LEA_Scintilla_Type; action : LEA_Common.Search_action);

  --  Semantics analysis ("smart editor" mode) for navigation
  --  through declarations.
  procedure Semantics (Editor : in out LEA_Scintilla_Type);

  procedure Set_Tip_Styles (Editor : in out LEA_Scintilla_Type);

  --  Bookmarks
  procedure Bookmark_Next (Editor : in out LEA_Scintilla_Type);
  procedure Bookmark_Previous (Editor : in out LEA_Scintilla_Type);
  procedure Bookmark_Toggle (Editor : in out LEA_Scintilla_Type; line : Integer);

  --  Duplicate current line if no selection, or duplicate selection. Shortcut: Ctrl-D.
  --  Clipboard remains untouched - that the cool aspect in this feature.
  --
  procedure Duplicate (Editor : in out LEA_Scintilla_Type);

  --  Get end-of-line string (EOL) of correct type (CR+LF, LF, CR).
  function EOL (Editor : LEA_Scintilla_Type) return GString;

  --  I/O
  procedure Load_Text (Editor : in out LEA_Scintilla_Type; contents : String);
  procedure Load_Text (Editor : in out LEA_Scintilla_Type);  --  Loads from File_Name
  procedure Save_Text (Editor : in out LEA_Scintilla_Type; under : GString);

  --  Propagate the Editor.syntax_kind value to the corresponding
  --  visible behaviour in the Scintilla widget.
  procedure Set_Scintilla_Syntax (Editor : in out LEA_Scintilla_Type);

  --  Smart Editor Navigation
  procedure Find_HAC_Declarations
    (Editor         : in out LEA_Scintilla_Type;
     pos            : in     Position;
     decl_1, decl_2 :    out HAC_Sys.Targets.Semantics.Declaration_Point;
     found          :    out Natural);

end LEA_GWin.Editor;
