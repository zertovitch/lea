with LEA_Common;                        use LEA_Common;

with GWindows.Base;
with GWindows.Scintilla;                use GWindows.Scintilla;
with GWindows.Windows;
with GWindows.Types;

with Interfaces.C;

package LEA_GWin.Editor is

  type LEA_Scintilla_Type is new Scintilla_Type with record
    --  Direct access to the window owning the editor widget.
    --  This is needed to reach the options (color theme, etc.).
    mdi_parent           : GWindows.Base.Pointer_To_Base_Window_Class;
    modified             : Boolean:= False;  --  Is the document modified from load or last save ?
    pos_last_update_UI   : Position := INVALID_POSITION;
    sel_a_last_update_UI : Position := INVALID_POSITION;
    sel_z_last_update_UI : Position := INVALID_POSITION;
  end record;

  overriding
  procedure On_Change (Editor : in out LEA_Scintilla_Type);

  overriding
  procedure On_Character_Added
    (Editor      : in out LEA_Scintilla_Type;
     Special_Key : in     GWindows.Windows.Special_Key_Type;
     Value       : in     GWindows.GCharacter);

  overriding
  procedure On_Create (Editor : in out LEA_Scintilla_Type);

  overriding
  procedure On_Message
    (Editor       : in out LEA_Scintilla_Type;
     message      : in     Interfaces.C.unsigned;
     wParam       : in     GWindows.Types.Wparam;
     lParam       : in     GWindows.Types.Lparam;
     Return_Value : in out GWindows.Types.Lresult);

  overriding
  procedure On_Save_Point_Reached (Editor : in out LEA_Scintilla_Type);

  overriding
  procedure On_Save_Point_Left (Editor : in out LEA_Scintilla_Type);

  overriding
  procedure On_Update_UI (Editor : in out LEA_Scintilla_Type);

  ----------------------------------------------------------
  --  Methods introduced in the LEA_Scintilla_Type class  --
  ----------------------------------------------------------

  procedure Apply_options (Editor : in out LEA_Scintilla_Type);
  function Get_current_line (Editor : LEA_Scintilla_Type) return Integer;
  procedure Set_current_line (Editor : in out LEA_Scintilla_Type; line: Integer);

  --  Comment / uncomment
  procedure Selection_comment (Editor : in out LEA_Scintilla_Type);
  procedure Selection_uncomment (Editor : in out LEA_Scintilla_Type);

  --  Search & replace actions
  procedure Search (Editor : in out LEA_Scintilla_Type; action : LEA_Common.Search_action);

  --  Duplicate current line if no selection, or selection. Shortcut: Ctrl-D.
  --  Clipboard remains untouched - that the cool aspect in this feature.
  --
  procedure Duplicate (Editor : in out LEA_Scintilla_Type);

  --  I/O
  procedure Load_text (Editor : in out LEA_Scintilla_Type);
  procedure Save_text (Editor : in out LEA_Scintilla_Type; under: GString);

  procedure Set_syntax (Editor : in out LEA_Scintilla_Type; syntax: Syntax_type);

end LEA_GWin.Editor;
