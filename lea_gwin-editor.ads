with GWindows.Base;
with GWindows.Scintilla;                use GWindows.Scintilla;
with GWindows.Windows;
with GWindows.Types;

with Interfaces.C;

package LEA_GWin.Editor is

  use GWindows;

  type LEA_Scintilla_Type is new Scintilla_Type with record
    --  Direct access to the window owning the editor widget.
    --  This is needed to reach the options (color theme, etc.).
    mdi_parent: GWindows.Base.Pointer_To_Base_Window_Class;
    modified  : Boolean:= False;  --  Is the document modified from load or last save ?
  end record;

  overriding
  procedure On_Change (Control : in out LEA_Scintilla_Type);

  overriding
  procedure On_Character_Added
    (Control     : in out LEA_Scintilla_Type;
     Special_Key : in     GWindows.Windows.Special_Key_Type;
     Value       : in     GWindows.GCharacter);

  overriding
  procedure On_Create (Window : in out LEA_Scintilla_Type);

  overriding
  procedure On_Message
    (Window       : in out LEA_Scintilla_Type;
     message      : in     Interfaces.C.unsigned;
     wParam       : in     GWindows.Types.Wparam;
     lParam       : in     GWindows.Types.Lparam;
     Return_Value : in out GWindows.Types.Lresult);

  overriding
  procedure On_Position_Changed (Control : in out LEA_Scintilla_Type;
                                 Pos     : in     Position);

  overriding
  procedure On_Save_Point_Reached (Control : in out LEA_Scintilla_Type);

  overriding
  procedure On_Save_Point_Left (Control : in out LEA_Scintilla_Type);

  overriding
  procedure On_Update_UI (Control : in out LEA_Scintilla_Type);

  --  Methods introduced in the LEA_Scintilla_Type class

  procedure Apply_options (Window : in out LEA_Scintilla_Type);
  procedure Selection_comment (Editor : in out LEA_Scintilla_Type);
  procedure Selection_uncomment (Editor : in out LEA_Scintilla_Type);
  procedure Load_text (Window : in out LEA_Scintilla_Type);
  procedure Save_text (Window : in out LEA_Scintilla_Type; under: GString);

end LEA_GWin.Editor;
