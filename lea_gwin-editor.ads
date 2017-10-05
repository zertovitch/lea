with GWindows.Base;
with GWindows.Scintilla;                use GWindows.Scintilla;

package LEA_GWin.Editor is

  type LEA_Scintilla_Type is new Scintilla_Type with record
    --  Direct access to the window owning the editor widget.
    --  This is needed to reach the options (color theme, etc.).
    mdi_parent: GWindows.Base.Pointer_To_Base_Window_Class;
  end record;

  overriding
  procedure On_Create (Window : in out LEA_Scintilla_Type);

  procedure Load_text (Window : in out LEA_Scintilla_Type);

end LEA_GWin.Editor;
