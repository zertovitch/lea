with GWindows.Scintilla;                use GWindows.Scintilla;

package LEA_GWin.Editor is

  type LEA_Scintilla_Type is new Scintilla_Type with null record;

  overriding
  procedure On_Create (Window : in out LEA_Scintilla_Type);

end LEA_GWin.Editor;
