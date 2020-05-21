with LEA_GWin.MDI_Child;                use LEA_GWin.MDI_Child;
with LEA_GWin.MDI_Main;                 use LEA_GWin.MDI_Main;

with HAC.Defs;
with HAC.UErrors;

with GWindows.Base;

with Ada.Strings.Wide_Unbounded;        use Ada.Strings.Wide_Unbounded;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with GWindows.Scintilla;

package body LEA_GWin.Repair is

  procedure Do_Repair (
    MDI_Main : in out LEA_GWin.MDI_Main.MDI_Main_Type;
    repair   : in out LEA_GWin.Messages.Editor_repair_information
  )
  is
    use HAC.Defs, HAC.UErrors, GWindows.Base;
    --
    procedure Repair_in_editor (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
      line_pos : GWindows.Scintilla.Position;
    begin
      if Any_Window /= null and then Any_Window.all in MDI_Child_Type'Class then
        declare
          pw: MDI_Child_Type renames MDI_Child_Type(Any_Window.all);
          function Optional_EOL return GString is
          begin
            if has_new_line (repair.kind) then
              return pw.Editor.EOL;
            else
              return "";
            end if;
          end Optional_EOL;
        begin
          if pw.File_Name = repair.file then
            pw.Focus;  --  Focus on document already open in our app.
            pw.Editor.BeginUndoAction;
            line_pos := pw.Editor.PositionFromLine (repair.line);
            case repair.kind is
              when none =>
                null;  --  We should not get here.
              when insert | insert_line =>
                pw.Editor.SetSel (line_pos + repair.col_a, line_pos + repair.col_a);
              when replace_token =>
                pw.Editor.SetSel (line_pos + repair.col_a, line_pos + repair.col_z);
                pw.Editor.Clear;
            end case;
            pw.Editor.InsertText (pw.Editor.GetCurrentPos, S2G (To_String (repair.text)) & Optional_EOL);
            pw.Editor.EndUndoAction;
          end if;
        end;
      end if;
    end Repair_in_editor;
  begin
    if repair.kind = none then
      return;
    end if;
    MDI_Main.Open_Child_Window_And_Load (repair.file, repair.line, repair.col_a, repair.col_z);
    --  At this point, focus is on the editor window (if the file still exists).
    Enumerate_Children(
      MDI_Client_Window (MDI_Main).all,
      Repair_in_editor'Unrestricted_Access
    );
    --  Disable repair:
    repair.kind := none;
  end Do_Repair;

end LEA_GWin.Repair;
