with LEA_GWin.MDI_Child;

with HAC_Sys.Defs,
     HAC_Sys.UErrors;

with HAL;

with GWindows.Base,
     GWindows.Scintilla;

with Ada.Strings.Wide_Unbounded;

package body LEA_GWin.Repair is

  procedure Do_Repair (
    MDI_Main : in out LEA_GWin.MDI_Main.MDI_Main_Type;
    repair   : in out LEA_GWin.Messages.Editor_repair_information
  )
  is
    use LEA_GWin.MDI_Child, LEA_GWin.MDI_Main,
        HAC_Sys.Defs, HAC_Sys.UErrors,
        GWindows.Base,
        Ada.Strings.Wide_Unbounded;
    --
    procedure Repair_in_editor (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
      use GWindows.Scintilla;
      line_pos, start_pos, end_pos : Position;
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
            pw.Editor.Begin_Undo_Action;
            --
            line_pos := pw.Editor.Position_From_Line (repair.line);
            start_pos := line_pos + Position (repair.col_a);
            end_pos   := line_pos + Position (repair.col_z);
            case repair.kind is
              when none =>
                null;  --  We should not get here.
              when insert | insert_line =>
                pw.Editor.Set_Sel (start_pos, start_pos);
              when replace_token =>
                pw.Editor.Set_Sel (start_pos, end_pos);
                pw.Editor.Clear;
            end case;
            pw.Editor.Insert_Text (pw.Editor.Get_Current_Pos, S2G (HAL.VStr_Pkg.To_String (repair.text)) & Optional_EOL);
            --
            pw.Editor.End_Undo_Action;
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
    Enumerate_Children (
      MDI_Client_Window (MDI_Main).all,
      Repair_in_editor'Unrestricted_Access
    );
    --  Disable repair:
    repair.kind := none;
  end Do_Repair;

end LEA_GWin.Repair;
