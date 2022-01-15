with LEA_GWin.MDI_Child;

with HAC_Sys.Errors;

with HAL;

with GWindows.Base,
     GWindows.Scintilla;

with Ada.Strings.Unbounded,
     Ada.Strings.Wide_Unbounded;

package body LEA_GWin.Repair is

  procedure Do_Repair (
    MDI_Main : in out LEA_GWin.MDI_Main.MDI_Main_Type;
    repair   : in out HAC_Sys.Defs.Diagnostic_Kit
  )
  is
    use LEA_GWin.MDI_Child, LEA_GWin.MDI_Main,
        HAC_Sys.Defs, HAC_Sys.Errors,
        GWindows.Base,
        Ada.Strings.Unbounded, Ada.Strings.Wide_Unbounded;
    --
    file_name : constant GString_Unbounded := G2GU (S2G (To_String (repair.file_name)));
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
            if has_new_line (repair.repair_kind) then
              return pw.Editor.EOL;
            else
              return "";
            end if;
          end Optional_EOL;
        begin
          if pw.File_Name = file_name then
            pw.Focus;  --  Focus on document already open in our app.
            pw.Editor.Begin_Undo_Action;
            --
            line_pos := pw.Editor.Position_From_Line (repair.line - 1);  --  Scintilla's lines are 0-based
            start_pos := line_pos + Position (repair.column_a);
            end_pos   := line_pos + Position (repair.column_z);
            case repair.repair_kind is
              when none =>
                null;  --  We should not get here.
              when insert | insert_line =>
                pw.Editor.Set_Sel (start_pos, start_pos);
              when replace_token =>
                pw.Editor.Set_Sel (start_pos, end_pos);
                pw.Editor.Clear;
            end case;
            pw.Editor.Insert_Text (pw.Editor.Get_Current_Pos,
              S2G (HAL.VStr_Pkg.To_String (repair.insert_or_replace)) & Optional_EOL);
            --
            pw.Editor.End_Undo_Action;
          end if;
        end;
      end if;
    end Repair_in_editor;
  begin
    if repair.repair_kind = none then
      return;
    end if;
    MDI_Main.Open_Child_Window_And_Load
      (file_name,
       repair.line,  --  Scintilla's lines are 0-based
       repair.column_a,
       repair.column_z);
    --  At this point, focus is on the editor window (if the file still exists).
    Enumerate_Children (
      MDI_Client_Window (MDI_Main).all,
      Repair_in_editor'Unrestricted_Access
    );
    --  Disable repair:
    repair.repair_kind := none;
  end Do_Repair;

end LEA_GWin.Repair;
