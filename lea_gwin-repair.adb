with LEA_Common.User_options;

with LEA_GWin.MDI_Child;

with HAC_Sys.Errors;

with HAT;

with GWindows.Base,
     GWindows.Scintilla;

with Ada.Strings.Unbounded,
     Ada.Strings.Wide_Unbounded;

package body LEA_GWin.Repair is

  options : LEA_Common.User_options.Option_Pack_Type renames LEA_Common.User_options.options;

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
    file_name : constant GString := S2G (To_String (repair.file_name));
    --
    procedure Repair_in_editor (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
      use GWindows.Scintilla;
      line_pos, start_pos, end_pos : Position;
      expanded : GString_Unbounded;
    begin
      if Any_Window /= null and then Any_Window.all in MDI_Child_Type'Class then
        declare
          pw : MDI_Child_Type renames MDI_Child_Type (Any_Window.all);

          procedure Expand is
            prev_is_backslash : Boolean := False;
            alt : constant GString := S2G (HAT.VStr_Pkg.To_String (repair.alternative));
            curr_ind : constant Integer :=
              pw.editor.Get_Line_Indentation (repair.location.line - 1);
          begin
            for c of alt loop
              case c is
                when 't' =>  --  Tab (\t), replaced by spaces matching indentation setting.
                  if prev_is_backslash then
                    expanded := expanded & (options.indentation) * ' ';
                  else
                    expanded := expanded & c;
                  end if;
                when 'n' =>  --  New line (\n), followed by current line's indentation.
                  if prev_is_backslash then
                    expanded := expanded & pw.editor.EOL & curr_ind * ' ';
                  else
                    expanded := expanded & c;
                  end if;
                when '\' =>
                  --  Delay the '\'.
                  null;
                when others =>
                  if prev_is_backslash then
                    --  Backslash has no effect on this value of c.
                    expanded := expanded & '\' & c;
                  else
                    expanded := expanded & c;
                  end if;
              end case;
              prev_is_backslash := c = '\';
            end loop;
            if prev_is_backslash then
              expanded := expanded & '\';
            end if;
          end Expand;

        begin
          if pw.ID.file_name = file_name then
            pw.Focus;  --  Focus on document already open in our app.
            --
            pw.editor.Begin_Undo_Action;
            --
            line_pos := pw.editor.Position_From_Line (repair.location.line - 1);  --  Scintilla's lines are 0-based
            start_pos := line_pos + Position (repair.location.column_start) - 1;
            end_pos   := line_pos + Position (repair.location.column_stop);
            case repair.repair_kind is
              when none =>
                --  We should not get here.
                null;
              when insert =>
                --  Set the caret right at the insersion position:
                pw.editor.Set_Sel (start_pos, start_pos);
              when replace_token =>
                --  Mark the string to be deleted and delete it:
                pw.editor.Set_Sel (start_pos, end_pos);
                pw.editor.Clear;
            end case;
            Expand;
            pw.editor.Insert_Text (pw.editor.Get_Current_Pos, GU2G (expanded));
            --
            pw.editor.End_Undo_Action;
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
       repair.location.line,  --  Scintilla's lines are 0-based
       repair.location.column_start - 1,
       repair.location.column_stop);
    --  At this point, focus is on the editor window (if the file still exists).
    Enumerate_Children (
      MDI_Client_Window (MDI_Main).all,
      Repair_in_editor'Unrestricted_Access
    );
    --  Disable repair:
    repair.repair_kind := none;
  end Do_Repair;

end LEA_GWin.Repair;
