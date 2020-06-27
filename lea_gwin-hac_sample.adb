with LEA_Common.HAC_Samples;

with LEA_GWin.Help;

with LEA_Resource_GUI;

with HAC_Pack;

with GWindows.Application,
     GWindows.Base,
     GWindows.Common_Controls,
     GWindows.Constants;

with Ada.Wide_Characters.Handling;

package body LEA_GWin.HAC_Sample is

  procedure Browse_and_Get (Main_Window : in out MDI_Main.MDI_Main_Type) is
    use HAC_Pack, LEA_Resource_GUI, LEA_Common, LEA_Common.HAC_Samples, GWindows.Common_Controls;
    box : HAC_example_box_Type;
    --
    row : Natural := 0;
    fn, dir : array (standard_sample'Range) of VString;
    --
    sel_top : Sample_Topic := Template;
    --
    procedure Find_Selected_Topic is
    begin
      for i in 1 .. box.Topic_box.Count loop
        if box.Topic_box.Selected (i) then
          sel_top := Sample_Topic'Val (i - 1);
        end if;
      end loop;
    end Find_Selected_Topic;
    --
    procedure Refresh_Cat is
      zb : List_View_Control_Type renames box.Zipped_file_box;
    begin
      zb.Clear;
      Find_Selected_Topic;
      row := 0;
      for i in standard_sample'Range loop
        if standard_sample (i).topic = sel_top then
          zb.Insert_Item (S2G (To_String (standard_sample (i).name)), row);
          zb.Set_Sub_Item (S2G (To_String (standard_sample (i).description)), row, 1);
          --  The list might be filtered by topic, so the row count
          --  is not always the sample count.
          row := row + 1;
          dir (row) := directory (sel_top);
          fn (row)  := standard_sample (i).name;
        end if;
      end loop;
    end Refresh_Cat;
    --
    procedure Set_Data is
      zb : List_View_Control_Type renames box.Zipped_file_box;
    begin
      zb.Set_Extended_Style (Full_Row_Select);
      zb.Insert_Column ("File name", 0, 100);
      zb.Insert_Column ("Description", 1, 350);
      for t in Sample_Topic loop
        declare
          wi : GString := Sample_Topic'Wide_Image (t);
        begin
          wi (wi'First + 1 .. wi'Last) := Ada.Wide_Characters.Handling.To_Lower (wi (wi'First + 1 .. wi'Last));
          box.Topic_box.Add (wi);
        end;
      end loop;
      box.Topic_box.Selected (Sample_Topic'Pos (sel_top) + 1);
      Refresh_Cat;
    end Set_Data;
    --
    sel : Natural := 0;
    procedure Remember_selection is
    begin
      sel := 0;
      for i in 0 .. row - 1 loop
        if box.Zipped_file_box.Is_Selected (i) then
          sel := i + 1;
        end if;
      end loop;
    end Remember_selection;

    --  A little headache: when you click OK, the line selection is canceled and lost !
    --  So a solution is to catch the selection *before* losing focus.
    --
    procedure On_list_quit (Window : in out GWindows.Base.Base_Window_Type'Class) is
      pragma Warnings (Off, Window);
    begin
      Remember_selection;
    end On_list_quit;

    procedure On_list_double_click (Window : in out GWindows.Base.Base_Window_Type'Class) is
      pragma Warnings (Off, Window);
    begin
      Remember_selection;
      if sel > 0 then
        box.End_Dialog (GWindows.Constants.IDOK);
      end if;
    end On_list_double_click;

    procedure On_topic_change (Window : in out GWindows.Base.Base_Window_Type'Class) is
    pragma Unreferenced (Window);
    begin
      Find_Selected_Topic;
      Refresh_Cat;
    end;

    --
    use GWindows.Application, GWindows.Constants;
  begin
    box.Create_Full_Dialog (Main_Window);
    Set_Data;
    box.Center;
    box.Zipped_file_box.On_Lost_Focus_Handler (On_list_quit'Unrestricted_Access);
    box.Zipped_file_box.On_Double_Click_Handler (On_list_double_click'Unrestricted_Access);
    box.Topic_box.On_Selection_Change_Handler (On_topic_change'Unrestricted_Access);
    case Show_Dialog (box, Main_Window) is
      when IDOK   =>
        if sel > 0 then
          LEA_GWin.Help.Show_sample (Main_Window, To_String (dir (sel)), To_String (fn (sel)));
        end if;
      when others =>
        null;  --  Contains the IDCANCEL case
    end case;
  end;

end LEA_GWin.HAC_Sample;
