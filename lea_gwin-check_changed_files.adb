with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Directories;
with GWindows.Application,
     GWindows.Base,
     GWindows.Common_Controls,
     GWindows.Constants;

with LEA_GWin.MDI_Child;
with LEA_Resource_GUI;

procedure LEA_GWin.Check_Changed_Files (main : in out LEA_GWin.MDI_Main.MDI_Main_Type) is

  use GWindows.Application, GWindows.Common_Controls, GWindows.Constants, LEA_GWin.MDI_Child;

  package Externally_Changed_Vectors is new Ada.Containers.Vectors (Natural, MDI_Child_Access);

  list : Externally_Changed_Vectors.Vector;

  procedure Check_Time_Stamp (any_window : GWindows.Base.Pointer_To_Base_Window_Class) is
    use Ada.Calendar, GWindows.Base;
  begin
    if any_window /= null and then any_window.all in MDI_Child_Type'Class then
      declare
        ca : constant MDI_Child_Access := MDI_Child_Access (any_window);
        cw : MDI_Child_Type renames ca.all;
        fn : constant String := G2S (GU2G (cw.ID.file_name));
        time_stamp : Time;
      begin
        if fn /= "" then
          time_stamp := Ada.Directories.Modification_Time (fn);
          if time_stamp > cw.last_save_time then
            list.Append (ca);
          end if;
        end if;
      end;
    end if;
  end Check_Time_Stamp;

  box : LEA_Resource_GUI.Reload_Files_Box_Type;

  procedure Get_Data (Window : in out GWindows.Base.Base_Window_Type'Class) is
    index : Integer := -1;
    memo_line : Integer;
  begin
      for ca of list loop
         index := index + 1;

         if box.Changed_Files_List.Is_Checked (index) then
            --  Reload text from external file and go to the line number of the previous version.
            memo_line := ca.editor.Get_Current_Line_Number;
            ca.editor.Begin_Undo_Action;
            ca.editor.Clear_All;
            ca.editor.Load_Text;
            ca.editor.End_Undo_Action;
            ca.editor.Set_Current_Line (memo_line);
         else
            --  Keep divergent text.
            null;
         end if;

         --  Don't ask again for that time stamp. We prevent it by aligning
         --  the internal save time on the external time stamp.
         --  A later modification time will again trigger the dialog.
         ca.last_save_time := Ada.Directories.Modification_Time (G2S (GU2G (ca.ID.file_name)));
      end loop;
  end Get_Data;

  last_index : Integer := -1;

  procedure Select_All (dummy : in out GWindows.Base.Base_Window_Type'Class) is
  begin
    for index in 0 .. last_index loop
       box.Changed_Files_List.Checked (index, True);
    end loop;
  end Select_All;

  procedure Unselect_All (dummy : in out GWindows.Base.Base_Window_Type'Class) is
  begin
    for index in 0 .. last_index loop
       box.Changed_Files_List.Checked (index, False);
    end loop;
  end Unselect_All;

begin
  main.MDI_Client_Window.Enumerate_Children (Check_Time_Stamp'Unrestricted_Access);

  if not list.Is_Empty then

    box.Create_Full_Dialog (main);
    box.Center (main);
    box.Changed_Files_List.Set_Extended_Style (Full_Row_Select);
    box.Changed_Files_List.Set_Extended_Style (Checkboxes);

    box.Changed_Files_List.Insert_Column ("Name", 0, 180);
    box.Changed_Files_List.Insert_Column ("Full Name", 1, 200);
    box.Changed_Files_List.Insert_Column ("Pending modification in LEA ?", 2, 210);
    for ca of list loop
      last_index := last_index + 1;
      box.Changed_Files_List.Insert_Item (Simple_Name (GU2G (ca.ID.short_name)), last_index);
      box.Changed_Files_List.Set_Sub_Item (GU2G (ca.ID.file_name), last_index, 1);
      box.Changed_Files_List.Set_Sub_Item
        ((if ca.editor.modified then "Yes! Changes will be lost on file reload" else "No"),
         last_index,
         2);
    end loop;
    box.Select_All_Button.Hide;
    box.Select_All_Button_permanent.Show;
    box.Select_All_Button_permanent.On_Click_Handler (Select_All'Unrestricted_Access);

    box.Unselect_All_Button.Hide;
    box.Unselect_All_Button_permanent.Show;
    box.Unselect_All_Button_permanent.On_Click_Handler (Unselect_All'Unrestricted_Access);

    box.On_Destroy_Handler (Get_Data'Unrestricted_Access);
    Select_All (box);

    if Show_Dialog (box, main) = IDOK then
      null;
    end if;
  end if;

end LEA_GWin.Check_Changed_Files;
