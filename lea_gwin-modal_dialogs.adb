--  Small modal dialogs

with LEA_Common.HAC_Samples;

with LEA_GWin.Help;

with LEA_Resource_GUI;

with HAC_Sys, Zip;

with HAT;

with GWindows.Application,
     GWindows.Base,
     GWindows.Common_Controls,
     GWindows.Constants,
     GWindows.Message_Boxes,
     GWindows.Static_Controls.Web,
     GWindows.Types;

with GNAT.Compiler_Version;

with Ada.Wide_Characters.Handling;

package body LEA_GWin.Modal_Dialogs is

  procedure Do_Go_to_Line (Child_Window: in out MDI_Child.MDI_Child_Type) is
    use LEA_Resource_GUI, GWindows.Application, GWindows.Constants, GWindows.Message_Boxes;
    box : Go_to_line_box_Type;
    new_line : Integer := 0;
    --
    procedure Get_Data (Window : in out GWindows.Base.Base_Window_Type'Class) is
    begin
      new_line:= Integer'Wide_Value(box.Line_value_box.Text);
    exception
      when others =>
        Message_Box (Window, "Invalid data", "Line number is invalid", OK_Box, Error_Icon);
        new_line := 0;
    end Get_Data;
    --
  begin
    box.Create_Full_Dialog(Child_Window);
    box.Center(Child_Window);
    box.Line_value_box.Text (Integer'Wide_Image (Child_Window.Editor.Get_current_line + 1));
    box.Line_value_box.Set_Selection (0, 10);
    box.Line_value_box.Focus;
    On_Destroy_Handler (box, Get_Data'Unrestricted_Access);
    case Show_Dialog (box, Child_Window) is
      when IDOK     =>
        if new_line > 0 then
          Child_Window.Editor.Set_current_line (new_line - 1);
        end if;
      when others   =>
        null;  --  Contains the IDCANCEL case
    end case;
  end Do_Go_to_Line;

  procedure Show_About_Box (Main_Window : in out MDI_Main.MDI_Main_Type) is
    use LEA_Resource_GUI, GWindows.Application, GWindows.Constants, GWindows.Static_Controls.Web;
    box : About_box_Type;
    url_lea, url_gnat, url_gnavi, url_hac, url_resedit, url_zipada : URL_Type;
    --
    package CVer is new GNAT.Compiler_Version;
    GNAT_Version_string : constant String := CVer.Version;
    --
  begin
    box.Create_Full_Dialog (Main_Window);
    box.Copyright_label.Text (S2G(Version_info.LegalCopyright));
    box.Version_label.Text (
      S2G(Version_info.FileVersion) & ", built as" &
      GWindows.GStrings.To_GString_From_String (Integer'Image (GWindows.Types.Wparam'Size)) &
      " bit app."
    );
    Create_and_Swap (url_lea,     box.LEA_URL,     box, S2G(LEA_Common.LEA_web_page));
    Create_and_Swap (url_gnat,    box.GNAT_URL,    box, "https://www.adacore.com/community");
    box.GNAT_Version.Text    (S2G("version: " & GNAT_Version_string));
    Create_and_Swap (url_gnavi,   box.GNAVI_URL,   box, "http://sf.net/projects/gnavi");
    Create_and_Swap (url_hac,     box.HAC_URL,     box, "https://hacadacompiler.sourceforge.io/");
    box.HAC_Version.Text    (S2G ("version: " & HAC_Sys.version & ", ref. " & HAC_Sys.reference));
    Create_and_Swap (url_zipada,  box.ZipAda_URL,  box, S2G (Zip.web));
    box.ZipAda_Version.Text (S2G ("version: " & Zip.version & ", ref. " & Zip.reference));
    box.Center;
    if Show_Dialog (box, Main_Window) = IDOK then
      null;
    end if;
  end Show_About_Box;

  procedure Browse_and_Get_Code_Sample (Main_Window : in out MDI_Main.MDI_Main_Type) is
    use LEA_Resource_GUI, LEA_Common, LEA_Common.HAC_Samples, GWindows.Common_Controls;
    box : HAC_example_box_Type;
    --
    row : Natural := 0;
    fn, dir : array (standard_sample'Range) of HAT.VString;
    --
    sel_topic : Sample_Topic := Template;
    --
    procedure Find_Selected_Topic is
    begin
      for i in 1 .. box.Topic_box.Count loop
        if box.Topic_box.Selected (i) then
          sel_topic := Sample_Topic'Val (i - 1);
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
        if standard_sample (i).topic = sel_topic then
          zb.Insert_Item (S2G (To_String (standard_sample (i).name)), row);
          zb.Set_Sub_Item (S2G (To_String (standard_sample (i).description)), row, 1);
          --  The list might be filtered by topic, so the row count
          --  is not always the sample count.
          row := row + 1;
          dir (row) := directory (sel_topic);
          fn (row)  := standard_sample (i).name;
        end if;
      end loop;
    end Refresh_Cat;
    --
    procedure Set_Data is
      zb : List_View_Control_Type renames box.Zipped_file_box;
    begin
      --
      --  List on the left, with topics (Algorithm, Script,... ).
      --
      for topic in Sample_Topic loop
        declare
          wi : GString := Sample_Topic'Wide_Image (topic);
        begin
          wi (wi'First + 1 .. wi'Last) := Ada.Wide_Characters.Handling.To_Lower (wi (wi'First + 1 .. wi'Last));
          box.Topic_box.Add (wi);
        end;
      end loop;
      box.Topic_box.Selected (Sample_Topic'Pos (sel_topic) + 1);
      --
      --  List on the right with samples for the selected topic.
      --
      zb.Set_Extended_Style (Full_Row_Select);
      zb.Insert_Column ("File name", 0, 100);
      zb.Insert_Column ("Description", 1, 350);
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
    begin
      Remember_selection;
    end On_list_quit;

    procedure On_list_double_click (Window : in out GWindows.Base.Base_Window_Type'Class) is
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
    end On_topic_change;

    --
    use GWindows.Application, GWindows.Constants;
  begin
    box.Create_Full_Dialog (Main_Window);
    Set_Data;
    box.Center;
    box.Zipped_file_box.On_Lost_Focus_Handler (On_list_quit'Unrestricted_Access);
    box.Zipped_file_box.On_Double_Click_Handler (On_list_double_click'Unrestricted_Access);
    box.Topic_box.On_Selection_Change_Handler (On_topic_change'Unrestricted_Access);
    box.Topic_box.Focus;  --  Better for using with keyboard, without mouse.
    case Show_Dialog (box, Main_Window) is
      when IDOK   =>
        if sel > 0 then
          LEA_GWin.Help.Show_sample (Main_Window, To_String (dir (sel)), To_String (fn (sel)));
        end if;
      when others =>
        null;  --  Contains the IDCANCEL case
    end case;
  end Browse_and_Get_Code_Sample;

end LEA_GWin.Modal_Dialogs;
