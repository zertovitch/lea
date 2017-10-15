with LEA_Resource_GUI;                  use LEA_Resource_GUI;

package body LEA_GWin.Search is

  procedure Create_search_box(main: in out MDI_Main_Type) is
    sb: Search_box_Type renames main.Search_box;
  begin
    sb.Create_Full_Dialog (Parent => main);
    --  Hide the versions of the buttons that close the dialog
    sb.Find_next_button.Hide;
    sb.Find_next_button_permanent.Show;
    sb.Find_previous_button.Hide;
    sb.Find_previous_button_permanent.Show;
    sb.Find_all_button.Hide;
    sb.Find_all_button_permanent.Show;
    sb.Replace_and_find_next_button.Hide;
    sb.Replace_and_find_next_button_permanent.Show;
    sb.Replace_all_button.Hide;
    sb.Replace_all_button_permanent.Show;
    sb.Close_search_box.Hide;
    sb.Close_search_box_permanent.Show;
    sb.Center;
    sb.Show;
  end Create_search_box;

end LEA_GWin.Search;
