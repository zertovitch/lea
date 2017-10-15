package body LEA_GWin.Search is

  overriding
  procedure On_Close (SB        : in out LEA_search_box_type;
                      Can_Close :    out Boolean)
  is
  begin
    Can_Close:= False;
    SB.Hide;
  end On_Close;

  procedure Create_as_search_box(
    SB     : in out LEA_search_box_type;
    Parent : in out GWindows.Base.Base_Window_Type'Class
  )
  is
  begin
    SB.Create_Full_Dialog (Parent);
    SB.Small_Icon ("Binoculars_Icon_Small");
    --  Hide the versions of the buttons that close the dialog
    SB.Find_next_button.Hide;
    SB.Find_next_button_permanent.Show;
    SB.Find_previous_button.Hide;
    SB.Find_previous_button_permanent.Show;
    SB.Find_all_button.Hide;
    SB.Find_all_button_permanent.Show;
    SB.Replace_and_find_next_button.Hide;
    SB.Replace_and_find_next_button_permanent.Show;
    SB.Replace_all_button.Hide;
    SB.Replace_all_button_permanent.Show;
    --
    SB.Center;
  end Create_as_search_box;

end LEA_GWin.Search;
