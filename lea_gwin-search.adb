with LEA_Common;                        use LEA_Common;

with LEA_GWin.MDI_Main;                 use LEA_GWin.MDI_Main;

package body LEA_GWin.Search is

  overriding
  procedure On_Close (SB        : in out LEA_search_box_type;
                      Can_Close :    out Boolean)
  is
  begin
    Can_Close:= False;
    SB.Hide;
  end On_Close;

  procedure Any_Search_Button_Clicked (
    Window : GWindows.Base.Base_Window_Type'Class;
    action : Search_action
  )
  is
    Parent: constant Pointer_To_Base_Window_Class := Window.Parent;
  begin
    if Parent /= null and then Parent.all in LEA_search_box_type'Class then  --  Should be always the case.
      declare
        SB      : LEA_search_box_type renames LEA_search_box_type(Parent.all);
        MDI_Top : MDI_Main_Type renames MDI_Main_Type(SB.The_real_MDI_parent.all);
      begin
        --  Ooof.. pointers are behind, we are back into the strong-typed value-type OO world.
        MDI_Top.Perform_Search(action);
      end;
    end if;
  end Any_Search_Button_Clicked;

  procedure Find_Next_Button_Clicked (Window : in out GWindows.Base.Base_Window_Type'Class) is
  begin
    Any_Search_Button_Clicked (Window, find_next);
  end Find_Next_Button_Clicked;

  procedure Find_Previous_Button_Clicked (Window : in out GWindows.Base.Base_Window_Type'Class) is
  begin
    Any_Search_Button_Clicked (Window, find_previous);
  end Find_Previous_Button_Clicked;

  procedure Find_All_Button_Clicked (Window : in out GWindows.Base.Base_Window_Type'Class) is
  begin
    Any_Search_Button_Clicked (Window, find_all);
  end Find_All_Button_Clicked;

  procedure Replace_And_Find_Next_Button_Clicked (Window : in out GWindows.Base.Base_Window_Type'Class) is
  begin
    Any_Search_Button_Clicked (Window, replace_and_find_next);
  end Replace_And_Find_Next_Button_Clicked;

  procedure Replace_All_Button_Clicked (Window : in out GWindows.Base.Base_Window_Type'Class) is
  begin
    Any_Search_Button_Clicked (Window, replace_all);
  end Replace_All_Button_Clicked;

  procedure Create_as_search_box(
    SB     : in out LEA_search_box_type;
    Parent : in out GWindows.Base.Base_Window_Type'Class
  )
  is
  begin
    SB.The_real_MDI_parent := Parent'Unrestricted_Access;
    SB.Create_Full_Dialog (Parent);
    SB.Small_Icon ("Binoculars_Icon_Small");
    --  Hide the versions of the buttons that close the dialog
    SB.Find_next_button.Hide;
    SB.Find_next_button_permanent.Show;
    SB.Find_next_button_permanent.On_Click_Handler(Find_Next_Button_Clicked'Unrestricted_Access);
    SB.Find_previous_button_permanent.On_Click_Handler(Find_Previous_Button_Clicked'Unrestricted_Access);
    SB.Find_all_button_permanent.On_Click_Handler(Find_All_Button_Clicked'Unrestricted_Access);
    SB.Replace_and_find_next_button_permanent.On_Click_Handler(Replace_And_Find_Next_Button_Clicked'Unrestricted_Access);
    SB.Replace_all_button_permanent.On_Click_Handler(Replace_All_Button_Clicked'Unrestricted_Access);
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
