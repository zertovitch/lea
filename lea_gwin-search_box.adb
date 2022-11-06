with LEA_Common;

with LEA_GWin.MDI_Main;

with GWindows.Buttons,
     GWindows.Key_States,
     GWindows.Scintilla;

package body LEA_GWin.Search_box is
  use LEA_Common, LEA_GWin.MDI_Main, GWindows.Key_States;

  procedure Update_drop_downs (SB: in out LEA_search_box_type) is

    procedure Update_drop_down (DD: in out Find_Replace_box_type) is
      max : constant := 10;
      dd_text: constant GString := DD.Text;
    begin
      --  If the text is empty or already in the list, do nothing.
      if dd_text'Length = 0 or else DD.Find_Exact (dd_text) > 0 then
        return;
      end if;
      --  We limit the list to a convenient maximum amount.
      if DD.Count >= max then
        DD.Delete (max);
      end if;
      DD.Add (1, dd_text);
    end Update_drop_down;

  begin
    Update_drop_down (SB.Find_box);
    Update_drop_down (SB.Replace_box);
  end Update_drop_downs;

  procedure Any_Search_Button_Clicked (
    Window : GWindows.Base.Base_Window_Type'Class;
    action : Search_action
  )
  is
    use GWindows.Base;
    Parent: constant Pointer_To_Base_Window_Class := Window.Parent;
  begin
    if Parent /= null and then Parent.all in LEA_search_box_type'Class then  --  Should be always the case.
      declare
        SB      : LEA_search_box_type renames LEA_search_box_type(Parent.all);
        MDI_Top : MDI_Main_Type renames MDI_Main_Type(SB.The_real_MDI_parent.all);
      begin
        --  Ooof.. pointers are behind, we are back into the strong-typed value-type OO world.
        Update_drop_downs (SB);
        MDI_Top.Perform_Search (action);
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

  procedure On_Message
     (FRB          : in out Find_Replace_box_type;
      message      : in     Interfaces.C.unsigned;
      wParam       : in     GWindows.Types.Wparam;
      lParam       : in     GWindows.Types.Lparam;
      Return_Value : in out GWindows.Types.Lresult)
  is
    CB_GETDROPPEDSTATE : constant := 343;
    do_find_next : Boolean := False;
    use Interfaces.C;
  begin
    if message = CB_GETDROPPEDSTATE then
      if Is_Key_Down (VK_ESCAPE) then
        --  FRB.parent_SB.Hide works, but LEA window's focus is lost!
        MDI_Main_Type(FRB.parent_SB.The_real_MDI_parent.all).close_this_search_box := True;
      elsif Is_Key_Down (VK_RETURN) then
        do_find_next := True;
      end if;
    end if;
    GWindows.Combo_Boxes.Drop_Down_Combo_Box_Type
      (FRB).On_Message (message, wParam, lParam, Return_Value);
    if do_find_next then
      Update_drop_downs (FRB.parent_SB.all);
      MDI_Main_Type (FRB.parent_SB.The_real_MDI_parent.all).Perform_Search (find_next);
    end if;

  end On_Message;

  procedure On_Message
     (SB           : in out LEA_search_box_type;
      message      : in     Interfaces.C.unsigned;
      wParam       : in     GWindows.Types.Wparam;
      lParam       : in     GWindows.Types.Lparam;
      Return_Value : in out GWindows.Types.Lresult)
  is
  begin
    if Is_Key_Down(VK_ESCAPE) then
      --  SB.Hide works, but LEA window's focus is lost!
      MDI_Main_Type(SB.The_real_MDI_parent.all).close_this_search_box := True;
    end if;
    LEA_Resource_GUI.Search_box_Type (SB).On_Message (message, wParam, lParam, Return_Value);
  end On_Message;

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
    SB.The_real_MDI_parent := Parent'Unrestricted_Access;
    SB.Create_Full_Dialog (Parent);
    --  The dialog is now created. We customize it a bit...
    SB.Small_Icon ("Binoculars_Icon_Small");
    --  Create text box for searching a text.
    SB.Find_box.Create (SB, "",
       SB.Model_find_box.Left, SB.Model_find_box.Top,
       SB.Model_find_box.Width, SB.Model_find_box.Height,
       False, ID => LEA_Resource_GUI.Model_find_box);
    --  Find_box is identical to Model_find_box, but with new methods.
    SB.Model_find_box.Hide;
    SB.Find_box.parent_SB := SB'Unrestricted_Access;
    --  Create text box for replacing a text with another one.
    SB.Replace_box.Create (SB, "",
       SB.Model_replace_box.Left, SB.Model_replace_box.Top,
       SB.Model_replace_box.Width, SB.Model_replace_box.Height,
       False, ID => LEA_Resource_GUI.Model_replace_box);
    --  Replace_box is identical to Model_replace_box, but with new methods.
    SB.Model_replace_box.Hide;
    SB.Replace_box.parent_SB := SB'Unrestricted_Access;
    --  Hide the versions of the buttons that would close the dialog.
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
    SB.Keyboard_Support;
  end Create_as_search_box;

  function Compose_Scintilla_search_flags (SB: in  LEA_search_box_type) return Integer is
    flags : Integer := 0;
    use GWindows.Scintilla, GWindows.Buttons;
  begin
    if SB.Match_case.State = Checked then
      flags := flags + SCFIND_MATCHCASE;
    end if;
    if SB.Whole_word.State = Checked then
      flags := flags + SCFIND_WHOLEWORD;
    end if;
    return flags;
  end Compose_Scintilla_search_flags;

end LEA_GWin.Search_box;
