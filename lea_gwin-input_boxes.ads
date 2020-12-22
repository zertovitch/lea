with GWindows.Base,
     GWindows.Message_Boxes;

package LEA_GWin.Input_Boxes is

  procedure String_Input (
    Parent     : in out GWindows.Base.Base_Window_Type'Class;
    Message    : in     GString;
    User_Input :    out GString_Unbounded;
    Result     :    out GWindows.Message_Boxes.Message_Box_Result
  );

  procedure Skip_Line (
    Parent  : in     GWindows.Base.Base_Window_Type'Class;
    Message : in     GString;
    Result  :    out GWindows.Message_Boxes.Message_Box_Result
  );

end LEA_GWin.Input_Boxes;
