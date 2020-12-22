with GWindows.Base,
     GWindows.Message_Boxes;

package LEA_GWin.Input_Boxes is

  function String_Input (
    Parent  : in out GWindows.Base.Base_Window_Type'Class;
    Message :        GString
  )
  return GString;

  procedure Skip_Line (
    Parent  : in  GWindows.Base.Base_Window_Type'Class;
    Message : in  GWindows.GString;
    Result  : out GWindows.Message_Boxes.Message_Box_Result
  );

end LEA_GWin.Input_Boxes;
