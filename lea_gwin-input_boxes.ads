with GWindows.Base;                     use GWindows.Base;

package LEA_GWin.Input_Boxes is

  function String_Input (
    Parent  : in out Base_Window_Type'Class;
    Message :        GString
  )
  return GString;

  procedure Skip_Line (
    Parent  : Base_Window_Type'Class;
    Message : GString
  );

end LEA_GWin.Input_Boxes;
