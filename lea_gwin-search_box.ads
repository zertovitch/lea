  --  The search box is common to all editor windows.
  --  It is created once, then:
  --    - shown on Ctrl+F or a click on binoculars
  --    - hidden on button Close or Esc.
  --  It is never closed until the end of the application.

with LEA_Resource_GUI;

with GWindows.Base,
     GWindows.Types,
     GWindows.Combo_Boxes;

with Interfaces.C;

package LEA_GWin.Search_Box is

  type LEA_Search_Box_Type;
  type LEA_Search_Box_Access_Type is access all LEA_Search_Box_Type;

  type Find_Replace_Box_Type is
    new GWindows.Combo_Boxes.Drop_Down_Combo_Box_Type with
  record
    parent_SB : LEA_Search_Box_Access_Type;
  end record;

  overriding
  procedure On_Message
    (FRB          : in out Find_Replace_Box_Type;
     message      : in     Interfaces.C.unsigned;
     wParam       : in     GWindows.Types.Wparam;
     lParam       : in     GWindows.Types.Lparam;
     Return_Value : in out GWindows.Types.Lresult);

  type LEA_Search_Box_Type is new LEA_Resource_GUI.Search_box_Type with record
    The_real_MDI_parent : GWindows.Base.Pointer_To_Base_Window_Class;
    Find_box,
    Replace_box         : Find_Replace_Box_Type;
  end record;

  overriding
  procedure On_Message
    (SB           : in out LEA_Search_Box_Type;
     message      : in     Interfaces.C.unsigned;
     wParam       : in     GWindows.Types.Wparam;
     lParam       : in     GWindows.Types.Lparam;
     Return_Value : in out GWindows.Types.Lresult);

  overriding
  procedure On_Close (SB        : in out LEA_Search_Box_Type;
                      Can_Close :    out Boolean);

  procedure Update_Drop_Downs (SB : in out LEA_Search_Box_Type);

  procedure Create_as_Search_Box
    (SB     : in out LEA_Search_Box_Type;
     Parent : in out GWindows.Base.Base_Window_Type'Class);

  function Compose_Scintilla_Search_Flags (SB : in  LEA_Search_Box_Type) return Integer;

end LEA_GWin.Search_Box;
