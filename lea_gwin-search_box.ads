  --  The search box is common to all editor windows.
  --  It is created once, then:
  --    - shown on Ctrl-F or a click on binoculars
  --    - hidden on button Close or Esc.
  --  It is never closed until the end of the application.

with LEA_Resource_GUI;                  use LEA_Resource_GUI;

with GWindows.Base;                     use GWindows.Base;

package LEA_GWin.Search_box is

  type LEA_search_box_type is new Search_box_Type with record
    The_real_MDI_parent: Pointer_To_Base_Window_Class;
  end record;

  overriding
  procedure On_Close (SB        : in out LEA_search_box_type;
                      Can_Close :    out Boolean);

  procedure Create_as_search_box (
    SB     : in out LEA_search_box_type;
    Parent : in out GWindows.Base.Base_Window_Type'Class
  );

    function Compose_Scintilla_search_flags (SB: in  LEA_search_box_type) return Integer;

end LEA_GWin.Search_box;
