  --  The search box is common to all editor windows.
  --  It is created once, then:
  --    - shown on Ctrl-F or a click on binoculars
  --    - hidden on button Close or Esc.
  --  It is never closed until the end of the application.

with LEA_Resource_GUI;                  use LEA_Resource_GUI;

with GWindows.Base;

package LEA_GWin.Search is

  type LEA_search_box_type is new Search_box_Type with null record;

  overriding
  procedure On_Close (SB        : in out LEA_search_box_type;
                      Can_Close :    out Boolean);

  procedure Create_as_search_box(
    SB     : in out LEA_search_box_type;
    Parent : in out GWindows.Base.Base_Window_Type'Class
  );

end LEA_GWin.Search;
