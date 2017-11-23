--  This package deals with unpacking and displaying
--  help. Currently help is a text file; could be a HTML document
--  (would need temp storage).

with LEA_GWin.MDI_Main;                 use LEA_GWin.MDI_Main;

package LEA_GWin.Help is

  procedure Show_help (MDI_Main : in out MDI_Main_Type);

end LEA_GWin.Help;
