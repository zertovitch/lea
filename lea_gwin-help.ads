--  This package deals with unpacking and displaying
--  help. Currently help is a text file; could be a HTML document
--  (would need temp storage).

with LEA_GWin.MDI_Main;

package LEA_GWin.Help is

  procedure Show_help (Main_Window : in out MDI_Main.MDI_Main_Type);

  procedure Show_sample (Main_Window : in out MDI_Main.MDI_Main_Type; Dir, File_Name : String);

end LEA_GWin.Help;
