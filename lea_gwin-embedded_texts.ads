--  This package deals with unpacking and displaying
--  Embedded_Texts. Currently help is a text file; could be a HTML document
--  (would need temp storage).

with LEA_GWin.MDI_Main;

package LEA_GWin.Embedded_Texts is

  procedure Show_Help (Main_Window : in out MDI_Main.MDI_Main_Type);

  procedure Show_Sample (Main_Window : in out MDI_Main.MDI_Main_Type; Dir, Sample_Name : String);

end LEA_GWin.Embedded_Texts;
