--  List of HAC samples with their description.

with HAC_Pack;

package LEA_Common.HAC_Samples is

  type Sample_Topic is (Algorithm, Script, Template);

  use HAC_Pack;

  type Sample_Item is record
    topic       : Sample_Topic;
    name        : VString;
    description : VString;
  end record;

  type Sample_List is array (Positive range <>) of Sample_Item;

  standard_sample : constant Sample_List :=
    (
      (Algorithm, +"ackermann.adb",     +"Ackermann recursive function"),
      (Algorithm, +"bwt.adb",           +"Burrows–Wheeler transform: preprocessing for data compression"),
      (Algorithm, +"mandelbrot.adb",    +"Mandelbrot set"),
      (Algorithm, +"random.adb",        +"Random generation: Rand (discrete) and Rnd (continuous)"),
      (Algorithm, +"merge_sort.adb",    +"Merge sort"),
      (Algorithm, +"shell_sort.adb",    +"Shell sort"),
      (Algorithm, +"three_lakes_s.adb", +"Three lakes: ordinary differential equation system"),
      --
      (Script,    +"env.adb",       +"System's Environment Variables: Set & Get"),
      (Script,    +"file_copy.adb", +"Reads a file (itself) and copies it to another file"),
      (Script,    +"file_read.adb", +"Reads a file (itself) and displays it on the console"),
      (Script,    +"timing.adb",    +"Demo for types Time & Duration"),
      --
      (Template,  +"hello.adb",     +"Tiny program (Hello World)")
    );

  directory : array (Sample_Topic) of VString :=
    ( Algorithm => +"algorithms",
      Script    => +"scripts",
      Template  => +"templates"
    );

end LEA_Common.HAC_Samples;
