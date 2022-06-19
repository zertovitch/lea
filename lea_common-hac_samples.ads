--  List of HAC samples with their description.

with HAL;

package LEA_Common.HAC_Samples is

  type Sample_Topic is
   (Algorithm,
    Mathematics,
    Script,
    Template,
    --
    Compatibility);

  use HAL;

  type Sample_Item is record
    topic       : Sample_Topic;
    name        : VString;
    description : VString;
  end record;

  type Sample_List is array (Positive range <>) of Sample_Item;

  standard_sample : constant Sample_List :=
    (
      (Algorithm, +"bwt.adb",           +"Burrows-Wheeler transform: preprocessing for data compression"),
      (Algorithm, +"maze_gen.adb",      +"Maze generator"),
      (Algorithm, +"merge_sort.adb",    +"Sorting: Merge sort"),
      (Algorithm, +"shell_sort.adb",    +"Sorting: Shell sort"),
      --
      (Mathematics, +"ackermann.adb",     +"Ackermann recursive function"),
      (Mathematics, +"barnes.adb",        +"John Barnes' puzzle at Ada-Europe 2022 dinner"),
      (Mathematics, +"covid_19_s.adb",    +"Model for simulating the Coronavirus (COVID-19) pandemic"),
      (Mathematics, +"hofstadter.adb",    +"Hofstadter mutual recursive functions"),
      (Mathematics, +"mandelbrot.adb",    +"Mandelbrot set"),
      (Mathematics, +"random.adb",        +"Random generation: Rand (discrete) and Rnd (continuous)"),
      (Mathematics, +"series.adb",        +"Series (e.g. 1 + x + x^2 + ...) and partial sums demo"),
      (Mathematics, +"three_lakes_s.adb", +"Three lakes: ordinary differential equation system"),
      --
      (Script,    +"env.adb",         +"System's Environment Variables: Set & Get"),
      (Script,    +"file_append.adb", +"Creates a file in multiple steps, via the Append procedure"),
      (Script,    +"file_copy.adb",   +"Reads a text file (itself) and copies it to another file"),
      (Script,    +"file_read.adb",   +"Reads a text file (itself) and displays it on the console"),
      (Script,    +"timing.adb",      +"Demo for types Time & Duration"),
      --
      (Template,  +"hello.adb",      +"Tiny program (Hello World)"),
      (Template,  +"hello_big.adb",  +"Larger ""Hello World"""),
      --
      (Compatibility, +"hal.ads",              +"Package specification of HAL for use with a full Ada system"),
      (Compatibility, +"hal.adb",              +"Package body of HAL for use with a full Ada system"),
      (Compatibility, +"hal-non_standard.adb", +"GNAT-specific part of HAL's body")
    );

  directory : array (Sample_Topic) of VString :=
     (Algorithm     => +"algorithms",
      Mathematics   => +"mathematics",
      Script        => +"scripts",
      Template      => +"templates",
      Compatibility => +"compatibility"
    );

end LEA_Common.HAC_Samples;
