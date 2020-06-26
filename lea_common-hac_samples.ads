--  List of HAC samples with their description.

with HAC_Pack;

package LEA_Common.HAC_Samples is

  type Sample_Topic is (Script, Template);

  use HAC_Pack;

  type Sample_Item is record
    topic       : Sample_Topic;
    name        : VString;
    description : VString;
  end record;

  type Sample_List is array (Positive range <>) of Sample_Item;

  standard_sample : constant Sample_List :=
    (
      (Script, +"file_read.adb", +"Reads a file (itself) and displays it on the console"),
      (Script, +"file_copy.adb", +"Reads a file (itself) and copies it to another file")
    );

  directory : array (Sample_Topic) of VString :=
    ( Script   => +"scripts",
      Template => +"templates"
    );

end LEA_Common.HAC_Samples;
