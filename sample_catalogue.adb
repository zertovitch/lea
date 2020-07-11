--  Outputs the CMD commands for copying HAC examples to
--  LEA's catalogue of HAC samples.
--
with HAC_Pack, LEA_Common.HAC_Samples;

procedure Sample_catalogue is
  use HAC_Pack, LEA_Common.HAC_Samples;
  --
  ml : Natural := 0;  --  This is just for cosmetics.
  f : File_Type;
begin
  for i in standard_sample'Range loop
    ml := Integer'Max (ml, Length (standard_sample (i).name));
  end loop;
  Create (f, "pack_data.cmd");
  Put_Line (f, "mkdir hac_samples");
  Put_Line (f, "mkdir hac_samples\algorithms");
  Put_Line (f, "mkdir hac_samples\scripts");
  Put_Line (f, "mkdir hac_samples\templates");
  New_Line (f);
  --
  for i in standard_sample'Range loop
    Put_Line (f,
      "copy ..\hac\exm\" &
      standard_sample (i).name &
      (ml - Length (standard_sample (i).name)) * ' ' &
      "  hac_samples\" &
      directory (standard_sample (i).topic)
    );
  end loop;
  --
  New_Line (f);
  Put_Line (f, "set samples=hac_samples\algorithms\* hac_samples\scripts\* hac_samples\templates\*");
  New_Line (f);
  Put_Line (f, "zipada -eps _lea_data.zip SciLexer.dll lea_help.txt %samples%");
  Close (f);
end;
