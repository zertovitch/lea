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
  Put_Line (f, "rem This script is automatically written by sample_catalogue.adb");
  New_Line (f);
  Put_Line (f, "mkdir hac_samples");
  for topic in Sample_Topic loop
    Put_Line (f, "mkdir hac_samples\" & directory (topic));
  end loop;
  New_Line (f);
  --
  for i in standard_sample'Range loop
    if standard_sample (i).topic = Compatibility then
      Put (f, "copy ..\hac\src\");
    else
      Put (f, "copy ..\hac\exm\");
    end if;
    Put_Line (f,
      standard_sample (i).name &
      (ml - Length (standard_sample (i).name)) * ' ' &
      "  hac_samples\" &
      directory (standard_sample (i).topic)
    );
  end loop;
  --
  New_Line (f);
  Put (f, "set samples=");
  for topic in Sample_Topic loop
    Put (f, "hac_samples\" & directory (topic) & "\* ");
  end loop;
  New_Line (f);
  New_Line (f);
  Put_Line (f, "zipada -eps _lea_data.zip SciLexer.dll lea_help.txt %samples%");
  Close (f);
end;
