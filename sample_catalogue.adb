--  Outputs the CMD commands for copying HAC examples to
--  LEA's catalogue of HAC samples.
--
with HAC_Pack, LEA_Common.HAC_Samples;

procedure Sample_catalogue is
  use HAC_Pack, LEA_Common.HAC_Samples;
  --
  ml : Natural := 0;  --  This is just for cosmetics.
begin
  for i in standard_sample'Range loop
    ml := Integer'Max (ml, Length (standard_sample (i).name));
  end loop;
  for i in standard_sample'Range loop
    Put_Line (
      "copy ..\hac\exm\" &
      standard_sample (i).name &
      (ml - Length (standard_sample (i).name)) * ' ' &
      "  hac_samples\" &
      directory (standard_sample (i).topic)
    );
  end loop;
end;
