--  Copies HAC examples to LEA's catalogue of HAC samples.
--
with HAL, LEA_Common.HAC_Samples;

procedure Sample_catalogue is
  use HAL, LEA_Common.HAC_Samples;
  --
  from, to, samples : VString;
  sep : constant Character := HAL.Directory_Separator;
  hac_project_dir : constant VString := +".." & sep & "hac";
  hac_samples_dir : constant VString := +"hac_samples";
begin
  HAL.Shell_Execute ("mkdir " & hac_samples_dir);
  for topic in Sample_Topic loop
    HAL.Shell_Execute ("mkdir " & hac_samples_dir & sep & directory (topic));
  end loop;
  --
  for i in standard_sample'Range loop
    if standard_sample (i).topic = Compatibility then
      samples := +"src";
    else
      samples := +"exm";
    end if;
    from := hac_project_dir & sep & samples & sep & standard_sample (i).name;
    to   := hac_samples_dir & sep &
              directory (standard_sample (i).topic) & sep & standard_sample (i).name;
    HAL.Copy_File (from, to);
  end loop;
  --
  for topic in Sample_Topic loop
    samples := samples & hac_samples_dir & sep & directory (topic) & sep & "* ";
  end loop;
  HAL.Shell_Execute (+"zipada -eps _lea_data.zip SciLexer.dll lea_help.txt " & samples);
end Sample_catalogue;
