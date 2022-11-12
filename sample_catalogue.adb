--  Copies HAC examples to LEA's catalogue of HAC samples.
--
with HAT, LEA_Common.HAC_Samples;

procedure Sample_catalogue is
  use HAT, LEA_Common.HAC_Samples;
  --
  from, to, from_subdir, samples : VString;
  sep : constant Character := Directory_Separator;
  hac_project_dir : constant VString := +".." & sep & "hac";
  hac_samples_dir : constant VString := +"hac_samples";
begin
  HAT.Shell_Execute ("mkdir " & hac_samples_dir);
  for topic in Sample_Topic loop
    HAT.Shell_Execute ("mkdir " & hac_samples_dir & sep & directory (topic));
  end loop;
  --
  for i in standard_sample'Range loop
    case standard_sample (i).topic is
      when Compatibility =>
        from_subdir := +"src";
      when Tasking =>
        from_subdir := +"exm/tasking";
      when others =>
        from_subdir := +"exm";
    end case;
    from := hac_project_dir & sep & from_subdir & sep & standard_sample (i).name;
    to   := hac_samples_dir & sep &
              directory (standard_sample (i).topic) & sep & standard_sample (i).name;
    HAT.Copy_File (from, to);
  end loop;
  --
  for topic in Sample_Topic loop
    samples := samples & hac_samples_dir & sep & directory (topic) & sep & "* ";
  end loop;
  HAT.Shell_Execute (+"zipada -eps _lea_data.zip SciLexer.dll lea_help.txt " & samples);
end Sample_catalogue;
