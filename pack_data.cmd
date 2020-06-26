mkdir hac_samples
mkdir hac_samples\scripts

copy ..\hac\exm\file_read.adb hac_samples\scripts
copy ..\hac\exm\file_copy.adb hac_samples\scripts

zipada -eps _lea_data.zip SciLexer.dll lea_help.txt hac_samples\scripts\*
