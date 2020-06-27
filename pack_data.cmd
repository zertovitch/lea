mkdir hac_samples
mkdir hac_samples\algorithms
mkdir hac_samples\scripts
mkdir hac_samples\templates

copy ..\hac\exm\ackermann.adb     hac_samples\algorithms
copy ..\hac\exm\bwt.adb           hac_samples\algorithms
copy ..\hac\exm\mandelbrot.adb    hac_samples\algorithms
copy ..\hac\exm\random.adb        hac_samples\algorithms
copy ..\hac\exm\merge_sort.adb    hac_samples\algorithms
copy ..\hac\exm\shell_sort.adb    hac_samples\algorithms
copy ..\hac\exm\three_lakes_s.adb hac_samples\algorithms

copy ..\hac\exm\env.adb       hac_samples\scripts
copy ..\hac\exm\file_copy.adb hac_samples\scripts
copy ..\hac\exm\file_read.adb hac_samples\scripts
copy ..\hac\exm\timing.adb    hac_samples\scripts

copy ..\hac\exm\hello.adb hac_samples\templates

set samples=hac_samples\algorithms\* hac_samples\scripts\* hac_samples\templates\*

zipada -eps _lea_data.zip SciLexer.dll lea_help.txt %samples%