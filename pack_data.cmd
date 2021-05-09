rem  *** This script is automatically written by sample_catalogue.adb
rem  *** Do not edit it, change lea_common-hac_samples.ads instead! 

mkdir hac_samples
mkdir hac_samples\algorithms
mkdir hac_samples\mathematics
mkdir hac_samples\scripts
mkdir hac_samples\templates
mkdir hac_samples\compatibility

copy ..\hac\exm\bwt.adb               hac_samples\algorithms
copy ..\hac\exm\maze_gen.adb          hac_samples\algorithms
copy ..\hac\exm\merge_sort.adb        hac_samples\algorithms
copy ..\hac\exm\shell_sort.adb        hac_samples\algorithms
copy ..\hac\exm\ackermann.adb         hac_samples\mathematics
copy ..\hac\exm\covid_19_s.adb        hac_samples\mathematics
copy ..\hac\exm\mandelbrot.adb        hac_samples\mathematics
copy ..\hac\exm\random.adb            hac_samples\mathematics
copy ..\hac\exm\series.adb            hac_samples\mathematics
copy ..\hac\exm\three_lakes_s.adb     hac_samples\mathematics
copy ..\hac\exm\env.adb               hac_samples\scripts
copy ..\hac\exm\file_append.adb       hac_samples\scripts
copy ..\hac\exm\file_copy.adb         hac_samples\scripts
copy ..\hac\exm\file_read.adb         hac_samples\scripts
copy ..\hac\exm\timing.adb            hac_samples\scripts
copy ..\hac\exm\hello.adb             hac_samples\templates
copy ..\hac\src\hal.ads               hac_samples\compatibility
copy ..\hac\src\hal.adb               hac_samples\compatibility
copy ..\hac\src\hal-non_standard.adb  hac_samples\compatibility

set samples=hac_samples\algorithms\* hac_samples\mathematics\* hac_samples\scripts\* hac_samples\templates\* hac_samples\compatibility\* 

zipada -eps _lea_data.zip SciLexer.dll lea_help.txt %samples%
