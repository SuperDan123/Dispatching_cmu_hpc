#delimit ;
set scheme s1mono, permanently;
global workpath="C:\Users\kohei\Documents\Dispatching\main\multihome";
global datapath="C:\Users\kohei\Documents\Dispatching\cleaned";
global outpath="C:\Users\kohei\Documents\Dispatching\output\daiji_results";
cap mkdir "$outpath";

do $workpath\03_transform_data\01_cltemp.do;
do $workpath\04_analyze_data\01_des.do;
do $workpath\04_analyze_data\02_reg.do;
exit;
