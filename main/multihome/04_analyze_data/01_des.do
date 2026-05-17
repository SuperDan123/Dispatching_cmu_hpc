#delimit ;
cap log close;
cap mkdir "$outpath";
log using $outpath\des.log, replace;
set more off;
set graphics on;
clear;
use $datapath\tempagency.dta;

tab year;
sum tempperm tempfixed temptot wage fee margin
rstaff rtraining 
rlength1d rlength2d_7d rlength8d_1m rlength3m rlength4m_6m rlength7m_12m 
rlength13m_36m rlengthmisc;

sutex2 tempperm tempfixed temptot wage fee margin
rstaff rtraining 
rlength1d rlength2d_7d rlength8d_1m rlength3m rlength4m_6m rlength7m_12m 
rlength13m_36m rlengthmisc,
perc(10 50 90) varlabels tabular replace 
saving($outpath\descstat.tex);

*graph;
histogram n_cz, fraction;
graph export $outpath\n_cz.pdf, as(pdf) replace;

histogram margin, fraction;
graph export $outpath\margin.pdf, as(pdf) replace;

set graphics off;
forvalues i=1/15 {;
histogram margin`i', name(margin`i', replace) xsc(r(0 1)) ysc(r(0 8)) xlabel(0 (0.2) 1) ylabel (0 (2) 8);
};
forvalues i=17/26 {;
histogram margin`i', name(margin`i', replace) xsc(r(0 1)) ysc(r(0 8)) xlabel(0 (0.2) 1) ylabel (0 (2) 8);
};
graph combine margin1 margin2 margin3 margin4 margin5 margin6 margin7 margin8 margin9;
graph export $outpath\margin1-9.pdf, as(pdf) replace;
graph combine margin10 margin11 margin12 margin13 margin14 margin15 margin17 margin18
margin19;
graph export $outpath\margin10-19.pdf, as(pdf) replace;
graph combine margin20 margin21 margin22 margin23 margin24 margin25 margin26;
graph export $outpath\margin20-26.pdf, as(pdf) replace;

log close;
exit;
