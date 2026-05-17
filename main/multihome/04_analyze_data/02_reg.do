#delimit ;
cap log close;
cap mkdir "$outpath";
log using $outpath\reg.log, replace;
set more off;
set graphics on;
clear;
use  $datapath\tempagency.dta;

histogram n_cz_10000;

qui reghdfe lfee n_cz_10000, absorb(cz year) vce(cluster cz);
eststo fee;
qui reghdfe lwage n_cz_10000, absorb(cz year) vce(cluster cz);
eststo wage;
qui reghdfe lfee_wage n_cz_10000, absorb(cz year) vce(cluster cz);
eststo feewage;
estout fee wage feewage, cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) stats(N) keep(n_cz_10000) title("Regression Results");
estout fee wage feewage, cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) stats(N) keep(n_cz_10000) title("Regression Results");
esttab using $outpath\regress.tex, se r2 star(* 0.1 ** 0.05 *** 0.01) b(3) replace;

set graphics off;
binscatter2 lfee n_cz_10000, absorb(cz year) n(50);
graph save $outpath\lfee, replace;
binscatter2 lwage n_cz_10000, absorb(cz year) n(50);
graph save $outpath\lwage, replace;
binscatter2 lfee_wage n_cz_10000, absorb(cz year) n(50);
graph save $outpath\lfee_wage, replace;
set graphics on;
graph combine $outpath\lfee.gph $outpath\lwage.gph $outpath\lfee_wage.gph;
graph export $outpath\n_cz.pdf, as(pdf) replace;

log close;
exit;

local panel="cz";
local cluster="cz";
local controls="rtraining rlength2d_7d rlength8d_1m rlength2m rlength3m rlength4m_6m rlength7m_12m rlengthmisc rlength_miss tempdaily tempperm tempfixed";

qui xtreg lfee hh_p i.year, i(`panel') cluster(`cluster');
eststo fee;
qui xtreg lwage hh_p i.year, i(`panel') cluster(`cluster');
eststo wage;
qui xtreg lfee_wage hh_p i.year, i(`panel') cluster(`cluster');
eststo feewage;
estout fee wage feewage, cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) stats(N) keep(hh_p) title("Regression Results");

qui xtreg lfee hh_p i.year `controls', i(`panel') cluster(`cluster');
eststo fee;
qui xtreg lwage hh_p i.year `controls', i(`panel') cluster(`cluster');
eststo wage;
qui xtreg lfee_wage hh_p i.year `controls', i(`panel') cluster(`cluster');
eststo feewage;
estout fee wage feewage, cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) stats(N) keep(hh_p) title("Regression Results");

qui xtreg lfee hh_zip3 i.year, i(`panel') cluster(`cluster');
eststo fee;
qui xtreg lwage hh_zip3 i.year, i(`panel') cluster(`cluster');
eststo wage;
qui xtreg lfee_wage hh_zip3 i.year, i(`panel') cluster(`cluster');
eststo feewage;
estout fee wage feewage, cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) stats(N) keep(hh_zip3) title("Regression Results");

qui xtreg lfee hh_zip3 i.year `controls', i(`panel') cluster(`cluster');
eststo fee;
qui xtreg lwage hh_zip3 i.year `controls', i(`panel') cluster(`cluster');
eststo wage;
qui xtreg lfee_wage hh_zip3 i.year `controls', i(`panel') cluster(`cluster');
eststo feewage;
estout fee wage feewage, cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) stats(N) keep(hh_zip3) title("Regression Results");

