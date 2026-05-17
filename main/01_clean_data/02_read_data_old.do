clear all
set more off

*------------------------------------------------------------------------
* Old format
*-------------------------------------------------------------------------

* Year 2010 (H22)
import delimited "rawdata/rawdata_old_csv/◎平成22年度報告原票データ_ippan.csv", varnames(nonames) stringcols(_all) encoding(utf8) rowrange(2) clear 

do "main/rename_old/rename_H22.do"
gen year = 2010
gen tokutei = 0

save "intermediate/data_H22_ippan.dta", replace


import delimited "rawdata/rawdata_old_csv/◎平成22年度報告原票データ_tokutei.csv", varnames(nonames) stringcols(_all) encoding(utf8) rowrange(2) clear 

do "main/rename_old/rename_H22.do"
gen year = 2010
gen tokutei = 1

save "intermediate/data_H22_tokutei.dta", replace


* Year 2011 (H23)
import delimited "rawdata/rawdata_old_csv/◎平成23年度報告原票データ_ippan.csv", varnames(nonames) stringcols(_all) encoding(utf8) rowrange(2) clear 

do "main/rename_old/rename_H23_ippan.do"
gen year = 2011
gen tokutei = 0

save "intermediate/data_H23_ippan.dta", replace


import delimited "rawdata/rawdata_old_csv/◎平成23年度報告原票データ_tokutei.csv", varnames(nonames) stringcols(_all) encoding(utf8) rowrange(2) clear 

do "main/rename_old/rename_H23_tokutei.do"
gen year = 2011
gen tokutei = 1

save "intermediate/data_H23_tokutei.dta", replace


* Year 2012 (H24)
import delimited "rawdata/rawdata_old_csv/◎平成24年度報告原票データ_ippan.csv", varnames(nonames) stringcols(_all) encoding(utf8) rowrange(2) clear 

do "main/rename_old/rename_H24.do"
gen year = 2012
gen tokutei = 0

save "intermediate/data_H24_ippan.dta", replace


import delimited "rawdata/rawdata_old_csv/◎平成24年度報告原票データ_tokutei.csv", varnames(nonames) stringcols(_all) encoding(utf8) rowrange(2) clear 

do "main/rename_old/rename_H24.do"
gen year = 2012
gen tokutei = 1

save "intermediate/data_H24_tokutei.dta", replace

* Year 2013 (H25)
import delimited "rawdata/rawdata_old_csv/◎平成25年度報告原票データ_ippan.csv", varnames(nonames) stringcols(_all) encoding(utf8) rowrange(2) clear 

do "main/rename_old/rename_H25.do"
gen year = 2013
gen tokutei = 0

save "intermediate/data_H25_ippan.dta", replace


import delimited "rawdata/rawdata_old_csv/◎平成25年度報告原票データ_tokutei.csv", varnames(nonames) stringcols(_all) encoding(utf8) rowrange(2) clear 

do "main/rename_old/rename_H25.do"
gen year = 2013
gen tokutei = 1

save "intermediate/data_H25_tokutei.dta", replace



* Year 2014 (H26)
import delimited "rawdata/rawdata_old_csv/◎平成26年度報告原票データ_ippan.csv", varnames(nonames) stringcols(_all) encoding(utf8) rowrange(2) clear 

do "main/rename_old/rename_H26.do"
gen year = 2014
gen tokutei = 0

save "intermediate/data_H26_ippan.dta", replace


import delimited "rawdata/rawdata_old_csv/◎平成26年度報告原票データ_tokutei.csv", varnames(nonames) stringcols(_all) encoding(utf8) rowrange(2) clear 

do "main/rename_old/rename_H26.do"
gen year = 2014
gen tokutei = 1

save "intermediate/data_H26_tokutei.dta", replace


* Year 2015 (H27_old)
// import delimited "rawdata/rawdata_old_csv/◎平成27年度報告原票データ（旧法様式）_ippan.csv", varnames(nonames) stringcols(_all) encoding(utf8) rowrange(2) clear 
//
// do "main/rename_old/rename_H27_old.do"
// gen year = 2015
// gen tokutei = 0
//
// save "intermediate/data_H27_old_ippan.dta", replace
//
//
// import delimited "rawdata/rawdata_old_csv/◎平成27年度報告原票データ（旧法様式）_tokutei.csv", varnames(nonames) stringcols(_all) encoding(utf8) rowrange(2) clear 
//
// do "main/rename_old/rename_H27_old.do"
// gen year = 2015
// gen tokutei = 1
//
// save "intermediate/data_H27_old_tokutei.dta", replace




* append
use "intermediate/data_H22_ippan.dta", clear
append using "intermediate/data_H22_tokutei"
append using "intermediate/data_H23_ippan.dta" 
append using "intermediate/data_H23_tokutei.dta"
append using "intermediate/data_H24_ippan.dta"
append using "intermediate/data_H24_tokutei.dta"
append using "intermediate/data_H25_ippan.dta" 
append using "intermediate/data_H25_tokutei.dta"
append using "intermediate/data_H26_ippan.dta"
append using "intermediate/data_H26_tokutei.dta"


drop blank // empty
drop if firm_id == "" // Each file has a row containing sums that does not have firm_id

order firm_id year tokutei


save "intermediate/data_2010_2014.dta", replace



exit, STATA clear
