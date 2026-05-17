#delimit cr
cap log close
clear
set more off

* Project paths adjusted for current repo layout.
global projectroot "/Users/songdan/Dispatching"
global datapath "$projectroot/cleaned"
global outpath "$projectroot/output/daiji_results"
cap mkdir "$outpath"

log using "$outpath/cltemp.log", replace

use "$datapath/data_establishments.dta", clear
merge m:1 zipcode using "$datapath/data_zipcode.dta"
tab _merge
keep if _merge==3
drop _merge

merge m:1 area_code using "$datapath/data_area.dta"
tab _merge
keep if _merge==3
drop _merge

merge m:1 pref year using "$datapath/data_pref_year_minimum_wage.dta"
tab _merge
keep if _merge==3
drop _merge

merge m:1 pref year using "$datapath/data_pref_year_partwage.dta"
tab _merge
keep if _merge==3
drop _merge

merge m:1 year using "$datapath/data_year.dta"
tab _merge
keep if _merge==3
drop _merge

gen temptot=tempperm+tempfixed

* size
gen tot=perm+fixed
gen rday=tempdaily/temptot
gen rperm=tempperm/temptot
gen rtemp=tempfixed/temptot

* staff
gen staffperm=perm-tempperm
replace staffperm=. if staffperm<0
gen stafffixed=fixed-tempfixed
replace stafffixed=. if stafffixed<0
gen staff=staffperm+stafffixed
replace staff=. if staff<0
gen rstaff=staff/tot

* training
gen rtraining=trainingnumber/temptot

* cz market
egen n_cz = count(temptot), by(year cz)
gen n_cz_10000 = n_cz/10000
egen mktsize_cz=sum(temptot), by(year cz)
egen mktsize_d_cz=sum(tempdaily), by(year cz)
egen mktsize_p_cz=sum(tempperm), by(year cz)
egen mktsize_f_cz=sum(tempfixed), by(year cz)
egen mktsize_sp_cz=sum(staffperm), by(year cz)
gen share_cz=temptot/mktsize_cz
gen share_d_cz=tempdaily/mktsize_d_cz
gen share_p_cz=tempperm/mktsize_p_cz
gen share_f_cz=tempfixed/mktsize_f_cz
gen share_sp_cz=staffperm/mktsize_sp_cz
gen share2_cz=share_cz^2
gen share_d2_cz=share_d_cz^2
gen share_p2_cz=share_p_cz^2
gen share_f2_cz=share_f_cz^2
gen share_sp2_cz=share_sp_cz^2
egen hh_cz=sum(share2_cz), by(year cz)
egen hh_d_cz=sum(share_d2_cz), by(year cz)
egen hh_p_cz=sum(share_p2_cz), by(year cz)
egen hh_f_cz=sum(share_f2_cz), by(year cz)
egen hh_sp_cz=sum(share_sp2_cz), by(year cz)
egen srank_cz=rank(share_cz), by(year cz)
gen sharealt_cz=share_cz if srank_cz>=1&srank_cz<=3
replace sharealt_cz=0 if srank_cz>=4&srank_cz<.
egen top3_cz=sum(sharealt_cz), by(year cz)

* zip three digits market
gen zip3=substr(zip, 1, 3)
destring zip3, replace

egen mktsize_zip3=sum(temptot), by(year zip3)
egen mktsize_d_zip3=sum(tempdaily), by(year zip3)
egen mktsize_p_zip3=sum(tempperm), by(year zip3)
egen mktsize_f_zip3=sum(tempfixed), by(year zip3)
gen share_zip3=temptot/mktsize_zip3
gen share_d_zip3=tempdaily/mktsize_d_zip3
gen share_p_zip3=tempperm/mktsize_p_zip3
gen share_f_zip3=tempfixed/mktsize_f_zip3
gen share2_zip3=share_zip3^2
gen share_d2_zip3=share_d_zip3^2
gen share_p2_zip3=share_p_zip3^2
gen share_f2_zip3=share_f_zip3^2
egen hh_zip3=sum(share2_zip3), by(year zip3)
egen hh_d_zip3=sum(share_d2_zip3), by(year zip3)
egen hh_p_zip3=sum(share_p2_zip3), by(year zip3)
egen hh_f_zip3=sum(share_f2_zip3), by(year zip3)
egen srank_zip3=rank(share_zip3), by(year zip3)
gen sharealt_zip3=share_zip3 if srank_zip3>=1&srank_zip3<=3
replace sharealt_zip3=0 if srank_zip3>=4&srank_zip3<.
egen top3_zip3=sum(sharealt_zip3), by(year zip3)

* prefecture market
egen mktsize_p=sum(temptot), by(year pref)
egen mktsize_d_p=sum(tempdaily), by(year pref)
egen mktsize_p_p=sum(tempperm), by(year pref)
egen mktsize_f_p=sum(tempfixed), by(year pref)
gen share_p=temptot/mktsize_p
gen share_d_p=tempdaily/mktsize_d_p
gen share_p_p=tempperm/mktsize_p_p
gen share_f_p=tempfixed/mktsize_f_p
gen share2_p=share_p^2
gen share_d2_p=share_d_p^2
gen share_p2_p=share_p_p^2
gen share_f2_p=share_f_p^2
egen hh_p=sum(share2_p), by(year pref)
egen hh_d_p=sum(share_d2_p), by(year pref)
egen hh_p_p=sum(share_p2_p), by(year pref)
egen hh_f_p=sum(share_f2_p), by(year pref)
egen srank_p=rank(share_p), by(year pref)
gen sharealt_p=share_p if srank_p>=1&srank_p<=3
replace sharealt_p=0 if srank_p>=4&srank_p<.
egen top3_p=sum(sharealt_p), by(year pref)

* margin
gen margin=(fee-wage)/fee
replace margin=. if margin<0
gen margindaily=(feedaily-wagedaily)/feedaily
replace margindaily=. if margindaily<0

forvalues i=1/26 {
    gen margin`i'=(fee`i'-wage`i')/fee`i'
    replace margin`i'=. if margin`i'<0
}

gen lfee=ln(fee)
gen lwage=ln(wage)
gen lfee_wage=lfee-lwage

foreach var of varlist length1d length2d_7d length8d_1m length2m length3m length4m_6m length7m_12m length13m_36m lengthmisc {
    replace `var' = 0 if `var'==.
}

egen lengthtotal=rowtotal(length1d length2d_7d length8d_1m length2m length3m length4m_6m length7m_12m length13m_36m lengthmisc)

foreach var of varlist length1d length2d_7d length8d_1m length2m length3m length4m_6m length7m_12m length13m_36m lengthmisc {
    gen r`var' = `var'/lengthtotal if lengthtotal~=0
    replace r`var' = 0 if lengthtotal==0
}

gen rlength_miss = (lengthtotal==0)

label variable tempdaily "派遣労働者：日雇い"
label variable tempperm "Daily average of dispatched workers: permanent"
label variable tempfixed "Daily average of dispatched workers: fixed term"
label variable temptot "Daily average of dispatched workers"
label variable register "登録派遣労働者"
label variable client "派遣先"
label variable wage "Daily average of dispatched workers"
label variable fee "Eight hours fee in JPY 10,000"
label variable margin "(Fee-Wage)/Fee"
label variable lfee "Ln(Fee)"
label variable lwage "Ln(Wage)"
label variable lfee_wage "Ln(Fee)-Ln(Wage)"
label variable wagedaily "日雇い8時間賃金"
label variable feedaily "日雇い8時間料金"
label variable margindaily "日雇い：(料金-賃金)/料金"
label variable staffperm "間接労働者：日雇い"
label variable staffperm "間接労働者：常用"
label variable stafffixed "間接労働者：有期"
label variable temptot "派遣労働者：常用+有期"
label variable rstaff "Staff/Dispatched"
label variable rtraining "Trainng/Dispatched"
label variable mktsize_p "年・都道府県の市場規模"
label variable share_p "年・都道府県内でのシェア"
label variable hh_p "HHI"
label variable hh_d_p "HHI: 日雇"
label variable hh_p_p "HHI: 常用"
label variable hh_f_p "HHI: 有期"
label variable top3_p "上位3事業所シェア"
label variable rlength1d "Contract duration: 1 day"
label variable rlength2d_7d "Contract duration: 2-7 days"
label variable rlength8d_1m "Contract duration: 8-31 days"
label variable rlength2m "Contract duration: 2 months"
label variable rlength3m "Contract duration: 3 months"
label variable rlength4m_6m "Contract duration: 4-6 months"
label variable rlength7m_12m "Contract duration: 7-12 months"
label variable rlength13m_36m "Contract duration: 13-36 months"
label variable rlengthmisc "Contract duration: Misc"

label variable margin1 "IT"
label variable margin2 "Machinery design"
label variable margin3 "Broadcasting, operator"
label variable margin4 "Broadcasting, production"
label variable margin5 "Machinery operation"
label variable margin6 "Translation"
label variable margin7 "Secretary"
label variable margin8 "Filing"
label variable margin9 "Research"
label variable margin10 "Accounting"
label variable margin11 "Trade"
label variable margin12 "Demo"
label variable margin13 "Tour conductor"
label variable margin14 "Clearning"
label variable margin15 "Maintenance"
label variable margin17 "R+D"
label variable margin18 "Planning"
label variable margin19 "Editor"
label variable margin20 "PR design"
label variable margin21 "Interior coodinator"
label variable margin22 "Announcer"
label variable margin23 "IT instructor"
label variable margin24 "Telemarketing"
label variable margin25 "Sales"
label variable margin26 "Broadcasting, staging"
label variable n_cz "N of Establishments in CZ"
label variable n_cz_10000 "N of Establishments in CZ in 10,000"

egen number=count(id), by(year id)

* sample construction
tab year
drop if year==2015
tab year
tab year if register==0
drop if temptot<=5
tab year
drop if staffperm==.
tab year
drop if stafffixed==.
tab year
drop if wage==.|fee==.
tab year
drop if wage>fee
tab year
drop if (wagedaily~=.&feedaily==.)|(wagedaily==.&feedaily~=.)
tab year
drop if wagedaily>feedaily
tab year

* summary statistics for final analysis data
summarize ///
    wage fee margin ///
    tempdaily tempperm tempfixed temptot ///
    register client ///
    rstaff rtraining ///
    hh_cz top3_cz hh_zip3 top3_zip3 hh_p top3_p

tabstat ///
    wage fee margin ///
    tempdaily tempperm tempfixed temptot ///
    register client ///
    rstaff rtraining ///
    hh_cz top3_cz hh_zip3 top3_zip3 hh_p top3_p, ///
    stat(n mean sd p25 p50 p75 min max) ///
    columns(statistics)

preserve
collapse ///
    (count) N = id ///
    (mean) mean_wage = wage ///
           mean_fee = fee ///
           mean_margin = margin ///
           mean_tempdaily = tempdaily ///
           mean_tempperm = tempperm ///
           mean_tempfixed = tempfixed ///
           mean_temptot = temptot ///
           mean_register = register ///
           mean_client = client ///
           mean_rstaff = rstaff ///
           mean_rtraining = rtraining ///
           mean_hh_cz = hh_cz ///
           mean_top3_cz = top3_cz ///
           mean_hh_zip3 = hh_zip3 ///
           mean_top3_zip3 = top3_zip3 ///
           mean_hh_p = hh_p ///
           mean_top3_p = top3_p ///
    (sd) sd_wage = wage ///
         sd_fee = fee ///
         sd_margin = margin ///
         sd_tempdaily = tempdaily ///
         sd_tempperm = tempperm ///
         sd_tempfixed = tempfixed ///
         sd_temptot = temptot ///
         sd_register = register ///
         sd_client = client ///
         sd_rstaff = rstaff ///
         sd_rtraining = rtraining ///
         sd_hh_cz = hh_cz ///
         sd_top3_cz = top3_cz ///
         sd_hh_zip3 = hh_zip3 ///
         sd_top3_zip3 = top3_zip3 ///
         sd_hh_p = hh_p ///
         sd_top3_p = top3_p

