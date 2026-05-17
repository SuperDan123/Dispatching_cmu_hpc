#delimit ;

cap log close;
log using $workpath\log\crtemp.log, replace;
set more off;

clear;
import delimited $workpath\data\zenkoku.csv, varnames(1);
*drop if 事業所フラグ==1;
rename 市区町村cd cityid;
rename 郵便番号 zip;
gen zip3=substr(zip, 1, 3);
destring zip3, replace;
keep cityid zip zip3;
order zip zip3 cityid;
egen seq=seq(), by(zip3);
drop if seq>=2&seq<.;
drop seq;
destring zip, ignore("-") replace;
label variable zip "ZIP code";
label variable zip3 "ZIP code first three digits";
label variable cityid "PrefID+CityID";
drop if zip==.|zip==0|zip3==.|zip3==0|cityid==.|cityid==0;
duplicates drop;
compress;
save $workpath\data\zip_cityid, replace;

clear;
import delimited $workpath\data\cz_2010_original.csv, varnames(1);
rename i cityid;
rename cluster cz;
* Recode Seireishitei Toshi;
replace cityid=1100 if cityid>=1101&cityid<=1110;
replace cityid=4100 if cityid>=4101&cityid<=4105;
replace cityid=11100 if cityid>=11101&cityid<=11110;
replace cityid=12100 if cityid>=12101&cityid<=12106;
replace cityid=13100 if cityid>=13101&cityid<=13123;
replace cityid=14100 if cityid>=14101&cityid<=14118;
replace cityid=14130 if cityid>=14131&cityid<=14137;
replace cityid=14150 if cityid>=14151&cityid<=14153;
replace cityid=15100 if cityid>=15101&cityid<=15108;
replace cityid=22100 if cityid>=22101&cityid<=22103;
replace cityid=22130 if cityid>=22131&cityid<=22137;
replace cityid=23100 if cityid>=23101&cityid<=23116;
replace cityid=26100 if cityid>=26101&cityid<=26111;
replace cityid=27100 if cityid>=27101&cityid<=27128;
replace cityid=27140 if cityid>=27141&cityid<=27147;
replace cityid=28100 if cityid>=28101&cityid<=28111;
replace cityid=33100 if cityid>=33101&cityid<=33104;
replace cityid=34100 if cityid>=34101&cityid<=34108;
replace cityid=40100 if cityid>=40101&cityid<=40109;
replace cityid=40130 if cityid>=40130&cityid<=40137;
replace cityid=43100 if cityid>=43101&cityid<=43104;
egen seq=seq(), by(cityid);
drop if seq>=2&seq<.;
drop seq;

drop if cityid==.|cityid==0|cz==.|cz==0;
compress;
save $workpath\data\cityid_cz, replace;

clear;
import excel $datapath\◎平成22年度報告原票データ.xlsx, sheet("22年度（一般）") firstrow;
foreach X of varlist _all {;
destring `X', replace ignore("-" "〓", asbyte);
};
gen year=2010;
save $workpath\data\2010, replace;

clear;
import excel $datapath\◎平成23年度報告原票データ.xlsx, sheet("23年度（一般）") firstrow;
foreach X of varlist _all {;
destring `X', replace ignore("-");
};
gen year=2011;
save $workpath\data\2011, replace;

clear;
import excel $datapath\◎平成24年度報告原票データ.xlsx, sheet("24年度（一般）") firstrow;
foreach X of varlist _all {;
destring `X', replace ignore("-");
};
gen year=2012;
save $workpath\data\2012, replace;

clear;
import excel $datapath\◎平成25年度報告原票データ.xlsx, sheet("25年度（一般）") firstrow;
foreach X of varlist _all {;
destring `X', replace ignore("-");
};
foreach X of varlist 労働者派遣の料金日額 派遣労働者の賃金日額 日雇労働者の賃金日額
実施日 {;
destring `X', replace force;
};
gen year=2013;
save $workpath\data\2013, replace;

clear;
import excel $datapath\◎平成26年度報告原票データ.xlsx, sheet("26年度（一般）") firstrow;
foreach X of varlist _all {;
destring `X', replace ignore("-");
};
gen year=2014;
save $workpath\data\2014, replace;
clear;
import excel $datapath\◎平成27年度報告原票データ（旧法様式）.xlsx, sheet("27年度（一般）") firstrow;
foreach X of varlist _all {;
destring `X', replace ignore("-");
};
gen year=2015;
save $workpath\data\2015, replace;


clear;
use $workpath\data\2010;
append using $workpath\data\2011;
append using $workpath\data\2012;
append using $workpath\data\2013;
append using $workpath\data\2014;
append using $workpath\data\2015;
compress;

rename ＩＤコード id;
rename 郵便番号 zip;
rename 都道府県番号所 pref;
rename 常用雇用労働者 perm;
rename 常用雇用労働者以外の労働者 fixed;
rename 日雇派遣労働者 tempdaily_fte;
rename G tempperm_fte;
rename H tempfixed_fte;
rename 登録者の数 register;
rename 日雇派遣労働 tempdaily;
rename 常用雇用労働者数 tempperm;
rename 常用雇用労働者数以外の労働者数 tempfixed;
rename 派遣先の実数 cliant;
rename 労働者派遣の料金日額 fee;
rename 従事した業務日額 feedaily;
forvalues i=1/26 {;
rename 政令`i'号日額 fee`i';
};
rename 派遣労働者の賃金日額 wage;
rename 日雇労働者の賃金日額 wagedaily;
local i=1;
foreach X of varlist AR-BQ {;
gen wage`i'=`X' if (year==2010|year==2011);
local i=`i'+1;
};
replace wage1=AU if year==2012;
replace wage2=AV if year==2012;
local i=5;
foreach X of varlist AW-BE {;
replace wage`i' = `X' if year==2012;
local i=`i'+1;
};
gen wage16_1=BF if year==2012;
gen wage16_2=BG if year==2012;
local i=17;
foreach X of varlist BH-BK {;
replace wage`i' = `X' if year==2012;
local i=`i'+1;
};
replace wage23=BL if year>=2012&year<=2015;
replace wage25=BM if year>=2012&year<=2015;
replace wage3=BN if year>=2012&year<=2015;
replace wage4=BO if year>=2012&year<=2015;
replace wage14=BP if year>=2012&year<=2015;
replace wage15=BQ if year>=2012&year<=2015;
replace wage16=BR if year>=2012&year<=2015;
replace wage21=BS if year>=2012&year<=2015;
replace wage22=BT if year>=2012&year<=2015;
replace wage24=BU if year>=2012&year<=2015;
gen wage28=BW if year>=2012&year<=2015;
replace wage1=AV if year>=2013&year<=2015;
replace wage2=AW if year>=2013&year<=2015;
local i=5;
foreach X of varlist AX-BF {;
replace wage`i'=`X' if year>=2013&year<=2015;
local i=`i'+1;
};
replace wage16_1=BG if year>=2013&year<=2015;
local i=17;
foreach X of varlist BH-BK {;
replace wage`i'=`X' if year>=2013&year<=2015;
local i=`i'+1;
};
drop AR-BQ;
rename 金額 sales;
rename 実績の有無 oversea;
rename 労働者数 overseanum;
rename 紹介予定派遣実績の有無 shokai;
rename 紹介予定派遣契約申し込み人数 shokaiapp;
rename 紹介予定派遣派遣労働者数 shokaiactual;
rename 紹介予定派遣紹介労働者数 shokaioffer;
rename 紹介予定派遣直接雇用労働者数 shokaihire;
rename 日以下 length1d;
rename 日を超え７日以下 length2d_7d;
rename 日を超え１月以下 length8d_31d;
rename 月を超え２月以下 length2m;
rename 月を超え３月以下 length3m;
rename 月を超え６月以下 length4m_6m;
rename 月を超え１２月以下 length7m_12m;
rename 年を超え３年以下 length13m_36m;
rename その他 lengthmisc;
rename 合計 lengthtotal;
rename 教育訓練の種類 trainingtype;
rename 実施人員 trainingnumber;
rename ＯＪＴ trainingojt;
rename ＯＦＦＪＴ trainingoffjt;
rename 有給 trainingpaid;
rename 無給 trainingunpaid;
rename 事業主 trainingown;
rename 委託 trainingoutsource;
rename CR trainingmisc;
rename 実施日 trainingday;
rename 実施月 trainingmonth;
rename 実施時間 traininghour;
rename 費用負担有 trainingcopay_yes;
rename 費用負担無 trainingcopay_no;
rename ー備考欄 trainingnote;
rename 兼業 concurrent;
replace shokai=BU if year==2011;
drop BU;
replace shokaiapp=派遣に係る if year==2011;
drop 派遣に係る;
replace shokaiactual=派遣により if year==2011;
drop 派遣により;
replace shokaioffer=派遣において if year==2011;
drop 派遣において;
replace shokaihire=直接雇用に if year==2011;
drop 直接雇用に;
replace fee1=政令41号旧１号日額 if year>=2012&year<=2015;
drop 政令41号旧１号日額;
replace fee2=政令42号旧２号日額 if year>=2012&year<=2015;
drop 政令42号旧２号日額;
replace fee3=政令51号旧3号日額 if year>=2012&year<=2015;
drop 政令51号旧3号日額;
replace fee4=政令52号旧4号日額 if year>=2012&year<=2015;
drop 政令52号旧4号日額;
replace fee5=政令43号旧５号日額 if year>=2012&year<=2015;
drop 政令43号旧５号日額;
replace fee6=政令44号旧６号日額 if year>=2012&year<=2015;
drop 政令44号旧６号日額;
replace fee7=政令45号旧７号日額 if year>=2012&year<=2015;
drop 政令45号旧７号日額;
replace fee8=政令46号旧８号日額 if year>=2012&year<=2015;
drop 政令46号旧８号日額;
replace fee9=政令47号旧９号日額 if year>=2012&year<=2015;
drop 政令47号旧９号日額;
replace fee10=政令48号旧10号日額 if year>=2012&year<=2015;
drop 政令48号旧10号日額;
replace fee11=政令49号旧11号日額 if year>=2012&year<=2015;
drop 政令49号旧11号日額;
replace fee12=政令410号旧12号日額 if year>=2012&year<=2015;
drop 政令410号旧12号日額;
replace fee13=政令411号旧13号日額 if year>=2012&year<=2015;
drop 政令411号旧13号日額;
replace fee14=政令53号旧14号日額 if year>=2012&year<=2015;
drop 政令53号旧14号日額;
replace fee15=政令54号旧15号日額 if year>=2012&year<=2015;
drop 政令54号旧15号日額;
replace fee16=政令55号旧16号日額 if year>=2012&year<=2015;
drop 政令55号旧16号日額;
replace fee17=政令413号旧17号日額 if year>=2012&year<=2015;
drop 政令413号旧17号日額;
replace fee18=政令414号旧18号日額 if year>=2012&year<=2015;
drop 政令414号旧18号日額;
replace fee19=政令415号旧19号日額 if year>=2012&year<=2015;
drop 政令415号旧19号日額;
replace fee20=政令416号旧20号日額 if year>=2012&year<=2015;
drop 政令416号旧20号日額;
replace fee21=政令56号旧21号日額 if year>=2012&year<=2015;
drop 政令56号旧21号日額;
replace fee22=政令57号旧22号日額 if year>=2012&year<=2015;
drop 政令57号旧22号日額;
replace fee23=政令417号旧23号日額 if year>=2012&year<=2015;
drop 政令417号旧23号日額;
replace fee24=政令58号旧24号日額 if year>=2012&year<=2015;
drop 政令58号旧24号日額;
replace fee25=政令418号旧25号日額 if year>=2012&year<=2015;
drop 政令418号旧25号日額;
replace fee26=政令59号旧26号日額 if year>=2012&year<=2015;
drop 政令59号旧26号日額;
replace fee16=政令412号日額 if fee16==. & year==2012;
drop 政令412号日額;
replace fee16=政令旧16号日額 if year==2012&fee16==.;
drop 政令旧16号日額;
rename 政令510号日額 fee28;
replace wage16_2=BR if year>=2012&year<=2015;
replace wage21=BS if year>=2012&year<=2015;
replace wage22=BT if year>=2012&year<=2015;
replace wage26=BV if year>=2012&year<=2015;
replace wage28=BW if year>=2012&year<=2015;
replace shokai=CA if year==2012;
drop BR-CA;
replace shokaiapp=紹介予定派遣申込人数 if year>=2012&year<=2015;
drop 紹介予定派遣申込人数;
replace shokaiactual=紹介予定派遣実施労働者数 if year>=2012&year<=2015;
drop 紹介予定派遣実施労働者数;
replace shokaioffer=紹介予定派遣後紹介労働者数 if year>=2012&year<=2015;
drop 紹介予定派遣後紹介労働者数;
replace shokaihire=紹介予定派遣で直接雇用労働者数 if year>=2012&year<=2015;
drop 紹介予定派遣で直接雇用労働者数;
replace trainingmisc=CX;
drop CX;
drop 日雇派遣労働者と常用雇用労働者以外の労働者の合計 N;
replace fee16=政令412号旧16号日額 if fee16==.&year>=2013&year<=2015;
drop 政令412号旧16号日額;
drop 万円未満 万円5000万円未満 万円１億円未満 億円５億円未満 億円10億円未満 億円以上;
replace shokai=CG if year>=2013&year<=2015;
drop CG;
replace trainingmisc=DD if year>=2013&year<=2015;
drop DD;
replace trainingcopay_yes=費用負担あり if year>=2013&year<=2015;
drop 費用負担あり;
replace trainingcopay_no=費用負担なし if year>=2013&year<=2015;
drop 費用負担なし;

foreach X of varlist _all {;
tabstat `X', by(year) s(n me);
};

foreach X of varlist fixed tempdaily_fte tempperm_fte tempfixed_fte register
tempdaily tempperm tempfixed oversea overseanum 
shokai shokaiapp shokaiactual shokaioffer
shokaihire length1d length2d_7d length8d_31d length2m length3m length4m_6m
length7m_12m length13m_36m lengthmisc 
trainingtype trainingnumber trainingojt trainingoffjt trainingpaid
trainingunpaid trainingown trainingoutsource
trainingday trainingmonth traininghour trainingcopay_yes trainingcopay_no
{;
replace `X'=0 if `X'==.&year==2011;
};

foreach X of varlist oversea overseanum shokai length1d length2d_7d length8d_31d
length2m length3m length4m_6m length7m_12m length13m_36m lengthmisc concurrent
{;
replace `X'=0 if `X'==.&year==2012;
};

foreach X of varlist overseanum shokaiapp shokaiactual shokaioffer
length1d length2d_7d length8d_31d
length2m length3m length4m_6m
length7m_12m length13m_36m lengthmisc 
trainingtype trainingnumber trainingojt trainingoffjt trainingpaid
trainingunpaid trainingown trainingoutsource
trainingtype trainingnumber trainingojt trainingoffjt trainingpaid 
trainingunpaid trainingown trainingoutsource
trainingday trainingmonth traininghour trainingcopay_yes trainingcopay_no
{;
replace `X'=0 if `X'==.&year==2013;
};

foreach X of varlist cliant fee feedaily fee1-fee26 fee28 
wage wagedaily wage1-wage26 wage16_1 wage16_2 wage28
sales {;
replace `X'=. if `X'==0;
};

foreach X of varlist _all {;
tabstat `X', by(year) s(n me);
};

replace shokai=1 if shokai==2;

count;
drop if zip==.|zip==0;
gen str10 zip7=string(zip);
gen zip3=substr(zip7, 1, 3);
destring zip7, replace;
replace zip=zip7;
drop zip7;
destring zip3, replace;
merge m:1 zip3 using $workpath\data\zip_cityid;
rename _merge _merge_zip_cityid;

count;
drop if cityid==.|cityid==0;
* Recode Seireishitei Toshi;
replace cityid=1100 if cityid>=1101&cityid<=1110;
replace cityid=4100 if cityid>=4101&cityid<=4105;
replace cityid=11100 if cityid>=11101&cityid<=11110;
replace cityid=12100 if cityid>=12101&cityid<=12106;
replace cityid=13100 if cityid>=13101&cityid<=13123;
replace cityid=14100 if cityid>=14101&cityid<=14118;
replace cityid=14130 if cityid>=14131&cityid<=14137;
replace cityid=14150 if cityid>=14151&cityid<=14153;
replace cityid=15100 if cityid>=15101&cityid<=15108;
replace cityid=22100 if cityid>=22101&cityid<=22103;
replace cityid=22130 if cityid>=22131&cityid<=22137;
replace cityid=23100 if cityid>=23101&cityid<=23116;
replace cityid=26100 if cityid>=26101&cityid<=26111;
replace cityid=27100 if cityid>=27101&cityid<=27128;
replace cityid=27140 if cityid>=27141&cityid<=27147;
replace cityid=28100 if cityid>=28101&cityid<=28111;
replace cityid=33100 if cityid>=33101&cityid<=33104;
replace cityid=34100 if cityid>=34101&cityid<=34108;
replace cityid=40100 if cityid>=40101&cityid<=40109;
replace cityid=40130 if cityid>=40130&cityid<=40137;
replace cityid=43100 if cityid>=43101&cityid<=43104;
merge m:1 cityid using $workpath\data\cityid_cz;
rename _merge _merge_cityid_cz;

save $workpath\data\ippan10-15, replace;















