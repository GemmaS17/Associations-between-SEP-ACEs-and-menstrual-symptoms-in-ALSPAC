**G0 SEP to Symptoms

clear all
set maxvar 3000
use "/Volumes/157/working/data/ACEtoSymptoms_G0_data.dta", clear


********************************************************************************
**#STARTING SAMPLE**************************************************************
********************************************************************************
*Standard exclusions
drop if mz005l==1 
drop if in_core==2
drop if qlet=="B" 
drop if in_core==.a 
drop if in_core==. 

********************************************************************************
**#DEFINING EXPOSURES***********************************************************
********************************************************************************
*SEP EXPOSURES - include parental education, own education, own social class, and financial difficulties

***Parental
**Parental education (c686a mother; c706a father)
label define ed_lbl 3"CSE/Vocational" 2"O level" 1"A level" 0"Degree", replace
recode c686a (-1=.) (1/2=3) (3=2) (4=1) (5=0), gen (matedu)
label values matedu ed_lbl
recode c706a (-1=.) (1/2=3) (3=2) (4=1) (5=0), gen (fathedu)
label values fathedu ed_lbl
*Highest
gen parentedu=.
replace parentedu=0 if matedu==0 | fathedu==0
replace parentedu=1 if matedu==1 | fathedu==1
replace parentedu=2 if matedu==2 | fathedu==2
replace parentedu=3 if matedu==3 | fathedu==3
label values parentedu ed_lbl
label variable parentedu "Highest parental education"

**Parental social class (maternal grandmother and maternal grandfather)
label define socclass_lbl 0"Non-manual" 1"Manual", replace
recode c_sc_mgm (1/3=0) (4/6=1), gen (matsclass) 
recode c_sc_mgf (1/3=0) (4/6=1), gen (patsclass)
*Highest
gen parentsclass=.
replace parentsclass=0 if matsclass==0 | patsclass==0
replace parentsclass=1 if matsclass==1 | patsclass==1
label values parentsclass socclass_lbl
label variable parentsclass "Highest parental social class"

***Own
**Education 
recode c645a (-1=.) (1/2=3) (3=2) (4=1) (5=0), gen (edu)
label values edu ed_lbl
label variable edu "Own highest education"

**Social class 
gen sclass=.
replace sclass=0 if c755==1 | c755==2 | c755==3  
replace sclass=1 if c755==4 | c755==5 | c755==6
label values sclass socclass_lbl
label variable sclass "Own social class"

**Financial difficulties (very difficult to afford food, clothing, heating, accomodation, or things for the baby)
gen findiff=.
label define findiff_lbl 0"Not very difficult" 1"Very difficult", replace
foreach var in c520 c521 c522 c523 c524 {
	replace `var'=. if `var'<0	
	replace findiff=0 if inrange(`var',2,4) & findiff!=1
	replace findiff=1 if `var'==1
}
label values findiff findiff_lbl
label variable findiff "Financial difficulties"

egen exposures=rmiss2(parentedu parentsclass edu sclass findiff)
tab exposures

********************************************************************************
**#DEFINING OUTCOMES************************************************************
********************************************************************************

**Pain, heavy, days, and reg from h, j, k, or l (earliest response to any of these)
*Pain
label define pain_lbl 0"Not Painful" 1"Mildly Painful" 2"Moderately Painful" 3"Very Painful", replace
gen painh_cat=h111
gen painj_cat=j142
gen paink_cat=k1291
gen painl_cat=l3351
foreach var in h j k l {
	recode pain`var'_cat (4=0) (3=1) (2=2) (1=3) (else=.)
	label values pain`var'_cat pain_lbl
	recode pain`var'_cat (0/1=0 "Not Painful") (2/3=1 "Painful"), gen (pain`var')
}
*Heavy
label define heavy_lbl 0"Not Heavy" 1"Mildly Heavy" 2"Moderately Heavy" 3"Very Heavy", replace
gen heavyh_cat=h110
gen heavyj_cat=j141
gen heavyk_cat=k1290
gen heavyl_cat=l3350
foreach var in h j k l {
	recode heavy`var'_cat (4=0) (3=1) (2=2) (1=3) (else=.)
	label values heavy`var'_cat heavy_lbl
	recode heavy`var'_cat (0/1=0 "Not Heavy") (2/3=1 "Heavy"), gen (heavy`var')
}
*Days
label define days_lbl 0"Less than 4 days" 1"4-6 days" 2"7 or more days"
recode h113 (1/3=0) (4/6=1) (7/60=2) (else=.), gen (daysh_cat)
label values daysh_cat days_lbl
recode j144 (1/3=0) (4/6=1) (7/60=2) (else=.), gen (daysj_cat)
label values daysj_cat days_lbl
recode k1293 (1/3=0) (4/6=1) (7/60=2) (else=.), gen (daysk_cat)
label values daysk_cat days_lbl
recode l3353 (1/3=0) (4/6=1) (7/60=2) (else=.), gen (daysl_cat)
label values daysl_cat days_lbl

foreach var in h j k l {
	recode days`var'_cat (0/1=0 "6 days or less") (2=1 "7 or more days"), gen (days`var')
}
*Irregular
label define irreg_lbl 0"Regular Cycles" 1"Mildly Irregular" 2"Moderately Irregular" 3"Very Irregular", replace
recode h112 (4=0) (3=1) (2=2) (1=3) (else=.), gen (irregh_cat)
label values irregh irreg_lbl
recode j143 (4=0) (3=1) (2=2) (1=3) (else=.), gen (irregj_cat)
label values irregj irreg_lbl
recode k1292 (4=0) (3=1) (2=2) (1=3) (else=.), gen (irregk_cat)
label values irregk irreg_lbl
recode l3352 (4=0) (3=1) (2=2) (1=3) (else=.), gen (irregl_cat)
label values irregl irreg_lbl

foreach var in h j k l {
	recode irreg`var'_cat (0/1=0 "Regular") (2/3=1 "Irregular"), gen (irreg`var')
}

*Earliest timepoint (for all symptoms - only want one timepoint per person)
egen h_symps=rmiss2(painh heavyh daysh irregh)
egen j_symps=rmiss2(painj heavyj daysj irregj)
egen k_symps=rmiss2(paink heavyk daysk irregk)
egen l_symps=rmiss2(painl heavyl daysl irregl)

gen timepoint="None"
replace timepoint="h" if inrange(h_symps,0,3) & timepoint=="None"
replace timepoint="j" if h_symps==4 & inrange(j_symps,0,3) & timepoint=="None"
replace timepoint="k" if j_symps==4 & inrange(k_symps,0,3) & timepoint=="None"
replace timepoint="l" if k_symps==4 & inrange(l_symps,0,3) & timepoint=="None"

foreach symp in pain heavy days irreg {
	gen `symp'_cat=`symp'h_cat if timepoint=="h"
	gen `symp'=`symp'h if timepoint=="h"
	foreach time in j k l {
		replace `symp'_cat=`symp'`time'_cat if timepoint=="`time'"
		replace `symp'=`symp'`time' if timepoint=="`time'"
	}
}
label define four_lev 0"Not At All" 1"Mildly" 2"Moderately" 3"Very", replace
label define pain_lbl 0"Not Painful" 1"Painful", replace
label define heavy_lbl 0"Not Heavy" 1"Heavy", replace
label define irreg_lbl 0"Regular" 1"Irregular", replace
label define days_bin_lbl 0"6 days or less" 1"7 or more days", replace 

foreach var in pain heavy irreg {
	label values `var'_cat four_lev
	label values `var' `var'_lbl
	tab1 `var'_cat `var'
}
label values days_cat days_lbl
label values days days_bin_lbl
tab1 days_cat days

*Sample with all 4 of these
count if pain!=. & heavy!=. & days!=. & irreg!=. 

**Length from n
label define length_lbl 0"Normal (24-38)" 1"Frequent (<24)" 2"Infrequent (>38)", replace
recode n1122 (0/9=.) (24/38=0) (10/23=1) (39/140=2) (else=.), gen (length_cat)
label values length_cat length_lbl
recode length_cat (0=0 "Normal (24-38)") (1/2=1 "Short or Long"), gen (length)
*Sample with data = 5,387
tab1 length length_cat

**PMS from l 
replace l3031=. if l3031<0
*l/35y l3360 l3361 l3370 l3371 l3380 l3381 l3390 l3391 l3400 l3401 
gen fatigue_before_35y=.
replace fatigue_before_35y=1 if l3360==1
replace fatigue_before_35y=0 if l3031!=. & fatigue_before_35y==.
label values fatigue_before_35y PMS_lbl
gen fatigue_during_35y=.
replace fatigue_during_35y=1 if l3361==1
replace fatigue_during_35y=0 if l3031!=. & fatigue_during_35y==.
label values fatigue_during_35y PMS_lbl
gen irritable_before_35y=.
replace irritable_before_35y=1 if l3370==1
replace irritable_before_35y=0 if l3031!=. & irritable_before_35y==.
label values irritable_before_35y PMS_lbl
gen irritable_during_35y=.
replace irritable_during_35y=1 if l3371==1
replace irritable_during_35y=0 if l3031!=. & irritable_during_35y==.
label values irritable_during_35y PMS_lbl
gen depressed_before_35y=.
replace depressed_before_35y=1 if l3380==1
replace depressed_before_35y=0 if l3031!=. & depressed_before_35y==.
label values depressed_before_35y PMS_lbl
gen depressed_during_35y=.
replace depressed_during_35y=1 if l3381==1
replace depressed_during_35y=0 if l3031!=. & depressed_during_35y==.
label values depressed_during_35y PMS_lbl
gen anxious_before_35y=.
replace anxious_before_35y=1 if l3390==1
replace anxious_before_35y=0 if l3031!=. & anxious_before_35y==.
label values anxious_before_35y PMS_lbl
gen anxious_during_35y=.
replace anxious_during_35y=1 if l3391==1
replace anxious_during_35y=0 if l3031!=. & anxious_during_35y==.
label values anxious_during_35y PMS_lbl
gen other_before_35y=.
replace other_before_35y=1 if l3400==1 | l3400==0
replace other_before_35y=0 if l3031!=. & other_before_35y==.
label values other_before_35y PMS_lbl
gen other_during_35y=.
replace other_during_35y=1 if l3401==1 | l3401==0
replace other_during_35y=0 if l3031!=. & other_during_35y==.
label values other_during_35y PMS_lbl
*l/35y
gen fatigue_bin_35y=. 
replace fatigue_bin_35y=0 if fatigue_before_35y==0 & fatigue_during_35y==0
replace fatigue_bin_35y=1 if fatigue_before_35y==1 | fatigue_during_35y==1
label values fatigue_bin_35y PMS_lbl
gen irritable_bin_35y=. 
replace irritable_bin_35y=0 if irritable_before_35y==0 & irritable_during_35y==0
replace irritable_bin_35y=1 if irritable_before_35y==1 | irritable_during_35y==1
label values irritable_bin_35y PMS_lbl
gen depressed_bin_35y=. 
replace depressed_bin_35y=0 if depressed_before_35y==0 & depressed_during_35y==0
replace depressed_bin_35y=1 if depressed_before_35y==1 | depressed_during_35y==1
label values depressed_bin_35y PMS_lbl
gen anxious_bin_35y=. 
replace anxious_bin_35y=0 if anxious_before_35y==0 & anxious_during_35y==0
replace anxious_bin_35y=1 if anxious_before_35y==1 | anxious_during_35y==1
label values anxious_bin_35y PMS_lbl
gen other_bin_35y=. 
replace other_bin_35y=0 if other_before_35y==0 & other_during_35y==0
replace other_bin_35y=1 if other_before_35y==1 | other_during_35y==1
label values other_bin_35y PMS_lbl
*Number of symptoms
gen pms_symp = fatigue_bin_35y + irritable_bin_35y + depressed_bin_35y + anxious_bin_35y + other_bin_35y
*Binary 
gen pms=.
replace pms=0 if pms_symp==0
replace pms=1 if inrange(pms_symp,1,5)
label define pms_lbl 0"No symptoms" 1"Any PMS symptom", replace
label values pms pms_lbl

**SAMPLE WITH ALL 
egen outcomes=rmiss2(pain heavy days irreg length pms_symp)
tab outcomes

********************************************************************************
**#COVARIATES*******************************************************************
********************************************************************************

**Confounders
*Ethnicity
recode c800 (-1=.) (1=0 "White") (2/9=1 "Non-White"), gen (ethnicity)

*Age 
recode mz028b (-10 -4 -2=.), gen (mat_age)

*Age at menarche
recode d010 (-1 77=.), gen (menarche) 
replace menarche=n1120 if menarche==. & inrange(n1120,8,24) 
replace menarche=r2080 if menarche==. & inrange(r2080,8,24) 
*Categorical menarche: early (>1 SD younger than the mean), normal, and late (>1 SD older than the mean)
summ menarche, det
return list 
*Menarche is integer values only unfortuantely so normative will have to be 11-14 (10 or less early; and 15 or more late)
gen menarche_cat=.
label define menarche_lbl 0"Early <11" 1"Normative 11-14" 2"Late >14", replace
replace menarche_cat=0 if inrange(menarche,8,10)
replace menarche_cat=1 if inrange(menarche,11,14)
replace menarche_cat=2 if inrange(menarche,15,24)
label values menarche_cat menarche_lbl

**Sensitivity Variables

*Contraception (at that timepoint)
foreach var in h044 j049 k1051 l3051 n1130 n1133 {
	replace `var'=. if `var'<0
}
label define cont_lbl 1"Hormonal contraception" 0"None / non-hormonal contraception", replace
*Main timepoints
gen cont_h=.
replace cont_h=0 if h098a==1 | h098d==1 | h098e==1 | h098f==1 | h098g==1 | h098j==1 
replace cont_h=1 if h098b==1 | h098c==1 
label values cont_h cont_lbl
gen cont_j=.
replace cont_j=0 if j120==1 | j113==1 | j116==1 | j117==1 | j118==1 | j119==1
replace cont_j=1 if j114==1 | j115==1
label values cont_j cont_lbl
gen cont_k=.
replace cont_k=0 if k1211==1 | k1210==1 | k1212==1 | k1203==1 | k1206==1 | k1207==1 | k1208==1 | k1209==1
replace cont_k=1 if k1204==1 | k1205==1
label values cont_k cont_lbl
gen cont_l=. 
replace cont_l=0 if l3308==1 | l3307==1 | l3309==1 | l3300==1 | l3303==1 | l3304==1 | l3305==1 | l3306==1
replace cont_l=1 if l3301==1 | l3302==1
label values cont_l cont_lbl
gen main_cont=.
foreach time in h j k l {
		replace main_cont=cont_`time' if timepoint=="`time'"
	}
label values main_cont cont_lbl
*Exclude
foreach var in pain heavy days irreg {
	gen `var'_cont=`var' if main_cont==0 
	label values `var'_cont `var'_lbl
}

*PMS timepoint
gen pmstime_cont=cont_l
label values pmstime_cont cont_lbl
*Exclude
gen pms_cont=pms if pmstime_cont==0
label values pms_cont pms_lbl

*Length timepoint
recode n1133 (-10 -1=.) (1=1 "Hormonal contraception") (2=0 "None / non-hormonal contraception"), gen (lengthtime_cont)
label values lengthtime_cont cont_lbl
*Exclude
gen length_cont=length if lengthtime_cont==0
label define length_lbl 0"Normal (24-38)" 1"Short or Long", replace
label values length_cont length_lbl

********************************************************************************
**#DEFINING SAMPLE(S)***********************************************************
********************************************************************************

**Exposure data - 6925 with all 4 (54.30%)
egen exp_miss=rmiss2(parentedu parentsclass edu sclass findiff)
tab exp_miss 
*Individual drop out
count if parentedu==. 
count if parentedu!=. & parentsclass==. 
count if parentedu!=. & parentsclass!=. & edu==. 
count if parentedu!=. & parentsclass!=. & edu!=. & sclass==. 
count if parentedu!=. & parentsclass!=. & edu!=. & sclass!=. & findiff==. 
**Confounder data - 6434 with all exposure and confounder data
count if exp_miss==0 & ethnicity==. 
count if exp_miss==0 & ethnicity!=. & mat_age==.  
count if exp_miss==0 & ethnicity!=. & mat_age!=. & menarche==.
egen exp_conf_miss=rmiss2(parentedu parentsclass edu sclass findiff ethnicity mat_age menarche)
tab exp_conf_miss 
**Outcomes
*Pain, heavy, days, and reg from h to l - 5,487 with all four (plus exp and confounder)
egen multq_out=rmiss2(parentedu parentsclass edu sclass findiff ethnicity mat_age menarche pain heavy days irreg)
tab multq_out
*Length from n - 3073 with data
count if exp_conf_miss==0 & length!=.
*PMS from l - 4801 with data
count if exp_conf_miss==0 & pms!=.

********************************************************************************
**#DESCRIPTIVES*****************************************************************
********************************************************************************

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/Complete Case/Descriptives/Simple_CrossTabs_Missing", replace

mark touse 
markout touse parentedu parentsclass edu sclass findiff ethnicity mat_age menarche
mark multuse
markout multuse parentedu parentsclass edu sclass findiff ethnicity mat_age menarche pain heavy days irreg

***SIMPLE DESCRIPTIVES 
**Exposures
foreach exp in parentedu parentsclass edu sclass findiff {
	di "IN ALL OBSERVED DATA"
	tab `exp'
	di "IN MULTUSE SAMPLE"
	tab `exp' if multuse==1
	di "IN TOUSE SAMPLE"
	tab `exp' if touse==1
}

**Outcomes
foreach out in pain_cat pain heavy_cat heavy days_cat days irreg_cat irreg {
	di "IN ALL OBSERVED DATA"
	tab `out'
	di "IN MULTUSE SAMPLE"
	tab `out' if multuse==1
}
foreach out in length_cat length pms_symp pms {
	di "IN ALL OBSERVED DATA"
	tab `out'
	di "IN TOUSE SAMPLE"
	tab `out' if touse==1
}

**Confounders
foreach cov in ethnicity menarche_cat {
	di "IN ALL OBSERVED DATA"
	tab `cov'
	di "IN MULTUSE SAMPLE"
	tab `cov' if multuse==1
	di "IN TOUSE SAMPLE"
	tab `cov' if touse==1
}
di "IN ALL OBSERVED DATA"
summ menarche, det
di "IN MULTUSE SAMPLE"
summ menarche if multuse==1, det 
di "IN TOUSE SAMPLE"
summ menarche if touse==1, det
di "IN ALL OBSERVED DATA"
summ mat_age, det
di "IN MULTUSE SAMPLE"
summ mat_age if multuse==1, det 
di "IN TOUSE SAMPLE"
summ mat_age if touse==1, det

**CROSS TAB DESCRIPTIVES
**Exposures and covariates by outcomes
foreach out in pain_cat pain heavy_cat heavy days_cat days irreg_cat irreg {
	foreach exp in parentedu parentsclass edu sclass findiff ethnicity menarche_cat {
		di "IN ALL OBSERVED DATA"
		tab `exp' `out', row col
		di "IN MULTUSE SAMPLE"
		tab `exp' `out' if multuse==1, row col
	}
	di "IN ALL OBSERVED DATA"
	bysort `out': summ menarche, det
	di "IN MULTUSE SAMPLE"
	bysort `out': summ menarche if multuse==1, det
	di "IN ALL OBSERVED DATA"
	bysort `out': summ mat_age, det
	di "IN MULTUSE SAMPLE"
	bysort `out': summ mat_age if multuse==1, det
}

foreach out in length_cat length pms_symp pms {
	foreach exp in parentedu parentsclass edu sclass findiff ethnicity menarche_cat {
		di "IN ALL OBSERVED DATA"
		tab `exp' `out', row col
		di "IN TOUSE SAMPLE"
		tab `exp' `out' if touse==1, row col
	}
	di "IN ALL OBSERVED DATA"
	bysort `out': summ menarche, det
	di "IN TOUSE SAMPLE"
	bysort `out': summ menarche if touse==1, det
	di "IN ALL OBSERVED DATA"
	bysort `out': summ mat_age, det
	di "IN TOUSE SAMPLE"
	bysort `out': summ mat_age if touse==1, det
}

**Covariates by exposures

foreach exp in parentedu parentsclass edu sclass findiff {
	foreach cov in ethnicity menarche_cat {
		di "IN ALL OBSERVED DATA"
		tab `exp' `cov', row col
		di "IN MULTUSE SAMPLE"
		tab `exp' `cov' if multuse==1, row col
		di "IN TOUSE SAMPLE"
		tab `exp' `cov' if touse==1, row col
		di "IN LENGTH SAMPLE"
		tab `exp' `cov' if touse==1 & length!=., row col
		di "IN PMS SAMPLE"
		tab `exp' `cov' if touse==1 & pms!=., row col		
	}
	di "IN ALL OBSERVED DATA"
	bysort `exp': summ menarche, det
	di "IN MULTUSE SAMPLE"
	bysort `exp': summ menarche if multuse==1, det
	di "IN TOUSE SAMPLE"
	bysort `exp': summ menarche if touse==1, det
	di "IN LENGTH SAMPLE"
	bysort `exp': summ menarche if touse==1 & length!=., det
	di "IN PMS SAMPLE"
	bysort `exp': summ menarche if touse==1 & pms!=., det
	di "IN ALL OBSERVED DATA"
	bysort `exp': summ mat_age, det
	di "IN MULTUSE SAMPLE"
	bysort `exp': summ mat_age if multuse==1, det
	di "IN TOUSE SAMPLE"
	bysort `exp': summ mat_age if touse==1, det
	di "IN LENGTH SAMPLE"
	bysort `exp': summ mat_age if touse==1 & length!=., det
	di "IN PMS SAMPLE"
	bysort `exp': summ mat_age if touse==1 & pms!=., det
}



***MISSING DATA PATTERNS
*Exposures, outcomes, and covariates in all observed data (as above), those with covariates, sep_use restricted (as above) and in each CC analysis model
foreach var in parentedu parentsclass edu sclass findiff ethnicity menarche_cat pain_cat pain heavy_cat heavy days_cat days irreg_cat irreg length_cat length pms_symp pms {
	di "IN ALL OBSERVED DATA"
	tab `var'
	di "IN SAMPLE WITH EXPOSURE DATA"
	tab `var' if exp_miss==0
	di "IN TOUSE SAMPLE (EXPOSURE AND CONFOUNDER)"
	tab `var' if touse==1
	di "IN MULTUSE SAMPLE"
	tab `var' if multuse==1
	di "IN LENGTH SAMPLE"
	tab `var' if touse==1 & length!=.
	di "IN PMS SAMPLE"
	tab `var' if touse==1 & pms!=.
}
di "IN ALL OBSERVED DATA"
summ menarche
di "IN SAMPLE WITH EXPOSURE DATA"
summ menarche if exp_miss==0, det
di "IN TOUSE SAMPLE (EXPOSURE AND CONFOUNDER)"
summ menarche if touse==1, det
di "IN MULTUSE SAMPLE"
summ menarche if multuse==1, det
di "IN LENGTH SAMPLE"
summ menarche if touse==1 & length!=., det
di "IN PMS SAMPLE"
summ menarche if touse==1 & pms!=., det
di "IN ALL OBSERVED DATA"
summ mat_age
di "IN SAMPLE WITH EXPOSURE DATA"
summ mat_age if exp_miss==0, det
di "IN TOUSE SAMPLE (EXPOSURE AND CONFOUNDER)"
summ mat_age if touse==1, det
di "IN MULTUSE SAMPLE"
summ mat_age if multuse==1, det
di "IN LENGTH SAMPLE"
summ mat_age if touse==1 & length!=., det
di "IN PMS SAMPLE"
summ mat_age if touse==1 & pms!=., det

**Patterns and amount of missing
gen multquest=.
replace multquest=1 if pain!=. & heavy!=. & days!=. & irreg!=. 

mdesc parentedu parentsclass edu sclass findiff ethnicity mat_age menarche_cat pain heavy days irreg multquest length pms

*Number of missing variables
egen allvars=rmiss2(parentedu parentsclass edu sclass findiff ethnicity mat_age menarche_cat pain heavy days irreg length pms)
tab allvars
egen collapsevars=rmiss2(parentedu parentsclass edu sclass findiff ethnicity mat_age menarche_cat multquest length pms)
tab collapsevars 

log close

********************************************************************************
**#CC ANALYSIS******************************************************************
********************************************************************************

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/Complete Case/Analysis/Main_Results_log", replace

mark touse 
markout touse parentedu parentsclass edu sclass findiff ethnicity mat_age menarche
mark multuse
markout multuse parentedu parentsclass edu sclass findiff ethnicity mat_age menarche pain heavy days irreg

*Binary outcomes
foreach exp in parentedu parentsclass edu sclass findiff {
	foreach out in pain heavy days irreg {
		di "CRUDE `exp' and `out'"
		logistic `out' i.`exp' if multuse==1
		di "ETHNICITY ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity if multuse==1
		di "PLUS AGE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age if multuse==1
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age menarche if multuse==1
	}
	foreach out in length pms {
		di "CRUDE `exp' and `out'"
		logistic `out' i.`exp' if touse==1
		di "ETHNICITY ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity if touse==1
		di "PLUS AGE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age if touse==1
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age menarche if touse==1
	}
}

*Categorical outcomes - need to signify baseoutcome 
foreach exp in parentedu parentsclass edu sclass findiff {
	foreach out in pain_cat heavy_cat irreg_cat {
		di "CRUDE `exp' and `out'"
		mlogit `out' i.`exp' if multuse==1, rr baseoutcome(0)
		di "ETHNICITY ADJUSTED `exp' and `out'"
		mlogit `out' i.`exp' ethnicity if multuse==1, rr baseoutcome(0)
		di "PLUS AGE ADJUSTED `exp' and `out'"
		mlogit `out' i.`exp' ethnicity mat_age if multuse==1, rr baseoutcome(0)
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		mlogit `out' i.`exp' ethnicity mat_age menarche if multuse==1, rr baseoutcome(0)
	}
	foreach out in days_cat  {
		di "CRUDE `exp' and `out'"
		mlogit `out' i.`exp' if multuse==1, rr
		di "ETHNICITY ADJUSTED `exp' and `out'"
		mlogit `out' i.`exp' ethnicity if multuse==1, rr
		di "PLUS AGE ADJUSTED `exp' and `out'"
		mlogit `out' i.`exp' ethnicity mat_age if multuse==1, rr
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		mlogit `out' i.`exp' ethnicity mat_age menarche if multuse==1, rr
	}
	foreach out in length_cat pms_symp {
		di "CRUDE `exp' and `out'"
		mlogit `out' i.`exp' if touse==1, rr baseoutcome(0)
		di "ETHNICITY ADJUSTED `exp' and `out'"
		mlogit `out' i.`exp' ethnicity if touse==1, rr baseoutcome(0)
		di "PLUS AGE ADJUSTED `exp' and `out'"
		mlogit `out' i.`exp' ethnicity mat_age if touse==1, rr baseoutcome(0)
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		mlogit `out' i.`exp' ethnicity mat_age menarche if touse==1, rr baseoutcome(0)
	}
}

log close				                   

********************************************************************************
**#SENSITIVITY ANALYSIS*********************************************************
********************************************************************************

***DESCRIPTIVES
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/Complete Case/Descriptives/Sensitivty_Descriptives", replace

*NOTE: 'main' descriptives includes categorical outcomes which are actually part of the sensitivity analyses (symptom severity - like doctor variables for G1)

mark touse 
markout touse parentedu parentsclass edu sclass findiff ethnicity mat_age menarche
mark multuse
markout multuse parentedu parentsclass edu sclass findiff ethnicity mat_age menarche pain heavy days irreg


*1* PARENT-SPECIFIC EDUCATION (matedu and fathedu - derived above)
**Simple
*Full sample
tab1 matedu fathedu
*Touse sample
tab1 matedu fathedu if touse==1
*Multuse sample
tab1 matedu fathedu if multuse==1

**Cross tab (by outcomes)
foreach exp in matedu fathedu {
	foreach out in pain heavy days irreg {
		di "IN ALL OBSERVED DATA"
		tab `exp' `out', row col
		di "IN MULTUSE SAMPLE"
		tab `exp' `out' if multuse==1, row col
	}
	foreach out in length pms {
		di "IN ALL OBSERVED DATA"
		tab `exp' `out', row col
		di "IN TOUSE SAMPLE"
		tab `exp' `out' if touse==1, row col
	}
}

*2* PARENT-SPECIFIC SOCIAL CLASS (matsclass and patsclass - derived above)
**Simple
*Full sample
tab1 matsclass patsclass
*Touse sample
tab1 matsclass patsclass if touse==1
*Multuse sample
tab1 matsclass patsclass if multuse==1

**Cross tab (by outcomes)
foreach exp in matsclass patsclass {
	foreach out in pain heavy days irreg {
		di "IN ALL OBSERVED DATA"
		tab `exp' `out', row col
		di "IN MULTUSE SAMPLE"
		tab `exp' `out' if multuse==1, row col
	}
	foreach out in matsclass patsclass {
		di "IN ALL OBSERVED DATA"
		tab `exp' `out', row col
		di "IN TOUSE SAMPLE"
		tab `exp' `out' if touse==1, row col
	}
}

*3* CONTRACEPTION (pain_cont heavy_cont days_cont irreg_cont pms_cont length_cont - derived above)
**Simple
foreach out in pain_cont heavy_cont days_cont irreg_cont {
	di "IN ALL OBSERVED DATA"
	tab `out'
	di "IN MULTUSE SAMPLE"
	tab `out' if multuse==1
}
foreach out in pms_cont length_cont {
	di "IN ALL OBSERVED DATA"
	tab `out'
	di "IN TOUSE SAMPLE"
	tab `out' if touse==1
}

**Cross tabs (exposures and covariates by contraception outcomes)
foreach out in pain_cont heavy_cont days_cont irreg_cont {
	foreach exp in parentedu parentsclass edu sclass findiff ethnicity menarche_cat {
		di "IN ALL OBSERVED DATA"
		tab `exp' `out', row col
		di "IN MULTUSE SAMPLE"
		tab `exp' `out' if multuse==1, row col
	}
	di "IN ALL OBSERVED DATA"
	bysort `out': summ menarche, det
	di "IN MULTUSE SAMPLE"
	bysort `out': summ menarche if multuse==1, det
	di "IN ALL OBSERVED DATA"
	bysort `out': summ mat_age, det
	di "IN MULTUSE SAMPLE"
	bysort `out': summ mat_age if multuse==1, det
}

foreach out in pms_cont length_cont {
	foreach exp in parentedu edu sclass findiff ethnicity menarche_cat {
		di "IN ALL OBSERVED DATA"
		tab `exp' `out', row col
		di "IN TOUSE SAMPLE"
		tab `exp' `out' if touse==1, row col
	}
	di "IN ALL OBSERVED DATA"
	bysort `out': summ menarche, det
	di "IN TOUSE SAMPLE"
	bysort `out': summ menarche if touse==1, det
	di "IN ALL OBSERVED DATA"
	bysort `out': summ mat_age, det
	di "IN TOUSE SAMPLE"
	bysort `out': summ mat_age if touse==1, det
}


log close


***ANALYSIS

capture drop touse 
capture drop multuse

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/Complete Case/Analysis/Sensitivty_Results_log", replace


*NOTE: 'main' analysis includes categorical outcomes which are actually part of the sensitivity analyses (symptom severity - like doctor variables for G1)

mark touse 
markout touse parentedu parentsclass edu sclass findiff ethnicity mat_age menarche
mark multuse
markout multuse parentedu parentsclass edu sclass findiff ethnicity mat_age menarche pain heavy days irreg

*1* PARENT-SPECIFIC EDUCATION (matedu and fathedu - derived above)
foreach exp in matedu fathedu {
	foreach out in pain heavy days irreg {
		di "CRUDE `exp' and `out'"
		logistic `out' i.`exp' if multuse==1
		di "ETHNICITY ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity if multuse==1
		di "PLUS AGE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age if multuse==1
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age menarche if multuse==1
	}
	foreach out in length pms {
		di "CRUDE `exp' and `out'"
		logistic `out' i.`exp' if touse==1
		di "ETHNICITY ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity if touse==1
		di "PLUS AGE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age if touse==1
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age menarche if touse==1
	}
}

*2* PARENT-SPECIFIC EDUCATION (matsclass and patsclass - derived above)
foreach exp in matsclass patsclass {
	foreach out in pain heavy days irreg {
		di "CRUDE `exp' and `out'"
		logistic `out' i.`exp' if multuse==1
		di "ETHNICITY ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity if multuse==1
		di "PLUS AGE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age if multuse==1
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age menarche if multuse==1
	}
	foreach out in length pms {
		di "CRUDE `exp' and `out'"
		logistic `out' i.`exp' if touse==1
		di "ETHNICITY ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity if touse==1
		di "PLUS AGE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age if touse==1
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age menarche if touse==1
	}
}

*3* CONTRACEPTION (main_cont pms_cont length_cont - derived above)
foreach exp in parentedu edu sclass findiff {
	foreach out in pain_cont heavy_cont days_cont irreg_cont  {
		di "CRUDE `exp' and `out'"
		logistic `out' i.`exp' if multuse==1
		di "ETHNICITY ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity if multuse==1
		di "PLUS AGE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age if multuse==1
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age menarche if multuse==1
	}
	foreach out in pms_cont length_cont {
		di "CRUDE `exp' and `out'"
		logistic `out' i.`exp' if touse==1
		di "ETHNICITY ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity if touse==1
		di "PLUS AGE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age if touse==1
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age menarche if touse==1
	}
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/Complete Case/Analysis/Sensitivty_Results_log", append

mark touse 
markout touse parentedu parentsclass edu sclass findiff ethnicity mat_age menarche
mark multuse
markout multuse parentedu parentsclass edu sclass findiff ethnicity mat_age menarche pain heavy days irreg


*3* CONTRACEPTION - parentsclass accidentally missed in the first version
foreach exp in parentsclass findiff {
	foreach out in pain_cont heavy_cont days_cont irreg_cont  {
		di "CRUDE `exp' and `out'"
		logistic `out' i.`exp' if multuse==1
		di "ETHNICITY ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity if multuse==1
		di "PLUS AGE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age if multuse==1
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age menarche if multuse==1
	}
	foreach out in pms_cont length_cont {
		di "CRUDE `exp' and `out'"
		logistic `out' i.`exp' if touse==1
		di "ETHNICITY ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity if touse==1
		di "PLUS AGE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age if touse==1
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		logistic `out' i.`exp' ethnicity mat_age menarche if touse==1
	}
}

log close

**#Categorical or continuous education exposures?
mark touse 
markout touse parentedu parentsclass edu sclass findiff ethnicity mat_age menarche
mark multuse
markout multuse parentedu parentsclass edu sclass findiff ethnicity mat_age menarche pain heavy days irreg


log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/Education_ContorCat_CC", replace

*LR tests for main fully adjusted models 

foreach exp in parentedu edu {
	foreach out in pain heavy days irreg {
		quietly logistic `out' i.`exp' ethnicity mat_age menarche if multuse==1
		estimates store `out'_`exp'_cat
		quietly logistic `out' `exp' ethnicity mat_age menarche if multuse==1
		estimates store `out'_`exp'_cont
		lrtest `out'_`exp'_cont `out'_`exp'_cat
	}
	foreach out in length pms {
		quietly logistic `out' i.`exp' ethnicity mat_age menarche if touse==1
		estimates store `out'_`exp'_cat
		quietly logistic `out' `exp' ethnicity mat_age menarche if touse==1
		estimates store `out'_`exp'_cont
		lrtest `out'_`exp'_cont `out'_`exp'_cat
	}
}

*Small p value for PMS

estimates clear 

*LR tests for sensitivity models 

*Categorical outcomes 
foreach exp in parentedu edu {
	foreach out in pain_cat heavy_cat irreg_cat {
		quietly mlogit `out' i.`exp' ethnicity mat_age menarche if multuse==1, rr baseoutcome(0)
		estimates store `out'_`exp'_cat
		quietly mlogit `out' `exp' ethnicity mat_age menarche if multuse==1, rr baseoutcome(0)
		estimates store `out'_`exp'_cont
		lrtest `out'_`exp'_cont `out'_`exp'_cat
	}
	foreach out in days_cat  {
		quietly mlogit `out' i.`exp' ethnicity mat_age menarche if multuse==1, rr 
		estimates store `out'_`exp'_cat
		quietly mlogit `out' `exp' ethnicity mat_age menarche if multuse==1, rr 
		estimates store `out'_`exp'_cont
		lrtest `out'_`exp'_cont `out'_`exp'_cat
	}
	foreach out in length_cat pms_symp {
		quietly mlogit `out' i.`exp' ethnicity mat_age menarche if touse==1, rr baseoutcome(0)
		estimates store `out'_`exp'_cat
		quietly mlogit `out' `exp' ethnicity mat_age menarche if touse==1, rr baseoutcome(0)
		estimates store `out'_`exp'_cont
		lrtest `out'_`exp'_cont `out'_`exp'_cat
	}
}

estimates clear

*Parent specific education
foreach exp in matedu fathedu {
	foreach out in pain heavy days irreg {
		quietly logistic `out' i.`exp' ethnicity mat_age menarche if multuse==1
		estimates store `out'_`exp'_cat
		quietly logistic `out' `exp' ethnicity mat_age menarche if multuse==1
		estimates store `out'_`exp'_cont
		lrtest `out'_`exp'_cont `out'_`exp'_cat
	}
	foreach out in length pms {
		quietly logistic `out' i.`exp' ethnicity mat_age menarche if touse==1
		estimates store `out'_`exp'_cat
		quietly logistic `out' `exp' ethnicity mat_age menarche if touse==1
		estimates store `out'_`exp'_cont
		lrtest `out'_`exp'_cont `out'_`exp'_cat
	}
}

estimates clear

*Contraception excluded
foreach exp in parentedu edu {
	foreach out in pain_cont heavy_cont days_cont irreg_cont {
		quietly logistic `out' i.`exp' ethnicity mat_age menarche if multuse==1
		estimates store `out'_`exp'_cat
		quietly logistic `out' `exp' ethnicity mat_age menarche if multuse==1
		estimates store `out'_`exp'_cont
		lrtest `out'_`exp'_cont `out'_`exp'_cat
	}
	foreach out in length_cont pms_cont {
		quietly logistic `out' i.`exp' ethnicity mat_age menarche if touse==1
		estimates store `out'_`exp'_cat
		quietly logistic `out' `exp' ethnicity mat_age menarche if touse==1
		estimates store `out'_`exp'_cont
		lrtest `out'_`exp'_cont `out'_`exp'_cat
	}
}

estimates clear
log close

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/Education_ContorCat_CC", append

**Updated analyses with continuous education (and categorical Ps for PMS at the end)

*Main
foreach exp in parentedu edu  {
	foreach out in pain heavy days irreg {
		logistic `out' `exp' if multuse==1
		logistic `out' `exp' ethnicity mat_age if multuse==1
		logistic `out' `exp' ethnicity mat_age menarche if multuse==1
	}
	foreach out in length pms {
		logistic `out' `exp' if touse==1
		logistic `out' `exp' ethnicity mat_age if touse==1
		logistic `out' `exp' ethnicity mat_age menarche if touse==1
	}
}

*SENSITIVITY
*Categorical
foreach exp in parentedu edu  {
	foreach out in pain_cat heavy_cat irreg_cat {
		mlogit `out' `exp' if multuse==1, rr baseoutcome(0)
		mlogit `out' `exp' ethnicity mat_age if multuse==1, rr baseoutcome(0)
		mlogit `out' `exp' ethnicity mat_age menarche if multuse==1, rr baseoutcome(0)
	}
	foreach out in days_cat {
		mlogit `out' `exp' if multuse==1, rr
		mlogit `out' `exp' ethnicity mat_age if multuse==1, rr 
		mlogit `out' `exp' ethnicity mat_age menarche if multuse==1, rr 
	}
	foreach out in length_cat pms_symp {
		mlogit `out' `exp' if touse==1, rr baseoutcome(0)
		mlogit `out' `exp' ethnicity mat_age if touse==1, rr baseoutcome(0)
		mlogit `out' `exp' ethnicity mat_age menarche if touse==1, rr baseoutcome(0)
	}
}

*Parent specific
foreach exp in matedu fathedu {
	foreach out in pain heavy days irreg {
		logistic `out' `exp' if multuse==1
		logistic `out' `exp' ethnicity mat_age if multuse==1
		logistic `out' `exp' ethnicity mat_age menarche if multuse==1
	}
	foreach out in length pms {
		logistic `out' `exp' if touse==1
		logistic `out' `exp' ethnicity mat_age if touse==1
		logistic `out' `exp' ethnicity mat_age menarche if touse==1
	}
}

*Contraception
foreach exp in parentedu edu  {
	foreach out in pain_cont heavy_cont days_cont irreg_cont {
		logistic `out' `exp' if multuse==1
		logistic `out' `exp' ethnicity mat_age if multuse==1
		logistic `out' `exp' ethnicity mat_age menarche if multuse==1
	}
	foreach out in length_cont pms_cont {
		logistic `out' `exp' if touse==1
		logistic `out' `exp' ethnicity mat_age if touse==1
		logistic `out' `exp' ethnicity mat_age menarche if touse==1
	}
}

*CATGEORICAL ONES WITH OVERALL P VALUE
*Days contraception excluded for own education 
logistic days_cont i.edu if multuse==1
testparm i.edu
logistic days_cont i.edu ethnicity mat_age if multuse==1
testparm i.edu
logistic days_cont i.edu ethnicity mat_age menarche if multuse==1
testparm i.edu
*PMS as categorical for own education (main and relevant sensitivity)
//Main
logistic pms i.edu if touse==1
testparm i.edu
logistic pms i.edu ethnicity mat_age if touse==1
testparm i.edu
logistic pms i.edu ethnicity mat_age menarche if touse==1
testparm i.edu
//Categorical outcome
quietly mlogit pms_symp i.edu if touse==1, rr baseoutcome(0)
test [1]: 1.edu 2.edu 3.edu
test [2]: 1.edu 2.edu 3.edu
test [3]: 1.edu 2.edu 3.edu
test [4]: 1.edu 2.edu 3.edu
test [5]: 1.edu 2.edu 3.edu
quietly mlogit pms_symp i.edu ethnicity mat_age if touse==1, rr baseoutcome(0)
test [1]: 1.edu 2.edu 3.edu
test [2]: 1.edu 2.edu 3.edu
test [3]: 1.edu 2.edu 3.edu
test [4]: 1.edu 2.edu 3.edu
test [5]: 1.edu 2.edu 3.edu
quietly mlogit pms_symp i.edu ethnicity mat_age menarche if touse==1, rr baseoutcome(0)
test [1]: 1.edu 2.edu 3.edu
test [2]: 1.edu 2.edu 3.edu
test [3]: 1.edu 2.edu 3.edu
test [4]: 1.edu 2.edu 3.edu
test [5]: 1.edu 2.edu 3.edu
//Contraception
logistic pms_cont i.edu if touse==1
testparm i.edu
logistic pms_cont i.edu ethnicity mat_age if touse==1
testparm i.edu
logistic pms_cont i.edu ethnicity mat_age menarche if touse==1
testparm i.edu

log close 


**Categorical with overall p value for everything 


log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/Education_Categorical_CC", replace
**MAIN CC RESULTS
foreach exp in parentedu edu  {
	foreach out in pain heavy days irreg {
		quietly logistic `out' i.`exp' if multuse==1
		di "CRUDE"
		testparm i.`exp'
		quietly logistic `out' i.`exp' ethnicity mat_age if multuse==1
		di "ETH AND AGE ADJ"
		testparm i.`exp'
		quietly logistic `out' i.`exp' ethnicity mat_age menarche if multuse==1
		di "PLUS MENARCHE"
		testparm i.`exp'
	}
	foreach out in length pms {
		quietly logistic `out' i.`exp' if touse==1
		di "CRUDE"
		testparm i.`exp'
		quietly logistic `out' i.`exp' ethnicity mat_age if touse==1
		di "ETH AND AGE ADJ"
		testparm i.`exp'
		quietly logistic `out' i.`exp' ethnicity mat_age menarche if touse==1
		di "PLUS MENARCHE"
		testparm i.`exp'
	}
}

*SENSITIVITY
*Categorical
foreach exp in parentedu edu  {
	foreach out in pain_cat heavy_cat irreg_cat {
		quietly mlogit `out' i.`exp' if multuse==1, rr baseoutcome(0)
		di "CRUDE"
		test [Mildly]: 1.`exp' 2.`exp' 3.`exp'
		test [Moderately]: 1.`exp' 2.`exp' 3.`exp'
		test [Very]: 1.`exp' 2.`exp' 3.`exp'
		quietly mlogit `out' i.`exp' ethnicity mat_age if multuse==1, rr baseoutcome(0)
		di "ETH AND AGE ADJ"
		test [Mildly]: 1.`exp' 2.`exp' 3.`exp'
		test [Moderately]: 1.`exp' 2.`exp' 3.`exp'
		test [Very]: 1.`exp' 2.`exp' 3.`exp'
		quietly mlogit `out' i.`exp' ethnicity mat_age menarche if multuse==1, rr baseoutcome(0)
		di "PLUS MENARCHE"
		test [Mildly]: 1.`exp' 2.`exp' 3.`exp'
		test [Moderately]: 1.`exp' 2.`exp' 3.`exp'
		test [Very]: 1.`exp' 2.`exp' 3.`exp'
	}
	foreach out in days_cat {
		quietly mlogit `out' i.`exp' if multuse==1, rr
		di "CRUDE"
		test [Less_than_4_days]: 1.`exp' 2.`exp' 3.`exp'
		test [7_or_more_days]: 1.`exp' 2.`exp' 3.`exp'
		quietly mlogit `out' i.`exp' ethnicity mat_age if multuse==1, rr 
		di "ETH AND AGE ADJ"
		test [Less_than_4_days]: 1.`exp' 2.`exp' 3.`exp'
		test [7_or_more_days]: 1.`exp' 2.`exp' 3.`exp'
		quietly mlogit `out' i.`exp' ethnicity mat_age menarche if multuse==1, rr 
		di "PLUS MENARCHE"
		test [Less_than_4_days]: 1.`exp' 2.`exp' 3.`exp'
		test [7_or_more_days]: 1.`exp' 2.`exp' 3.`exp'
	}
	foreach out in length_cat {
		quietly mlogit `out' i.`exp' if touse==1, rr baseoutcome(0)
		di "CRUDE"
		test [Short_or_Long]: 1.`exp' 2.`exp' 3.`exp'
		test [2]: 1.`exp' 2.`exp' 3.`exp'
		quietly mlogit `out' i.`exp' ethnicity mat_age if touse==1, rr baseoutcome(0)
		di "ETH AND AGE ADJ"
		test [Short_or_Long]: 1.`exp' 2.`exp' 3.`exp'
		test [2]: 1.`exp' 2.`exp' 3.`exp'
		quietly mlogit `out' i.`exp' ethnicity mat_age menarche if touse==1, rr baseoutcome(0)
		di "PLUS MENARCHE"
		test [Short_or_Long]: 1.`exp' 2.`exp' 3.`exp'
		test [2]: 1.`exp' 2.`exp' 3.`exp'
	}
	foreach out in pms_symp {
		quietly mlogit `out' i.`exp' if touse==1, rr baseoutcome(0)
		di "CRUDE"
		test [1]: 1.`exp' 2.`exp' 3.`exp'
		test [2]: 1.`exp' 2.`exp' 3.`exp'
		test [3]: 1.`exp' 2.`exp' 3.`exp'
		test [4]: 1.`exp' 2.`exp' 3.`exp'
		test [5]: 1.`exp' 2.`exp' 3.`exp'
		quietly mlogit `out' i.`exp' ethnicity mat_age if touse==1, rr baseoutcome(0)
		di "ETH AND AGE ADJ"
		test [1]: 1.`exp' 2.`exp' 3.`exp'
		test [2]: 1.`exp' 2.`exp' 3.`exp'
		test [3]: 1.`exp' 2.`exp' 3.`exp'
		test [4]: 1.`exp' 2.`exp' 3.`exp'
		test [5]: 1.`exp' 2.`exp' 3.`exp'
		quietly mlogit `out' i.`exp' ethnicity mat_age menarche if touse==1, rr baseoutcome(0)
		di "PLUS MENARCHE"
		test [1]: 1.`exp' 2.`exp' 3.`exp'
		test [2]: 1.`exp' 2.`exp' 3.`exp'
		test [3]: 1.`exp' 2.`exp' 3.`exp'
		test [4]: 1.`exp' 2.`exp' 3.`exp'
		test [5]: 1.`exp' 2.`exp' 3.`exp'
	}
}

*Parent specific
foreach exp in matedu fathedu {
	foreach out in pain heavy days irreg {
		quietly logistic `out' i.`exp' if multuse==1
		di "CRUDE"
		testparm i.`exp'
		quietly logistic `out' i.`exp' ethnicity mat_age if multuse==1
		di "ETH AND AGE ADJ"
		testparm i.`exp'
		quietly logistic `out' i.`exp' ethnicity mat_age menarche if multuse==1
		di "PLUS MENARCHE"
		testparm i.`exp'
	}
	foreach out in length pms {
		quietly logistic `out' i.`exp' if touse==1
		di "CRUDE"
		testparm i.`exp'
		quietly logistic `out' i.`exp' ethnicity mat_age if touse==1
		di "ETH AND AGE ADJ"
		testparm i.`exp'
		quietly logistic `out' i.`exp' ethnicity mat_age menarche if touse==1
		di "PLUS MENARCHE"
		testparm i.`exp'
	}
}

*Contraception
foreach exp in parentedu edu  {
	foreach out in pain_cont heavy_cont days_cont irreg_cont {
		quietly logistic `out' i.`exp' if multuse==1
		di "CRUDE"
		testparm i.`exp'
		quietly logistic `out' i.`exp' ethnicity mat_age if multuse==1
		di "ETH AND AGE ADJ"
		testparm i.`exp'
		quietly logistic `out' i.`exp' ethnicity mat_age menarche if multuse==1
		di "PLUS MENARCHE"
		testparm i.`exp'
	}
	foreach out in length_cont pms_cont {
		quietly logistic `out' i.`exp' if touse==1
		di "CRUDE"
		testparm i.`exp'
		quietly logistic `out' i.`exp' ethnicity mat_age if touse==1
		di "ETH AND AGE ADJ"
		testparm i.`exp'
		quietly logistic `out' i.`exp' ethnicity mat_age menarche if touse==1
		di "PLUS MENARCHE"
		testparm i.`exp'
	}
}

log close

********************************************************************************
**#IPW PREP*********************************************************************
********************************************************************************

**Baseline predictor vars
*Maternal age at delivery mz028b - derived above (confounder)
tab mat_age
*A quest; marital status a525 (Y/N), phone a051(y/n or incoming only), car a053 (y/n), housing tenure a006 (own/mortgage, private rented, council/HA/other), number of rooms a045 (N), crowding index a551 (<=0.5, >0.5-0.75, >0.75-1, >1), double glazing a060 (none or full/partial)
recode a525 (-1=.) (1/4=1 "No") (5 6=0 "Yes"), gen (marital_stat)
recode a051 (-7 -1=.) (1=0 "Yes") (2/3=1 "No/incoming only"), gen (phone)
recode a053 (-7 -1=.) (1=0 "Yes") (2=1 "No"), gen (car)
recode a006 (-7 -1=.) (0 1=0 "Own/Mortgage") (3 4=1 "Private rented") (2 5 6=2 "Council/HA/Other"), gen (housing)
recode a045 (-7 -1=.), gen (rooms)
recode a551 (-7 -1=.) (1=1 "<= 0.5") (2=2 ">0.5 - 0.75") (3=3 ">0.75 - 1") (4=4 ">1") , gen (crowding)
recode a060 (-7 -1=.) (1 2=0 "All or some") (3=1 "None"), gen (dbl_glaze)
*B quest; first preg b023 (<20, 20-24, 25+), smoking in preg b665/b667 (y/n), smoking ever b650 (y/n), depression b371 (score), parity b032 (0, 1, 2+)
recode b023 (-1=.) (10/19=0 "<20") (20/24=1 "20-24") (25/44=2 "25+"), gen (first_preg)
gen smoke_preg=.
label define smoke_lbl 0"No" 1"Yes"
replace smoke_preg=0 if b665==1 | b667==1
replace smoke_preg=1 if inrange(b665,2,5) | inrange(b667,2,5)
label values smoke_preg smoke_lbl
recode b650 (-1=.) (1=1 "Yes") (2=0 "No"), gen (smoke_ever)
recode b371 (-7 -1=.), gen (epds)
recode b032 (-7 -2 -1=.) (0=0) (1=1) (2/22=2 "2+"), gen (parity)
*C quest; ethnicity c800 (non-white / white), education c645a (O level lower, A level, degree - maternal or both?), financial difficulties c525 (score), social class c755 c765 (manual v non-manual)
recode c525 (-7 -1=.), gen (findiff_ipw)
gen partsclass=.
replace partsclass=0 if c765==1 | c765==2 | c765==3 
replace partsclass=1 if c765==4 | c765==5 | c765==6
gen highsclass=.
replace highsclass=0 if (sclass==0 & partsclass==0) | (sclass==. & partsclass==0) | (sclass==0 & partsclass==.)
replace highsclass=1 if (sclass==1 & partsclass==1) | (sclass==. & partsclass==1) | (sclass==1 & partsclass==.) | (sclass==0 & partsclass==1) | (sclass==1 & partsclass==0)
label values highsclass socclass_lbl
//same as exposure/confounder one
gen edu_ipw=edu
label values edu_ipw ed_lbl
recode c800 (-1=.) (1=0 "White") (2/9=1 "Non-white"), gen (mat_ethnicity)

*Child-based; breastfeeding ka035/ka036, kb279/kb280, kc403/kc404 (4wks, 6months, 15months)
label define bfed_lbl 0"Never/<1 mnth" 1"1 to <3 mnths" 2"3 to <6 mnths" 3"6 mnths +", replace
gen bfed_dur=.
replace bfed_dur=0 if inrange(ka036,1,5)													//never, 1st day, 1st week, 2nd week, 3rd week
replace bfed_dur=1 if ka036==6																//4th week (max possible in this quest)
replace bfed_dur=0 if inrange(kb280,1,2) & bfed_dur!=1										//never or <1 month (and haven't already been classified as 1 month)
replace bfed_dur=1 if kb280==3																//1-<3
replace bfed_dur=2 if kb280==4																//3-<6								
replace bfed_dur=3 if kb280==5																//6+
replace bfed_dur=0 if kc403==-2	& bfed_dur==. | kc403==0 & bfed_dur==.						//never (-2) or 0 months
replace bfed_dur=1 if inrange(kc403,1,2) & bfed_dur<1 | inrange(kc403,1,2) & bfed_dur==. 	//1 or 2 months
replace bfed_dur=2 if inrange(kc403,3,5) & bfed_dur<2 | inrange(kc403,3,5) & bfed_dur==. 	//3 to 5 months
replace bfed_dur=3 if inrange(kc403,6,77) & bfed_dur<3 | inrange(kc403,6,77) & bfed_dur==.  //6+ or still (77)
label values bfed_dur bfed_lbl
*final vars (18): mat_age marital_stat phone car housing rooms crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mat_ethnicity edu_ipw findiff_ipw highsclass bfed_dur

**Additonal IPW vars from first Cornish paper - FAI
*Score of 1 if adversity is present per item 

*1 Early parenthood (mz028b, b023)
gen early_parent=0 if mz028b>0 & mz028b<. | b023>0 & b023<.
replace early_parent=1 if inrange(mz028b,15,19) | inrange(b023,12,16) 

*2 Housing inadequacy (a551 - crowding; people per room, b593 - homeless)
gen house_inadequacy=0 if a551>0 & a551<. | b593>0 & b593<.
replace house_inadequacy=1 if  a551==4 | inrange(b593,1,4)
*3 Basic living (a040 a041 a042 a046 a047 a048)
gen basic_living=.
foreach var in a040 a041 a042 a046 a047 a048 {
	replace basic_living=0 if inrange(`var',1,3)
}
replace basic_living=1 if a040==3 & a041==3 		//sit and eat kitchen AND cooking only kitchen no
replace basic_living=1 if a042==3 					//indoor WC
replace basic_living=1 if a046==3 					//running hot water
replace basic_living=1 if a047==3 & a048==3  		//bath and shower no
*4 Housing defects (a070 a071 a074 a082 a090)
gen house_defects=.
foreach var in a070 a071 a074 a082 a090 {
	replace house_defects=0 if inrange(`var',0,4)
}
replace house_defects=1 if a082==4 | a090==3 | inrange(a070,1,2) | inrange(a071,1,2) | inrange(a074,1,2)

*5 No educational attainment (c643 c662 c645/c666?)
*Need to merge other vars in c642 and c662 
gen no_qual=0 if c642==2 | c643==2 | c662==2 | c663==2
replace no_qual=1 if c642==1 | c643==1 | c662==1 | c663==1

*6 Financial difficulties c525
gen findiff_8=0 if inrange(c525,0,8)
replace findiff_8=1 if inrange(c525,9,15)

*7 Single a525
gen single=0 if inrange(a525,5,6)
replace single=1 if inrange(a525,1,4)
*8 Lack of partner affection (d370 d373)
gen partner_affect=0 if inrange(d370,6,17) | inrange(d373,8,15)
replace partner_affect=1 if inrange(d370,18,30) | inrange(d373,3,7)

*9 Partner cruelty (b592 b607)
gen partner_cruel=0 if inrange(b592,4,5) | inrange(b607,4,5)
replace partner_cruel=1 if inrange(b592,1,3) | inrange(b607,1,3)
*10 No support from partner (d794 d791 d796) - long format
gen nopartner_support=0 if inrange(d794,2,4) | inrange(d791,1,3) | inrange(d796,1,3)
replace nopartner_support=1 if d794==1 | d791==4 | d796==4

*11 Parity
gen fam_size=0 if inrange(b032,0,3)
replace fam_size=1 if inrange(b032,4,8)
*12 Major care-giving problems b006
gen child_incare=0 if inrange(b006,-1,1)
replace child_incare=1 if b006==2

*13 No emotional support from network (d790 d774) - long format
gen emot_support=0 if inrange(d790,3,4) | inrange(d774,2,4)
replace emot_support=1 if inrange(d790,1,2) | d774==1
*14 No practical support from network (d776 d797 d777) - long format
gen prac_support=0 if inrange(d776,2,4) | inrange(d797,1,3) | inrange(d777,2,4)
replace prac_support=1 if d776==1 | d797==4 | d777==1

*15 Psychopathology of mother (b352a b354a b371 b597 c574 c580 c601)
gen mat_psych=.
foreach var in b352a b354a b371 b597 c574 c580 c601 {
	replace mat_psych=0 if `var'>=0 & `var'<.
}
replace mat_psych=1 if inrange(b352a,11,16) | inrange(b354a,10,16) | inrange(b371,13,28) | inrange(b597,1,4) | inrange(c574,11,16) | inrange(c580,10,16) | inrange(c601,13,29)

*16 Substance abuse (d168 b714 b721 b723 b730 c360)
gen substance_abuse=.
foreach var in d168 b714 b721 b723 b730 c360 {
	replace substance_abuse=0 if `var'>=0 & `var'<.
}
replace substance_abuse=1 if d168==1 | b714==1 | inrange(b721,4,6) | inrange(b723,4,5) | inrange(b730,5,6) | inrange(c360,4,5)

*17 Crime trouble with police (b577 b586)
gen police=0 if b577==5 | b586==5
replace police=1 if inrange(b577,1,4) | inrange(b586,1,4)
*18 Convictions b598
gen convictions=0 if b598==5
replace convictions=1 if inrange(b598,1,4)


**Total family adversity index, long version - need 50% and assume no adversity for the missing ones
egen failong_miss=rmiss2(early_parent house_inadequacy basic_living house_defects no_qual findiff_8 single partner_affect partner_cruel nopartner_support fam_size child_incare emot_support prac_support mat_psych substance_abuse police convictions )
recode failong_miss (0/9=1) (10/18=0)		//1 if they have at least half
foreach var in early_parent house_inadequacy basic_living house_defects no_qual findiff_8 single partner_affect partner_cruel nopartner_support fam_size child_incare emot_support prac_support mat_psych substance_abuse police convictions {
	replace `var'=0 if `var'==. & failong_miss==1
}
gen fai_long=early_parent + house_inadequacy + basic_living + house_defects + no_qual + findiff_8 + single + partner_affect + partner_cruel + nopartner_support + fam_size + child_incare + emot_support + prac_support + mat_psych + substance_abuse + police + convictions 


log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/IPW info/IPW_Vars_Patterns", replace

**PRIOR TO MODEL SELECTION - LOOKING AT ALL POSISBLE IPW VARIABLES BEING CONSIDERED (19 in total)
*Simple Descriptives
foreach var in marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever parity mat_ethnicity edu_ipw highsclass bfed_dur {
	tab `var'
}

foreach var in mat_age rooms epds findiff_ipw fai_long {
	summ `var', det
}

*Missing data percentages
mdesc mat_age marital_stat phone car housing rooms crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mat_ethnicity edu_ipw findiff_ipw highsclass bfed_dur fai_long

*Number of missing variables
egen ipw_vars=rmiss2(mat_age marital_stat phone car housing rooms crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mat_ethnicity edu_ipw findiff_ipw highsclass bfed_dur fai_long)
tab ipw_vars

log close
drop ipw_vars

**MODEL SELECTION

**Create has_outcome variables for each analytical sample
label define has_outcome 0"Data missing" 1"Data available", replace
*Multiple outcomes - pain, heavy, days, and irreg (9,459 available; 4,692 missing)
mark has_mult
markout has_mult menarche pain heavy days irreg
label values has_mult has_outcome
*Length (4,947 available; 9,204 missing)
mark has_length
markout has_length menarche length
label values has_length has_outcome
*PMS (7,960 available; 6,191 missing)
mark has_pms
markout has_pms menarche pms
label values has_pms has_outcome

**Lasso model per analytical sample
*Mult
lasso logit has_mult mat_age marital_stat phone car i.housing rooms i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity mat_ethnicity i.edu_ipw findiff_ipw highsclass i.bfed_dur fai_long
lassocoef 
//model fit
logistic has_mult mat_age phone car i.housing i.crowding i.first_preg smoke_preg epds mat_ethnicity i.edu_ipw highsclass i.bfed_dur fai_long
estat gof				//x(9043)^2 = 9085.95; p = 0.3730
estat gof, group(10)	//x(8)^2 = 6.46; p = 0.5956

*Length
lasso logit has_length mat_age marital_stat phone car i.housing rooms i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity mat_ethnicity i.edu_ipw findiff_ipw highsclass i.bfed_dur fai_long
lassocoef 
//model fit
logistic has_length mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg epds i.parity mat_ethnicity i.edu_ipw findiff_ipw highsclass i.bfed_dur fai_long
estat gof				//x(9012)^2 = 9025.80; p = 0.4571
estat gof, group(10)	//x(8)^2 = 5.67; p = 0.6843

*PMS
lasso logit has_pms mat_age marital_stat phone car i.housing rooms i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity mat_ethnicity i.edu_ipw findiff_ipw highsclass i.bfed_dur fai_long
lassocoef 
//model fit
logistic has_pms mat_age marital_stat phone car i.housing rooms i.crowding i.first_preg smoke_preg epds i.parity mat_ethnicity i.edu_ipw findiff_ipw i.bfed_dur 
estat gof				//x(9745)^2 = 9804.35; p = 0.3339
estat gof, group(10)	//x(8)^2 = 3.07; p = 0.9300

**Fully inclusive model (everything except smoke_ever AND ROOMS)
*Restrict observations to compare models
mark ipw 
markout ipw mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity mat_ethnicity edu_ipw findiff_ipw highsclass bfed_dur fai_long
*Mult 
quietly logistic has_mult mat_age phone car i.housing i.crowding i.first_preg smoke_preg epds mat_ethnicity i.edu_ipw highsclass i.bfed_dur fai_long if ipw==1
estimates store mult_lasso
quietly logistic has_mult mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg epds i.parity mat_ethnicity i.edu_ipw findiff_ipw highsclass i.bfed_dur fai_long if ipw==1
estimates store mult_full
estat gof
estat gof, group(10)
lrtest mult_lasso mult_full //.9458

*Length 
quietly logistic has_length mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg epds i.parity mat_ethnicity i.edu_ipw findiff_ipw highsclass i.bfed_dur fai_long if ipw==1
estimates store length_lasso
quietly logistic has_length mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg epds i.parity mat_ethnicity i.edu_ipw findiff_ipw highsclass i.bfed_dur fai_long if ipw==1
estimates store length_full
estat gof
estat gof, group(10)
lrtest length_lasso length_full // ERROR: models are identical

*PMS (remove rooms)
quietly logistic has_pms mat_age marital_stat phone car i.housing i.crowding i.first_preg smoke_preg epds i.parity mat_ethnicity i.edu_ipw findiff_ipw i.bfed_dur if ipw==1
estimates store pms_lasso
quietly logistic has_pms mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg epds i.parity mat_ethnicity i.edu_ipw findiff_ipw highsclass i.bfed_dur fai_long if ipw==1
estimates store pms_full
estat gof
estat gof, group(10)
lrtest pms_lasso pms_full //0.8516

estimates clear



********************************************************************************
**#IMPUTATION PREP**************************************************************
********************************************************************************

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/IPW info/IPW_Vars_Patterns", append

**AFTER MODEL SELECTION - FULLY INCLUSIVE MODELS USING 18 OF THE ORIGINAL 19 IPW VARIABLES (smoke_ever removed)
*To impute - at least one SEP exposure and half of IPW variables
egen sep=rmiss2(parentedu parentsclass edu sclass findiff)
tab sep
drop if sep==5 
*with age data
drop if mat_age==. 
*Remove those with less han half IPW variables (all except rooms and smoke_ever = 17 variables - need 9 available so max 8 mising)
egen ipw_vars=rmiss2(mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity mat_ethnicity edu_ipw findiff_ipw highsclass bfed_dur fai_long)
drop if inrange(ipw_vars,9,17) //further 159 missing more than half IPW (12,074)

*Menarche
summ menarche if sep!=4 & inrange(ipw_vars,0,9) 
drop ipw_vars 

**MISSING EXPOSURE AND CONFOUNDER DATA
mdesc parentedu parentsclass edu sclass findiff ethnicity mat_age menarche
egen model_miss=rmiss2(parentedu parentsclass edu sclass findiff ethnicity mat_age)
tab model_miss

**MISSING IPW DATA
mdesc mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity ethnicity edu findiff_ipw highsclass bfed_dur fai_long
egen ipw_vars=rmiss2(mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity ethnicity edu findiff_ipw highsclass bfed_dur fai_long)
tab ipw_vars

**ALL TO BE IMPUTED
egen all=rmiss2(parentedu parentsclass edu sclass findiff ethnicity marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity findiff_ipw highsclass bfed_dur fai_long)
tab all

**OUTCOME DATA
tab has_mult
tab has_length
tab has_pms

log close

********************************************************************************
**#IMPUTATION*******************************************************************
********************************************************************************

*Focus dataset to relevant variables 
keep parentedu parentsclass edu sclass findiff ///
	 pain heavy days irreg length pms ///
	 ethnicity mat_age menarche ///
	 has_mult has_length has_pms  ///
	 marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity findiff_ipw highsclass bfed_dur fai_long ///
	 aln 

**Missing data patterns
*Complete 
mdesc has_mult has_length has_pms mat_age
*Substantive variables (missing)
mdesc parentedu parentsclass edu sclass findiff pain heavy days irreg length pms ethnicity menarche
*IPW variables (missing)
mdesc marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity findiff_ipw highsclass bfed_dur fai_long

**Set up imputation
mi set flong
mi register imputed parentedu parentsclass edu sclass findiff pain heavy days irreg length pms ethnicity menarche marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity findiff_ipw highsclass bfed_dur fai_long
mi register regular has_mult has_length has_pms aln mat_age

**Dryrun
mi impute chained (ologit) parentedu edu housing first_preg parity bfed_dur crowding ///
				  (logit, omit(i.highsclass)) sclass ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (logit) parentsclass ethnicity marital_stat phone car dbl_glaze smoke_preg pain heavy days irreg length pms ///
				  (logit, omit(i.sclass)) highsclass ///
				  (pmm, knn(5)) epds fai_long menarche ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_mult has_length has_pms mat_age,  dryrun
				  
				  
**Trace plot version
mi impute chained (ologit) parentedu edu housing first_preg parity bfed_dur crowding ///
				  (logit, omit(i.highsclass)) sclass ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (logit) parentsclass ethnicity marital_stat phone car dbl_glaze smoke_preg pain heavy days irreg length pms ///
				  (logit, omit(i.sclass)) highsclass ///
				  (pmm, knn(5)) epds fai_long menarche ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_mult has_length has_pms mat_age,  burnin(100) rseed(1371931) dots chainonly noisily showcommand ///
				  savetrace ("/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/MI_TraceData.dta", replace)			  
				  
				  
/*Trace checks
use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/MI_TraceData.dta", clear
			 
describe 
tsset iter

cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/Trace"

foreach cov in parentedu parentsclass edu sclass findiff pain heavy days irreg length pms ethnicity menarche marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity findiff_ipw highsclass bfed_dur fai_long {
	tsline `cov'_mean, title(Mean imputed values of `cov') legend(off) 
	graph export `cov'_mean.png, replace
	tsline `cov'_sd, title(Standard deviation imputed values of `cov') legend(off)
	graph export `cov'_sd.png, replace
}				  
			  
*Everything else looks relatively stable - especially by 50 burnins so go with this.				  
*/			  
				  

**Run imputation model				  
mi impute chained (ologit) parentedu edu housing first_preg parity bfed_dur crowding ///
				  (logit, omit(i.highsclass)) sclass ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (logit) parentsclass ethnicity marital_stat phone car dbl_glaze smoke_preg pain heavy days irreg length pms ///
				  (logit, omit(i.sclass)) highsclass ///
				  (pmm, knn(5)) epds fai_long menarche ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_mult has_length has_pms mat_age,  add(75) burnin(50) rseed(1371931) dots			  

*Save
save "/Volumes/157/working/data/G0_SEP_Imputed_dataset.dta", replace				  
				  
********************************************************************************
**#POST-IMPUTATION CHECKS*******************************************************
********************************************************************************		  

*Open
use "/Volumes/157/working/data/G0_SEP_Imputed_dataset.dta", clear		  

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/Post_Imp_Checks", replace
		  
*Checks
label define imputed 0"Observed" 1"Imputed", replace
gen imputed=0 if _mi_m==0
replace imputed=1 if _mi_m>0
label values imputed imputed 

*Proportions
foreach var in parentedu parentsclass edu sclass findiff ethnicity marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg parity highsclass bfed_dur {
	tab `var' imputed, row col
}
*Means
foreach var in epds findiff_ipw fai_long {
	summ `var' if imputed==0, det
	summ `var' if imputed==1, det
}

log close

*Distributions 
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted"

foreach var in epds findiff_ipw fai_long {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck.png, replace 
}

**FMI and Mcerror checks 

foreach var in pain heavy days irreg {
	replace `var'=. if has_mult==0
}
replace length=. if has_length==0
replace pms=. if has_pms==0
replace menarche=. if has_mult==0 & has_length==0 & has_pms==0

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/Post_Imp_Checks", append
**FMI and Mcerror checks 
**loop through main adjusted models (outcomes and menarche already deleted if observed missing)
*1-mcerror of coefficient should be 10% or less of effect SE
*2-mcerror of tstat should be 0.1 or less
*3-mcerror of p should be 0.01 or less if p is 0.05 or 0.02 if p is 0.1
foreach exp in parentedu parentsclass edu sclass findiff {
	foreach out in pain heavy days irreg length pms {
		mi estimate, mcerror: logistic `out' i.`exp' ethnicity mat_age menarche
	}
}

log close
*Across all models, largest FMI: 0.2181 (all good)
*Mcerror concerns - sclass pain t is .1 now so all good 

				  
********************************************************************************
**#DERIVING WEIGHTS*************************************************************
********************************************************************************					  
*Open
use "/Volumes/157/working/data/G0_SEP_Imputed_dataset.dta", clear	
label define imputed 0"Observed" 1"Imputed", replace
gen imputed=0 if _mi_m==0
replace imputed=1 if _mi_m>0
label values imputed imputed 				  
*Deletion approach for outcome variables and menarche 
foreach var in pain heavy days irreg {
	replace `var'=. if has_mult==0
}
replace length=. if has_length==0
replace pms=. if has_pms==0
replace menarche=. if has_mult==0 & has_length==0 & has_pms==0
				  
**IPW for each imputed dataset per analytical sample
*Mult
forvalues j = 1/75 {
	logistic has_mult mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg epds i.parity ethnicity i.edu findiff_ipw highsclass i.bfed_dur fai_long if _mi_m==`j'
	predict multp`j' if _mi_m==`j'
}				  
*Length
forvalues j = 1/75 {
	logistic has_length mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg epds i.parity ethnicity i.edu findiff_ipw highsclass i.bfed_dur fai_long if _mi_m==`j'
	predict lengthp`j' if _mi_m==`j'
}					  
*PMS
forvalues j = 1/75 {
	logistic has_pms mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg epds i.parity ethnicity i.edu findiff_ipw highsclass i.bfed_dur fai_long if _mi_m==`j'
	predict pmsp`j' if _mi_m==`j'
}	  

**Consistent name across datasets
foreach var in mult length pms {
	gen `var'p = `var'p1
	forvalues j = 2/75 {
		replace `var'p=`var'p`j' if `var'p==.
	}
}				  
				  
**Create probability and weights
foreach var in mult length pms {
	gen prob_`var'=`var'p if has_`var'==1
	replace prob_`var'=1-`var'p if has_`var'==0
	gen ipw_`var'=1/prob_`var'
}				  
*Quick check (first 15 obs of imputation 1)
list _mi_m has_mult multp prob_mult ipw_mult in 12075/12089
*Imputation 2
list _mi_m has_mult multp prob_mult ipw_mult in 24149/24163

**Tidy up (remove original p per dataset variables as have overall one with all information)
forvalues j = 1/75 {
	foreach var in mult length pms {
		drop `var'p`j'
	}
}				  
				  
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/IPW info/Summary_Weights", replace
*Summary of weights
foreach var in mult length pms {
	summ ipw_`var', det
}
log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/IPW info"

foreach var in mult length pms {
	histogram ipw_`var'
	graph export `var'_weights.png, replace 
}	
 
*Replace dataset now outcomes deleted and weights derived
save "/Volumes/157/working/data/G0_SEP_Imputed_dataset.dta", replace				  
				  
********************************************************************************
**#WEIGHTED DESCRIPTIVES********************************************************
********************************************************************************	
use "/Volumes/157/working/data/G0_SEP_Imputed_dataset.dta", clear				  
				  
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/Descriptives/Weighted_Descriptives", replace

**Simple - full sample
*Exposures
foreach var in parentedu parentsclass edu sclass findiff {
	mi estimate: proportion `var'
}

*Outcomes
foreach var in pain heavy days irreg length pms {
	tab `var' if imputed==0
}

*Confounders
mi estimate: proportion ethnicity

foreach var in mat_age menarche {
	summ `var' if imputed==0, det
}


**Simple - analysis samples
*Exposures
foreach var in parentedu parentsclass edu sclass findiff {
	foreach sample in mult length pms {
		mi estimate: proportion `var' if has_`sample'==1
	}
}

*Confounders
foreach sample in mult length pms {
	mi estimate: proportion ethnicity if has_`sample'==1
	foreach var in mat_age menarche {
		summ `var' if imputed==0 & has_`sample'==1, det
	}
}

**Cross tabs - analysis samples (exp and cov by out)
foreach out in pain heavy days irreg length pms  {
	foreach var in parentedu parentsclass edu sclass findiff ethnicity {
		mi estimate: proportion `var', over(`out')	
	}
	foreach var in mat_age menarche {
		bysort `out': summ `var' if imputed==0, det
	}
}


**Cross tabs - analysis samples (IPW by out; mat_age, ethnicity, and edu above)
foreach out in pain heavy days irreg length pms  {
	foreach var in marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg parity highsclass bfed_dur  {
		mi estimate: proportion `var', over(`out')	
	}
	foreach var in epds findiff_ipw fai_long {
		mi estimate: mean `var', over(`out')
	}
}

log close

********************************************************************************
**#WEIGHTED ANALYSIS************************************************************
********************************************************************************
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/Analysis/Weighted_Analysis_log", replace

*Mult outcomes
foreach exp in parentedu parentsclass edu sclass findiff {
	foreach out in pain heavy days irreg {
		eststo `exp'_`out'c: mi estimate, or post: logistic `out' i.`exp' [pw=ipw_mult]
		eststo `exp'_`out'eth: mi estimate, or post: logistic `out' i.`exp' ethnicity [pw=ipw_mult]
		eststo `exp'_`out'age: mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age [pw=ipw_mult]
		eststo `exp'_`out'men: mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age menarche [pw=ipw_mult]
	}
}	
	
*Length and pms
foreach exp in parentedu parentsclass edu sclass findiff {
	foreach out in length pms {
		eststo `exp'_`out'c: mi estimate, or post: logistic `out' i.`exp' [pw=ipw_`out']
		eststo `exp'_`out'eth: mi estimate, or post: logistic `out' i.`exp' ethnicity [pw=ipw_`out']
		eststo `exp'_`out'age: mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age [pw=ipw_`out']
		eststo `exp'_`out'men: mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age menarche [pw=ipw_`out']
	}
}	
	
log close

**Output to excel file
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/Analysis"
capture erase "main_results.xls"
*Parent education
estout parentedu_painc parentedu_paineth parentedu_painage parentedu_painmen ///
	parentedu_heavyc parentedu_heavyeth parentedu_heavyage parentedu_heavymen ///
	parentedu_daysc parentedu_dayseth parentedu_daysage parentedu_daysmen ///
	parentedu_irregc parentedu_irregeth parentedu_irregage parentedu_irregmen ///
	parentedu_lengthc parentedu_lengtheth parentedu_lengthage parentedu_lengthmen ///
	parentedu_pmsc parentedu_pmseth parentedu_pmsage parentedu_pmsmen ///
	using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity mat_age menarche) eform title(parent education) 
*Parent social class
estout parentsclass_painc parentsclass_paineth parentsclass_painage parentsclass_painmen ///
	parentsclass_heavyc parentsclass_heavyeth parentsclass_heavyage parentsclass_heavymen ///
	parentsclass_daysc parentsclass_dayseth parentsclass_daysage parentsclass_daysmen ///
	parentsclass_irregc parentsclass_irregeth parentsclass_irregage parentsclass_irregmen ///
	parentsclass_lengthc parentsclass_lengtheth parentsclass_lengthage parentsclass_lengthmen ///
	parentsclass_pmsc parentsclass_pmseth parentsclass_pmsage parentsclass_pmsmen ///
	using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity mat_age menarche) eform title(parent social class) 
*Own education
estout edu_painc edu_paineth edu_painage edu_painmen ///
	edu_heavyc edu_heavyeth edu_heavyage edu_heavymen ///
	edu_daysc edu_dayseth edu_daysage edu_daysmen ///
	edu_irregc edu_irregeth edu_irregage edu_irregmen ///
	edu_lengthc edu_lengtheth edu_lengthage edu_lengthmen ///
	edu_pmsc edu_pmseth edu_pmsage edu_pmsmen ///
	using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity mat_age menarche) eform title(own education) 
*Own social class
estout sclass_painc sclass_paineth sclass_painage sclass_painmen ///
	sclass_heavyc sclass_heavyeth sclass_heavyage sclass_heavymen ///
	sclass_daysc sclass_dayseth sclass_daysage sclass_daysmen ///
	sclass_irregc sclass_irregeth sclass_irregage sclass_irregmen ///
	sclass_lengthc sclass_lengtheth sclass_lengthage sclass_lengthmen ///
	sclass_pmsc sclass_pmseth sclass_pmsage sclass_pmsmen ///
	using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity mat_age menarche) eform title(own social class) 
*Financial difficulties
estout findiff_painc findiff_paineth findiff_painage findiff_painmen ///
	findiff_heavyc findiff_heavyeth findiff_heavyage findiff_heavymen ///
	findiff_daysc findiff_dayseth findiff_daysage findiff_daysmen ///
	findiff_irregc findiff_irregeth findiff_irregage findiff_irregmen ///
	findiff_lengthc findiff_lengtheth findiff_lengthage findiff_lengthmen ///
	findiff_pmsc findiff_pmseth findiff_pmsage findiff_pmsmen ///
	using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity mat_age menarche) eform title(financial difficulties) 

			  
***TRUNCATED WEIGHTS*************************************************************

**Generate trunacted weights
*Mult
summ ipw_mult, det
local mult95=r(p95)			
local mult99=r(p99)	
gen trunc95_mult=ipw_mult
replace trunc95_mult=`mult95' if trunc95_mult>`mult95' & trunc95_mult<.  
gen trunc99_mult=ipw_mult
replace trunc99_mult=`mult99' if trunc99_mult>`mult99' & trunc99_mult<.
*Length
summ ipw_length, det
local length95=r(p95)			
local length99=r(p99)	
gen trunc95_length=ipw_length
replace trunc95_length=`length95' if trunc95_length>`length95' & trunc95_length<.  
gen trunc99_length=ipw_length
replace trunc99_length=`length99' if trunc99_length>`length99' & trunc99_length<.
*PMS
summ ipw_pms, det
local pms95=r(p95)			
local pms99=r(p99)	
gen trunc95_pms=ipw_pms
replace trunc95_pms=`pms95' if trunc95_pms>`pms95' & trunc95_pms<.  
gen trunc99_pms=ipw_pms
replace trunc99_pms=`pms99' if trunc99_pms>`pms99' & trunc99_pms<.


log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/Analysis/Truncated_Weights", replace				  
				  
**Mult sample
*Original vs truncated weights
summ ipw_mult, det
summ trunc95_mult, det
summ trunc99_mult, det
*Main models
foreach exp in parentedu parentsclass edu sclass findiff {
	foreach out in pain heavy days irreg {
		di "95th percentile weights"
		mi estimate, or post: logistic `out' i.`exp' [pw=trunc95_mult]
		mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age [pw=trunc95_mult]
		mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age menarche [pw=trunc95_mult]
		di "99th percentile weights"
		mi estimate, or post: logistic `out' i.`exp' [pw=trunc99_mult]
		mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age [pw=trunc99_mult]
		mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age menarche [pw=trunc99_mult]
	}
}					  
				  
*Length sample 
summ ipw_length, det
summ trunc95_length, det
summ trunc99_length, det

*PMS sample
summ ipw_pms, det
summ trunc95_pms, det
summ trunc99_pms, det

*Length and PMS models
foreach exp in parentedu parentsclass edu sclass findiff {
	foreach out in length pms {
		di "95th percentile weights"
		mi estimate, or post: logistic `out' i.`exp' [pw=trunc95_`out']
		mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age [pw=trunc95_`out']
		mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age menarche [pw=trunc95_`out']
		di "99th percentile weights"
		mi estimate, or post: logistic `out' i.`exp' [pw=trunc99_`out']
		mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age [pw=trunc99_`out']
		mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age menarche [pw=trunc99_`out']
	}
}		

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/Analysis/Truncated_Weights", append	
*USING CORRECT 99TH PERCENTILE WEIGHTS FOR LENGTH AND PMS (first run code was wrong as I accidentally repeated the 95th analysis)

foreach exp in parentedu parentsclass edu sclass findiff {
	foreach out in length pms {
		di "99th percentile weights"
		mi estimate, or post: logistic `out' i.`exp' [pw=trunc99_`out']
		mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age [pw=trunc99_`out']
		mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age menarche [pw=trunc99_`out']
	}
}				  
				  
log close				  
				  
				  
**#Continuous education exposures
use "/Volumes/157/working/data/G0_SEP_Imputed_dataset.dta", clear	 				  
				  
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/Education_ContorCat_MI", append

*THE MAIN RESULTS
foreach exp in parentedu edu  {
	foreach out in pain heavy days irreg {
		mi estimate, or post: logistic `out' `exp' [pw=ipw_mult]
		mi estimate, or post: logistic `out' `exp' ethnicity mat_age [pw=ipw_mult]
		mi estimate, or post: logistic `out' `exp' ethnicity mat_age menarche [pw=ipw_mult]
	}
	foreach out in length pms {
		mi estimate, or post: logistic `out' `exp' [pw=ipw_`out']
		mi estimate, or post: logistic `out' `exp' ethnicity mat_age [pw=ipw_`out']
		mi estimate, or post: logistic `out' `exp' ethnicity mat_age menarche [pw=ipw_`out']
	}
}	
			  
*Categorical for PMS and own education		  
mi estimate, or post: logistic pms i.edu [pw=ipw_pms] 
testparm i.edu
mi estimate, or post: logistic pms i.edu ethnicity mat_age [pw=ipw_pms]
testparm i.edu
mi estimate, or post: logistic pms i.edu ethnicity mat_age menarche [pw=ipw_pms] 
testparm i.edu


*TRUNCATED WEIGHTS
*Create weights
//Mult
summ ipw_mult, det
local mult95=r(p95)			
local mult99=r(p99)	
gen trunc95_mult=ipw_mult
replace trunc95_mult=`mult95' if trunc95_mult>`mult95' & trunc95_mult<.  
gen trunc99_mult=ipw_mult
replace trunc99_mult=`mult99' if trunc99_mult>`mult99' & trunc99_mult<.
//Length
summ ipw_length, det
local length95=r(p95)			
local length99=r(p99)	
gen trunc95_length=ipw_length
replace trunc95_length=`length95' if trunc95_length>`length95' & trunc95_length<.  
gen trunc99_length=ipw_length
replace trunc99_length=`length99' if trunc99_length>`length99' & trunc99_length<.
//PMS
summ ipw_pms, det
local pms95=r(p95)			
local pms99=r(p99)	
gen trunc95_pms=ipw_pms
replace trunc95_pms=`pms95' if trunc95_pms>`pms95' & trunc95_pms<.  
gen trunc99_pms=ipw_pms
replace trunc99_pms=`pms99' if trunc99_pms>`pms99' & trunc99_pms<.

*Continuous results
foreach exp in parentedu edu  {
	foreach out in pain heavy days irreg {
		di "95th percentile"
		mi estimate, or post: logistic `out' `exp' [pw=trunc95_mult]
		mi estimate, or post: logistic `out' `exp' ethnicity mat_age [pw=trunc95_mult]
		mi estimate, or post: logistic `out' `exp' ethnicity mat_age menarche [pw=trunc95_mult]
		di "99th percentile"
		mi estimate, or post: logistic `out' `exp' [pw=trunc99_mult]
		mi estimate, or post: logistic `out' `exp' ethnicity mat_age [pw=trunc99_mult]
		mi estimate, or post: logistic `out' `exp' ethnicity mat_age menarche [pw=trunc99_mult]
	}
	foreach out in length pms {
		di "95th percentile"
		mi estimate, or post: logistic `out' `exp' [pw=trunc95_`out']
		mi estimate, or post: logistic `out' `exp' ethnicity mat_age [pw=trunc95_`out']
		mi estimate, or post: logistic `out' `exp' ethnicity mat_age menarche [pw=trunc95_`out']
		di "99th percentile"
		mi estimate, or post: logistic `out' `exp' [pw=trunc99_`out']
		mi estimate, or post: logistic `out' `exp' ethnicity mat_age [pw=trunc99_`out']
		mi estimate, or post: logistic `out' `exp' ethnicity mat_age menarche [pw=trunc99_`out']
	}
}	
			  
*Categorical for PMS and own education		
//95th percentile  
mi estimate, or post: logistic pms i.edu [pw=trunc95_pms] 
testparm i.edu
mi estimate, or post: logistic pms i.edu ethnicity mat_age [pw=trunc95_pms]
testparm i.edu
mi estimate, or post: logistic pms i.edu ethnicity mat_age menarche [pw=trunc95_pms] 
testparm i.edu
//99th percentile  
mi estimate, or post: logistic pms i.edu [pw=trunc99_pms] 
testparm i.edu
mi estimate, or post: logistic pms i.edu ethnicity mat_age [pw=trunc99_pms]
testparm i.edu
mi estimate, or post: logistic pms i.edu ethnicity mat_age menarche [pw=trunc99_pms] 
testparm i.edu

log close


log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/Education_Cat_OverallP_MI", replace

**MAIN RESULTS
foreach exp in parentedu edu  {
	foreach out in pain heavy days irreg {
		quietly mi estimate, or post: logistic `out' i.`exp' [pw=ipw_mult]
		di "CRUDE"
		testparm i.`exp'
		quietly mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age [pw=ipw_mult]
		di "ETH and AGE ADJ"
		testparm i.`exp'
		quietly mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age menarche [pw=ipw_mult]
		di "PLUS MENARCHE ADJ"
		testparm i.`exp'
	}
	foreach out in length pms {
		quietly mi estimate, or post: logistic `out' i.`exp' [pw=ipw_`out']
		di "CRUDE"
		testparm i.`exp'
		quietly mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age [pw=ipw_`out']
		di "ETH and AGE ADJ"
		testparm i.`exp'
		quietly mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age menarche [pw=ipw_`out']
		di "PLUS MENARCHE ADJ"
		testparm i.`exp'
	}
}	
			  
**TRUNCATED RESULTS
foreach exp in parentedu edu  {
	foreach out in pain heavy days irreg {
		di "95th percentile"
		quietly mi estimate, or post: logistic `out' i.`exp' [pw=trunc95_mult]
		di "CRUDE"
		testparm i.`exp'
		quietly mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age [pw=trunc95_mult]
		di "ETH and AGE ADJ"
		testparm i.`exp'
		quietly mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age menarche [pw=trunc95_mult]
		di "PLUS MENARCHE ADJ"
		testparm i.`exp'
		di "99th percentile"
		quietly mi estimate, or post: logistic `out' i.`exp' [pw=trunc99_mult]
		di "CRUDE"
		testparm i.`exp'
		quietly mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age [pw=trunc99_mult]
		di "ETH and AGE ADJ"
		testparm i.`exp'
		quietly mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age menarche [pw=trunc99_mult]
		di "PLUS MENARCHE ADJ"
		testparm i.`exp'
	}
	foreach out in length pms {
		di "95th percentile"
		quietly mi estimate, or post: logistic `out' i.`exp' [pw=trunc95_`out']
		di "CRUDE"
		testparm i.`exp'
		quietly mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age [pw=trunc95_`out']
		di "ETH and AGE ADJ"
		testparm i.`exp'
		quietly mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age menarche [pw=trunc95_`out']
		di "PLUS MENARCHE ADJ"
		testparm i.`exp'
		di "99th percentile"
		quietly mi estimate, or post: logistic `out' i.`exp' [pw=trunc99_`out']
		di "CRUDE"
		testparm i.`exp'
		quietly mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age [pw=trunc99_`out']
		di "ETH and AGE ADJ"
		testparm i.`exp'
		quietly mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age menarche [pw=trunc99_`out']
		di "PLUS MENARCHE ADJ"
		testparm i.`exp'
	}
}	

log close 
