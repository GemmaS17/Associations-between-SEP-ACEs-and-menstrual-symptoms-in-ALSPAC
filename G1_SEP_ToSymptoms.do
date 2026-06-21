**SEP to menstrual symptoms analysis in ALSPAC G1

**Data file 
clear all
set maxvar 30000
use "/Volumes/157/working/data/SEPandACEs_G1_data.dta", clear

********************************************************************************
**#DEFINING EXPOSURES***********************************************************
********************************************************************************

*SEP EXPOSURES - include parental education, parental social class, and financial difficulties
**Maternal education 
label define mated_lbl 3"CSE/Vocational" 2"O level" 1"A level" 0"Degree", replace
recode c645a (-1=.) (1/2=3) (3=2) (4=1) (5=0), gen (mated)
label values mated mated_lbl
**Partner education
recode c666a (-1=.) (1/2=3) (3=2) (4=1) (5=0), gen (pated)
label values pated mated_lbl
**Highest parental education
gen highed=.
replace highed=0 if mated==0 | pated==0
replace highed=1 if mated==1 | pated==1
replace highed=2 if mated==2 | pated==2
replace highed=3 if mated==3 | pated==3
label values highed mated_lbl
**Parental social class 
label define socclass_lbl 0"Non-manual" 1"Manual", replace
gen mat_binary_social_class_1=.
replace mat_binary_social_class_1=0 if c755==1 | c755==2 | c755==3  
replace mat_binary_social_class_1=1 if c755==4 | c755==5 | c755==6
gen pat_binary_social_class_1=.
replace pat_binary_social_class_1=0 if c765==1 | c765==2 | c765==3 
replace pat_binary_social_class_1=1 if c765==4 | c765==5 | c765==6
gen sclass=.
replace sclass=0 if (mat_binary_social_class_1==0 & pat_binary_social_class_1==0) | (mat_binary_social_class_1==. & pat_binary_social_class_1==0) | (mat_binary_social_class_1==0 & pat_binary_social_class_1==.)
replace sclass=1 if (mat_binary_social_class_1==1 & pat_binary_social_class_1==1) | (mat_binary_social_class_1==. & pat_binary_social_class_1==1) | (mat_binary_social_class_1==1 & pat_binary_social_class_1==.) | (mat_binary_social_class_1==0 & pat_binary_social_class_1==1) | (mat_binary_social_class_1==1 & pat_binary_social_class_1==0)
label values sclass socclass_lbl

**Financial difficulties (very difficult to afford food, clothing, heating, accomodation, or things for the baby)
gen findiff=.
label define findiff_lbl 0"Not very difficult" 1"Very difficult", replace
foreach var in c520 c521 c522 c523 c524 {
	replace `var'=. if `var'<0	
	replace findiff=0 if inrange(`var',2,4) & findiff!=1
	replace findiff=1 if `var'==1
}
label values findiff findiff_lbl

********************************************************************************
**#DEFINING OUTCOMES************************************************************
********************************************************************************

**Pain, heavy, days, and length from pub9
*Age
replace pub997a=. if pub997a<0
rename pub997a pub9_age
summ pub9_age, det
di r(mean)/12
di r(min)/12
di r(max)/12
*Pain
label define pain_lbl 0"No pain" 1"Pain", replace
recode pub922 (1=1) (2=0) (else=.), gen (pain)
label values pain pain_lbl
*Heavy
label define heavy_lbl 0"Not heavy" 1"Heavy", replace
recode pub920 (1=1) (2=0) (else=.), gen (heavy)
label values heavy heavy_lbl
*Days - categorical
label define days_lbl 0"Less than 4 days" 1"4-6 days" 2"7 days or more", replace 
gen days_cat=.
replace days_cat=2 if inrange(pub915,7,60) | pub916==3
replace days_cat=1 if inrange(pub915,4,6) | pub916==2
replace days_cat=0 if inrange(pub915,1,3) | pub916==1 
label values days_cat days_lbl
*Days - binary
label define days_bin_lbl 0"6 days or less" 1"7 days or more", replace
gen days_bin=.
replace days_bin=0 if inrange(days_cat,0,1)
replace days_bin=1 if days_cat==2
label values days_bin days_bin_lbl
*Cycle length
label define length_lbl 0"Normal (24-38)" 1"Frequent (<24)" 2"Infrequent (>38)", replace
recode pub917 (0/9=.) (24/38=0) (10/23=1) (39/140=2) (else=.), gen (length)
label values length length_lbl

**Irregular from tf4 
*Age
rename FJ003a tf4_age
replace tf4_age=. if tf4_age<0
summ tf4_age, det
display r(mean)/12
display r(min)/12
display r(max)/12
*Irregular
label define reg_lbl 0"Regular" 1"Irregular", replace
recode FJMS040 (1/3=0) (4=1) (else=.), gen (irreg)
label values irreg reg_lbl

**PMS from 21
*Age
rename YPA9020 q21_age
replace q21_age=. if q21_age<0
summ q21_age, det
display r(mean)/12
display r(min)/12
display r(max)/12
*PMS
recode YPA7060 (-10 -1=.)
label define pms_lbl 0"No symptoms" 1"Any symptoms", replace
*Generating variables for each symptom  (before/during/not at all) - out of all who responded to YPA7060
gen fatigue_before=1 if YPA7061==1
gen fatigue_during=1 if YPA7062==1
gen fatigue_no=1 if YPA7063==1
gen irritable_before=1 if YPA7064==1
gen irritable_during=1 if YPA7065==1
gen irritable_no=1 if YPA7066==1
gen depressed_before=1 if YPA7067==1
gen depressed_during=1 if YPA7068==1
gen depressed_no=1 if YPA7069==1
gen anxious_before=1 if YPA7070==1
gen anxious_during=1 if YPA7071==1
gen anxious_no=1 if YPA7072==1
gen other_before=1 if YPA7073==1
gen other_during=1 if YPA7074==1
gen other_no=1 if YPA7075==1
foreach var in fatigue_before fatigue_during fatigue_no irritable_before irritable_during irritable_no depressed_before depressed_during depressed_no anxious_before anxious_during anxious_no other_before other_during other_no {
	replace `var'=0 if `var'==. & YPA7060!=. 
	label values `var' pms_lbl
}																		// make denominator for each those who responded to YPA7060
*Before only
foreach symp in fatigue irritable depressed anxious other {
	gen `symp'_bin_bef=.
	replace `symp'_bin_bef=0 if `symp'_no==1 | YPA7060==2
	replace `symp'_bin_bef=1 if `symp'_before==1 
	label values `symp'_bin_bef pms_lbl
}
**PMS - number of symptoms 
gen pms_bef = fatigue_bin_bef + irritable_bin_bef + depressed_bin_bef + anxious_bin_bef + other_bin_bef
*Binary
gen pms_bin_bef=0 if pms_bef==0
replace pms_bin_bef=1 if inrange(pms_bef,1,5)
label values pms_bin_bef pms_lbl
*During only
foreach symp in fatigue irritable depressed anxious other {
	gen `symp'_bin_dur=.
	replace `symp'_bin_dur=0 if `symp'_no==1 | YPA7060==2
	replace `symp'_bin_dur=1 if `symp'_during==1 
	label values `symp'_bin_dur pms_lbl
}
**PMS - number of symptoms 
gen pms_dur = fatigue_bin_dur + irritable_bin_dur + depressed_bin_dur + anxious_bin_dur + other_bin_dur
*Binary
gen pms_bin_dur=0 if pms_dur==0
replace pms_bin_dur=1 if inrange(pms_dur,1,5)
label values pms_bin_dur pms_lbl
*Before or during 
**Binary version for each symptom (e.g. fatigue at any time vs. no fatigue)
foreach symp in fatigue irritable depressed anxious other {
	gen `symp'_bin=.
	replace `symp'_bin=0 if `symp'_no==1 | YPA7060==2
	replace `symp'_bin=1 if `symp'_before==1 | `symp'_during==1
	label values `symp'_bin pms_lbl
}
**PMS - number of symptoms (before or during)
gen pms = fatigue_bin + irritable_bin + depressed_bin + anxious_bin + other_bin
*Binary
gen pms_bin=0 if pms==0
replace pms_bin=1 if inrange(pms,1,5)
label values pms_bin pms_lbl

********************************************************************************																		  
**Boost N with pub8*************************************************************

**Pain, heavy, days, and length from pub8
*Age
replace pub897a=. if pub897a<0
rename pub897a pub8_age
summ pub8_age, det
di r(mean)/12
di r(min)/12
di r(max)/12
*Pain
recode pub822 (1=1) (2=0) (else=.), gen (pain_pub8)
label values pain_pub8 pain_lbl
*Heavy
recode pub820 (1=1) (2=0) (else=.), gen (heavy_pub8)
label values heavy_pub8 heavy_lbl
*Days - categorical
gen days_cat_pub8=.
replace days_cat_pub8=2 if inrange(pub815,7,60) | pub816==3
replace days_cat_pub8=1 if inrange(pub815,4,6) | pub816==2
replace days_cat_pub8=0 if inrange(pub815,1,3) | pub816==1 
label values days_cat_pub8 days_lbl
*Days - binary
gen days_bin_pub8=.
replace days_bin_pub8=0 if inrange(days_cat_pub8,0,1)
replace days_bin_pub8=1 if days_cat_pub8==2
label values days_bin_pub8 days_bin_lbl
*Cycle length
recode pub817 (0/9=.) (24/38=0) (10/23=1) (39/140=2) (else=.), gen (length_pub8)
label values length_pub8 length_lbl

**Missing at pub9 with pub8 data per symptom
gen data_pub8_pain=1 if pain==. & pain_pub8!=.				
gen data_pub8_heavy=1 if heavy==. & heavy_pub8!=.			
gen data_pub8_days_cat=1 if days_cat==. & days_cat_pub8!=.
gen data_pub8_length=1 if length==. & length_pub8!=.	

*Missing all symptoms at pub9 instead
gen no_pub9=1 if pain==. & heavy==. & days_cat==. & length==.		
replace no_pub9=0 if pain!=. | heavy!=. | days_cat!=. | length!=. 
foreach var in pain heavy days_cat length {
	replace data_pub8_`var'=0 if data_pub8_`var'==1 & no_pub9==0 
	tab data_pub8_`var'
}

*Combine outcomes and then check overvall ages/menarche/TSM factors to compare as well
gen pub8_tsm=pub8_age-menarche_month
summ pub8_tsm, det
di r(mean)/12
di r(min)/12
di r(max)/12

label define days_cat_lbl 0"Less than 4 days" 1"4-6 days" 2"7 days or more", replace 
gen data_pub8_days_bin=data_pub8_days_cat
foreach var in pain heavy days_cat days_bin length {
	capture drop `var'_both
	gen `var'_both=`var'
	replace `var'_both=`var'_pub8 if data_pub8_`var'==1
	label values `var'_both `var'_lbl
}

label define length_bin_lbl 0"Normal (24-38)" 1"Freq. or Infreq.", replace
gen length_bin_both=.
replace length_bin_both=0 if length_both==0
replace length_bin_both=1 if inrange(length_both,1,2)
label values length_bin_both length_bin_lbl


gen pubboth_age=pub9_age
replace pubboth_age=pub8_age if no_pub9==1
gen pub_tsm=pub9_tsm
replace pub_tsm=pub8_tsm if no_pub9==1

********************************************************************************
**#DEFINING COVARIATES**********************************************************
********************************************************************************

**Menarche
replace clon070=. if clon070<0 
rename clon070 menarche
gen menarche_month=menarche*12
replace clon071=. if clon071<0
rename clon071 menarche_cat 
*Correct categories 
replace menarche_cat=4 if menarche_cat==1
replace menarche_cat=1 if menarche_cat==3
replace menarche_cat=3 if menarche_cat==4
tab menarche_cat
bysort menarche_cat: summ menarche
*Started at relevant times
label define menarche_lbl 0"Not started period" 1"Started period", replace
foreach var in pub9 tf4 q21 {
	capture drop `var'_menarche
	gen `var'_menarche=.
	replace `var'_menarche=0 if `var'_age<menarche_month & `var'_age!=. & menarche_month!=.
	replace `var'_menarche=1 if `var'_age>=menarche_month & `var'_age!=. & menarche_month!=.
	label values `var'_menarche menarche_lbl
	tab `var'_menarche														
}
*CHECK - What does the AAM distribution look like in outcome samples?
tab menarche_cat // 15.49% early, 61.35% normative, 23.16% late
foreach var in pain heavy days_cat length irreg pms_bin {
	tab menarche_cat if `var'!=.
}		
*These are all mostly similar to the overall distribution - tend to have a bit less early (pubs 14.57-15.37; tf4 13.61; 21 12.88) and more late (pubs 23.47-26.21; tf4 25.84; 21 25.57)
*CHECK - what does TSM look like at the relevant times?
foreach var in pub9 tf4 q21 {
	capture drop `var'_tsm
	gen `var'_tsm=`var'_age-menarche_month
	summ `var'_tsm, det
	di r(mean)/12
	di r(min)/12
	di r(max)/12
}

**Ethnicity 
label define ethnicity_lbl 1"Non-white" 0"White", replace
recode c804 (-1=.) (1=0) (2=1), gen (ethnicity)
label values ethnicity ethnicity_lbl

***Sensitivity covariates
**Contraception
label define cont_lbl 1"Hormonal contraception" 0"None or non-hormonal contraception", replace
*Puberty
foreach var in pub8 pub9 {
	recode `var'27 (-10 -7 -6 -2 -1 9=.) (1=1) (2=0), gen (`var'_cont)
	label  values `var'_cont cont_lbl
}
gen pub_cont=pub9_cont
replace pub_cont=pub8_cont if no_pub9==1
*tf4
gen tf4_cont=.
replace tf4_cont=0 if FJMS010==2 | FJMS011==2 | FJMS012==2 | FJMS013==2 | FJMS014==2 
replace tf4_cont=1 if FJMS010==1 | FJMS011==1 | FJMS012==1 | FJMS013==1 | FJMS014==1
label values tf4_cont cont_lbl
*q21
gen q21_cont=.
replace q21_cont=0 if YPA3270==1 | YPA3289==1 | YPA3290==1 | YPA3291==1 | YPA3271==1 | YPA3272==1 | YPA3278==1 | YPA3279==1 | YPA3280==1 | YPA3281==1 | YPA3282==1 | YPA3283==1 | YPA3284==1 | YPA3285==1 | YPA3288==1 
replace q21_cont=1 if YPA3273==1 | YPA3274==1 | YPA3275==1 | YPA3276==1 | YPA3277==1 | YPA3286==1 | YPA3287==1 
label values q21_cont cont_lbl


********************************************************************************
**#DEFINING SAMPLE(S)***********************************************************
********************************************************************************
*Flowchart
*1* Standard exclusions
drop if kz011b==2
drop if in_core==.a
drop if qlet=="B"
drop if in_core==2
*2*SEP exposures
count if highed==.								
count if highed!=. & sclass==.			
count if highed!=. & sclass!=. & findiff==.	
*3*Ethnicity
count if highed!=. & sclass!=. & findiff!=. & ethnicity==.		
*4*Menarche 
count if highed!=. & sclass!=. & findiff!=. & ethnicity!=. & menarche==.	
*Check so far
mark sep_use
markout sep_use highed sclass findiff ethnicity menarche
*5*Outcomes
count if sep_use==1 & pain_both!=. & heavy_both!=. & days_bin_both!=.		//2600 
mark pub_use 
markout pub_use highed sclass findiff ethnicity menarche pain_both heavy_both days_bin_both
count if sep_use==1 & length_bin_both!=.
count if sep_use==1 & irreg!=.			
count if sep_use==1 & pms_bin!=.

********************************************************************************
**#DESCRIPTIVES*****************************************************************
********************************************************************************
capture drop sep_use pub_use
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Complete Case/Descriptives/Simple_CrossTabs_Missing", replace
*Dropped ineligible ppts (7930 to 6661)
/*
drop if kz011b==2
drop if in_core==.a
drop if qlet=="B"
drop if in_core==2
*/
mark sep_use
markout sep_use highed sclass findiff ethnicity menarche
mark pub_use 
markout pub_use highed sclass findiff ethnicity menarche pain_both heavy_both days_bin_both

***SIMPLE DESCRIPTIVES 
**Exposures
foreach var in highed sclass findiff {
	di "`var' IN ALL OBSERVED DATA"
	tab `var'
	di "`var' IN TOUSE SAMPLE"
	tab `var' if sep_use==1
}
**Outcomes
foreach var in pain_both heavy_both days_cat_both days_bin_both {
	di "`var' IN ALL OBSERVED DATA"
	tab `var'
	di "`var' IN TOUSE SAMPLE"
	tab `var' if pub_use==1	
}
foreach var in length_both length_bin_both irreg pms_bin {
	di "`var' IN ALL OBSERVED DATA"
	tab `var'
	di "`var' IN TOUSE SAMPLE"
	tab `var' if sep_use==1	
}
**Covariates
foreach var in menarche_cat ethnicity {
	di "`var' IN ALL OBSERVED DATA"
	tab `var'
	di "`var' IN TOUSE SAMPLE"
	tab `var' if sep_use==1		
}
di "menarche IN ALL OBSERVED DATA"
summ menarche, det 
di "menarche IN TOUSE SAMPLE"
summ menarche if sep_use==1, det 

***CROSS TAB DESCRIPTIVES 
**Exposures and covariates by outcomes
foreach out in pain_both heavy_both days_cat_both days_bin_both {
	foreach exp_cov in highed sclass findiff menarche_cat ethnicity {
		di "IN ALL OBSERVED DATA"
		tab `out' `exp_cov', row col	
		di "IN TOUSE SAMPLE"
		tab `out' `exp_cov' if pub_use==1, row col	
	}
	di "IN ALL OBSERVED DATA"
	bysort `out': summ menarche, det
	di "IN TOUSE SAMPLE"
	bysort `out': summ menarche if pub_use==1, det
}
foreach out in length_both length_bin_both irreg pms_bin {
	foreach exp_cov in highed sclass findiff menarche_cat ethnicity {
		di "IN ALL OBSERVED DATA"
		tab `out' `exp_cov', row col	
		di "IN TOUSE SAMPLE"
		tab `out' `exp_cov' if sep_use==1, row col	
	}
	di "IN ALL OBSERVED DATA"
	bysort `out': summ menarche, det
	di "IN TOUSE SAMPLE"
	bysort `out': summ menarche if sep_use==1, det
}
**Covariates by exposures
foreach exp in highed sclass findiff {
	foreach cov in menarche_cat ethnicity {
		di "IN ALL OBSERVED DATA"
		tab `exp' `cov', row col	
		di "IN TOUSE SAMPLE"
		tab `exp' `cov' if sep_use==1, row col	
	}
	di "IN ALL OBSERVED DATA"
	bysort `exp': summ menarche, det
	di "IN TOUSE SAMPLE"
	bysort `exp': summ menarche if sep_use==1, det
}

***MISSING DATA PATTERNS
*Exposures, outcomes, and covariates in all observed data (as above), those with covariates, sep_use restricted (as above) and in each CC analysis model
foreach var in highed sclass findiff pain_both heavy_both days_cat_both days_bin_both length_both length_bin_both irreg pms_bin menarche_cat ethnicity {
	di "IN ALL OBSERVED DATA"
	tab `var'
	di "IN SEP SAMPLE"
	tab `var' if highed!=. & sclass!=. & findiff!=.
	di "IN SEP AND ETHNICITY SAMPLE"
	tab `var' if highed!=. & sclass!=. & findiff!=. & ethnicity!=.
	di "IN TOUSE SAMPLE"
	tab `var' if sep_use==1
	di "IN MAIN PUB ANALYSIS"
	tab `var' if pub_use==1 
	di "IN LENGTH ANALYSIS"
	tab `var' if sep_use==1 & length_both!=.
	di "IN REGULARITY ANALYSIS"
	tab `var' if sep_use==1 & irreg!=.
	di "IN PMS ANALYSIS"
	tab `var' if sep_use==1 & pms_bin!=. 
}
di "IN ALL OBSERVED DATA"
summ menarche, det
di "IN SEP SAMPLE"
summ menarche if highed!=. & sclass!=. & findiff!=., det
di "IN SEP AND ETHNICITY SAMPLE"
summ menarche if highed!=. & sclass!=. & findiff!=. & ethnicity!=., det
di "IN TOUSE SAMPLE"
summ menarche if sep_use==1, det
di "IN MAIN PUB ANALYSIS"
summ menarche if pub_use==1 
di "IN LENGTH ANALYSIS"
summ menarche if sep_use==1 & length_both!=., det
di "IN REGULARITY ANALYSIS"
summ menarche if sep_use==1 & irreg!=., det
di "IN PMS ANALYSIS"
summ menarche if sep_use==1 & pms_bin!=., det

**Patterns and amount of missing
replace pub_use=. if pub_use==0 

mdesc highed sclass findiff ethnicity menarche pain_both heavy_both days_bin_both pub_use length_both irreg pms_bin 

*Number of missing variables
egen key_vars=rmiss2(highed sclass findiff ethnicity menarche pain_both heavy_both days_bin_both length_both irreg pms_bin)
tab key_vars
egen key_vars_simple=rmiss2(highed sclass findiff ethnicity menarche pub_use length_both irreg pms_bin)
tab key_vars_simple

log close

********************************************************************************
**#MAIN CC ANALYSIS*************************************************************
********************************************************************************
rename days_bin_both days_both
rename length_both length_cat_both
rename length_bin_both length_both


capture drop sep_use pub_use
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Complete Case/Analysis/MainResults", replace
mark sep_use
markout sep_use highed sclass findiff ethnicity menarche
mark pub_use 
markout pub_use highed sclass findiff ethnicity menarche pain_both heavy_both days_both

foreach exp in highed sclass findiff {
	foreach out in pain_both heavy_both days_both  {
		logistic `out' i.`exp' if pub_use==1 
		logistic `out' i.`exp' ethnicity if pub_use==1 
	}
	foreach out in length_both irreg pms_bin {
		logistic `out' i.`exp' if sep_use==1 
		logistic `out' i.`exp' ethnicity if sep_use==1 
	}
}

foreach exp in highed sclass findiff {
	mlogit days_cat_both i.`exp' if pub_use==1, rr
	mlogit days_cat_both i.`exp' ethnicity if pub_use==1, rr
	mlogit length_cat_both i.`exp' if sep_use==1, rr
	mlogit length_cat_both i.`exp' ethnicity if sep_use==1, rr
}

log close

**Determining if education should be treated as continuous or categorical
rename days_bin_both days_both
rename length_both length_cat_both
rename length_bin_both length_both

mark sep_use
markout sep_use highed sclass findiff ethnicity menarche
mark pub_use 
markout pub_use highed sclass findiff ethnicity menarche pain_both heavy_both days_both

estimates clear

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Education_ContorCat_Analysis", replace

*Complete case results
foreach out in pain_both heavy_both days_both  {
	logistic `out' i.highed ethnicity if pub_use==1 
	estimates store `out'_cat
	logistic `out' highed ethnicity if pub_use==1 
	estimates store `out'_cont
	lrtest `out'_cont `out'_cat
}


foreach out in length_both irreg pms_bin {
	logistic `out' i.highed ethnicity if sep_use==1 
	estimates store `out'_cat
	logistic `out' highed ethnicity if sep_use==1 
	estimates store `out'_cont
	lrtest `out'_cont `out'_cat
}


*Using the continuous results instead - need the unadjusted results 
foreach out in pain_both heavy_both days_both  {
	logistic `out' highed if pub_use==1 
}
foreach out in length_both irreg pms_bin {
	logistic `out' highed if sep_use==1 
}

log close

********************************************************************************
**#SENSITIVITY CC ANALYSIS******************************************************
********************************************************************************
rename days_bin_both days_both
rename length_both length_cat_both
rename length_bin_both length_both
rename mat_binary_social_class_1 mat_sclass
rename pat_binary_social_class_1 pat_sclass

capture drop sep_use pub_use

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Complete Case/Analysis/Sensitivity_Results", replace
*1* Other ed exposures (mated; pated; mat_sclass; pat_sclass)
mark sep_use
markout sep_use highed sclass findiff ethnicity menarche
mark pub_use 
markout pub_use highed sclass findiff ethnicity menarche pain_both heavy_both days_both

foreach exp in mated pated mat_sclass pat_sclass {
	foreach out in pain_both heavy_both days_both  {
		logistic `out' i.`exp' if pub_use==1 
		logistic `out' i.`exp' ethnicity if pub_use==1 
	}
	foreach out in length_both irreg pms_bin {
		logistic `out' i.`exp' if sep_use==1 
		logistic `out' i.`exp' ethnicity if sep_use==1 
	}
}

log close

*Next - Exclude those using hormonal contraception (1) at relevant timepoint
foreach var in pain heavy days length {
	gen `var'_cont=`var'_both
	replace `var'_cont=. if pub_cont==1
}
gen irreg_cont=irreg
replace irreg_cont=. if tf4_cont==1
gen pms_cont=pms_bin
replace pms_cont=. if q21_cont==1

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Complete Case/Analysis/Sensitivity_Results", append
*2* Contraception (pub_cont; tf4_cont; q21_cont)

foreach exp in highed sclass findiff {
	foreach out in pain_cont heavy_cont days_cont  {
		logistic `out' i.`exp' if pub_use==1 
		logistic `out' i.`exp' ethnicity if pub_use==1 
	}
	foreach out in length_cont irreg_cont pms_cont {
		logistic `out' i.`exp' if sep_use==1 
		logistic `out' i.`exp' ethnicity if sep_use==1 
	}
}

log close

*Next - Exclude those less than 3 years (36 months) since menarche 
foreach var in pain heavy days length {
	gen `var'_tsm=`var'_both
	replace `var'_tsm=. if inrange(pub_tsm,0,36)
}
gen irreg_tsm=irreg
replace irreg_tsm=. if inrange(tf4_tsm,0,36)
gen pms_tsm=pms_bin
replace pms_tsm=. if inrange(q21_tsm,0,36)


log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Complete Case/Analysis/Sensitivity_Results", append
*3* 3 years (pub_tsm; tf4_tsm; q21_tsm)

foreach exp in highed sclass findiff {
	foreach out in pain_tsm heavy_tsm days_tsm  {
		logistic `out' i.`exp' if pub_use==1 
		logistic `out' i.`exp' ethnicity if pub_use==1 
	}
	foreach out in length_tsm irreg_tsm pms_tsm {
		logistic `out' i.`exp' if sep_use==1 
		logistic `out' i.`exp' ethnicity if sep_use==1 
	}
}

log close

*Next - doctor pain and heavy
*Pain - pub823 pub923
label define pain_doctor_lbl 2"Went to doctor" 1"No doctor" 0"No pain" , replace
gen pain_doc=pain_both
replace pain_doc=2 if pub823==1 & no_pub9==1 & pain_doc<.| pub923==1 & pain_doc<.
label values pain_doc pain_doctor_lbl

*Heavy - pub821 pub921
label define heavy_doctor_lbl 2"Went to doctor" 1"No doctor" 0"No heavy bleeding" , replace
gen heavy_doc=heavy_both
replace heavy_doc=2 if pub821==1 & no_pub9==1 & heavy_doc<. & heavy_doc!=0 | pub921==1 & heavy_doc<.
label values heavy_doc heavy_doctor_lbl

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Complete Case/Analysis/Sensitivity_Results", append
*4* Doctor outcomes 
foreach exp in highed sclass findiff {
	foreach out in pain_doc heavy_doc {
		mlogit `out' i.`exp' if pub_use==1, rr
		mlogit `out' i.`exp' ethnicity if pub_use==1, rr
	}
}

log close

*Next - 4 level heavy or prolonged
label define heavycat_lbl 0"Neither" 1"Prolonged only" 2"Heavy only" 3"Both", replace
gen heavy_days_cat=.
replace heavy_days_cat=3 if heavy_both==1 & days_both==1
replace heavy_days_cat=2 if heavy_both==1 & days_both==0
replace heavy_days_cat=1 if heavy_both==0 & days_both==1
replace heavy_days_cat=0 if heavy_both==0 & days_both==0
label values heavy_days_cat heavycat_lbl

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Complete Case/Analysis/Sensitivity_Results", append
*5* Separating heavy from prolonged bleeding attempt 
foreach exp in highed sclass findiff {
	mlogit heavy_days_cat i.`exp' if pub_use==1, rr
	mlogit heavy_days_cat i.`exp' ethnicity if pub_use==1, rr
}

log close

*Some descriptives for these variables
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Complete Case/Descriptives/Sensitivity_Variables", replace
*1* Other ed exposures (ed and sclass) in various samples 
tab1 mated pated mat_sclass pat_sclass
tab1 mated pated mat_sclass pat_sclass if pub_use==1
tab1 mated pated mat_sclass pat_sclass if sep_use==1
tab1 mated pated mat_sclass pat_sclass if length_both!=.
tab1 mated pated mat_sclass pat_sclass if irreg!=.
tab1 mated pated mat_sclass pat_sclass if pms_bin!=.

*2* Contraception (pub_cont; tf4_cont; q21_cont)
tab1 pain_cont heavy_cont days_cont
tab1 length_cont irreg_cont pms_cont

tab1 pain_cont heavy_cont days_cont if pub_use==1
tab1 length_cont irreg_cont pms_cont if sep_use==1

*3* 3 years (pub_tsm; tf4_tsm; q21_tsm)
tab1 pain_tsm heavy_tsm days_tsm
tab1 length_tsm irreg_tsm pms_tsm

tab1 pain_tsm heavy_tsm days_tsm if pub_use==1
tab1 length_tsm irreg_tsm pms_tsm if sep_use==1

*4* Doctor
tab1 heavy_days_cat heavy_doc
tab1 pain_doc heavy_doc if pub_use==1

*5* Separating heavy form prolonged
tab heavy_days_cat
tab heavy_days_cat if pub_use==1

log close


*Continuous education compared with categorical in these senitivity variables 

/* Generate vars if needed
foreach var in pain heavy days length {
	gen `var'_cont=`var'_both
	replace `var'_cont=. if pub_cont==1
}
gen irreg_cont=irreg
replace irreg_cont=. if tf4_cont==1
gen pms_cont=pms_bin
replace pms_cont=. if q21_cont==1

foreach var in pain heavy days length {
	gen `var'_tsm=`var'_both
	replace `var'_tsm=. if inrange(pub_tsm,0,36)
}
gen irreg_tsm=irreg
replace irreg_tsm=. if inrange(tf4_tsm,0,36)
gen pms_tsm=pms_bin
replace pms_tsm=. if inrange(q21_tsm,0,36)

label define pain_doctor_lbl 2"Went to doctor" 1"No doctor" 0"No pain" , replace
gen pain_doc=pain_both
replace pain_doc=2 if pub823==1 & no_pub9==1 & pain_doc<.| pub923==1 & pain_doc<.
label values pain_doc pain_doctor_lbl

label define heavy_doctor_lbl 2"Went to doctor" 1"No doctor" 0"No heavy bleeding" , replace
gen heavy_doc=heavy_both
replace heavy_doc=2 if pub821==1 & no_pub9==1 & heavy_doc<. & heavy_doc!=0 | pub921==1 & heavy_doc<.
label values heavy_doc heavy_doctor_lbl

label define heavycat_lbl 0"Neither" 1"Prolonged only" 2"Heavy only" 3"Both", replace
gen heavy_days_cat=.
replace heavy_days_cat=3 if heavy_both==1 & days_both==1
replace heavy_days_cat=2 if heavy_both==1 & days_both==0
replace heavy_days_cat=1 if heavy_both==0 & days_both==1
replace heavy_days_cat=0 if heavy_both==0 & days_both==0
label values heavy_days_cat heavycat_lbl
*/

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Education_ContorCat_Analysis", append

*Sensitivity complete case results

estimates clear

*Other exposures (parental specific)
foreach exp in mated pated {
	foreach out in pain_both heavy_both days_both  {
	logistic `out' i.`exp' ethnicity if pub_use==1 
	estimates store `out'_`exp'_cat
	logistic `out' `exp' ethnicity if pub_use==1 
	estimates store `out'_`exp'_cont
	lrtest `out'_`exp'_cont `out'_`exp'_cat
}
foreach out in length_both irreg pms_bin {
	logistic `out' i.`exp' ethnicity if sep_use==1 
	estimates store `out'_`exp'_cat
	logistic `out' `exp' ethnicity if sep_use==1 
	estimates store `out'_`exp'_cont
	lrtest `out'_`exp'_cont `out'_`exp'_cat
}
}

estimates clear

*Other outcomes (cont; menarche)
foreach out in pain_cont heavy_cont days_cont pain_tsm heavy_tsm days_tsm {
	logistic `out' i.highed ethnicity if pub_use==1 
	estimates store `out'_cat
	logistic `out' highed ethnicity if pub_use==1 
	estimates store `out'_cont
	lrtest `out'_cont `out'_cat
}
foreach out in length_cont irreg_cont pms_cont length_tsm irreg_tsm pms_tsm {
	logistic `out' i.highed ethnicity if sep_use==1 
	estimates store `out'_cat
	logistic `out' highed ethnicity if sep_use==1 
	estimates store `out'_cont
	lrtest `out'_cont `out'_cat
}

estimates clear

*Other outcomes - mlogit (doctor; heavy from prolonged)
foreach out in pain_doc heavy_doc heavy_days_cat {
	mlogit `out' i.highed ethnicity if pub_use==1, rr 
	estimates store `out'_cat
	mlogit `out' highed ethnicity if pub_use==1, rr
	estimates store `out'_cont
	lrtest `out'_cont `out'_cat
}


*Using the continuous ones - need the unadjusted results

*Parent-specific
foreach exp in mated pated {
	foreach out in pain_both heavy_both days_both  {
	logistic `out' `exp' if pub_use==1 
}
foreach out in length_both irreg pms_bin {
	logistic `out' `exp' if sep_use==1 
}
}


*Other outcomes (cont; menarche)
foreach out in pain_cont heavy_cont days_cont pain_tsm heavy_tsm days_tsm {
	logistic `out' highed if pub_use==1 
}
foreach out in length_cont irreg_cont pms_cont length_tsm irreg_tsm pms_tsm {
	logistic `out' highed if sep_use==1 
}

*Other outcomes - mlogit (doctor; heavy from prolonged)
foreach out in pain_doc heavy_doc heavy_days_cat {
	mlogit `out' highed if pub_use==1, rr
}

log close

**#CONTINUOUS EDUCATION WITH ONE OVERALL P VALUE

mark sep_use
markout sep_use highed sclass findiff ethnicity menarche
mark pub_use 
markout pub_use highed sclass findiff ethnicity menarche pain_both heavy_both days_both

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Education_Categorical_OverallP_CC", replace

**MAIN RESULTS
foreach out in pain_both heavy_both days_both  {
	quietly logistic `out' i.highed if pub_use==1 
	di "CRUDE"
	testparm i.highed
	quietly logistic `out' i.highed ethnicity if pub_use==1 
	di "ADJUSTED"
	testparm i.highed
}
foreach out in length_both irreg pms_bin {
	quietly logistic `out' i.highed if sep_use==1 
	di "CRUDE"
	testparm i.highed
	quietly logistic `out' i.highed ethnicity if sep_use==1 
	di "ADJUSTED"
	testparm i.highed
}

**SENSITIVITY RESULTS
*Parent specific
foreach exp in mated pated  {
	foreach out in pain_both heavy_both days_both  {
		quietly logistic `out' i.`exp' if pub_use==1 
		di "CRUDE"
		testparm i.`exp'
		quietly logistic `out' i.`exp' ethnicity if pub_use==1 
		di "ADJUSTED"
		testparm i.`exp'
	}
	foreach out in length_both irreg pms_bin {
		quietly logistic `out' i.`exp' if sep_use==1 
		di "CRUDE"
		testparm i.`exp'
		quietly logistic `out' i.`exp' ethnicity if sep_use==1 
		di "ADJUSTED"
		testparm i.`exp'
	}
}

*Contraception 
foreach out in pain_cont heavy_cont days_cont  {
	quietly logistic `out' i.highed if pub_use==1 
	di "CRUDE"
	testparm i.highed
	quietly logistic `out' i.highed ethnicity if pub_use==1 
	di "ADJUSTED"
	testparm i.highed
}
foreach out in length_cont irreg_cont pms_cont {
	quietly logistic `out' i.highed if sep_use==1 
	di "CRUDE"
	testparm i.highed
	quietly logistic `out' i.highed ethnicity if sep_use==1 
	di "ADJUSTED"
	testparm i.highed
}


*Menarche
foreach out in pain_tsm heavy_tsm days_tsm  {
	quietly logistic `out' i.highed if pub_use==1 
	di "CRUDE"
	testparm i.highed
	quietly logistic `out' i.highed ethnicity if pub_use==1 
	di "ADJUSTED"
	testparm i.highed
}
foreach out in length_tsm irreg_tsm pms_tsm {
	quietly logistic `out' i.highed if sep_use==1 
	di "CRUDE"
	testparm i.highed
	quietly logistic `out' i.highed ethnicity if sep_use==1 
	di "ADJUSTED"
	testparm i.highed
}

*Doctor
foreach out in pain_doc heavy_doc {
	quietly mlogit `out' i.highed if pub_use==1, rr
	di "CRUDE"
	test [No_doctor]: 1.highed 2.highed 3.highed
	test [Went_to_doctor]: 1.highed 2.highed 3.highed
	quietly mlogit `out' i.highed ethnicity if pub_use==1, rr
	di "ADJUSTED"
	test [No_doctor]: 1.highed 2.highed 3.highed
	test [Went_to_doctor]: 1.highed 2.highed 3.highed
}

*Heavy from prolonged
quietly mlogit heavy_days_cat i.highed if pub_use==1, rr
di "CRUDE"
test [Prolonged_only]: 1.highed 2.highed 3.highed
test [Heavy_only]: 1.highed 2.highed 3.highed
test [Both]: 1.highed 2.highed 3.highed
quietly mlogit heavy_days_cat i.highed ethnicity if pub_use==1, rr
di "ADJUSTED"
test [Prolonged_only]: 1.highed 2.highed 3.highed
test [Heavy_only]: 1.highed 2.highed 3.highed
test [Both]: 1.highed 2.highed 3.highed

log close

********************************************************************************
**#IPW PREP*********************************************************************
********************************************************************************

**Baseline predictor vars
*Maternal age at delivery mz028b
recode mz028b (-10 -4 -2=.), gen (mat_age)
*A quest; marital status a525 (Y/N), phone a051(y/n or incoming only), car a053 (y/n), housing tenure a006 (own/mortgage, private rented, council/HA/other), number of rooms a045 (N), crowding index a551 (<=0.5, >0.5-0.75, >0.75-1, >1), double glazing a060 (none or full/partial)
recode a525 (-1=.) (1/4=1 "No") (5 6=0 "Yes"), gen (marital_stat)
recode a051 (-7 -1=.) (1=0 "Yes") (2/3=1 "No/incoming only"), gen (phone)
recode a053 (-7 -1=.) (1=0 "Yes") (2=1 "No"), gen (car)
recode a006 (-7 -1=.) (0 1=0 "Own/Mortgage") (3 4=1 "Private rented") (2 5 6=2 "Council/HA/Other"), gen (housing)
recode a045 (-7 -1=.), gen (rooms)
recode a551 (-7 -1=.) (1=1 "<= 0.5") (2=2 ">0.5 - 0.75") (3=3 ">0.75 - 1") (4=4 ">1") , gen (crowding)
recode a060 (-7 -1=.) (1 2=0 "All or some") (3=1 "None"), gen (dbl_glaze)
*B quest; first preg b023 (<20, 20-24, 25+), smoking in preg b665/b667 (y/n), smoking ever b650 (y/n), depression b371 (score), parity b032 (0, 1, 2+)
recode b023 (-1=.) (12/19=0 "<20") (20/24=1 "20-24") (25/44=2 "25+"), gen (first_preg)
gen smoke_preg=.
label define smoke_lbl 0"No" 1"Yes"
replace smoke_preg=0 if b665==1 | b667==1
replace smoke_preg=1 if inrange(b665,2,5) | inrange(b667,2,5)
label values smoke_preg smoke_lbl
recode b650 (-1=.) (1=1 "Yes") (2=0 "No"), gen (smoke_ever)
recode b371 (-7 -1=.), gen (epds)
recode b032 (-7 -2 -1=.) (0=0) (1=1) (2/8=2 "2+"), gen (parity)
*C quest; ethnicity c800 (non-white / white), education c645a (O level lower, A level, degree - maternal or both?), financial difficulties c525 (score), social class c755 c765 (manual v non-manual)
recode c800 (-1=.) (1=0 "White") (2/9=1 "Non-white"), gen (mat_ethnicity)
recode c645a (-1=.) (1/3=0 "O level or lower") (4=1 "A level") (5=2 "Degree"), gen (mated_ipw)
gen edu_ipw=highed
label values edu_ipw mated_lbl
recode c525 (-7 -1=.), gen (findiff_ipw)
gen sclass_ipw=sclass
label define social_class_lbl 0"Non-Manual" 1"Manual", replace
label values sclass_ipw social_class_lbl
*Child-based; breastfeeding ka035/ka036, kb279/kb280, kc403/kc404 (4wks, 6months, 15months)
*Need to derive a breastfeeding duration variable (paper did categorical - never/<1 month; 1-<3months; 3 to <6months; and 6 months+)
*KA time - asked about feeding method each week; 035 is if they said breast at any week; 036 is latest report of breastfeeding
*KB time - 279 is age stopped in weeks and 280 is duration grouped in months
*KC time - 404 is derived from 403 (grouping the months, and including those who said still BF in 6 months+)
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

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Complete Case/Descriptives/IPW_Vars_Patterns", replace
*Descriptives
foreach var in marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever parity mat_ethnicity mated sclass_ipw bfed_dur fai_long {
	tab `var'
}

foreach var in mat_age rooms epds findiff_ipw fai_long {
	summ `var', det
}

*Missing data patterns
mdesc mat_age marital_stat phone car housing rooms crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mat_ethnicity mated findiff_ipw sclass_ipw bfed_dur fai_long
*Number of missing variables
egen ipw_vars=rmiss2(mat_age marital_stat phone car housing rooms crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mat_ethnicity mated findiff_ipw sclass_ipw bfed_dur fai_long)
tab ipw_vars

log close

********************************************************************************
**#PREDICTING MISSINGNESS*******************************************************
********************************************************************************

***Complete case - which IPW vars are associated with missing / which model gives a good fit?

*Derive missing outcome variables
*Missing pain/heavy/days variable (2844 available; 3817 missing)
gen has_pub=0
replace has_pub=1 if menarche!=. & pain_both!=. & heavy_both!=. & days_bin_both!=.
label define has_outcome 0"Data missing" 1"Data available", replace
label values has_pub has_outcome
*Missing length outcome variable (1384 available; 5277 missing)
gen has_length=0
replace has_length=1 if menarche!=. & length_bin_both!=.
label values has_length has_outcome
*Missing irreg outcome variable (2315 data available; 4346 data missing)
gen has_irreg=0
replace has_irreg=1 if menarche!=. & irreg!=.
label values has_irreg has_outcome
*Missing pms outcome variable (1452 data available; 5209 data missing)
gen has_pms=0
replace has_pms=1 if menarche!=. & pms_bin!=.
label values has_pms has_outcome
*Missing menarche (3890 available; 2771 missing)
gen has_menarche=0
replace has_menarche=1 if menarche!=. 
label values has_menarche has_outcome

**Pain/heavy/days outcomes 
**Lasso approach
lasso logit has_pub mat_age marital_stat phone car i.housing rooms i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass_ipw i.bfed_dur fai_long  mat_ethnicity
lassocoef
*Model
logistic has_pub mat_age phone car i.housing i.crowding i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass_ipw i.bfed_dur fai_long
estat gof				//x(4616)^2 = 4637.99; p = 0.4069
estat gof, group(10)	//x(8)^2 = 3.90; p = 0.8659

**Length
*Lasso
lasso logit has_length mat_age marital_stat phone car i.housing rooms i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass_ipw i.bfed_dur fai_long mat_ethnicity
lassocoef
logistic has_length mat_age phone car i.housing i.crowding dbl_glaze i.first_preg smoke_ever epds i.parity i.mated i.bfed_dur fai_long 
estat gof				//x(4588)^2 = 4613.02; p = 0.3945
estat gof, group(10)	//x(8)^2 = 4.89; p = 0.7696

**Irregular
*Lasso
lasso logit has_irreg mat_age marital_stat phone car i.housing rooms i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass_ipw i.bfed_dur fai_long mat_ethnicity
lassocoef
logistic has_irreg mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_ever epds i.parity i.mated findiff_ipw i.bfed_dur fai_long 
estat gof				//x(4562)^2 = 4599.83; p = 0.3439
estat gof, group(10)	//x(8)^2 = 6.34; p = 0.6088

**PMS
*Lasso
lasso logit has_pms mat_age marital_stat phone car i.housing rooms i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass_ipw i.bfed_dur fai_long mat_ethnicity
lassocoef
logistic has_pms mat_age car i.housing i.crowding i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass_ipw i.bfed_dur 
estat gof				//x(4618)^2 = 4625.70; p = 0.4653
estat gof, group(10)	//x(8)^2 = 4.19; p = 0.8393

**LR tests to compare lasso models with fully inclusive models (same for each outcomes - All variables in the inclusive model other than rooms and mat_ethnicity)
*Need the same observations (i.e. all IPW data)
mark ipw 
markout ipw mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass_ipw bfed_dur fai_long
*Pub outcomes - p = 0.9210
logistic has_pub mat_age phone car i.housing i.crowding i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass_ipw i.bfed_dur fai_long if ipw==1
estimates store pub_lasso 
logistic has_pub mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass_ipw i.bfed_dur fai_long if ipw==1
estimate store pub_full
lrtest pub_lasso pub_full 
*Length - p = 0.9266
logistic has_length mat_age phone car i.housing i.crowding dbl_glaze i.first_preg smoke_ever epds i.parity i.mated i.bfed_dur fai_long if ipw==1
estimate store length_lasso
logistic has_length mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass_ipw i.bfed_dur fai_long if ipw==1
estimate store length_full
lrtest length_lasso length_full 
*Irregular - p = 0.8849
logistic has_irreg mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_ever epds i.parity i.mated findiff_ipw i.bfed_dur fai_long if ipw==1
estimates store irreg_lasso
logistic has_irreg mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass_ipw i.bfed_dur fai_long if ipw==1
estimate store irreg_full
lrtest irreg_lasso irreg_full
*PMS - p = 0.8871
logistic has_pms mat_age car i.housing i.crowding i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass_ipw i.bfed_dur  if ipw==1
estimates store pms_lasso
logistic has_pms mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass_ipw i.bfed_dur fai_long if ipw==1
estimate store pms_full
lrtest pms_lasso pms_full
*Check gof
estimates clear
foreach var in pub length irreg pms {
	quietly logistic has_`var' mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass_ipw i.bfed_dur fai_long
	estat gof
	estat gof, group(10)
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Complete Case/Descriptives/IPW_Vars_Patterns", append
**UPDATED MISSINGNESS PATTERNS - two variables (rooms and maternal ethnicity) are not being used in any of the models so remove them

mdesc mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass_ipw bfed_dur fai_long
*Number of missing variables
egen ipw_vars_short=rmiss2(mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass_ipw bfed_dur fai_long)
tab ipw_vars_short

*Collinearity checks
pwcorr mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass_ipw bfed_dur fai_long

log close


********************************************************************************
**#MI/IPW SENSITIVITY PREP/FIGURING OUT*****************************************
********************************************************************************
tab1 has_pub has_length has_irreg has_pms 
*pub=2844
*length=1384
*irreg=2315
*pms=1452

*Main approach imputes all data in 5894 ppts
tab impute 

*How many have all IPW variables = 4449
egen ipws=rmiss2(mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass_ipw bfed_dur fai_long)
tab ipws if impute==1
di 4449/5894 //75% or the first impute to sample 

*Are the people missing some of the IPW variables ppts with or without outcome data? I.e. how many can we bring back with MI
recode ipws (0=0 "All available") (1/8=1 "Missing any"), gen(ipw_bin)
*This will be outcome dependent 
bysort has_pub: tab ipw_bin if impute==1 //70% missing outcome have all IPW variables and 80% with outcome data have all 
*would lose 915 from the 5894 if we imputed only in those with outcome data 
bysort has_length: tab ipw_bin if impute==1 //74% missing outcome have all IPW and 80% with outcome data have all
*would lose 1,183 from the 5894 we imputed 
bysort has_irreg: tab ipw_bin if impute==1 //71% have all if missing outcome and 82% with outcome have all
*would lose 1,053
bysort has_pms: tab ipw_bin if impute==1 //73% have all if missing outcome and 83% have all with outcome data
*would lose 1,210

*What if we just did complete case IPW? So ppts with all IPW vars, plus exposures and confounders
tab sep if ipw_bin==0 & ethnicity!=.
*What would the outcome Ns be?
tab has_pub if sep==0 & ipw_bin==0 & ethnicity!=.
tab has_length if sep==0 & ipw_bin==0 & ethnicity!=.
tab has_irreg if sep==0 & ipw_bin==0 & ethnicity!=.
tab has_pms if sep==0 & ipw_bin==0 & ethnicity!=.


********************************************************************************
**#MULTIPLE IMPUTATION**********************************************************
********************************************************************************

**Impute to sample?
*At least one SEP exposure (5960; 89.48%)
egen sep=rmiss2(highed sclass findiff)
recode sep (0/2=1 "At least one available") (3=0 "Missing all"), gen(sep_bin)
*At least half of IPW vars (17 vars - max 8 missing // 6253; 93.87%)
egen ipw_vars_short=rmiss2(mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass_ipw bfed_dur fai_long)
recode ipw_vars_short (0/8=1 "At least half available") (9/16=0 "More than half missing"), gen(ipw_vars_miss)
*Both (5894; 88.49%)
gen impute=0
replace impute=1 if sep_bin==1 & ipw_vars_miss==1

*Check this hasn't disproportionately got rid of people with certain variables (i.e. is reductio in % missing roughly similar across all variables)
replace pub_use=. if pub_use==0 
mdesc highed sclass findiff ethnicity menarche pub_use length_both irreg pms_bin if impute==1
mdesc mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass_ipw bfed_dur fai_long if impute==1
*How many imputations? % missing data - but which variables do we consider here?
drop if impute==0
mark basic_impute 		//exposures, ethnicity, IPW vars
markout basic_impute highed sclass findiff ethnicity mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass_ipw bfed_dur fai_long
mark basic_impute_menarche 		//exposures, ethnicity, IPW vars AND menarche
markout basic_impute_menarche highed sclass findiff ethnicity mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass_ipw bfed_dur fai_long menarche
mark outcomes_impute 		//exposures, ethnicity, IPW vars AND OUTCOMES
markout outcomes_impute highed sclass findiff ethnicity mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass_ipw bfed_dur fai_long pub_use length_both irreg pms_bin
mark outcomes_impute_menarche 		//exposures, ethnicity, IPW vars AND OUTCOMES AND MENARCHE
markout outcomes_impute_menarche highed sclass findiff ethnicity mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass_ipw bfed_dur fai_long pub_use length_both irreg pms_bin menarche

tab1 basic_impute basic_impute_menarche outcomes_impute outcomes_impute_menarche
*Basic - 25.64% missing
*Plus menarche - 50.29% missing
*Plus outcomes (and menarche; same) - 93.47% missing 

*So at the impute to sample of 5,894 - what would the analysis Ns be?
count if has_pub==1		//2757
count if has_length==1	//1345
count if has_irreg==1	//2247
count if has_pms==1		//1421


*Focus dataset to relevant variables (not including menarche [or has_menarche] anymore)
keep highed sclass findiff ///
	 pain_both heavy_both days_bin_both length_bin_both irreg pms_bin ///
	 ethnicity ///
	 has_pub has_length has_irreg has_pms  ///
	 mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long ///
	 aln 

rename pain_both pain
rename heavy_both heavy 
rename days_bin_both days_bin
rename length_bin_both length_bin
	 
**Missing data patterns
*Complete 
mdesc has_pub has_length has_irreg has_pms mat_age
*Substantive variables (missing)
mdesc highed sclass findiff pain heavy days_bin length_bin irreg pms_bin ethnicity  
*IPW variables (missing)
mdesc marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long

**Set up imputation
mi set flong
mi register imputed highed sclass findiff pain heavy days_bin length_bin irreg pms_bin ethnicity marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long
mi register regular has_pub has_length has_irreg has_pms mat_age aln

**Dryrun
mi impute chained (ologit, omit(i.mated)) highed ///
				  (ologit) housing crowding first_preg parity bfed_dur ///
				  (ologit, omit(i.highed)) mated ///
				  (logit) sclass ethnicity marital_stat phone car dbl_glaze smoke_preg smoke_ever pain heavy days_bin length_bin irreg pms_bin ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (pmm, knn(5)) epds fai_long ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_pub has_length has_irreg has_pms mat_age,  dryrun

**Trace plot version
mi impute chained (ologit, omit(i.mated)) highed ///
				  (ologit) housing crowding first_preg parity bfed_dur ///
				  (ologit, omit(i.highed)) mated ///
				  (logit) sclass ethnicity marital_stat phone car dbl_glaze smoke_preg smoke_ever pain heavy days_bin length_bin irreg pms_bin ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (pmm, knn(5)) epds fai_long ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_pub has_length has_irreg has_pms mat_age, burnin(100) rseed(928364) dots chainonly noisily showcommand savetrace("/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Analysis/IPW/MI_IPWVars_Trace.dta", replace) 

*Trace checks
use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Analysis/IPW/MI_IPWVars_Trace.dta", clear
			 
describe 
tsset iter

cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Analysis/Imputation/Trace"

foreach cov in highed sclass findiff pain heavy days_bin length_bin irreg pms_bin ethnicity marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long {
	tsline `cov'_mean, title(Mean imputed values of `cov') legend(off) 
	graph export `cov'_mean.png, replace
	tsline `cov'_sd, title(Standard deviation imputed values of `cov') legend(off)
	graph export `cov'_sd.png, replace
}				  


*Everything looks relatively stable - especially by 50 burnins so go with this. 

**Run imputation model		  
mi impute chained (ologit, omit(i.mated)) highed ///
				  (ologit) housing crowding first_preg parity bfed_dur ///
				  (ologit, omit(i.highed)) mated ///
				  (logit) sclass ethnicity marital_stat phone car dbl_glaze smoke_preg smoke_ever pain heavy days_bin length_bin irreg pms_bin ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (pmm, knn(5)) epds fai_long ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_pub has_length has_irreg has_pms mat_age, add(50) burnin(50) rseed(928364) dots 
				  
*Save
save "/Volumes/157/working/data/G1_SEP_Imputed_dataset.dta", replace
*Open
use "/Volumes/157/working/data/G1_SEP_Imputed_dataset.dta", clear		  

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Analysis/Imputation/Post_Imp_Checks", replace
		  
*Checks
label define imputed 0"Observed" 1"Imputed", replace
gen imputed=0 if _mi_m==0
replace imputed=1 if _mi_m>0
label values imputed imputed 

*Proportions
foreach var in highed sclass findiff ethnicity marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever parity mated bfed_dur {
	tab `var' imputed, row col
}
*Means
foreach var in epds findiff_ipw fai_long {
	summ `var' if imputed==0, det
	summ `var' if imputed==1, det
}

log close

*Distributions 
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Analysis/Imputation"

foreach var in epds findiff_ipw fai_long {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck.png, replace 
}

**FMI and Mcerror checks
foreach var in pain heavy days_bin {
	replace `var'=. if has_pub==0
}
replace length_bin=. if has_length==0
replace irreg=. if has_irreg==0
replace pms_bin=. if has_pms==0

*Loop through models now
foreach exp in highed sclass findiff {
	foreach out in pain heavy days_bin length_bin irreg pms_bin {
		mi estimate, mcerror : logistic `out' `exp' ethnicity
	}
}
*Across all models, largest FMI: 0.0575
*Mcerror concerns - all good 

********************************************************************************
**#DERIVING WEIGHTS*************************************************************
********************************************************************************
/*Open
use "/Volumes/157/working/data/G1_SEP_Imputed_dataset.dta", clear	
*Deletion approach
foreach var in pain heavy days_bin {
	replace `var'=. if has_pub==0
}
replace length_bin=. if has_length==0
replace irreg=. if has_irreg==0
replace pms_bin=. if has_pms==0
replace menarche=. if has_menarche==0*/

**Get IPW for each imputed dataset and give same throughout datasets
*Pub
forvalues j = 1/50 {
	logistic has_pub mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if _mi_m==`j'
	predict pubp`j' if _mi_m==`j'
	}
*Length
forvalues j = 1/50 {
	logistic has_length mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if _mi_m==`j'
	predict lengthp`j' if _mi_m==`j'
	} 
*Irreg
forvalues j = 1/50 {
	logistic has_irreg mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if _mi_m==`j'
	predict irregp`j' if _mi_m==`j'
	}
*PMS
forvalues j = 1/50 {
	logistic has_pms mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if _mi_m==`j'
	predict pmsp`j' if _mi_m==`j'
	}	
*Consistent name across datasets
foreach var in pub length irreg pms {
	gen `var'p = `var'p1
	forvalues j = 2/50 {
		replace `var'p = `var'p`j' if `var'p==.
	}
}
*Create probability and weights
foreach var in pub length irreg pms {
	gen prob_`var'=`var'p if has_`var'==1
	replace prob_`var'=1-`var'p if has_`var'==0
	gen ipw_`var'=1/prob_`var'
}

*Quick check (first 15 obs of imputation 1)
list _mi_m has_pub pubp prob_pub ipw_pub in 5895/5910
*Imputation 2
list _mi_m has_pub pubp prob_pub ipw_pub in 11789/11800

*Tidy up (remove original p per dataset variables as have overall one with all information)
forvalues j = 1/50 {
	foreach var in pub length irreg pms {
		drop `var'p`j'
	}
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Analysis/IPW/Summary_Weights", replace
*Summary of weights
foreach var in pub length irreg pms {
	summ ipw_`var', det
}
log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Analysis/IPW"

foreach var in pub length irreg pms {
	histogram ipw_`var'
	graph export `var'_weights.png, replace 
}	
 
*Replace dataset now outcomes deleted and weights derived
save "/Volumes/157/working/data/G1_SEP_Imputed_dataset.dta", replace

********************************************************************************
**#WEIGHTED ANALYSIS************************************************************
********************************************************************************

*Open
clear all
set maxvar 30000
use "/Volumes/157/working/data/G1_SEP_Imputed_dataset.dta", clear	

*Descriptives
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI and Weighted/Weighted_Descriptives", replace

**Simple - full sample 
*Exposures
foreach var in highed sclass findiff {
	mi estimate: proportion `var'
}
*Confounders
mi estimate: proportion ethnicity
*Outcomes
foreach exp in pain heavy days_bin length_bin irreg pms_bin {
	tab `exp' if imputed==0
}

**Simple - analysis samples
*Exposures and ethnicity
foreach var in highed sclass findiff ethnicity {
	foreach sample in pub length irreg pms {
		mi estimate: proportion `var' if has_`sample'==1
	}
}

**Cross tabs - analysis samples
foreach cov in highed sclass findiff ethnicity {
	foreach out in pain heavy days_bin length_bin irreg pms_bin  {
		mi estimate: proportion `cov', over(`out')	
	}
}

log close

*Main models
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI and Weighted/Main_Weighted_Results", replace

foreach out in pain heavy days_bin {
	foreach exp in highed sclass findiff {
		mi estimate, or: logistic `out' i.`exp' [pw=ipw_pub]
		mi estimate, or: logistic `out' i.`exp' ethnicity [pw=ipw_pub]
	}
}

foreach exp in highed sclass findiff {
		mi estimate, or: logistic length_bin i.`exp' [pw=ipw_length]
		mi estimate, or: logistic length_bin i.`exp' ethnicity [pw=ipw_length]
	}

foreach exp in highed sclass findiff {
		mi estimate, or: logistic irreg i.`exp' [pw=ipw_irreg]
		mi estimate, or: logistic irreg i.`exp' ethnicity [pw=ipw_irreg]
	}
	
foreach exp in highed sclass findiff {
		mi estimate, or: logistic pms_bin i.`exp' [pw=ipw_pms]
		mi estimate, or: logistic pms_bin i.`exp' ethnicity [pw=ipw_pms]
	}

log close

**Truncated weights 

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI and Weighted/Truncated_Weighted_Results", replace

*Pub outcomes
summ ipw_pub, det
local pub95=r(p95)			
local pub99=r(p99)	
gen trunc95_pub=ipw_pub
replace trunc95_pub=`pub95' if trunc95_pub>`pub95' & trunc95_pub<.  
gen trunc99_pub=ipw_pub
replace trunc99_pub=`pub99' if trunc99_pub>`pub99' & trunc99_pub<.
			  
foreach out in pain heavy days_bin {
	foreach exp in highed sclass findiff {
		di "95th percentile weights"
		mi estimate, or: logistic `out' i.`exp' [pw=trunc95_pub]
		mi estimate, or: logistic `out' i.`exp' ethnicity [pw=trunc95_pub]
		di "99th percentile weights"
		mi estimate, or: logistic `out' i.`exp' [pw=trunc99_pub]
		mi estimate, or: logistic `out' i.`exp' ethnicity [pw=trunc99_pub]
	}
}

*Length
summ ipw_length, det
local leng95=r(p95)			
local leng99=r(p99)	
gen trunc95_leng=ipw_length
replace trunc95_leng=`leng95' if trunc95_leng>`leng95' & trunc95_leng<.  
gen trunc99_leng=ipw_length
replace trunc99_leng=`leng99' if trunc99_leng>`leng99' & trunc99_leng<.

foreach exp in highed sclass findiff {
	di "95th percentile weights"
	mi estimate, or: logistic length_bin i.`exp' [pw=trunc95_leng]
	mi estimate, or: logistic length_bin i.`exp' ethnicity [pw=trunc95_leng]
	di "99th percentile weights"
	mi estimate, or: logistic length_bin i.`exp' [pw=trunc99_leng]
	mi estimate, or: logistic length_bin i.`exp' ethnicity [pw=trunc99_leng]
}
	
*Irregular
summ ipw_irreg, det
local irreg95=r(p95)			
local irreg99=r(p99)	
gen trunc95_irreg=ipw_irreg
replace trunc95_irreg=`irreg95' if trunc95_irreg>`irreg95' & trunc95_irreg<.  
gen trunc99_irreg=ipw_irreg
replace trunc99_irreg=`irreg99' if trunc99_irreg>`irreg99' & trunc99_irreg<.

foreach exp in highed sclass findiff {
	di "95th percentile weights"
	mi estimate, or: logistic irreg i.`exp' [pw=trunc95_irreg]
	mi estimate, or: logistic irreg i.`exp' ethnicity [pw=trunc95_irreg]
	di "99th percentile weights"
	mi estimate, or: logistic irreg i.`exp' [pw=trunc99_irreg]
	mi estimate, or: logistic irreg i.`exp' ethnicity [pw=trunc99_irreg]
}

*PMS
summ ipw_pms, det
local pms95=r(p95)			
local pms99=r(p99)	
gen trunc95_pms=ipw_pms
replace trunc95_pms=`pms95' if trunc95_pms>`pms95' & trunc95_pms<.  
gen trunc99_pms=ipw_pms
replace trunc99_pms=`pms99' if trunc99_pms>`pms99' & trunc99_pms<.
	
foreach exp in highed sclass findiff {
	di "95th percentile weights"
	mi estimate, or: logistic pms_bin i.`exp' [pw=trunc95_pms]
	mi estimate, or: logistic pms_bin i.`exp' ethnicity [pw=trunc95_pms]
	di "99th percentile weights"
	mi estimate, or: logistic pms_bin i.`exp' [pw=trunc99_pms]
	mi estimate, or: logistic pms_bin i.`exp' ethnicity [pw=trunc99_pms]
}
	  
log close			 

 
**ALTERNATIVE EDUCATION (CONTINUOUS OR CATEGORICAL?)
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Education_ContorCat_Analysis_MainMI", replace

*Main results - MI and WEIGHTED
*Continuous education results only (no LR or similar test)
 
foreach out in pain heavy days_bin {
	mi estimate, or: logistic `out' highed [pw=ipw_pub]
	mi estimate, or: logistic `out' highed ethnicity [pw=ipw_pub]
}

mi estimate, or: logistic length_bin highed [pw=ipw_length]
mi estimate, or: logistic length_bin highed ethnicity [pw=ipw_length]

mi estimate, or: logistic irreg highed [pw=ipw_irreg]
mi estimate, or: logistic irreg highed ethnicity [pw=ipw_irreg]

mi estimate, or: logistic pms_bin highed [pw=ipw_pms]
mi estimate, or: logistic pms_bin highed ethnicity [pw=ipw_pms]
 
log close
 

 
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Education_ContorCat_Analysis_MainMI", append

*Trunacted weights results

*Pub outcomes
summ ipw_pub, det
local pub95=r(p95)			
local pub99=r(p99)	
gen trunc95_pub=ipw_pub
replace trunc95_pub=`pub95' if trunc95_pub>`pub95' & trunc95_pub<.  
gen trunc99_pub=ipw_pub
replace trunc99_pub=`pub99' if trunc99_pub>`pub99' & trunc99_pub<.
			  
foreach out in pain heavy days_bin {
		di "95th percentile weights"
		mi estimate, or: logistic `out' highed [pw=trunc95_pub]
		mi estimate, or: logistic `out' highed ethnicity [pw=trunc95_pub]
		di "99th percentile weights"
		mi estimate, or: logistic `out' highed [pw=trunc99_pub]
		mi estimate, or: logistic `out' highed ethnicity [pw=trunc99_pub]
}

*Length
summ ipw_length, det
local leng95=r(p95)			
local leng99=r(p99)	
gen trunc95_leng=ipw_length
replace trunc95_leng=`leng95' if trunc95_leng>`leng95' & trunc95_leng<.  
gen trunc99_leng=ipw_length
replace trunc99_leng=`leng99' if trunc99_leng>`leng99' & trunc99_leng<.

di "95th percentile weights"
mi estimate, or: logistic length_bin highed [pw=trunc95_leng]
mi estimate, or: logistic length_bin highed ethnicity [pw=trunc95_leng]
di "99th percentile weights"
mi estimate, or: logistic length_bin highed [pw=trunc99_leng]
mi estimate, or: logistic length_bin highed ethnicity [pw=trunc99_leng]
	
*Irregular
summ ipw_irreg, det
local irreg95=r(p95)			
local irreg99=r(p99)	
gen trunc95_irreg=ipw_irreg
replace trunc95_irreg=`irreg95' if trunc95_irreg>`irreg95' & trunc95_irreg<.  
gen trunc99_irreg=ipw_irreg
replace trunc99_irreg=`irreg99' if trunc99_irreg>`irreg99' & trunc99_irreg<.

di "95th percentile weights"
mi estimate, or: logistic irreg highed [pw=trunc95_irreg]
mi estimate, or: logistic irreg highed ethnicity [pw=trunc95_irreg]
di "99th percentile weights"
mi estimate, or: logistic irreg highed [pw=trunc99_irreg]
mi estimate, or: logistic irreg highed ethnicity [pw=trunc99_irreg]


*PMS
summ ipw_pms, det
local pms95=r(p95)			
local pms99=r(p99)	
gen trunc95_pms=ipw_pms
replace trunc95_pms=`pms95' if trunc95_pms>`pms95' & trunc95_pms<.  
gen trunc99_pms=ipw_pms
replace trunc99_pms=`pms99' if trunc99_pms>`pms99' & trunc99_pms<.
	
di "95th percentile weights"
mi estimate, or: logistic pms_bin highed [pw=trunc95_pms]
mi estimate, or: logistic pms_bin highed ethnicity [pw=trunc95_pms]
di "99th percentile weights"
mi estimate, or: logistic pms_bin highed [pw=trunc99_pms]
mi estimate, or: logistic pms_bin highed ethnicity [pw=trunc99_pms]
	  
			  
log close			 

**#Categorical education with overall p value 

*Create truncated weights if needed
summ ipw_pub, det
local pub95=r(p95)			
local pub99=r(p99)	
gen trunc95_pub=ipw_pub
replace trunc95_pub=`pub95' if trunc95_pub>`pub95' & trunc95_pub<.  
gen trunc99_pub=ipw_pub
replace trunc99_pub=`pub99' if trunc99_pub>`pub99' & trunc99_pub<.

summ ipw_length, det
local leng95=r(p95)			
local leng99=r(p99)	
gen trunc95_leng=ipw_length
replace trunc95_leng=`leng95' if trunc95_leng>`leng95' & trunc95_leng<.  
gen trunc99_leng=ipw_length
replace trunc99_leng=`leng99' if trunc99_leng>`leng99' & trunc99_leng<.

summ ipw_irreg, det
local irreg95=r(p95)			
local irreg99=r(p99)	
gen trunc95_irreg=ipw_irreg
replace trunc95_irreg=`irreg95' if trunc95_irreg>`irreg95' & trunc95_irreg<.  
gen trunc99_irreg=ipw_irreg
replace trunc99_irreg=`irreg99' if trunc99_irreg>`irreg99' & trunc99_irreg<.

summ ipw_pms, det
local pms95=r(p95)			
local pms99=r(p99)	
gen trunc95_pms=ipw_pms
replace trunc95_pms=`pms95' if trunc95_pms>`pms95' & trunc95_pms<.  
gen trunc99_pms=ipw_pms
replace trunc99_pms=`pms99' if trunc99_pms>`pms99' & trunc99_pms<.


log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Education_Categorical_OverallP_MI", replace

**MAIN RESULTS
 
foreach out in pain heavy days_bin {
	quietly mi estimate, or post: logistic `out' i.highed [pw=ipw_pub]
	di "CRUDE"
	testparm i.highed
	quietly mi estimate, or post: logistic `out' i.highed ethnicity [pw=ipw_pub]
	di "ADJUSTED"
	testparm i.highed
}

quietly mi estimate, or post: logistic length_bin i.highed [pw=ipw_length]
di "CRUDE"
testparm i.highed
quietly mi estimate, or post: logistic length_bin i.highed ethnicity [pw=ipw_length]
di "ADJUSTED"
testparm i.highed

quietly mi estimate, or post: logistic irreg i.highed [pw=ipw_irreg]
di "CRUDE"
testparm i.highed
quietly mi estimate, or post: logistic irreg i.highed ethnicity [pw=ipw_irreg]
di "ADJUSTED"
testparm i.highed

quietly mi estimate, or post: logistic pms_bin i.highed [pw=ipw_pms]
di "CRUDE"
testparm i.highed
quietly mi estimate, or post: logistic pms_bin i.highed ethnicity [pw=ipw_pms]
di "ADJUSTED"
testparm i.highed

**TRUNCATED RESULTS

*Pub outcomes
foreach out in pain heavy days_bin {
	quietly mi estimate, or post: logistic `out' i.highed [pw=trunc95_pub]
	di "95th: CRUDE"
	testparm i.highed
	quietly mi estimate, or post: logistic `out' i.highed ethnicity [pw=trunc95_pub]
	di "95th: ADJUSTED"
	testparm i.highed
	quietly mi estimate, or post: logistic `out' i.highed [pw=trunc99_pub]
	di "99th: CRUDE"
	testparm i.highed
	quietly mi estimate, or post: logistic `out' i.highed ethnicity [pw=trunc99_pub]
	di "99th: ADJUSTED"
	testparm i.highed
}

*Length
quietly mi estimate, or post: logistic length_bin i.highed [pw=trunc95_leng]
di "95th: CRUDE"
testparm i.highed
quietly mi estimate, or post: logistic length_bin i.highed ethnicity [pw=trunc95_leng]
di "95th: ADJUSTED"
testparm i.highed
quietly mi estimate, or post: logistic length_bin i.highed [pw=trunc99_leng]
di "99th: CRUDE"
testparm i.highed
quietly mi estimate, or post: logistic length_bin i.highed ethnicity [pw=trunc99_leng]
di "99th: ADJUSTED"
testparm i.highed

	
*Irregular
quietly mi estimate, or post: logistic irreg i.highed [pw=trunc95_irreg]
di "95th: CRUDE"
testparm i.highed
quietly mi estimate, or post: logistic irreg i.highed ethnicity [pw=trunc95_irreg]
di "95th: ADJUSTED"
testparm i.highed
quietly mi estimate, or post: logistic irreg i.highed [pw=trunc99_irreg]
di "99th: CRUDE"
testparm i.highed
quietly mi estimate, or post: logistic irreg i.highed ethnicity [pw=trunc99_irreg]

*PMS
quietly mi estimate, or post: logistic pms_bin i.highed [pw=trunc95_pms]
di "95th: CRUDE"
testparm i.highed
quietly mi estimate, or post: logistic pms_bin i.highed ethnicity [pw=trunc95_pms]
di "95th: ADJUSTED"
testparm i.highed
quietly mi estimate, or post: logistic pms_bin i.highed [pw=trunc99_pms]
di "99th: CRUDE"
testparm i.highed
quietly mi estimate, or post: logistic pms_bin i.highed ethnicity [pw=trunc99_pms]
di "99th: ADJUSTED"
testparm i.highed

log close

