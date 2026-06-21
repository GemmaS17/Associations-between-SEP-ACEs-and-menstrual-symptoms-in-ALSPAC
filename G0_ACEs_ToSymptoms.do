***G0 ACEs to Symptoms

*Dataset
clear all
set maxvar 3000

use "/Volumes/157/working/data/ACEtoSymptoms_G0_data.dta", replace

********************************************************************************
**#STARTING SAMPLE**************************************************************
********************************************************************************
drop if mz005l==1
drop if in_core==2 
drop if qlet=="B" 
drop if in_core==.a | in_core==. 

********************************************************************************
**#DEFINING EXPOSURES***********************************************************
********************************************************************************
label define coded 1"Yes" 0"No", replace

**PHYSICAL ABUSE - c416a ( physcrul in the do.file) and h136
recode c416a (1=1) (2=0) (else=.), gen (physcrul)
label values physcrul coded
recode h136 (1 2 =1) (3=0) (else=.), gen (mphysab)
label values mphysab coded
*Combine 
gen phys_abuse=.
replace phys_abuse=0 if physcrul==0 | mphysab==0
replace phys_abuse=1 if physcrul==1 | mphysab==1
label values phys_abuse coded
label variable phys_abuse "Physical abuse"

**SEXUAL ABUSE - c888 c948 c968 c868 c928
gen sex_abuse=.
replace sex_abuse=0 if c888==3 | c948==3 | c968==3 | c868==3 | c928==3 
replace sex_abuse=1 if inrange(c888,1,2) | inrange(c948,1,2) | inrange(c968,1,2) | inrange(c868,1,2) | inrange(c928,1,2)
label values sex_abuse coded
label variable sex_abuse "Sexual abuse"

**EMOTIONAL ABUSE - c420a (pemocrul in this do.file)
recode c420a (1=1) (2=0) (else=.), gen (emot_abuse)
label values emot_abuse coded
label variable emot_abuse "Emotional abuse"

**EMOTIONAL NEGLECT - h134 (emonegl in this do.file)
recode h134 (1 2=1) (3=0) (else=.), gen (emot_negl)
label values emot_negl coded
label variable emot_negl "Emotional neglect"

**SUBSTANCE ABUSE IN HOUSEHOLD - d528 d529 (malcohol in this do.file) d578 d579 (falcohol in this do.file)
*Mother
gen m_alc_prob= d528
replace m_alc=1 if d529==1
replace m_alc=. if m_alc==-1
replace m_alc=. if m_alc==9
replace m_alc=2 if d529==2 & m_alc==.
recode m_alc (2=0)
tab m_alc
label variable m_alc_prob "nat mother or mother fig had alcohol prob"
label values m_alc_prob coded
*Father
gen f_alc_prob= d578
replace f_alc=1 if d579==1
replace f_alc=. if f_alc==-1
replace f_alc=. if f_alc==9
replace f_alc=2 if d579==2 & f_alc==.
recode f_alc (2=0)
tab f_alc
label variable f_alc_prob "nat father or father fig had alcohol prob"
label values f_alc_prob coded
*Combine
gen substance =. 
replace substance = 0 if m_alc_prob == 0 & f_alc_prob == 0
replace substance = 1 if m_alc_prob == 1 | f_alc_prob  == 1
label values substance coded
label variable substance "Substance abuse in the household"

**VIOLENCE BETWEEN PARENTS - h137a
recode h137a (1/3=1) (4=0) (else=.), gen (viol_prnt)
label values viol_prnt coded
label variable viol_prnt "Violence between parents"

**PARENTAL MENTAL HEALTH - d580 d581 (f_schizo in this do.file) d530 d531( m_schizo in this do.file) d586 d587 (f_dep in this do.file) d536 d537 (m_dep in this do.file) c423a
*Mother
gen m_schizo= d530
replace m_schizo=1 if d531==1
replace m_schizo=. if m_schizo==-1
replace m_schizo=. if m_schizo==9
replace m_schizo=2 if d531==2 & m_schizo==.
recode m_schizo (2=0)
tab m_schizo
label variable m_schizo "nat mother or mother fig had schizophrenia"
gen m_dep= d536
replace m_dep=1 if d537==1
replace m_dep=. if m_dep==-1
replace m_dep=. if m_dep==9
replace m_dep=2 if d537==2 & m_dep==.
recode m_dep (2=0)
tab m_dep
label variable m_dep "nat mother or mother fig had depression"
*Father
gen f_schizo= d580
replace f_schizo=1 if d581==1
replace f_schizo=. if f_schizo==-1
replace f_schizo=. if f_schizo==9
replace f_schizo=2 if d581==2 & f_schizo==.
recode f_schizo (2=0)
tab f_schizo
label variable f_schizo "nat father or father fig had schizophrenia"
gen f_dep= d586
replace f_dep=1 if d587==1
replace f_dep=. if f_dep==-1
replace f_dep=. if f_dep==9
replace f_dep=2 if d587==2 & f_dep==.
recode f_dep (2=0)
tab f_dep
label variable f_dep "nat father or father fig had depression"
*Either
gen mental_ill=.
replace mental_ill=0 if  m_schizo==0| f_schizo==0| m_dep==0 |f_dep==0
replace mental_ill=1 if  m_schizo==1| f_schizo==1| m_dep==1 |f_dep==1
label values mental_ill coded

*Combine
gen prnt_mh=.
replace prnt_mh=0 if c423a==0 | mental_ill==0
replace prnt_mh=1 if c423a==1 | mental_ill==1
label values prnt_mh coded
label variable prnt_mh "Parental mental ill-health"


**PARENTAL SEPARATION - c417 (separatd in this do.file)
recode c417 (1/4=1) (5=0) (else=.), gen (separatd)
label values separatd coded
label variable separatd "Parental separation"


*Create ACE score*
egen ace_score=rowtotal(phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd)
replace ace_score =. if phys_abuse ==. | sex_abuse ==. | emot_abuse ==. | emot_negl ==. | substance ==. | viol_prnt ==. | prnt_mh ==. | separatd ==.

gen ace_four = ace_score
recode ace_four (0=0) (1=1) (2=2) (3=3) (4/8=4)
tab ace_four
label define scores 0"none" 1"1" 2"2" 3"3" 4"4 or more", replace
label values ace_four scores
label variable ace_four "Number of ACEs"


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

*Sample with all 4 of these = 10,141
count if pain!=. & heavy!=. & days!=. & irreg!=. 

**Length from n
label define length_lbl 0"Normal (24-38)" 1"Frequent (<24)" 2"Infrequent (>38)", replace
recode n1122 (0/9=.) (24/38=0) (10/23=1) (39/140=2) (else=.), gen (length_cat)
label values length_cat length_lbl
recode length_cat (0=0 "Normal (24-38)") (1/2=1 "Short or Long"), gen (length)
*Sample with data = 5,110
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

egen outcomes=rmiss2(pain heavy days irreg length pms_symp)
tab outcomes

********************************************************************************
**#COVARIATES*******************************************************************
********************************************************************************

***CONFOUNDERS******************************************************************

***Ethnicity
recode c800 (-1=.) (1=0 "White") (2/9=1 "Non-White"), gen (ethnicity)

***Age 
recode mz028b (-10 -4 -2=.), gen (mat_age)

***Parental SEP
**Education (c686a mother; c706a father)
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

**Social class (maternal grandmother and maternal grandfather)
label define socclass_lbl 0"Non-manual" 1"Manual", replace
recode c_sc_mgm (1/3=0) (4/6=1), gen (matsclass) 
recode c_sc_mgf (1/3=0) (4/6=1), gen (patsclass)
*Highest
gen parentsclass=.
replace parentsclass=0 if matsclass==0 | patsclass==0
replace parentsclass=1 if matsclass==1 | patsclass==1
label values parentsclass socclass_lbl
label variable parentsclass "Highest parental social class"

***Age at menarche
recode d010 (-1 77=.), gen (menarche) //11256 responses
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

egen conf=rmiss2(ethnicity mat_age parentedu parentsclass menarche)
tab conf

***OTHERS FOR SENSITIVITY*******************************************************

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
count 
**Baseline confounder data 
egen base_miss=rmiss2(parentedu parentsclass ethnicity mat_age)
tab base_miss
*Individual drop 
count if mat_age==. 
count if mat_age!=. & ethnicity==. 
count if mat_age!=. & ethnicity!=. & parentedu==. 
count if mat_age!=. & ethnicity!=. & parentedu!=. & parentsclass==. 
**Exposure data
count if base_miss==0 & ace_four!=.
egen exp_baseconf=rmiss2(parentedu parentsclass ethnicity mat_age ace_four)
**Menarche 
count if base_miss==0 & ace_four!=. & menarche==.
egen exp_conf=rmiss2(parentedu parentsclass ethnicity mat_age ace_four menarche)
tab exp_conf
**Outcomes
egen multq_out=rmiss2(parentedu parentsclass ethnicity mat_age ace_four menarche pain heavy days irreg)
tab multq_out
count if exp_conf==0 & length!=.
count if exp_conf==0 & pms!=.

********************************************************************************
**#DESCRIPTIVES*****************************************************************
********************************************************************************
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Complete Case/Descriptives/Main_CC_Descriptives", replace

mark touse
markout touse parentedu parentsclass ethnicity mat_age ace_four menarche
mark multuse 
markout multuse parentedu parentsclass ethnicity mat_age ace_four menarche pain heavy days irreg

***SIMPLE DESCRIPTIVES 
**Exposures
foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_score ace_four {
	di "IN ALL OBSERVED DATA"
	tab `exp'
	di "IN MULTUSE SAMPLE"
	tab `exp' if multuse==1
	di "IN TOUSE SAMPLE"
	tab `exp' if touse==1
}
di "IN ALL OBSERVED DATA"
summ ace_score, det
di "IN MULTUSE SAMPLE"
summ ace_score if multuse==1, det 
di "IN TOUSE SAMPLE"
summ ace_score if touse==1, det

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
foreach cov in parentedu parentsclass ethnicity menarche_cat {
	di "IN ALL OBSERVED DATA"
	tab `cov'
	di "IN MULTUSE SAMPLE"
	tab `cov' if multuse==1
	di "IN TOUSE SAMPLE"
	tab `cov' if touse==1
}
foreach cov in mat_age menarche {
	di "IN ALL OBSERVED DATA"
	summ `cov', det
	di "IN MULTUSE SAMPLE"
	summ `cov' if multuse==1, det 
	di "IN TOUSE SAMPLE"
	summ `cov' if touse==1, det
}

**CROSS TAB DESCRIPTIVES
**Exposures and covariates by outcomes
foreach out in pain_cat pain heavy_cat heavy days_cat days irreg_cat irreg {
	foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four parentedu parentsclass ethnicity menarche_cat {
		di "IN ALL OBSERVED DATA"
		tab `exp' `out', row col
		di "IN MULTUSE SAMPLE"
		tab `exp' `out' if multuse==1, row col
	}
	foreach exp in ace_score mat_age menarche {
		di "IN ALL OBSERVED DATA"
		bysort `out': summ menarche, det
		di "IN MULTUSE SAMPLE"
		bysort `out': summ menarche if multuse==1, det
	}
}
foreach out in length_cat length pms_symp pms {
	foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four parentedu parentsclass ethnicity menarche_cat {
		di "IN ALL OBSERVED DATA"
		tab `exp' `out', row col
		di "IN TOUSE SAMPLE"
		tab `exp' `out' if multuse==1, row col
	}
	foreach exp in ace_score mat_age menarche {
		di "IN ALL OBSERVED DATA"
		bysort `out': summ menarche, det
		di "IN TOUSE SAMPLE"
		bysort `out': summ menarche if multuse==1, det
	}
}
**Covariates by exposures
foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four {
	foreach cov in parentedu parentsclass ethnicity menarche_cat{
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
	foreach cov in mat_age menarche {
		di "IN ALL OBSERVED DATA"
		bysort `exp': summ `cov', det
		di "IN MULTUSE SAMPLE"
		bysort `exp': summ `cov' if multuse==1, det
		di "IN TOUSE SAMPLE"
		bysort `exp': summ `cov' if touse==1, det
		di "IN LENGTH SAMPLE"
		bysort `exp': summ `cov' if touse==1 & length!=., det
		di "IN PMS SAMPLE"
		bysort `exp': summ `cov' if touse==1 & pms!=., det
	}
}

***MISSING DATA PATTERNS
*Exposures, outcomes, and covariates in all observed data (as above), those with covariates, sep_use restricted (as above) and in each CC analysis model
foreach var in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four parentedu parentsclass ethnicity menarche_cat pain_cat pain heavy_cat heavy days_cat days irreg_cat irreg length_cat length pms_symp pms {
	di "IN ALL OBSERVED DATA"
	tab `var'
	di "IN SAMPLE WITH EXPOSURE DATA"
	tab `var' if ace_four!=.
	di "IN TOUSE SAMPLE (EXPOSURE AND CONFOUNDER)"
	tab `var' if touse==1
	di "IN MULTUSE SAMPLE"
	tab `var' if multuse==1
	di "IN LENGTH SAMPLE"
	tab `var' if touse==1 & length!=.
	di "IN PMS SAMPLE"
	tab `var' if touse==1 & pms!=.
}

foreach var in ace_score mat_age menarche {
	di "IN ALL OBSERVED DATA"
	summ `var'
	di "IN SAMPLE WITH EXPOSURE DATA"
	summ `var' if ace_four!=., det
	di "IN TOUSE SAMPLE (EXPOSURE AND CONFOUNDER)"
	summ `var' if touse==1, det
	di "IN MULTUSE SAMPLE"
	summ `var' if multuse==1, det
	di "IN LENGTH SAMPLE"
	summ `var' if touse==1 & length!=., det
	di "IN PMS SAMPLE"
	summ `var' if touse==1 & pms!=., det
}

**Patterns and amount of missing
gen multquest=.
replace multquest=1 if pain!=. & heavy!=. & days!=. & irreg!=. 

mdesc phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four parentedu parentsclass ethnicity menarche mat_age multquest length pms 

*Number of missing variables
egen allvars=rmiss2(phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four parentedu parentsclass ethnicity menarche mat_age multquest length pms)
tab allvars

log close

********************************************************************************
**#CC ANALYSIS******************************************************************
********************************************************************************
drop touse multuse

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Complete Case/Analysis/Main_CC_Analysislog", replace

mark touse
markout touse parentedu parentsclass ethnicity mat_age ace_four menarche
mark multuse 
markout multuse parentedu parentsclass ethnicity mat_age ace_four menarche pain heavy days irreg

*Binary outcomes
foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd {
	foreach out in pain heavy days irreg {
		di "CRUDE `exp' and `out'"
		eststo `exp'_`out'c: logistic `out' `exp' if multuse==1
		di "ADJUSTED for parent SEP, ethnicity, and age `exp' and `out'"
		eststo `exp'_`out'adj: logistic `out' `exp' parentedu parentsclass ethnicity mat_age if multuse==1
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		eststo `exp'_`out'men: logistic `out' `exp' parentedu parentsclass ethnicity mat_age menarche if multuse==1
	}
	foreach out in length pms {
		di "CRUDE `exp' and `out'"
		eststo `exp'_`out'c: logistic `out' `exp' if touse==1
		di "ETHNICITY ADJUSTED `exp' and `out'"
		eststo `exp'_`out'adj: logistic `out' `exp' parentedu parentsclass ethnicity mat_age if touse==1
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		eststo `exp'_`out'men: logistic `out' `exp' parentedu parentsclass ethnicity mat_age menarche if touse==1
	}
}

foreach out in pain heavy days irreg {
	di "CRUDE score and `out'"
	eststo score_`out'c: logistic `out' i.ace_four if multuse==1
	di "ADJUSTED for parent SEP, ethnicity, and age score and `out'"
	eststo score_`out'adj: logistic `out' i.ace_four parentedu parentsclass ethnicity mat_age if multuse==1
	di "PLUS MENARCHE ADJUSTED score and `out'"
	eststo score_`out'men: logistic `out' i.ace_four parentedu parentsclass ethnicity mat_age menarche if multuse==1
}
foreach out in length pms {
	di "CRUDE score and `out'"
	eststo score_`out'c: logistic `out' i.ace_four if touse==1
	di "ETHNICITY ADJUSTED score and `out'"
	eststo score_`out'adj: logistic `out' i.ace_four parentedu parentsclass ethnicity mat_age if touse==1
	di "PLUS MENARCHE ADJUSTED score and `out'"
	eststo score_`out'men: logistic `out' i.ace_four parentedu parentsclass ethnicity mat_age menarche if touse==1
}

*Categorical outcomes
//note: not using length_cat as only 12 ppts in the infrequent category 
foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd {
	foreach out in pain_cat heavy_cat irreg_cat {
		di "CRUDE `exp' and `out'"
		eststo `exp'_`out'c: mlogit `out' `exp' if multuse==1, rr baseoutcome(0)
		di "ADJUSTED for parent SEP, ethnicity, and age `exp' and `out'"
		eststo `exp'_`out'adj: mlogit `out' `exp' parentedu parentsclass ethnicity mat_age if multuse==1, rr baseoutcome(0)
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		eststo `exp'_`out'men: mlogit `out' `exp' parentedu parentsclass ethnicity mat_age menarche if multuse==1, rr baseoutcome(0)
	}
	foreach out in days_cat {
		di "CRUDE `exp' and `out'"
		eststo `exp'_`out'c: mlogit `out' `exp' if multuse==1, rr baseoutcome(1)
		di "ADJUSTED for parent SEP, ethnicity, and age `exp' and `out'"
		eststo `exp'_`out'adj: mlogit `out' `exp' parentedu parentsclass ethnicity mat_age if multuse==1, rr baseoutcome(1)
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		eststo `exp'_`out'men: mlogit `out' `exp' parentedu parentsclass ethnicity mat_age menarche if multuse==1, rr baseoutcome(1)
	}
	foreach out in pms_symp {
		di "CRUDE `exp' and `out'"
		eststo `exp'_`out'c: mlogit `out' `exp' if touse==1, rr baseoutcome(0)
		di "ETHNICITY ADJUSTED `exp' and `out'"
		eststo `exp'_`out'adj: mlogit `out' `exp' parentedu parentsclass ethnicity mat_age if touse==1, rr baseoutcome(0)
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		eststo `exp'_`out'men: mlogit `out' `exp' parentedu parentsclass ethnicity mat_age menarche if touse==1, rr baseoutcome(0)
	}
}

foreach out in pain_cat heavy_cat irreg_cat {
	di "CRUDE score and `out'"
	eststo score_`out'c: mlogit `out' i.ace_four if multuse==1, rr baseoutcome(0)
	di "ADJUSTED for parent SEP, ethnicity, and age score and `out'"
	eststo score_`out'adj: mlogit `out' i.ace_four parentedu parentsclass ethnicity mat_age if multuse==1, rr baseoutcome(0)
	di "PLUS MENARCHE ADJUSTED score and `out'"
	eststo score_`out'men: mlogit `out' i.ace_four parentedu parentsclass ethnicity mat_age menarche if multuse==1, rr baseoutcome(0)
}
foreach out in days_cat {
	di "CRUDE score and `out'"
	eststo score_`out'c: mlogit `out' i.ace_four if multuse==1, rr baseoutcome(1)
	di "ADJUSTED for parent SEP, ethnicity, and age score and `out'"
	eststo score_`out'adj: mlogit `out' i.ace_four parentedu parentsclass ethnicity mat_age if multuse==1, rr baseoutcome(1)
	di "PLUS MENARCHE ADJUSTED score and `out'"
	eststo score_`out'men: mlogit `out' i.ace_four parentedu parentsclass ethnicity mat_age menarche if multuse==1, rr baseoutcome(1)
}
foreach out in pms_symp {
	di "CRUDE score and `out'"
	eststo score_`out'c: mlogit `out' i.ace_four if touse==1, rr baseoutcome(0)
	di "ETHNICITY ADJUSTED score and `out'"
	eststo score_`out'adj: mlogit `out' i.ace_four parentedu parentsclass ethnicity mat_age if touse==1, rr baseoutcome(0)
	di "PLUS MENARCHE ADJUSTED score and `out'"
	eststo score_`out'men: mlogit `out' i.ace_four parentedu parentsclass ethnicity mat_age menarche if touse==1, rr baseoutcome(0)
}

log close

*Output to excel
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Complete Case/Analysis"
capture erase "main_results.xls"                                   
*Physical abuse
estout phys_abuse_painc phys_abuse_painadj phys_abuse_painmen phys_abuse_pain_catc phys_abuse_pain_catadj phys_abuse_pain_catmen ///
	phys_abuse_heavyc phys_abuse_heavyadj phys_abuse_heavymen phys_abuse_heavy_catc phys_abuse_heavy_catadj phys_abuse_heavy_catmen ///
	phys_abuse_daysc phys_abuse_daysadj phys_abuse_daysmen phys_abuse_days_catc phys_abuse_days_catadj phys_abuse_days_catmen ///
	phys_abuse_irregc phys_abuse_irregadj phys_abuse_irregmen phys_abuse_irreg_catc phys_abuse_irreg_catadj phys_abuse_irreg_catmen ///
	phys_abuse_lengthc phys_abuse_lengthadj phys_abuse_lengthmen ///
	phys_abuse_pmsc phys_abuse_pmsadj phys_abuse_pmsmen phys_abuse_pms_sympc phys_abuse_pms_sympadj phys_abuse_pms_sympmen ///
	using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(physical abuse) 	
*Sexual abuse
estout sex_abuse_painc sex_abuse_painadj sex_abuse_painmen sex_abuse_pain_catc sex_abuse_pain_catadj sex_abuse_pain_catmen ///
	sex_abuse_heavyc sex_abuse_heavyadj sex_abuse_heavymen sex_abuse_heavy_catc sex_abuse_heavy_catadj sex_abuse_heavy_catmen ///
	sex_abuse_daysc sex_abuse_daysadj sex_abuse_daysmen sex_abuse_days_catc sex_abuse_days_catadj sex_abuse_days_catmen ///
	sex_abuse_irregc sex_abuse_irregadj sex_abuse_irregmen sex_abuse_irreg_catc sex_abuse_irreg_catadj sex_abuse_irreg_catmen ///
	sex_abuse_lengthc sex_abuse_lengthadj sex_abuse_lengthmen ///
	sex_abuse_pmsc sex_abuse_pmsadj sex_abuse_pmsmen sex_abuse_pms_sympc sex_abuse_pms_sympadj sex_abuse_pms_sympmen ///
	using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(sexual abuse) 
*Emotional abuse
estout emot_abuse_painc emot_abuse_painadj emot_abuse_painmen emot_abuse_pain_catc emot_abuse_pain_catadj emot_abuse_pain_catmen ///
	emot_abuse_heavyc emot_abuse_heavyadj emot_abuse_heavymen emot_abuse_heavy_catc emot_abuse_heavy_catadj emot_abuse_heavy_catmen ///
	emot_abuse_daysc emot_abuse_daysadj emot_abuse_daysmen emot_abuse_days_catc emot_abuse_days_catadj emot_abuse_days_catmen ///
	emot_abuse_irregc emot_abuse_irregadj emot_abuse_irregmen emot_abuse_irreg_catc emot_abuse_irreg_catadj emot_abuse_irreg_catmen ///
	emot_abuse_lengthc emot_abuse_lengthadj emot_abuse_lengthmen ///
	emot_abuse_pmsc emot_abuse_pmsadj emot_abuse_pmsmen emot_abuse_pms_sympc emot_abuse_pms_sympadj emot_abuse_pms_sympmen ///
	using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(emotional abuse) 
*Emotional neglect
estout emot_negl_painc emot_negl_painadj emot_negl_painmen emot_negl_pain_catc emot_negl_pain_catadj emot_negl_pain_catmen ///
	emot_negl_heavyc emot_negl_heavyadj emot_negl_heavymen emot_negl_heavy_catc emot_negl_heavy_catadj emot_negl_heavy_catmen ///
	emot_negl_daysc emot_negl_daysadj emot_negl_daysmen emot_negl_days_catc emot_negl_days_catadj emot_negl_days_catmen ///
	emot_negl_irregc emot_negl_irregadj emot_negl_irregmen emot_negl_irreg_catc emot_negl_irreg_catadj emot_negl_irreg_catmen ///
	emot_negl_lengthc emot_negl_lengthadj emot_negl_lengthmen ///
	emot_negl_pmsc emot_negl_pmsadj emot_negl_pmsmen emot_negl_pms_sympc emot_negl_pms_sympadj emot_negl_pms_sympmen ///
	using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(emotional neglect) 
*Substance use
estout substance_painc substance_painadj substance_painmen substance_pain_catc substance_pain_catadj substance_pain_catmen ///
	substance_heavyc substance_heavyadj substance_heavymen substance_heavy_catc substance_heavy_catadj substance_heavy_catmen ///
	substance_daysc substance_daysadj substance_daysmen substance_days_catc substance_days_catadj substance_days_catmen ///
	substance_irregc substance_irregadj substance_irregmen substance_irreg_catc substance_irreg_catadj substance_irreg_catmen ///
	substance_lengthc substance_lengthadj substance_lengthmen ///
	substance_pmsc substance_pmsadj substance_pmsmen substance_pms_sympc substance_pms_sympadj substance_pms_sympmen ///
	using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(substance abuse) 
*Violence between parents
estout viol_prnt_painc viol_prnt_painadj viol_prnt_painmen viol_prnt_pain_catc viol_prnt_pain_catadj viol_prnt_pain_catmen ///
	viol_prnt_heavyc viol_prnt_heavyadj viol_prnt_heavymen viol_prnt_heavy_catc viol_prnt_heavy_catadj viol_prnt_heavy_catmen ///
	viol_prnt_daysc viol_prnt_daysadj viol_prnt_daysmen viol_prnt_days_catc viol_prnt_days_catadj viol_prnt_days_catmen ///
	viol_prnt_irregc viol_prnt_irregadj viol_prnt_irregmen viol_prnt_irreg_catc viol_prnt_irreg_catadj viol_prnt_irreg_catmen ///
	viol_prnt_lengthc viol_prnt_lengthadj viol_prnt_lengthmen ///
	viol_prnt_pmsc viol_prnt_pmsadj viol_prnt_pmsmen viol_prnt_pms_sympc viol_prnt_pms_sympadj viol_prnt_pms_sympmen ///
	using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(violence between parents) 
*Parental mental health
estout prnt_mh_painc prnt_mh_painadj prnt_mh_painmen prnt_mh_pain_catc prnt_mh_pain_catadj prnt_mh_pain_catmen ///
	prnt_mh_heavyc prnt_mh_heavyadj prnt_mh_heavymen prnt_mh_heavy_catc prnt_mh_heavy_catadj prnt_mh_heavy_catmen ///
	prnt_mh_daysc prnt_mh_daysadj prnt_mh_daysmen prnt_mh_days_catc prnt_mh_days_catadj prnt_mh_days_catmen ///
	prnt_mh_irregc prnt_mh_irregadj prnt_mh_irregmen prnt_mh_irreg_catc prnt_mh_irreg_catadj prnt_mh_irreg_catmen ///
	prnt_mh_lengthc prnt_mh_lengthadj prnt_mh_lengthmen ///
	prnt_mh_pmsc prnt_mh_pmsadj prnt_mh_pmsmen prnt_mh_pms_sympc prnt_mh_pms_sympadj prnt_mh_pms_sympmen ///
	using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(parental mental health) 
*Parental separation
estout separatd_painc separatd_painadj separatd_painmen separatd_pain_catc separatd_pain_catadj separatd_pain_catmen ///
	separatd_heavyc separatd_heavyadj separatd_heavymen separatd_heavy_catc separatd_heavy_catadj separatd_heavy_catmen ///
	separatd_daysc separatd_daysadj separatd_daysmen separatd_days_catc separatd_days_catadj separatd_days_catmen ///
	separatd_irregc separatd_irregadj separatd_irregmen separatd_irreg_catc separatd_irreg_catadj separatd_irreg_catmen ///
	separatd_lengthc separatd_lengthadj separatd_lengthmen ///
	separatd_pmsc separatd_pmsadj separatd_pmsmen separatd_pms_sympc separatd_pms_sympadj separatd_pms_sympmen ///
	using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(parental separation) 
*Score
estout score_painc score_painadj score_painmen score_pain_catc score_pain_catadj score_pain_catmen ///
	score_heavyc score_heavyadj score_heavymen score_heavy_catc score_heavy_catadj score_heavy_catmen ///
	score_daysc score_daysadj score_daysmen score_days_catc score_days_catadj score_days_catmen ///
	score_irregc score_irregadj score_irregmen score_irreg_catc score_irreg_catadj score_irreg_catmen ///
	score_lengthc score_lengthadj score_lengthmen ///
	score_pmsc score_pmsadj score_pmsmen score_pms_sympc score_pms_sympadj score_pms_sympmen ///
	using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(overall ACE score) 

********************************************************************************
**#SENSITIVITY ANALYSIS*********************************************************
********************************************************************************

drop touse multuse

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Complete Case/Analysis/Contraception_SensitivityLog", replace

mark touse
markout touse parentedu parentsclass ethnicity mat_age ace_four menarche
mark multuse 
markout multuse parentedu parentsclass ethnicity mat_age ace_four menarche pain heavy days irreg


*1* CONTRACEPTION (pain_cont heavy_cont days_cont irreg_cont pms_cont length_cont - derived above)
*DESCRIPTIVES*******************************************************************
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
	foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four parentedu parentsclass ethnicity menarche_cat {
		di "IN ALL OBSERVED DATA"
		tab `exp' `out', row col
		di "IN MULTUSE SAMPLE"
		tab `exp' `out' if multuse==1, row col
	}
	foreach var in ace_score mat_age menarche {
		di "IN ALL OBSERVED DATA"
		bysort `out': summ `var', det
		di "IN MULTUSE SAMPLE"
		bysort `out': summ `var' if multuse==1, det
	}
}

foreach out in pms_cont length_cont {
	foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four parentedu parentsclass ethnicity menarche_cat  {
		di "IN ALL OBSERVED DATA"
		tab `exp' `out', row col
		di "IN TOUSE SAMPLE"
		tab `exp' `out' if touse==1, row col
	}
	foreach var in ace_score mat_age menarche {
		di "IN ALL OBSERVED DATA"
		bysort `out': summ `var', det
		di "IN MULTUSE SAMPLE"
		bysort `out': summ `var' if touse==1, det
	}
}

*ANALYSIS***********************************************************************

*Binary outcomes
foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd {
	foreach out in pain_cont heavy_cont days_cont irreg_cont {
		di "CRUDE `exp' and `out'"
		logistic `out' `exp' if multuse==1
		di "ADJUSTED for parent SEP, ethnicity, and age `exp' and `out'"
		logistic `out' `exp' parentedu parentsclass ethnicity mat_age if multuse==1
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		logistic `out' `exp' parentedu parentsclass ethnicity mat_age menarche if multuse==1
	}
	foreach out in pms_cont length_cont {
		di "CRUDE `exp' and `out'"
		logistic `out' `exp' if touse==1
		di "ETHNICITY ADJUSTED `exp' and `out'"
		logistic `out' `exp' parentedu parentsclass ethnicity mat_age if touse==1
		di "PLUS MENARCHE ADJUSTED `exp' and `out'"
		logistic `out' `exp' parentedu parentsclass ethnicity mat_age menarche if touse==1
	}
}

foreach out in pain_cont heavy_cont days_cont irreg_cont {
	di "CRUDE score and `out'"
	logistic `out' i.ace_four if multuse==1
	di "ADJUSTED for parent SEP, ethnicity, and age score and `out'"
	logistic `out' i.ace_four parentedu parentsclass ethnicity mat_age if multuse==1
	di "PLUS MENARCHE ADJUSTED score and `out'"
	logistic `out' i.ace_four parentedu parentsclass ethnicity mat_age menarche if multuse==1
}
foreach out in pms_cont length_cont {
	di "CRUDE score and `out'"
	logistic `out' i.ace_four if touse==1
	di "ETHNICITY ADJUSTED score and `out'"
	logistic `out' i.ace_four parentedu parentsclass ethnicity mat_age if touse==1
	di "PLUS MENARCHE ADJUSTED score and `out'"
	logistic `out' i.ace_four parentedu parentsclass ethnicity mat_age menarche if touse==1
}

log close

********************************************************************************
***#CONTINNUOUS OR CATEGORICAL SCORE********************************************
********************************************************************************

mark touse
markout touse parentedu parentsclass ethnicity mat_age ace_four menarche
mark multuse 
markout multuse parentedu parentsclass ethnicity mat_age ace_four menarche pain heavy days irreg

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Complete Case/Categorical exposure/Score_ContorCat", replace

***STEP 1: LR TESTS (complete case data only)

foreach out in pain heavy days irreg {
	logistic `out' i.ace_four if multuse==1
	estimates store `out'_cat
	logistic `out' ace_four if multuse==1
	estimates store `out'_cont
	lrtest `out'_cont `out'_cat 
}
foreach out in length pms {
	logistic `out' i.ace_four if touse==1
	estimates store `out'_cat
	logistic `out' ace_four if touse==1
	estimates store `out'_cont
	lrtest `out'_cont `out'_cat 
}


***STEP 2: LINEAR P VALUE APPROPRIATE?
*Pain = 0.2419 = YES 
*Heavy = 0.9172 = YES 
*Days = 0.1920 = YES
*Length = 0.7030 = YES 
*Irreg = 0.1287 = YES
*PMS = 0.0025 = NO (keep testparm p value)

***STEP 2.5:
*adjusted complete case p values (continuous)
foreach out in pain heavy days irreg {
	logistic `out' ace_four parentedu parentsclass ethnicity mat_age if multuse==1 
	logistic `out' ace_four parentedu parentsclass ethnicity mat_age menarche if multuse==1 
	}
	
foreach out in length pms {
	logistic `out' ace_four parentedu parentsclass ethnicity mat_age if touse==1 
	logistic `out' ace_four parentedu parentsclass ethnicity mat_age menarche if touse==1 
}

*complete case testparm for pms
logistic pms i.ace_four if touse==1
testparm i.ace_four
logistic pms i.ace_four parentedu parentsclass ethnicity mat_age if touse==1
testparm i.ace_four
logistic pms i.ace_four parentedu parentsclass ethnicity mat_age menarche if touse==1
testparm i.ace_four

log close 

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Complete Case/Categorical exposure/Score_ContorCat", append

***STEP 3: UPDATED P VALUES IN MI ANALYSES 

*Mult outcomes
foreach out in pain heavy days irreg {
   	mi estimate, or post: logistic `out' ace_four [pw=ipw_mult]
	mi estimate, or post: logistic `out' ace_four parentedu parentsclass ethnicity mat_age [pw=ipw_mult]
	mi estimate, or post: logistic `out' ace_four parentedu parentsclass ethnicity mat_age menarche [pw=ipw_mult]
}

*Length and pms
foreach out in length pms {
   	mi estimate, or post: logistic `out' ace_four [pw=ipw_`out']
	mi estimate, or post: logistic `out' ace_four parentedu parentsclass ethnicity mat_age [pw=ipw_`out']
	mi estimate, or post: logistic `out' ace_four parentedu parentsclass ethnicity mat_age menarche [pw=ipw_`out']
}

log close



********************************************************************************
**#IPW PREP*********************************************************************
********************************************************************************

**Baseline predictor vars
*Maternal age at delivery mz028b
tab mat_age //same as confounder one derived above
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
*C quest; ethnicity c800 (non-white / white), own education c645a (O level lower, A level, degree), financial difficulties c525 (score), social class c755 c765 (manual v non-manual)
tab ethnicity //same as confounder one derived above
recode c645a (-1=.) (1/2=3) (3=2) (4=1) (5=0), gen (edu)
label values edu ed_lbl
recode c525 (-7 -1=.), gen (findiff_ipw)
label define socclass_lbl 0"Non-manual" 1"Manual", replace
gen sclass=.
replace sclass=0 if c755==1 | c755==2 | c755==3  
replace sclass=1 if c755==4 | c755==5 | c755==6
gen partsclass=.
replace partsclass=0 if c765==1 | c765==2 | c765==3 
replace partsclass=1 if c765==4 | c765==5 | c765==6
gen highsclass=.
replace highsclass=0 if (sclass==0 & partsclass==0) | (sclass==. & partsclass==0) | (sclass==0 & partsclass==.)
replace highsclass=1 if (sclass==1 & partsclass==1) | (sclass==. & partsclass==1) | (sclass==1 & partsclass==.) | (sclass==0 & partsclass==1) | (sclass==1 & partsclass==0)
label values highsclass socclass_lbl

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

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/IPW info/IPW_Missing_Patterns", replace

**PRIOR TO MODEL SELECTION - LOOKING AT ALL POSISBLE IPW VARIABLES BEING CONSIDERED (19 in total)
*Simple Descriptives
foreach var in marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever parity ethnicity edu highsclass bfed_dur {
	tab `var'
}

foreach var in mat_age rooms epds findiff_ipw fai_long {
	summ `var', det
}

*Missing data percentages
mdesc mat_age marital_stat phone car housing rooms crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity ethnicity edu findiff_ipw highsclass bfed_dur fai_long

*Number of missing variables
egen ipw_vars=rmiss2(mat_age marital_stat phone car housing rooms crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity ethnicity edu findiff_ipw highsclass bfed_dur fai_long)
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

**Process has been conducted for the SEP analysis and would be identical here so not going to repeat the code
*Resulted in same variable for all 3 analytical samples, including all except smoke_ever and rooms (17 IPW variables)

********************************************************************************
**#IMPUTATION PREP**************************************************************
********************************************************************************

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/IPW info/IPW_Missing_Patterns", append
*To impute sample - at least one ACE, half IPW, and age at delivery
egen ace_miss=rmiss2(phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd) 
tab ace_miss //1216 missing all 8 - DROP
drop if ace_miss==8

*Age
count if ace_miss!=8 & mat_age==.
drop if mat_age==. //futher 58 dropped

*Half IPW variables
egen ipw_vars=rmiss2(mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity ethnicity edu findiff_ipw highsclass bfed_dur fai_long)
tab ipw_vars
count if ace_miss!=8 & mat_age!=. & inrange(ipw_vars,9,17)
drop if inrange(ipw_vars,9,17) // further 340 dropped

**AFTER MODEL SELECTION AND IN IMPUTE TO SAMPLE - LOOKING AT ALL POSISBLE IPW VARIABLES BEING USED (17 in total, ecluding smoke_ever and rooms)
*Simple Descriptives
foreach var in marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg parity ethnicity edu highsclass bfed_dur {
	tab `var'
}

foreach var in mat_age epds findiff_ipw fai_long {
	summ `var', det
}

*Missing data percentages
mdesc mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity ethnicity edu findiff_ipw highsclass bfed_dur fai_long

*Number of missing variables
egen ipw_vars=rmiss2(mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity ethnicity edu findiff_ipw highsclass bfed_dur fai_long)
tab ipw_vars

log close
drop ipw_vars

***AUXILIARY VARIABLES
**Already have
tab1 parentedu edu parentsclass mat_age parity ethnicity

**To derive 
label define binary 1"Yes" 0"No", replace

*Family got poorer c429a
recode c429a (-1=.) (1=1) (2=0), gen (fam_poorer)
label values fam_poorer binary

*Control attempted by mother d706
recode d706 (-1=.) (1=0 "Never") (2=1 "Sometimes") (3=2 "Usually"), gen (control)

*Privacy invaded by mother d707
recode d707 (-1=.) (1=0 "Never") (2=1 "Sometimes") (3=2 "Usually"), gen (priv_invade)

*Felt unwanted by mother d709
recode d709 (-1=.) (1=0 "Never") (2=1 "Sometimes") (3=2 "Usually"), gen (felt_unwanted)

*Local authority (included adopted d380, in LA care d385, lived in foster parents home d395, and stayed in childrens home d402a)
//d380, d385, d395 - original vars 1 is yes (the risk cat) and 2 is no
//d402a - original var 2 is yes (the risk cat.) and 1 is no 
gen local_authority=. 
replace local_authority=0 if d385 ==2 | d395 ==2 | d402a ==1 | d380 ==2
replace local_authority=1 if d385 ==1 | d395 ==1 | d402a ==2 | d380 ==1
label values local_authority binary

*Maternal absence (includes between 0-5 years d420, 6-11 years d421 and between 12 and 16 years d422)
//original vars are 'mother in household' 1 is yes (so not absent) and 2 is no (so absent)
gen m_absence=. 
replace m_absence=0 if d420 ==1 | d421 ==1 | d422 ==1
replace m_absence=1 if d420 ==2 | d421 ==2 | d422 ==2
label values m_absence binary

*Paternal absence (includes between 0-5 years d423, 6-11 years d424 and between 12 and 16 years d425)
//original vars same as maternal ones above
gen p_absence =. 
replace p_absence=0 if d423 ==1 | d424 ==1 | d425 ==1 
replace p_absence=1 if d423 ==2 | d424 ==2 | d425 ==2
label values p_absence binary

*Parent relationship frigtening h137e
recode h137e (-2 -1=.) (4=0 "No") (3=1 "Sometimes") (1/2=2 "Frequently or Always"), gen (prnt_rltn_fright)

*Parent relationship remote h137h
recode h137h (-2 -1=.) (4=0 "No") (3=1 "Sometimes") (1/2=2 "Frequently or Always"), gen (prnt_rltn_remote)


*Childhood happiness (includes childhood happiness d763, memories of childhood 0-5 years c441, 6-11 years c442, and 12-15 years c443)
//all original - 1/2 very/mod happy; 3/4/5 not happy/quite/very unhappy; 6 can't remember
gen happy =.
replace happy =1 if c441 <=2>=1 | c442 <=2>=1 | c443 <=2>=1 | d763 <=2>=1 
replace happy =0 if c441 ==6>=3 | c442 ==6>=3 | c443 ==6>=3 | d763 ==6>=3
label values happy binary

/*House type g352 (not using anymore as perfect prediction issues in imputation)
recode g352 (-1=.)
rename g352 house_aux*/

*Smoking during pregnancy (includes number smoked per day in first trimester b670 and number smoked per day in last 2 weeks b671)
label define smoke 0"No smoking" 1"Smoked in 1st tri OR last 2 wks" 2"Smoked in 1st tri AND last 2 wks", replace
gen matsm = 0 if b670==0 & b671==0
replace matsm = 2 if matsm==. & (b670>0 & b670!=.) & (b671>0 & b671!=.)
replace matsm = 1 if matsm==. & ((b670>0 & b670!=.) | (b671>0 & b671!=.))
label variable matsm "Maternal smoking during pregnancy"
label values matsm smoke

*Alcohol use during pregnancy (includes consumption in first trimester b721, consumption since baby first moved b722, and total units per week c373)
//b721 and b722 - 1 is never; 2/3 is <1/1 glass per week (defined here as moderate); 4/5/6 is 1-2/3-9/10+ glasses per day (defined here as heavy)
//c373 - number of units; between 1 and 6 is considered moderate here; 7 or more is considered heavy
gen alc_use =. 
replace alc_use =1 if b721 <=3>=2 | b722 <=3>=2 | c373 <=6>=1
replace alc_use =2 if b721 ==6>=4 | b722 ==6>=4 | c373 ==65>=7
replace alc_use =0 if b721==1 & b722==1 & c373 == 0
replace alc_use =0 if b721==1 & b722==1 & c373 == .
label define drinking 0"None" 1"Moderate" 2"Heavy", replace
label values alc_use drinking 

********************************************************************************
**#IMPUTATION*******************************************************************
********************************************************************************

*Focus dataset to relevant variables 
keep phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ///
	 pain heavy days irreg length pms ///
	 parentedu parentsclass ethnicity mat_age menarche ///
	 has_mult has_length has_pms  ///
	 marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity edu findiff_ipw highsclass bfed_dur fai_long ///
	 fam_poorer control priv_invade felt_unwanted local_authority m_absence p_absence prnt_rltn_fright prnt_rltn_remote happy matsm alc_use ///
	 aln 

**Missing data patterns
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/Missing_Data_Patterns", replace
*Complete 
mdesc has_mult has_length has_pms mat_age
tab1 has_mult has_length has_pms
*Substantive variables (missing)
mdesc phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd pain heavy days irreg length pms parentedu parentsclass ethnicity menarche
egen key_vars=rmiss2(phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd parentedu parentsclass ethnicity menarche mat_age)
tab key_vars
*IPW variables (missing)
mdesc marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity edu findiff_ipw highsclass bfed_dur fai_long
*Auxiliary variables (missing)
mdesc fam_poorer control priv_invade felt_unwanted local_authority m_absence p_absence prnt_rltn_fright prnt_rltn_remote happy house_aux matsm alc_use

log close
drop key_vars

**Set up imputation
mi set flong
mi register imputed phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd pain heavy days irreg length pms parentedu parentsclass ethnicity menarche marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity edu findiff_ipw highsclass bfed_dur fai_long fam_poorer control priv_invade felt_unwanted local_authority m_absence p_absence prnt_rltn_fright prnt_rltn_remote happy matsm alc_use
mi register regular has_mult has_length has_pms aln mat_age

**Dryrun
mi impute chained (logit) phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd pain heavy days irreg length pms ///
				  (logit) ethnicity parentsclass marital_stat phone car dbl_glaze highsclass fam_poorer local_authority m_absence p_absence happy ///
				  (logit, omit(i.matsm)) smoke_preg ///
				  (ologit, omit(i.smoke_preg)) matsm ///
				  (ologit) parentedu crowding first_preg parity edu bfed_dur control priv_invade felt_unwanted prnt_rltn_fright prnt_rltn_remote alc_use housing ///
				  (pmm, knn(5)) menarche epds findiff_ipw fai_long ///
				  = has_mult has_length has_pms mat_age, dryrun

**Trace plot version
mi impute chained (logit) phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd pain heavy days irreg length pms ///
				  (logit) ethnicity parentsclass marital_stat phone car dbl_glaze highsclass fam_poorer local_authority m_absence p_absence happy ///
				  (logit, omit(i.matsm)) smoke_preg ///
				  (ologit, omit(i.smoke_preg)) matsm ///
				  (ologit) parentedu crowding first_preg parity edu bfed_dur control priv_invade felt_unwanted prnt_rltn_fright prnt_rltn_remote alc_use housing ///
				  (pmm, knn(5)) menarche epds findiff_ipw fai_long ///
				  = has_mult has_length has_pms mat_age, burnin(100) rseed(13519368) dots chainonly noisily showcommand ///
				  savetrace ("/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/MI_TraceData.dta", replace)					  
				 				  
/*Trace checks
use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/MI_TraceData.dta", clear
			 
describe 
tsset iter

cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/Trace"

foreach cov in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd parentedu parentsclass ethnicity marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity edu findiff_ipw highsclass bfed_dur fai_long {
	tsline `cov'_mean, title(Mean imputed values of `cov') legend(off) 
	graph export `cov'_mean.png, replace
	tsline `cov'_sd, title(Standard deviation imputed values of `cov') legend(off)
	graph export `cov'_sd.png, replace
}	

*Mostly stable - slight increase in means of emotional/physical abuse, but stable by 60th iteration

*/

**Run imputation model
mi impute chained (logit) phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd pain heavy days irreg length pms ///
				  (logit) ethnicity parentsclass marital_stat phone car dbl_glaze highsclass fam_poorer local_authority m_absence p_absence happy ///
				  (logit, omit(i.matsm)) smoke_preg ///
				  (ologit, omit(i.smoke_preg)) matsm ///
				  (ologit) parentedu crowding first_preg parity edu bfed_dur control priv_invade felt_unwanted prnt_rltn_fright prnt_rltn_remote alc_use housing ///
				  (pmm, knn(5)) menarche epds findiff_ipw fai_long ///
				  = has_mult has_length has_pms mat_age, add(60) burnin(60) rseed(1371931) dots
				  
*Save
save "/Volumes/157/working/data/G0_ACEs_Imputed_dataset.dta", replace				  
				  
********************************************************************************
**#POST-IMPUTATION CHECKS*******************************************************
********************************************************************************				  
				  
*Open				  
use "/Volumes/157/working/data/G0_ACEs_Imputed_dataset.dta", clear					  

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/Post_Imp_Checks", replace 
				  
*Checks
label define imputed 0"Observed" 1"Imputed", replace
gen imputed=0 if _mi_m==0
replace imputed=1 if _mi_m>0
label values imputed imputed 

*Proportions
foreach var in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ethnicity parentsclass marital_stat phone car dbl_glaze highsclass fam_poorer local_authority m_absence p_absence happy smoke_preg matsm ///
parentedu crowding first_preg parity edu bfed_dur control priv_invade felt_unwanted prnt_rltn_fright prnt_rltn_remote alc_use housing {
	tab `var' imputed, row col
}
*Means
foreach var in epds findiff_ipw fai_long {
	summ `var' if imputed==0, det
	summ `var' if imputed==1, det
}

log close

*Distributions 
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted"

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

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/Post_Imp_Checks", append

foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd {
	foreach out in pain heavy days irreg length pms {
		mi estimate, mcerror: logistic `out' i.`exp' parentedu parentsclass ethnicity mat_age menarche
	}
}

log close
*Across all models, largest FMI: 0.2072
*Mcerror concerns - all good
				  
********************************************************************************
**#DERIVING WEIGHTS*************************************************************
********************************************************************************					  

**IPW for each imputed dataset per analytical sample
*Mult
forvalues j = 1/60 {
	logistic has_mult mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg epds i.parity ethnicity i.edu findiff_ipw highsclass i.bfed_dur fai_long if _mi_m==`j'
	predict multp`j' if _mi_m==`j'
}				  
*Length
forvalues j = 1/60 {
	logistic has_length mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg epds i.parity ethnicity i.edu findiff_ipw highsclass i.bfed_dur fai_long if _mi_m==`j'
	predict lengthp`j' if _mi_m==`j'
}					  
*PMS
forvalues j = 1/60 {
	logistic has_pms mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg epds i.parity ethnicity i.edu findiff_ipw highsclass i.bfed_dur fai_long if _mi_m==`j'
	predict pmsp`j' if _mi_m==`j'
}	  

**Consistent name across datasets
foreach var in mult length pms {
	gen `var'p = `var'p1
	forvalues j = 2/60 {
		replace `var'p=`var'p`j' if `var'p==.
	}
}				  
				  
**Create probability and weights
foreach var in mult length pms {
	gen prob_`var'=`var'p if has_`var'==1
	replace prob_`var'=1-`var'p if has_`var'==0
	gen ipw_`var'=1/prob_`var'
}	
			  
*Quick check (first 12 obs of imputation 1)
list _mi_m has_mult multp prob_mult ipw_mult in 12538/12550
*Imputation 2
list _mi_m has_mult multp prob_mult ipw_mult in 25075/25090

**Tidy up (remove original p per dataset variables as have overall one with all information)
forvalues j = 1/60 {
	foreach var in mult length pms {
		drop `var'p`j'
	}
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/IPW info/Summary_Weights", replace
*Summary of weights
foreach var in mult length pms {
	summ ipw_`var', det
}
log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/IPW info"

foreach var in mult length pms {
	histogram ipw_`var'
	graph export `var'_weights.png, replace 
}	
 
*Replace dataset now outcomes deleted and weights derived
save "/Volumes/157/working/data/G0_ACEs_Imputed_dataset.dta", replace

********************************************************************************
**#WEIGHTED DESCRIPTIVES********************************************************
********************************************************************************

*Passively derive ace score
egen ace_score=rowtotal(phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd)

gen ace_four = ace_score
recode ace_four (0=0) (1=1) (2=2) (3=3) (4/8=4)
tab ace_four
label define scores 0"none" 1"1" 2"2" 3"3" 4"4 or more", replace
label values ace_four scores
label variable ace_four "Number of ACEs"

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/Descriptives/Weighted_Descriptives", replace

***SIMPLE - full sample
**Exposures
foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four {
	mi estimate: proportion `exp'
}

**Outcomes
foreach out in pain heavy days irreg length pms {
	tab `out' if imputed==0
}

**Confounders
foreach cov in parentedu parentsclass ethnicity {
	mi estimate: proportion `cov'
}
foreach cov in mat_age menarche {
	summ `cov' if imputed==0
}

***SIMPLE - analysis samples
**Exposures
foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four {
	foreach sample in mult length pms {
		mi estimate: proportion `exp' if has_`sample'==1
	}
}

**Confounders
foreach sample in mult length pms {
	foreach cov in parentedu parentsclass ethnicity {
		mi estimate: proportion `cov' if has_`sample'==1
	}
	foreach cov in mat_age menarche {
		summ `cov' if imputed==0 & has_`sample'==1
	}
}

***CROSS TABS - exp/cov by outcomes 				  
foreach out in pain heavy days irreg length pms  {
	foreach var in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four parentedu parentsclass ethnicity {
		mi estimate: proportion `var', over(`out')	
	}
	foreach var in mat_age menarche {
		bysort `out': summ `var' if imputed==0, det
	}
}	

***CROSS TABS - ipw by outcomes
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
use "/Volumes/157/working/data/G0_ACEs_Imputed_dataset.dta", clear

*Passively derive ace score
egen ace_score=rowtotal(phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd)

gen ace_four = ace_score
recode ace_four (0=0) (1=1) (2=2) (3=3) (4/8=4)
tab ace_four
label define scores 0"none" 1"1" 2"2" 3"3" 4"4 or more", replace
label values ace_four scores
label variable ace_four "Number of ACEs"

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/Analysis/Weighted_Analysis_log", replace

*Mult outcomes
foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four {
	foreach out in pain heavy days irreg {
		eststo `exp'_`out'c: mi estimate, or post: logistic `out' i.`exp' [pw=ipw_mult]
		eststo `exp'_`out'adj: mi estimate, or post: logistic `out' i.`exp' parentedu parentsclass ethnicity mat_age [pw=ipw_mult]
		eststo `exp'_`out'men: mi estimate, or post: logistic `out' i.`exp' parentedu parentsclass ethnicity mat_age menarche [pw=ipw_mult]
	}
}	
	
*Length and pms
foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four {
	foreach out in length pms {
		eststo `exp'_`out'c: mi estimate, or post: logistic `out' i.`exp' [pw=ipw_`out']
		eststo `exp'_`out'adj: mi estimate, or post: logistic `out' i.`exp' parentedu parentsclass ethnicity mat_age [pw=ipw_`out']
		eststo `exp'_`out'men: mi estimate, or post: logistic `out' i.`exp' parentedu parentsclass ethnicity mat_age menarche [pw=ipw_`out']
	}
}	
	
log close

**Output to excel file 
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/Analysis"
capture erase "main_results.xls"
*Physical abuse 
estout phys_abuse_painc phys_abuse_painadj phys_abuse_painmen phys_abuse_heavyc phys_abuse_heavyadj phys_abuse_heavymen ///
		phys_abuse_daysc phys_abuse_daysadj phys_abuse_daysmen phys_abuse_irregc phys_abuse_irregadj phys_abuse_irregmen ///
		phys_abuse_lengthc phys_abuse_lengthadj phys_abuse_lengthmen phys_abuse_pmsc phys_abuse_pmsadj phys_abuse_pmsmen ///
		using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(physical abuse) 		
*Sexual abuse 
estout sex_abuse_painc sex_abuse_painadj sex_abuse_painmen sex_abuse_heavyc sex_abuse_heavyadj sex_abuse_heavymen ///
		sex_abuse_daysc sex_abuse_daysadj sex_abuse_daysmen sex_abuse_irregc sex_abuse_irregadj sex_abuse_irregmen ///
		sex_abuse_lengthc sex_abuse_lengthadj sex_abuse_lengthmen sex_abuse_pmsc sex_abuse_pmsadj sex_abuse_pmsmen ///
		using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(sexual abuse)		
*Emotional abuse 
estout emot_abuse_painc emot_abuse_painadj emot_abuse_painmen emot_abuse_heavyc emot_abuse_heavyadj emot_abuse_heavymen ///
		emot_abuse_daysc emot_abuse_daysadj emot_abuse_daysmen emot_abuse_irregc emot_abuse_irregadj emot_abuse_irregmen ///
		emot_abuse_lengthc emot_abuse_lengthadj emot_abuse_lengthmen emot_abuse_pmsc emot_abuse_pmsadj emot_abuse_pmsmen ///
		using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(emotional abuse)		
*Emotional neglect
estout emot_negl_painc emot_negl_painadj emot_negl_painmen emot_negl_heavyc emot_negl_heavyadj emot_negl_heavymen ///
		emot_negl_daysc emot_negl_daysadj emot_negl_daysmen emot_negl_irregc emot_negl_irregadj emot_negl_irregmen ///
		emot_negl_lengthc emot_negl_lengthadj emot_negl_lengthmen emot_negl_pmsc emot_negl_pmsadj emot_negl_pmsmen ///
		using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(emotional neglect)		
*Substance abuse in household
estout substance_painc substance_painadj substance_painmen substance_heavyc substance_heavyadj substance_heavymen ///
		substance_daysc substance_daysadj substance_daysmen substance_irregc substance_irregadj substance_irregmen ///
		substance_lengthc substance_lengthadj substance_lengthmen substance_pmsc substance_pmsadj substance_pmsmen ///
		using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(substance)		
*Violence between parents
estout viol_prnt_painc viol_prnt_painadj viol_prnt_painmen viol_prnt_heavyc viol_prnt_heavyadj viol_prnt_heavymen ///
		viol_prnt_daysc viol_prnt_daysadj viol_prnt_daysmen viol_prnt_irregc viol_prnt_irregadj viol_prnt_irregmen ///
		viol_prnt_lengthc viol_prnt_lengthadj viol_prnt_lengthmen viol_prnt_pmsc viol_prnt_pmsadj viol_prnt_pmsmen ///
		using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(violence btw parents)		
*Parental mental health
estout prnt_mh_painc prnt_mh_painadj prnt_mh_painmen prnt_mh_heavyc prnt_mh_heavyadj prnt_mh_heavymen ///
		prnt_mh_daysc prnt_mh_daysadj prnt_mh_daysmen prnt_mh_irregc prnt_mh_irregadj prnt_mh_irregmen ///
		prnt_mh_lengthc prnt_mh_lengthadj prnt_mh_lengthmen prnt_mh_pmsc prnt_mh_pmsadj prnt_mh_pmsmen ///
		using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(parent mental health)		
*Separation 
estout separatd_painc separatd_painadj separatd_painmen separatd_heavyc separatd_heavyadj separatd_heavymen ///
		separatd_daysc separatd_daysadj separatd_daysmen separatd_irregc separatd_irregadj separatd_irregmen ///
		separatd_lengthc separatd_lengthadj separatd_lengthmen separatd_pmsc separatd_pmsadj separatd_pmsmen ///
		using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(separated)		
*Score  
estout ace_four_painc ace_four_painadj ace_four_painmen ace_four_heavyc ace_four_heavyadj ace_four_heavymen ///
		ace_four_daysc ace_four_daysadj ace_four_daysmen ace_four_irregc ace_four_irregadj ace_four_irregmen ///
		ace_four_lengthc ace_four_lengthadj ace_four_lengthmen ace_four_pmsc ace_four_pmsadj ace_four_pmsmen ///
		using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons parentedu parentsclass ethnicity mat_age menarche) eform title(number of aces)
		
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


log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/Analysis/Truncated_Weights", replace				  
				  
**Mult sample
*Original vs truncated weights
summ ipw_mult, det
summ trunc95_mult, det
summ trunc99_mult, det
*Main models
foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four {
	foreach out in pain heavy days irreg {
		di "95th percentile weights"
		mi estimate, or post: logistic `out' i.`exp' [pw=trunc95_mult]
		mi estimate, or post: logistic `out' i.`exp' parentedu parentsclass ethnicity mat_age [pw=trunc95_mult]
		mi estimate, or post: logistic `out' i.`exp' parentedu parentsclass ethnicity mat_age menarche [pw=trunc95_mult]
		di "99th percentile weights"
		mi estimate, or post: logistic `out' i.`exp' [pw=trunc99_mult]
		mi estimate, or post: logistic `out' i.`exp' parentedu parentsclass ethnicity mat_age [pw=trunc99_mult]
		mi estimate, or post: logistic `out' i.`exp' parentedu parentsclass ethnicity mat_age menarche [pw=trunc99_mult]
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
foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four {
	foreach out in length pms {
		di "95th percentile weights"
		mi estimate, or post: logistic `out' i.`exp' [pw=trunc95_`out']
		mi estimate, or post: logistic `out' i.`exp' parentedu parentsclass ethnicity mat_age [pw=trunc95_`out']
		mi estimate, or post: logistic `out' i.`exp' parentedu parentsclass ethnicity mat_age menarche [pw=trunc95_`out']
		di "99th percentile weights"
		mi estimate, or post: logistic `out' i.`exp' [pw=trunc95_`out']
		mi estimate, or post: logistic `out' i.`exp' parentedu parentsclass ethnicity mat_age [pw=trunc95_`out']
		mi estimate, or post: logistic `out' i.`exp' parentedu parentsclass ethnicity mat_age menarche [pw=trunc95_`out']
	}
}					  
				  
log close		
		

***CATEGORICAL ACE SCORE WITH OVERALL P VALUE		
use "/Volumes/157/working/data/G0_ACEs_Imputed_dataset.dta", clear

*Passively derive ace score
egen ace_score=rowtotal(phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd)

gen ace_four = ace_score
recode ace_four (0=0) (1=1) (2=2) (3=3) (4/8=4)
tab ace_four
label define scores 0"none" 1"1" 2"2" 3"3" 4"4 or more", replace
label values ace_four scores
label variable ace_four "Number of ACEs"		
		
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/MI and Weighted/Analysis/ACEScore_OverallP_log", replace

*Mult outcomes
foreach out in pain heavy days irreg {
	quietly mi estimate, or post: logistic `out' i.ace_four [pw=ipw_mult]
	di "CRUDE"
	testparm i.ace_four
	quietly mi estimate, or post: logistic `out' i.ace_four parentedu parentsclass ethnicity mat_age [pw=ipw_mult]
	di "MODEL 1"
	testparm i.ace_four
	quietly mi estimate, or post: logistic `out' i.ace_four parentedu parentsclass ethnicity mat_age menarche [pw=ipw_mult]
	di "MODEL 2"
	testparm i.ace_four
}

	
*Length and pms
foreach out in length pms {
	quietly mi estimate, or post: logistic `out' i.ace_four [pw=ipw_`out']
	di "CRUDE"
	testparm i.ace_four
	quietly mi estimate, or post: logistic `out' i.ace_four parentedu parentsclass ethnicity mat_age [pw=ipw_`out']
	di "MODEL 1"
	testparm i.ace_four
	quietly mi estimate, or post: logistic `out' i.ace_four parentedu parentsclass ethnicity mat_age menarche [pw=ipw_`out']
	di "MODEL 2"
	testparm i.ace_four
}	
	
log close		
		
		
		
		
		
			  