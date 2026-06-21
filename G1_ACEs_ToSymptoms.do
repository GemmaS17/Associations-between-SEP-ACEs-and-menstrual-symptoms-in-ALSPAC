**ACE to symptoms analysis 

**Data file 
clear all
set maxvar 30000

use "/Volumes/157/working/data/SEPandACEs_G1_data.dta", clear

************************************************************************************************************************************
************************************************************************************************************************************
**#*CREATING DATASET OF OUTCOMES, COVARIATES, AND IPW VARIALES TO COMBINE WITH BF DATASET OF EXPOSURE VARIABLES (ACES)***************
************************************************************************************************************************************
************************************************************************************************************************************

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

**Boost with pub8 
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

**Missing at pub9 with pub8 data
gen data_pub8_pain=1 if pain==. & pain_pub8!=.				
gen data_pub8_heavy=1 if heavy==. & heavy_pub8!=.			
gen data_pub8_days_cat=1 if days_cat==. & days_cat_pub8!=.	
gen data_pub8_length=1 if length==. & length_pub8!=.		

gen no_pub9=1 if pain==. & heavy==. & days_cat==. & length==.	
replace no_pub9=0 if pain!=. | heavy!=. | days_cat!=. | length!=. 
foreach var in pain heavy days_cat length 
	replace data_pub8_`var'=0 if data_pub8_`var'==1 & no_pub9==0   
	tab data_pub8_`var'
}

*Combine outcomes 
label define days_cat_lbl 0"Less than 4 days" 1"4-6 days" 2"7 days or more", replace 
gen data_pub8_days_bin=data_pub8_days_cat
foreach var in pain heavy days_cat days_bin length {
	capture drop `var'_both
	gen `var'_both=`var'
	replace `var'_both=`var'_pub8 if data_pub8_`var'==1
	label values `var'_both `var'_lbl
}
*Length binary
label define length_bin_lbl 0"Normal (24-38)" 1"Freq. or Infreq.", replace
gen length_bin_both=.
replace length_bin_both=0 if length_both==0
replace length_bin_both=1 if inrange(length_both,1,2)
label values length_bin_both length_bin_lbl

*One age variable for pubboth (take pub9 but then replace with pub8 if they gave no symptoms at pub9)
gen pubboth_age=pub9_age
replace pubboth_age=pub8_age if no_pub9==1


********************************************************************************
**#DEFINING COVARIATES**********************************************************
********************************************************************************

*SEP confounders - include parental education, parental social class, and financial difficulties
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

**Ethnicity 
label define ethnicity_lbl 1"Non-white" 0"White", replace
recode c804 (-1=.) (1=0) (2=1), gen (ethnicity)
label values ethnicity ethnicity_lbl

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
	tab `var'_menarche															//all have started period at these times 
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

*Pub8
gen pub8_tsm=pub8_age-menarche_month
summ pub8_tsm, det
di r(mean)/12
di r(min)/12
di r(max)/12

*One tsm variable for pubboth
gen pub_tsm=pub9_tsm
replace pub_tsm=pub8_tsm if no_pub9==1

***Sensitivity variables
rename mat_binary_social_class_1 mat_sclass
rename pat_binary_social_class_1 pat_sclass

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

*Exclude those using hormonal contraception at relevant timepoint
foreach var in pain heavy days_bin length {
	gen `var'_cont=`var'_both
	replace `var'_cont=. if pub_cont==1
}
gen irreg_cont=irreg
replace irreg_cont=. if tf4_cont==1
gen pms_cont=pms_bin
replace pms_cont=. if q21_cont==1

**Exclude those less than 3 years (36 months) since menarche 
foreach var in pain heavy days_bin length {
	gen `var'_tsm=`var'_both
	replace `var'_tsm=. if inrange(pub_tsm,0,36)
}
gen irreg_tsm=irreg
replace irreg_tsm=. if inrange(tf4_tsm,0,36)
gen pms_tsm=pms_bin
replace pms_tsm=. if inrange(q21_tsm,0,36)

**Medical care for pain and heavy
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

**Separating heavy from prolonged bleeding 
label define heavycat_lbl 0"Neither" 1"Prolonged only" 2"Heavy only" 3"Both", replace
gen heavy_days_cat=.
replace heavy_days_cat=3 if heavy_both==1 & days_bin_both==1
replace heavy_days_cat=2 if heavy_both==1 & days_bin_both==0
replace heavy_days_cat=1 if heavy_both==0 & days_bin_both==1
replace heavy_days_cat=0 if heavy_both==0 & days_bin_both==0
label values heavy_days_cat heavycat_lbl


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


********************************************************************************
**#DEFINING SAMPLE(S)***********************************************************
********************************************************************************
*Flowchart - starting at 7930 obs
*1*Standard exclusions
drop if kz011b==2
drop if in_core==.a
drop if qlet=="B"
drop if in_core==2
*2*Exclude if no SEP data or less than half IPW variables
egen sep=rmiss2(highed sclass findiff)
recode sep (0/2=1 "At least one available") (3=0 "Missing all"), gen(sep_bin)
drop if sep_bin==0
*Will drop based on half of IPW variables after merging with ACEs and doing the lasso approach to decide which ones included in model

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

********************************************************************************
**#DATASET TO MERGE WITH ACE DATASET********************************************
********************************************************************************

rename days_bin_both days_both
rename length_both length_cat_both
rename length_bin_both length_both

keep aln qlet ///
pain_both heavy_both days_both length_both irreg pms_bin ///
highed sclass findiff ethnicity ///
menarche mated pated mat_sclass pat_sclass ///
pain_cont heavy_cont days_bin_cont length_cont irreg_cont pms_cont ///
pain_tsm heavy_tsm days_bin_tsm length_tsm irreg_tsm pms_tsm ///
pain_doc heavy_doc heavy_days_cat ///
mat_age marital_stat phone car housing rooms crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long mat_ethnicity ///
has_pub has_length has_irreg has_pms 

save "/Volumes/157/working/data/ToMergeWithACEs_GS_15Nov24.dta", replace 

************************************************************************************************************************************
************************************************************************************************************************************
**#*COMPLETE CASE ANALYSIS: POST-MERGING WITH BF DATASET AND CALCULATING ACE CONSTRUCTS WITH R SCRIPTS 1 AND 2**********************
************************************************************************************************************************************
************************************************************************************************************************************

*Dataset (sample down to 5567 once those with less than 10% of ACEs variables dropped)
clear all
set maxvar 30000
use "/Volumes/Studies/ALSPAC Menstrual PhD/alspacKids_ACE_0_10ALL2.dta", clear

********************************************************************************
**#ACE EXPOSURES***************************************************************
********************************************************************************

*Classic 10 (physical abuse, emotional abuse, emotional neglect, sexual abuse, bullying, violence between parents, parental mental health, substance use, parental conviction, and parental separation)
tab1 physical_abuse_0_10yrs emotional_abuse_0_10yrs emotional_neglect_0_10yrs sexual_abuse_0_10yrs bullying_0_10yrs violence_between_parnts_0_10yrs mentl_hlth_prblms_r_scd_0_10yrs substance_household_0_10yrs parent_convicted_offenc_0_10yrs parental_separation_0_10yrs
label define ace_bin_lbl 0"No" 1"Yes", replace
foreach var in physical_abuse emotional_abuse emotional_neglect sexual_abuse bullying violence_between_parnts mentl_hlth_prblms_r_scd substance_household parent_convicted_offenc parental_separation {
	rename `var'_0_10yrs `var'
	label values `var' ace_bin_lbl
}
rename physical_abuse phys_abus
rename emotional_abuse emot_abus
rename emotional_neglect emot_neg
rename sexual_abuse sexu_abus
rename bullying bully
rename violence_between_parnts viol_parent
rename mentl_hlth_prblms_r_scd prnt_mntlhlth
rename substance_household subs_hshld
rename parent_convicted_offenc prnt_convict
rename parental_separation prnt_sep

*Also have classic 10 ACE score 
tab ACEscore_classic_0_10yrs

********************************************************************************
**#RENAME MY ORIGINAL VARIABLES*************************************************
********************************************************************************

foreach var in pain_both heavy_both days_both length_both irreg pms_bin highed sclass findiff ethnicity menarche mated pated mat_sclass pat_sclass pain_cont heavy_cont days_bin_cont length_cont irreg_cont pms_cont pain_tsm heavy_tsm days_bin_tsm length_tsm irreg_tsm pms_tsm pain_doc heavy_doc heavy_days_cat mat_age marital_stat phone car housing rooms crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity findiff_ipw bfed_dur fai_long mat_ethnicity has_pub has_length has_irreg has_pms {
	rename `var'_org `var'
}

*Correct coding (switched to 1 or 2 instead of 0 or 1)

*Main outcomes
label define pain_both_lbl 0"No pain" 1"Pain", replace
label define heavy_both_lbl 0"Not heavy" 1"Heavy", replace
label define days_both_lbl 0"6 days or less" 1"7 days or more", replace
label define length_both_lbl 0"Normal (24-38)" 1"Freq. or Infreq.", replace
label define irreg_lbl 0"Regular" 1"Irregular", replace
label define pms_bin_lbl 0"No symptoms" 1"Any symptoms", replace
foreach var in pain_both heavy_both days_both length_both irreg pms_bin {
	recode `var' (1=0) (2=1)
	label values `var' `var'_lbl
}

*Restore to 0/1 coding
label define has_outcome 0"Data missing" 1"Data available", replace
foreach var in pub length irreg pms {
	recode has_`var' (1=0) (2=1)
	label values has_`var' has_outcome
}

   
********************************************************************************
**#TOUSE SAMPLE*****************************************************************
********************************************************************************
*Already gone from 7930 to 6661 
*All baseline confounder data (SEP and ethnicity)
count if highed!=. & sclass!=. & findiff!=.
count if highed!=. & sclass!=. & findiff!=. & ethnicity!=.
mark baseline
markout baseline highed sclass findiff ethnicity 	
*All ACEs data
egen aces_miss=rmiss2(phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep) //2305 with all
count if aces_miss==0 & baseline==1
count if baseline==1 & ACEscore_classic_0_10yrs!=. 
*Menarche data 
count if baseline==1 & ACEscore_classic_0_10yrs!=. & menarche!=.
**Outcomes
*Pub 
count if baseline==1 & ACEscore_classic_0_10yrs!=. & has_pub==1	
*Length
count if baseline==1 & ACEscore_classic_0_10yrs!=. & has_length==1
*Irreg
count if baseline==1 & ACEscore_classic_0_10yrs!=. & has_irreg==1
*PMS
count if baseline==1 & ACEscore_classic_0_10yrs!=. & has_pms==1

********************************************************************************
**#CC DESCRIPTIVES**************************************************************
********************************************************************************
capture drop touse
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Complete Case/Descriptives/Simple_CrossTabs_Missing", replace
mark touse
markout touse ACEscore_classic_0_10yrs highed sclass findiff ethnicity menarche

***SIMPLE DESCRIPTIVES 
**Exposures - individual ACEs
foreach var in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep {
	di "`var' IN ALL OBSERVED DATA"
	tab `var'
	di "`var' IN TOUSE SAMPLE"
	tab `var' if touse==1
}
**Exposure - ACE score
di "ACE score IN ALL OBSERVED DATA"
tab ACEscore_classic_0_10yrs
summ ACEscore_classic_0_10yrs, det
di "ACE score IN TOUSE SAMPLE"
tab ACEscore_classic_0_10yrs if touse==1
summ ACEscore_classic_0_10yrs if touse==1, det

**Outcomes 
foreach var in pain_both heavy_both days_both {
	di "`var' IN ALL OBSERVED DATA"
	tab `var'
	di "`var' IN TOUSE SAMPLE"
	tab `var' if touse==1 & has_pub==1
}
foreach var in length_both  irreg pms_bin {
	di "`var' IN ALL OBSERVED DATA"
	tab `var'
	di "`var' IN TOUSE SAMPLE"
	tab `var' if touse==1	
}
**Covariates
recode menarche (7.5/11.49=1 "Early <11.5") (11.5/13.5=2 "Normative 11.5-13.5") (13.501/17=3 "Late >13.5"), gen (menarche_cat)

foreach var in highed sclass findiff ethnicity menarche_cat {
	di "`var' IN ALL OBSERVED DATA"
	tab `var'
	di "`var' IN TOUSE SAMPLE"
	tab `var' if touse==1		
}
di "menarche IN ALL OBSERVED DATA"
summ menarche, det 
di "menarche IN TOUSE SAMPLE"
summ menarche if touse==1, det 

***CROSS TAB DESCRIPTIVES 
**Exposures by outcomes
foreach out in pain_both heavy_both days_both {
	foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep ACEscore_classic_0_10yrs {
		di "IN ALL OBSERVED DATA"
		tab `out' `exp', row col	
		di "IN TOUSE SAMPLE"
		tab `out' `exp' if touse==1 & has_pub==1, row col	
	}
	di "IN ALL OBSERVED DATA"
	bysort `out': summ ACEscore_classic_0_10yrs, det
	di "IN TOUSE SAMPLE"
	bysort `out': summ ACEscore_classic_0_10yrs if touse==1 & has_pub==1, det
}

foreach out in length_both irreg pms_bin {
	foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep ACEscore_classic_0_10yrs {
		di "IN ALL OBSERVED DATA"
		tab `out' `exp', row col	
		di "IN TOUSE SAMPLE"
		tab `out' `exp' if touse==1, row col	
	}
	di "IN ALL OBSERVED DATA"
	bysort `out': summ ACEscore_classic_0_10yrs, det
	di "IN TOUSE SAMPLE"
	bysort `out': summ ACEscore_classic_0_10yrs if touse==1, det
}

**Exposures by covariates
foreach out in pain_both heavy_both days_both {
	foreach cov in highed sclass findiff menarche_cat ethnicity {
		di "IN ALL OBSERVED DATA"
		tab `out' `cov', row col	
		di "IN TOUSE SAMPLE"
		tab `out' `cov' if touse==1 & has_pub==1, row col	
	}
	di "IN ALL OBSERVED DATA"
	bysort `out': summ menarche, det
	di "IN TOUSE SAMPLE"
	bysort `out': summ menarche if touse==1 & has_pub==1, det
}

foreach out in length_both irreg pms_bin {
	foreach cov in highed sclass findiff menarche_cat ethnicity {
		di "IN ALL OBSERVED DATA"
		tab `out' `cov', row col	
		di "IN TOUSE SAMPLE"
		tab `out' `cov' if touse==1, row col	
	}
	di "IN ALL OBSERVED DATA"
	bysort `out': summ menarche, det
	di "IN TOUSE SAMPLE"
	bysort `out': summ menarche if touse==1, det
}

**Covariates by exposures
foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep ACEscore_classic_0_10yrs{
	foreach cov in highed sclass findiff menarche_cat ethnicity {
		di "IN ALL OBSERVED DATA"
		tab `exp' `cov', row col	
		di "IN TOUSE SAMPLE"
		tab `exp' `cov' if touse==1, row col	
	}
	di "IN ALL OBSERVED DATA"
	bysort `exp': summ menarche, det
	di "IN TOUSE SAMPLE"
	bysort `exp': summ menarche if touse==1, det
}

foreach cov in highed sclass findiff menarche_cat ethnicity {
	di "IN ALL OBSERVED DATA"
	bysort `cov': summ ACEscore_classic_0_10yrs, det
	di "IN TOUSE SAMPLE"
	bysort `cov': summ ACEscore_classic_0_10yrs if touse==1, det
}

**Categorical ACE score 0, 1, 2, 3, 4+
recode ACEscore_classic_0_10yrs (0=0) (1=1) (2=2) (3=3) (4/10=4), gen (ace_four)
*Simple
tab ace_four
tab ace_four if touse==1
*Cross tab with outcomes
foreach out in pain_both heavy_both days_both {
	tab `out' ace_four, row col
	tab `out' ace_four if touse==1 & has_pub==1, row col
}
foreach out in length_both irreg pms_bin {
	tab `out' ace_four, row col
	tab `out' ace_four if touse==1, row col
}
*Cross tab with covariates
foreach cov in highed sclass findiff menarche_cat ethnicity {
	tab `cov' ace_four, row col
	tab `cov' ace_four if touse==1, row col	
}
bysort ace_four: summ menarche, det
bysort ace_four: summ menarche if touse==1, det


**Patterns and amount of missing
gen pub_vars=. 
replace pub_vars=1 if has_pub==1 

mdesc phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep ACEscore_classic_0_10yrs highed sclass findiff ethnicity menarche pain_both heavy_both days_both pub_vars length_both irreg pms_bin 

*Number of missing variables
egen aces_miss=rmiss2(phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep) 
egen impute_miss=rmiss2(phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep highed sclass findiff ethnicity)
egen all_vars=rmiss2(phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep highed sclass findiff ethnicity menarche pub_vars length_both irreg pms_bin)
tab1 aces_miss impute_miss all_vars


log close

********************************************************************************
**#MAIN CC ANALYSIS*************************************************************
********************************************************************************
capture drop touse pubuse
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Complete Case/Analysis/ACE_MainResults_log.smcl", replace
mark touse
markout touse ACEscore_classic_0_10yrs highed sclass findiff ethnicity menarche
mark pubuse 
markout pubuse ACEscore_classic_0_10yrs highed sclass findiff ethnicity menarche pain_both heavy_both days_both

*Individual ACEs (binary) and ACE score as continuous 
foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep ACEscore_classic_0_10yrs {
	foreach out in pain_both heavy_both days_both {
		di "CRUDE `exp' and `out' "
		logistic `out' `exp' if pubuse==1
		di "ADJUSTED `exp' and `out' "
		logistic `out' `exp' ethnicity highed sclass findiff if pubuse==1
	}
}

foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep ACEscore_classic_0_10yrs  {
	foreach out in length_both irreg pms_bin {
		di "CRUDE `exp' and `out' "
		logistic `out' `exp' if touse==1
		di "ADJUSTED `exp' and `out' "
		logistic `out' `exp' ethnicity highed sclass findiff if touse==1
	}
}

*Score as categorical
foreach out in pain_both heavy_both days_both {
	di "CRUDE categorical score and `out' "
	logistic `out' i.ACEscore_classic_0_10yrs if pubuse==1
	di "ADJUSTED categorical score and `out' "
	logistic `out' i.ACEscore_classic_0_10yrs ethnicity highed sclass findiff if pubuse==1
	}
	
foreach out in length_both irreg pms_bin {
	di "CRUDE categorical score and `out' "
	logistic `out' i.ACEscore_classic_0_10yrs if touse==1
	di "ADJUSTED categorical score and `out' "
	logistic `out' i.ACEscore_classic_0_10yrs ethnicity highed sclass findiff if touse==1
}

*Score as categorical (four plus)
foreach out in pain_both heavy_both days_both {
	di "CRUDE categorical score and `out' "
	logistic `out' i.ace_four if pubuse==1
	di "ADJUSTED categorical score and `out' "
	logistic `out' i.ace_four ethnicity highed sclass findiff if pubuse==1
	}
	
foreach out in length_both irreg pms_bin {
	di "CRUDE categorical score and `out' "
	logistic `out' i.ace_four if touse==1
	di "ADJUSTED categorical score and `out' "
	logistic `out' i.ace_four ethnicity highed sclass findiff if touse==1
}

log close

********************************************************************************
**#SENSITIVITY ANALYSIS*********************************************************
********************************************************************************

**Additional prep
*Length should be binary
rename length_cont length_cat_cont
recode length_cat_cont (0=0 "Normal") (1/2=1 "Freq. or Infreq."), gen (length_cont)
rename length_tsm length_cat_tsm
recode length_cat_tsm (0=0 "Normal") (1/2=1 "Freq. or Infreq."), gen (length_tsm)

*Baseline as 0
label define pain_doctor_lbl 2"Went to doctor" 1"No doctor" 0"No pain" , replace
recode pain_doc (1=0) (2=1) (3=2)
label values pain_doc pain_doctor_lbl
label define heavy_doctor_lbl 2"Went to doctor" 1"No doctor" 0"No heavy bleeding" , replace
recode heavy_doc (1=0) (2=1) (3=2)
label values heavy_doc heavy_doctor_lbl
label define heavycat_lbl 0"Neither" 1"Prolonged only" 2"Heavy only" 3"Both", replace
recode heavy_days_cat (1=0) (2=1) (3=2) (4=3)
label values heavy_days_cat heavycat_lbl

**Categorical ACE score 0, 1, 2, 3, 4+
recode ACEscore_classic_0_10yrs (0=0) (1=1) (2=2) (3=3) (4/10=4), gen (ace_four)
*Simple
tab ace_four
tab ace_four if touse==1

**Sensitivity Descriptives 
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Complete Case/Descriptives/Sensitivity_Descriptives.smcl", replace
mark touse
markout touse ACEscore_classic_0_10yrs highed sclass findiff ethnicity menarche
mark pubuse 
markout pubuse ACEscore_classic_0_10yrs highed sclass findiff ethnicity menarche pain_both heavy_both days_both

*1* Contraception 
tab1 pain_cont heavy_cont days_bin_cont
tab1 length_cont irreg_cont pms_cont

tab1 pain_cont heavy_cont days_bin_cont if pubuse==1
tab1 length_cont irreg_cont pms_cont if touse==1

*2* 3 years 
tab1 pain_tsm heavy_tsm days_bin_tsm
tab1 length_tsm irreg_tsm pms_tsm

tab1 pain_tsm heavy_tsm days_bin_tsm if pubuse==1
tab1 length_tsm irreg_tsm pms_tsm if touse==1

*3* Doctor
tab1 pain_doc heavy_doc
tab1 pain_doc heavy_doc if pubuse==1

*4* Separating heavy form prolonged
tab heavy_days_cat
tab heavy_days_cat if pubuse==1

log close

**Analysis
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Complete Case/Analysis/Sensitivty_Analysis_log", replace 
mark touse
markout touse ACEscore_classic_0_10yrs highed sclass findiff ethnicity menarche
mark pubuse 
markout pubuse ACEscore_classic_0_10yrs highed sclass findiff ethnicity menarche pain_both heavy_both days_both

*1* Contraception
*Individual ACEs and full score (continuous)
foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep ACEscore_classic_0_10yrs {
	foreach out in pain_cont heavy_cont days_bin_cont {
		di "CRUDE `exp' and `out' "
		logistic `out' `exp' if pubuse==1
		di "ADJUSTED `exp' and `out' "
		logistic `out' `exp' highed sclass findiff ethnicity if pubuse==1
	}
	foreach out in length_cont irreg_cont pms_cont {
		di "CRUDE `exp' and `out' "
		logistic `out' `exp' if touse==1
		di "ADJUSTED `exp' and `out' "
		logistic `out' `exp' highed sclass findiff ethnicity if touse==1
	}
}
*ACE four categorical score 
foreach out in pain_cont heavy_cont days_bin_cont {
	di "CRUDE `exp' and `out' "
	logistic `out' i.ace_four if pubuse==1
	di "ADJUSTED `exp' and `out' "
	logistic `out' i.ace_four highed sclass findiff ethnicity if pubuse==1
}
foreach out in length_cont irreg_cont pms_cont {
	di "CRUDE `exp' and `out' "
	logistic `out' i.ace_four if touse==1
	di "ADJUSTED `exp' and `out' "
	logistic `out' i.ace_four highed sclass findiff ethnicity if touse==1
}

*2* 3 years TSM
*Individual ACEs and full score (continuous)
foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep ACEscore_classic_0_10yrs {
	foreach out in pain_tsm heavy_tsm days_bin_tsm {
		di "CRUDE `exp' and `out' "
		logistic `out' `exp' if pubuse==1
		di "ADJUSTED `exp' and `out' "
		logistic `out' `exp' highed sclass findiff ethnicity if pubuse==1
	}
	foreach out in length_tsm irreg_tsm pms_tsm {
		di "CRUDE `exp' and `out' "
		logistic `out' `exp' if touse==1
		di "ADJUSTED `exp' and `out' "
		logistic `out' `exp' highed sclass findiff ethnicity if touse==1
	}
}
*ACE four categorical score 
foreach out in pain_tsm heavy_tsm days_bin_tsm {
	di "CRUDE `exp' and `out' "
	logistic `out' i.ace_four if pubuse==1
	di "ADJUSTED `exp' and `out' "
	logistic `out' i.ace_four highed sclass findiff ethnicity if pubuse==1
}
foreach out in length_tsm irreg_tsm pms_tsm {
	di "CRUDE `exp' and `out' "
	logistic `out' i.ace_four if touse==1
	di "ADJUSTED `exp' and `out' "
	logistic `out' i.ace_four highed sclass findiff ethnicity if touse==1
}

*3/4* Doctor and heavy/prolonged 
foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep ACEscore_classic_0_10yrs {
	foreach out in pain_doc heavy_doc heavy_days_cat {
		di "CRUDE `exp' and `out' "
		mlogit `out' `exp' if pubuse==1, rr
		di "ADJUSTED `exp' and `out' "
		mlogit `out' `exp' highed sclass findiff ethnicity if pubuse==1, rr
	}
}
*ACE four categorical score 
foreach out in pain_doc heavy_doc heavy_days_cat {
	di "CRUDE `exp' and `out' "
	mlogit `out' i.ace_four if pubuse==1, rr
	di "ADJUSTED `exp' and `out' "
	mlogit `out' i.ace_four highed sclass findiff ethnicity if pubuse==1, rr
}

log close 


********************************************************************************
***#CONTINNUOUS OR CATEGORICAL SCORE********************************************
********************************************************************************

mark touse
markout touse ACEscore_classic_0_10yrs highed sclass findiff ethnicity menarche
mark pubuse 
markout pubuse ACEscore_classic_0_10yrs highed sclass findiff ethnicity menarche pain_both heavy_both days_both

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Complete Case/Categorical exposure/Score_ContorCat", replace

***STEP 1: LR TESTS (complete case data only)

*Score as categorical (four plus)
foreach out in pain_both heavy_both days_both {
	logistic `out' i.ace_four if pubuse==1
	estimates store `out'_cat
	logistic `out' ace_four if pubuse==1
	estimates store `out'_cont
	lrtest `out'_cont `out'_cat 	
	}
	
foreach out in length_both irreg pms_bin {
	logistic `out' i.ace_four if touse==1
	estimates store `out'_cat
	logistic `out' ace_four if touse==1
	estimates store `out'_cont
	lrtest `out'_cont `out'_cat 
}

***STEP 2: LINEAR P VALUE APPROPRIATE?
*Pain = 0.0239 = NO = keep original p value (testparm)
*Heavy = 0.1797 = YES = p is 0.000
*Days = 0.3201 = YES = p is 0.742
*Length = 0.4056 = YES = p is 0.226
*Irreg = 0.2297 = YES = p is 0.046
*PMS = 0.1542 = YES = p is 0.006

***STEP 2.5:
*adjusted complete case p values (continuous)
foreach out in pain_both heavy_both days_both {
	logistic `out' ace_four ethnicity highed sclass findiff if pubuse==1 
	}
	
foreach out in length_both irreg pms_bin {
	logistic `out' ace_four ethnicity highed sclass findiff if touse==1 
}

*complete case testparm for pain
logistic pain_both i.ace_four if pubuse==1
testparm i.ace_four
logistic pain_both i.ace_four ethnicity highed sclass findiff if pubuse==1 
testparm i.ace_four

log close 

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Complete Case/Categorical exposure/Score_ContorCat", append

***STEP 3: UPDATED P VALUES IN MI ANALYSES 

foreach out in pain_both heavy_both days_both {
		mi estimate, or post: logistic `out' number_aces [pw=ipw_pub]
		mi estimate, or post: logistic `out' number_aces ethnicity highed sclass findiff [pw=ipw_pub]
	}

		mi estimate, or post: logistic length_both number_aces [pw=ipw_length]
		mi estimate, or post: logistic length_both number_aces ethnicity highed sclass findiff [pw=ipw_length]

		mi estimate, or post: logistic irreg number_aces [pw=ipw_irreg]
		mi estimate, or post: logistic irreg number_aces ethnicity highed sclass findiff [pw=ipw_irreg]
		
		mi estimate, or post: logistic pms_bin number_aces [pw=ipw_pms]
		mi estimate, or post: logistic pms_bin number_aces ethnicity highed sclass findiff [pw=ipw_pms]
		
		
log close

************************************************************************************************************************************
************************************************************************************************************************************
**#*PRE-IMPUTATION******************************************************************************************************************
************************************************************************************************************************************
************************************************************************************************************************************


********************************************************************************
***#IPW VARIABLE SELECTION******************************************************
********************************************************************************

**Pain/heavy/days outcomes 
**Lasso approach
lasso logit has_pub mat_age marital_stat phone car i.housing rooms i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long  mat_ethnicity
lassocoef
logistic has_pub mat_age phone car i.housing i.crowding i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur 
estat gof				//x(4471)^2 = 4494.56; p = 0.3991
estat gof, group(10)	//x(8)^2 = 4.20; p = 0.8386
**Length
*Lasso
lasso logit has_length mat_age marital_stat phone car i.housing rooms i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long mat_ethnicity
lassocoef
logistic has_length mat_age phone car i.housing i.crowding i.first_preg smoke_ever epds i.parity i.mated i.bfed_dur fai_long 
estat gof				//x(4580)^2 = 4595.72; p = 0.4321
estat gof, group(10)	//x(8)^2 = 5.65; p = 0.6860
**Irregular
*Lasso
lasso logit has_irreg mat_age marital_stat phone car i.housing rooms i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long mat_ethnicity
lassocoef
logistic has_irreg mat_age i.housing i.crowding dbl_glaze i.first_preg smoke_ever epds i.parity i.mated findiff_ipw i.bfed_dur  
estat gof				//x(4416)^2 = 4448.24; p = 0.3634
estat gof, group(10)	//x(8)^2 = 4.89; p = 0.7696
**PMS
*Lasso
lasso logit has_pms mat_age marital_stat phone car i.housing rooms i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long mat_ethnicity
lassocoef
logistic has_pms mat_age car i.housing i.crowding smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur 
estat gof				//x(4463)^2 = 4473.34; p = 0.4537
estat gof, group(10)	//x(8)^2 = 3.16; p = 0.9239

**LR tests to compare lasso models with fully inclusive models (same for each outcomes - All variables in the inclusive model other than marital_stat , rooms, and mat_ethnicity)
*Need the same observations (i.e. all IPW data)
mark ipw 
markout ipw mat_age phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass bfed_dur fai_long
*Pub outcomes - p = 0.8653 (no difference btw models)
logistic has_pub mat_age phone car i.housing i.crowding i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur if ipw==1
estimates store pub_lasso 
logistic has_pub mat_age phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if ipw==1
estimate store pub_full
lrtest pub_lasso pub_full 
*Length - p = 0.4278
logistic has_length mat_age i.housing i.crowding dbl_glaze i.first_preg smoke_ever epds i.parity i.mated findiff_ipw i.bfed_dur if ipw==1
estimate store length_lasso
logistic has_length mat_age phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if ipw==1
estimate store length_full
lrtest length_lasso length_full 
*Irregular - p = 0.9793
logistic has_irreg mat_age i.housing i.crowding dbl_glaze i.first_preg smoke_ever epds i.parity i.mated findiff_ipw i.bfed_dur if ipw==1
estimates store irreg_lasso
logistic has_irreg mat_age phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if ipw==1
estimate store irreg_full
lrtest irreg_lasso irreg_full
*PMS - p = 0.9259
logistic has_pms mat_age car i.housing i.crowding smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur if ipw==1
estimates store pms_lasso
logistic has_pms mat_age phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if ipw==1
estimate store pms_full
lrtest pms_lasso pms_full
*Check gof
estimates clear
foreach var in pub length irreg pms {
	quietly logistic has_`var' mat_age phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long
	estat gof
	estat gof, group(10)
}


***SOME CONVREGENCE ISSUES WITH IMPUTATION - SAMPLE NEEDS TO BE DIFFERENT = 5,245
*With ethnicity, findiff_ipw, and fai_long
mark impute
markout impute ethnicity findiff_ipw fai_long
drop if impute==0
*Then 50% of IPW variables
egen ipw_vars=rmiss2(mat_age phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass bfed_dur fai_long)
gen ipw_drop=0
replace ipw_drop=1 if inrange(ipw_vars,9,14) 
drop if ipw_drop==1

*What are the outcome Ns?
tab has_pub
tab has_length
tab has_irreg
tab has_pms

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/MI and IPW/Missing_Data_Patterns", replace
**Missing data patterns in this to be imputed sample

**Patterns and amount of missing
gen pub_vars=. 
replace pub_vars=1 if has_pub==1 

**My variables
mdesc phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep ACEscore_classic_0_10yrs highed sclass findiff ethnicity menarche pain_both heavy_both days_both pub_vars length_both irreg pms_bin mat_age phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass bfed_dur fai_long

*Number of missing variables
egen aces_miss=rmiss2(phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep) 
egen impute_miss=rmiss2(phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep highed sclass findiff ethnicity mat_age phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long)
egen all_vars=rmiss2(phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep highed sclass findiff ethnicity menarche pub_vars length_both irreg pms_bin mat_age phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long)
tab1 aces_miss impute_miss all_vars

/**Auxiliary variables
mdesc c804_org kz030_org kz029_org dw002_org dw042_org a006_org mz028b_org a525_org c645a_org c666a_org pb325a_org pb342a_org kz021_org b593 c472_org c520_org c522_org b370_org a600_org pb188a_org b587 sc_household_18wgest_org pb130_org pb098_org t3336 t5404 t5300 fa5404 t3316 ypa5005_dup_org ypa5007_dup ypa5009_dup_org ypa5011_dup_org ypa5013_dup_org ypa5015_dup ypa5017_dup_org ypa5050 c521_org c523_org h731_org h733_org h734_org c525_org social_class_0_10yrs financial_difficulties_0_10yrs neighbourhood_0_10yrs social_support_child_0_10yrs social_support_parent_0_10yrs physical_illness_child_0_10yrs physical_illness_parent_0_10yrs parent_child_bond_0_10yrs*/

*Number
egen aux_miss=rmiss2(c804_org kz030_org kz029_org dw002_org dw042_org a006_org mz028b_org a525_org c645a_org c666a_org pb325a_org pb342a_org kz021_org b593 c472_org c520_org c522_org b370_org a600_org pb188a_org b587 sc_household_18wgest_org pb130_org pb098_org t3336 t5404 t5300 fa5404 t3316 ypa5005_dup_org ypa5007_dup ypa5009_dup_org ypa5011_dup_org ypa5013_dup_org ypa5015_dup ypa5017_dup_org ypa5050 c521_org c523_org h731_org h733_org h734_org c525_org social_class_0_10yrs financial_difficulties_0_10yrs neighbourhood_0_10yrs social_support_child_0_10yrs social_support_parent_0_10yrs physical_illness_child_0_10yrs physical_illness_parent_0_10yrs parent_child_bond_0_10yrs)
tab aux_miss

log close

************************************************************************************************************************************
************************************************************************************************************************************
**#*POST-IMPUTATION (R SCRIPT 3)****************************************************************************************************
************************************************************************************************************************************
************************************************************************************************************************************

use "/Volumes/Studies/ALSPAC Menstrual PhD/alspacKids_ACE_0_10ALSPAC_imputedGemma.dta", clear

*Rename
foreach var in physical_abuse emotional_abuse emotional_neglect sexual_abuse bullying violence_between_parnts mentl_hlth_prblms_r_scd substance_household parent_convicted_offenc parental_separation {
	rename `var'_0_10yrs `var'
}
rename physical_abuse phys_abus
rename emotional_abuse emot_abus
rename emotional_neglect emot_neg
rename sexual_abuse sexu_abus
rename bullying bully
rename violence_between_parnts viol_parent
rename mentl_hlth_prblms_r_scd prnt_mntlhlth
rename substance_household subs_hshld
rename parent_convicted_offenc prnt_convict
rename parental_separation prnt_sep

foreach var in social_class financial_difficulties neighbourhood social_support_child social_support_parent physical_illness_child physical_illness_parent parent_child_bond ACEscore_extended ACEscore_classic {
	rename `var'_0_10yrs `var'
}
rename sc_household_18wgest_org sc_18wgest

rename financial_difficulties fin_diffs
rename social_support_child support_child
rename social_support_parent support_prnt
rename physical_illness_child physill_child
rename physical_illness_parent physill_prnt
rename parent_child_bond prntchild_bond
rename ACEscore_extended score_ext
rename ACEscore_classic score_classic

*Save
save "/Volumes/157/working/data/Imputed_G1_ACEs_28Nov24", replace

*Register as imputed
mi import flong, m(_imp) id(_id) imputed (kz030_org dw002_org dw042_org a006_org a525_org c645a_org c666a_org pb325a_org pb342a_org b593 c472_org a600_org pb188a_org b587 sc_18wgest pb130_org pb098_org t3336 t5404 t5300 fa5404 t3316 ypa5050 h731_org h733_org h734_org pain_both_org heavy_both_org days_both_org length_both_org irreg_org pms_bin_org highed_org sclass_org phone_org car_org housing_org crowding_org dbl_glaze_org first_preg_org smoke_preg_org smoke_ever_org epds_org parity_org mated_org bfed_dur_org phys_abus sexu_abus emot_abus emot_neg bully viol_parent subs_hshld prnt_mntlhlth prnt_convict prnt_sep social_class fin_diffs neighbourhood support_child support_prnt physill_child physill_prnt prntchild_bond score_ext score_classic)

*Not imputed 
mi register regular (_imp _id c804_org kz029_org mz028b_org kz021_org c520_org c522_org b370_org ypa5005_dup_org ypa5007_dup ypa5009_dup_org ypa5011_dup_org ypa5013_dup_org ypa5017_dup_org ypa5015_dup c521_org c523_org c525_org has_pub_org has_length_org has_irreg_org has_pms_org findiff_org ethnicity_org mat_age_org findiff_ipw_org fai_long_org)

*Check all variables registered
mi describe

*Save
save "/Volumes/157/working/data/Imputed_G1_ACEs_28Nov24", replace

********************************************************************************
**#IMPUTATION CHECKS************************************************************
********************************************************************************
label define imputed 0"Observed" 1"Imputed", replace
gen imputed=0 if _mi_m==0
replace imputed=1 if _mi_m>0
label values imputed imputed

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Imputation/Post_Imp_Checks", replace
**Exposures and covariates that have been imputed
*Complete variables included ethnicity findiff mat_age findiff_ipw and fai_long

*Proportions
foreach var in highed_org sclass_org phone_org car_org housing_org crowding_org dbl_glaze_org first_preg_org smoke_preg_org smoke_ever_org parity_org mated_org bfed_dur_org phys_abus sexu_abus emot_abus emot_neg bully viol_parent subs_hshld prnt_mntlhlth prnt_convict prnt_sep score_classic {
	tab `var' imputed, row col
}
*Means
foreach var in epds_org score_classic {
	summ `var' if imputed==0, det
	summ `var' if imputed==1, det
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/MI and IPW/Post_Imp_Checks.smcl", append
*IPW Vars that haven't been imputed 
foreach var in findiff ethnicity  {
	tab `var' imputed, row col
}
*Means
foreach var in mat_age findiff_ipw fai_long {
	summ `var' if imputed==0, det
	summ `var' if imputed==1, det
}

log close

*Distributions 
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Imputation"

foreach var in epds_org score_classic {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck.png, replace 
}

**FMI and Mcerror checks - loop through main adjusted models 
**Deletion approach for outcome variables and menarche - then check FMIs and mcerror
foreach var in pain_both_org heavy_both_org days_both_org {
	replace `var'=. if has_pub_org==1
}
replace length_both_org=. if has_length_org==1
replace irreg_org=. if has_irreg_org==1
replace pms_bin_org=. if has_pms_org==1

*Rename/restore to 0/1 coding
foreach var in pain_both heavy_both days_both length_both irreg pms_bin {
	rename `var'_org `var'
}

*Main outcomes
label define pain_both_lbl 0"No pain" 1"Pain", replace
label define heavy_both_lbl 0"Not heavy" 1"Heavy", replace
label define days_both_lbl 0"6 days or less" 1"7 days or more", replace
label define length_both_lbl 0"Normal (24-38)" 1"Freq. or Infreq.", replace
label define irreg_lbl 0"Regular" 1"Irregular", replace
label define pms_bin_lbl 0"No symptoms" 1"Any symptoms", replace
foreach var in pain_both heavy_both days_both length_both irreg pms_bin {
	recode `var' (1=0) (2=1)
	label values `var' `var'_lbl
}

*Loop through models               
foreach exp in phys_abus sexu_abus emot_abus emot_neg bully viol_parent subs_hshld prnt_mntlhlth prnt_convict prnt_sep {
	foreach out in pain heavy days_both length_both irreg pms_bin {
		mi estimate, mcerror : logistic `out' `exp' ethnicity_org highed_org sclass_org findiff_org
	}
}
*Across all models, largest FMI: 0.3535

*Mcerror checks - all good 

********************************************************************************
**#PASSIVELY DERIVE SCORE VARIABLE**********************************************
********************************************************************************
label define ace_bin_lbl 0"No" 1"Yes", replace
foreach var in phys_abus sexu_abus emot_abus emot_neg bully viol_parent subs_hshld prnt_mntlhlth prnt_convict prnt_sep {
	recode `var' (1=0) (2=1)
	label values `var' ace_bin_lbl
}

replace score_classic=. if _mi_m>0
forvalues j = 1/50 {
	replace score_classic=phys_abus + sexu_abus + emot_abus + emot_neg + bully + viol_parent + subs_hshld + prnt_mntlhlth + prnt_convict + prnt_sep if _mi_m==`j'
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Imputation/Post_Imp_Checks", append

*Passively re-derived score
*replace score_classic=. if _mi_m>0
*forvalues j = 1/50 {
*	replace score_classic=phys_abus + sexu_abus + emot_abus + emot_neg + bully + viol_parent + subs_hshld + prnt_mntlhlth + prnt_convict + prnt_sep if _mi_m==`j'
*}

summ score_classic if imputed==0, det
summ score_classic if imputed==1, det

log close

*Save
save "/Volumes/157/working/data/Imputed_G1_ACEs_28Nov24", replace


********************************************************************************
**#GENERATING WEIGHTS FOR ANALYSIS**********************************************
********************************************************************************
*Open dataset
use "/Volumes/157/working/data/Imputed_G1_ACEs_28Nov24", clear

*Restore names and 0/1 coding in outcomes
foreach var in has_pub has_length has_irreg has_pms highed sclass findiff ethnicity mat_age phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long {
	rename `var'_org `var'
}
label define has_outcome 0"Data missing" 1"Data available"
foreach var in has_pub has_length has_irreg has_pms {
	recode `var' (1=0) (2=1)
	label values `var' has_outcome
}

**IPW for each imputed dataset (per outcome)
*Pub
forvalues j = 1/50 {
	logistic has_pub mat_age phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if _mi_m==`j'
	predict pubp`j' if _mi_m==`j'
	}
*Length
forvalues j = 1/50 {
	logistic has_length mat_age phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if _mi_m==`j'
	predict lengthp`j' if _mi_m==`j'
	} 
*Irreg
forvalues j = 1/50 {
	logistic has_irreg mat_age phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if _mi_m==`j'
	predict irregp`j' if _mi_m==`j'
	}
*PMS
forvalues j = 1/50 {
	logistic has_pms mat_age phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if _mi_m==`j'
	predict pmsp`j' if _mi_m==`j'
	}	
	
**Consistent name across datasets
foreach var in pub length irreg pms {
	gen `var'p = `var'p1
	forvalues j = 2/50 {
		replace `var'p = `var'p`j' if `var'p==.
	}
}

**Create probability and weights
foreach var in pub length irreg pms {
	gen prob_`var'=`var'p if has_`var'==1
	replace prob_`var'=1-`var'p if has_`var'==0
	gen ipw_`var'=1/prob_`var'
}

*Quick check (first 15 obs of imputation 1)
list _mi_m has_pub pubp prob_pub ipw_pub in 5246/5260
*Imputation 2
list _mi_m has_pub pubp prob_pub ipw_pub in 10491/10505

*Tidy up (remove original p per dataset variables as have overall one with all information)
forvalues j = 1/50 {
	foreach var in pub length irreg pms {
		drop `var'p`j'
	}
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/MI and IPW/Summary_Weights", replace
*Summary of weights
foreach var in pub length irreg pms {
	summ ipw_`var', det
}
log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/MI and IPW"

foreach var in pub length irreg pms {
	histogram ipw_`var'
	graph export `var'_weights.png, replace 
}	
 
*Replace dataset now outcomes deleted and weights derived
save "/Volumes/157/working/data/Imputed_G1_ACEs_28Nov24", replace

********************************************************************************
**#WEIGHTED ANALYSIS************************************************************
********************************************************************************
use "/Volumes/157/working/data/Imputed_G1_ACEs_28Nov24", clear


gen number_aces = 0
replace number_aces=1 if score_classic==1
replace number_aces=2 if score_classic==2
replace number_aces=3 if score_classic==3
replace number_aces=4 if inrange(score_classic,4,10)
label define score_lbl 0"0" 1"1" 2"2" 3"3" 4"4 or more", replace
label values number_aces score_lbl


**Descriptives
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/MI and IPW/Weighted_Descriptives", replace

**Simple - full sample
*Exposures
foreach var in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep number_aces {
	mi estimate: proportion `var'
}

*Confounders
foreach var in highed sclass findiff ethnicity {
	mi estimate: proportion `var'
}

*Outcomes
foreach out in pain_both heavy_both days_both length_both irreg pms_bin {
	tab `out' if imputed==0
}


**Simple - analysis samples
*Exposures 
foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep number_aces {
	foreach sample in pub length irreg pms {
		mi estimate: proportion `exp' if has_`sample'==1
	}
}

*Confounders  
foreach var in highed sclass findiff ethnicity {
	foreach sample in pub length irreg pms {
		mi estimate: proportion `var' if has_`sample'==1
	}
}


**Cross tabs - analysis samples (exp and cov by out)
foreach var in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep number_aces highed sclass findiff ethnicity {
	foreach out in pain_both heavy_both days_both length_both irreg pms_bin {
		mi estimate: proportion `var', over(`out')	
	}
}

**Cross tabs - analysis samples (IPW variables by out)
foreach var in phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever parity mated sclass bfed_dur {
	foreach out in pain_both heavy_both days_both length_both irreg pms_bin {
		mi estimate: proportion `var', over(`out')	
	}
}


foreach var in mat_age epds findiff_ipw {
	foreach out in pain_both heavy_both days_both length_both irreg pms_bin {
		mi estimate: mean `var', over(`out')
	}
}


log close

**Analysis 

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/MI and IPW/MainResults_log", replace

*Individual ACEs (binary) 
foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep {
	foreach out in pain_both heavy_both days_both {
		mi estimate, or post: logistic `out' `exp' [pw=ipw_pub]
		estimates store `exp'_`out'c
		mi estimate, or post: logistic `out' `exp' ethnicity highed sclass findiff [pw=ipw_pub]
		estimates store `exp'_`out'
	}
}
	   
foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep {
		mi estimate, or post: logistic length_both `exp' [pw=ipw_length]
		estimates store `exp'_length_bothc
		mi estimate, or post: logistic length_both `exp' ethnicity highed sclass findiff [pw=ipw_length]
		estimates store `exp'_length_both

}
	   
foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep {
		mi estimate, or post: logistic irreg `exp' [pw=ipw_irreg]
		estimates store `exp'_irregc
		 mi estimate, or post: logistic irreg `exp' ethnicity highed sclass findiff [pw=ipw_irreg]
		estimates store `exp'_irreg

}

foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep {
		mi estimate, or post: logistic pms_bin `exp' [pw=ipw_pms]
		estimates store `exp'_pms_binc
		mi estimate, or post: logistic pms_bin `exp' ethnicity highed sclass findiff [pw=ipw_pms]
		estimates store `exp'_pms_bin
}


*Score as categorical (four plus)
foreach out in pain_both heavy_both days_both {
		mi estimate, or post: logistic `out' i.number_aces [pw=ipw_pub]
		estimates store four_`out'c
		mi estimate, or post: logistic `out' i.number_aces ethnicity highed sclass findiff [pw=ipw_pub]
		estimates store four_`out'	
	}

		mi estimate, or post: logistic length_both i.number_aces [pw=ipw_length]
		estimates store four_lengthc
		mi estimate, or post: logistic length_both i.number_aces ethnicity highed sclass findiff [pw=ipw_length]
		estimates store four_length

		mi estimate, or post: logistic irreg i.number_aces [pw=ipw_irreg]
		estimates store four_irregc
		mi estimate, or post: logistic irreg i.number_aces ethnicity highed sclass findiff [pw=ipw_irreg]
		estimates store four_irreg
		
		mi estimate, or post: logistic pms_bin i.number_aces [pw=ipw_pms]
		estimates store four_pmsc
		mi estimate, or post: logistic pms_bin i.number_aces ethnicity highed sclass findiff [pw=ipw_pms]
		estimates store four_pms
		
log close

*Excel
capture erase "main_results.xls"

estout phys_abus_pain_bothc emot_abus_pain_bothc emot_neg_pain_bothc sexu_abus_pain_bothc bully_pain_bothc viol_parent_pain_bothc prnt_mntlhlth_pain_bothc subs_hshld_pain_bothc prnt_convict_pain_bothc prnt_sep_pain_bothc four_pain_bothc using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons) eform title(pain) note(crude)
estout phys_abus_pain_both emot_abus_pain_both emot_neg_pain_both sexu_abus_pain_both bully_pain_both viol_parent_pain_both prnt_mntlhlth_pain_both subs_hshld_pain_both prnt_convict_pain_both prnt_sep_pain_both four_pain_both using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity highed sclass findiff) eform note(adjusted)
			
estout phys_abus_heavy_bothc emot_abus_heavy_bothc emot_neg_heavy_bothc sexu_abus_heavy_bothc bully_heavy_bothc viol_parent_heavy_bothc prnt_mntlhlth_heavy_bothc subs_hshld_heavy_bothc prnt_convict_heavy_bothc prnt_sep_heavy_bothc four_heavy_bothc using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons) eform title(heavy) note(crude)
estout phys_abus_heavy_both emot_abus_heavy_both emot_neg_heavy_both sexu_abus_heavy_both bully_heavy_both viol_parent_heavy_both prnt_mntlhlth_heavy_both subs_hshld_heavy_both prnt_convict_heavy_both prnt_sep_heavy_both four_heavy_both using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity highed sclass findiff) eform note(adjusted)		
		
estout phys_abus_days_bothc emot_abus_days_bothc emot_neg_days_bothc sexu_abus_days_bothc bully_days_bothc viol_parent_days_bothc prnt_mntlhlth_days_bothc subs_hshld_days_bothc prnt_convict_days_bothc prnt_sep_days_bothc four_days_bothc using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons) eform title(days) note(crude)
estout phys_abus_days_both emot_abus_days_both emot_neg_days_both sexu_abus_days_both bully_days_both viol_parent_days_both prnt_mntlhlth_days_both subs_hshld_days_both prnt_convict_days_both prnt_sep_days_both four_days_both using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity highed sclass findiff) eform note(adjusted)
		
estout phys_abus_length_bothc emot_abus_length_bothc emot_neg_length_bothc sexu_abus_length_bothc bully_length_bothc viol_parent_length_bothc prnt_mntlhlth_length_bothc subs_hshld_length_bothc prnt_convict_length_bothc prnt_sep_length_bothc four_lengthc using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons) eform title(length) note(crude)
estout phys_abus_length_both emot_abus_length_both emot_neg_length_both sexu_abus_length_both bully_length_both viol_parent_length_both prnt_mntlhlth_length_both subs_hshld_length_both prnt_convict_length_both prnt_sep_length_both four_length using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity highed sclass findiff) eform note(adjusted)		
		
estout phys_abus_irregc emot_abus_irregc emot_neg_irregc sexu_abus_irregc bully_irregc viol_parent_irregc prnt_mntlhlth_irregc subs_hshld_irregc prnt_convict_irregc prnt_sep_irregc four_irregc using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons) eform title(irreg) note(crude)
estout phys_abus_irreg emot_abus_irreg emot_neg_irreg sexu_abus_irreg bully_irreg viol_parent_irreg prnt_mntlhlth_irreg subs_hshld_irreg prnt_convict_irreg prnt_sep_irreg four_irreg using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity highed sclass findiff) eform note(adjusted)		
		
estout phys_abus_pms_binc emot_abus_pms_binc emot_neg_pms_binc sexu_abus_pms_binc bully_pms_binc viol_parent_pms_binc prnt_mntlhlth_pms_binc subs_hshld_pms_binc prnt_convict_pms_binc prnt_sep_pms_binc four_pmsc using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons) eform title(pms) note(crude)
estout phys_abus_pms_bin emot_abus_pms_bin emot_neg_pms_bin sexu_abus_pms_bin bully_pms_bin viol_parent_pms_bin prnt_mntlhlth_pms_bin subs_hshld_pms_bin prnt_convict_pms_bin prnt_sep_pms_bin four_pms using "main_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity highed sclass findiff) eform note(adjusted)		


**Truncated weights

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/MI and IPW/TruncatedWeights_Results_log", replace 

*Pub outcomes
summ ipw_pub, det
local pub95=r(p95)			
local pub99=r(p99)	
gen trunc95_pub=ipw_pub
replace trunc95_pub=`pub95' if trunc95_pub>`pub95' & trunc95_pub<.  
gen trunc99_pub=ipw_pub
replace trunc99_pub=`pub99' if trunc99_pub>`pub99' & trunc99_pub<.
			  
			  
foreach out in pain_both heavy_both days_both {
	foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep number_aces {
		di "95th percentile weights"
		mi estimate, or: logistic `out' i.`exp' [pw=trunc95_pub]
		mi estimate, or: logistic `out' i.`exp' ethnicity highed sclass findiff [pw=trunc95_pub]
		di "99th percentile weights"
		mi estimate, or: logistic `out' i.`exp' [pw=trunc99_pub]
		mi estimate, or: logistic `out' i.`exp' ethnicity highed sclass findiff [pw=trunc99_pub]
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

foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep number_aces {
	di "95th percentile weights"
	mi estimate, or: logistic length_both i.`exp' [pw=trunc95_leng]
	mi estimate, or: logistic length_both i.`exp' ethnicity highed sclass findiff [pw=trunc95_leng]
	di "99th percentile weights"
	mi estimate, or: logistic length_both i.`exp' [pw=trunc99_leng]
	mi estimate, or: logistic length_both i.`exp' ethnicity highed sclass findiff [pw=trunc99_leng]
}
	
*Irregular
summ ipw_irreg, det
local irreg95=r(p95)			
local irreg99=r(p99)	
gen trunc95_irreg=ipw_irreg
replace trunc95_irreg=`irreg95' if trunc95_irreg>`irreg95' & trunc95_irreg<.  
gen trunc99_irreg=ipw_irreg
replace trunc99_irreg=`irreg99' if trunc99_irreg>`irreg99' & trunc99_irreg<.

foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep number_aces {
	di "95th percentile weights"
	mi estimate, or: logistic irreg i.`exp' [pw=trunc95_irreg]
	mi estimate, or: logistic irreg i.`exp' ethnicity highed sclass findiff [pw=trunc95_irreg]
	di "99th percentile weights"
	mi estimate, or: logistic irreg i.`exp' [pw=trunc99_irreg]
	mi estimate, or: logistic irreg i.`exp' ethnicity highed sclass findiff [pw=trunc99_irreg]
}

*PMS
summ ipw_pms, det
local pms95=r(p95)			
local pms99=r(p99)	
gen trunc95_pms=ipw_pms
replace trunc95_pms=`pms95' if trunc95_pms>`pms95' & trunc95_pms<.  
gen trunc99_pms=ipw_pms
replace trunc99_pms=`pms99' if trunc99_pms>`pms99' & trunc99_pms<.
	
foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep number_aces {
	di "95th percentile weights"
	mi estimate, or: logistic pms_bin i.`exp' [pw=trunc95_pms]
	mi estimate, or: logistic pms_bin i.`exp' ethnicity highed sclass findiff [pw=trunc95_pms]
	di "99th percentile weights"
	mi estimate, or: logistic pms_bin i.`exp' [pw=trunc99_pms]
	mi estimate, or: logistic pms_bin i.`exp' ethnicity highed sclass findiff [pw=trunc99_pms]
}
	  

log close


***SCORE OUTCOME WITH OVERALL P VALUE
gen number_aces = 0
replace number_aces=1 if score_classic==1
replace number_aces=2 if score_classic==2
replace number_aces=3 if score_classic==3
replace number_aces=4 if inrange(score_classic,4,10)
label define score_lbl 0"0" 1"1" 2"2" 3"3" 4"4 or more", replace
label values number_aces score_lbl

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/MI and IPW/ACE_score_overallP.log", replace

foreach out in pain_both heavy_both days_both {
	quietly mi estimate, or post: logistic `out' i.number_aces [pw=ipw_pub]
	di "CRUDE"
	testparm i.number_aces
	quietly mi estimate, or post: logistic `out' i.number_aces ethnicity highed sclass findiff [pw=ipw_pub]
	di "ADJUSTED"
	testparm i.number_aces
}

quietly mi estimate, or post: logistic length_both i.number_aces [pw=ipw_length]
di "CRUDE"
testparm i.number_aces
quietly mi estimate, or post: logistic length_both i.number_aces ethnicity highed sclass findiff [pw=ipw_length]
di "ADJUSTED"
testparm i.number_aces

quietly mi estimate, or post: logistic irreg i.number_aces [pw=ipw_irreg]
di "CRUDE"
testparm i.number_aces
quietly mi estimate, or post: logistic irreg i.number_aces ethnicity highed sclass findiff [pw=ipw_irreg]
di "ADJUSTED"
testparm i.number_aces

quietly mi estimate, or post: logistic pms_bin i.number_aces [pw=ipw_pms]
di "CRUDE"
testparm i.number_aces
quietly mi estimate, or post: logistic pms_bin i.number_aces ethnicity highed sclass findiff [pw=ipw_pms]
di "ADJUSTED"
testparm i.number_aces
		
log close








		