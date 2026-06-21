*G1 ACEs to Symptoms - alternative MI
*Following Alternative_Script3_Imp_PerOutcome.R

**#*PUB OUTCOMES****************************************************************
*Imputation conducted in R 

use "/Volumes/Studies/ALSPAC Menstrual PhD/alspacKids_ACE_0_10ALSPAC_pub.dta", clear

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

save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/G1_ACEs_PubOutcomes.dta", replace

*Register as imputed
mi import flong, m(_imp) id(_id) imputed (kz030_org dw002_org dw042_org a006_org a525_org c645a_org c666a_org pb325a_org pb342a_org c472_org a600_org b587 sc_18wgest pb098_org t3336 t5404 fa5404 t3316 ypa5050 h731_org h733_org h734_org highed_org sclass_org phone_org car_org housing_org crowding_org dbl_glaze_org first_preg_org smoke_preg_org smoke_ever_org epds_org parity_org mated_org bfed_dur_org phys_abus sexu_abus emot_abus emot_neg bully viol_parent subs_hshld prnt_mntlhlth prnt_convict prnt_sep social_class fin_diffs neighbourhood support_child support_prnt physill_child physill_prnt prntchild_bond score_ext score_classic)

*Not imputed 
mi register regular (_imp _id c804_org kz029_org mz028b_org kz021_org c522_org b370_org ypa5005_dup_org ypa5007_dup ypa5009_dup_org ypa5011_dup ypa5013_dup_org ypa5017_dup_org ypa5015_dup c521_org c523_org c525_org has_pub_org pain_both_org heavy_both_org days_both_org findiff_org ethnicity_org mat_age_org findiff_ipw_org fai_long_org)

*Check all variables registered
mi describe

*Save
save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/G1_ACEs_PubOutcomes.dta", replace

**#Imputation checks
label define imputed 0"Observed" 1"Imputed", replace
gen imputed=0 if _mi_m==0
replace imputed=1 if _mi_m>0
label values imputed imputed

*Passively derive score variable
label define ace_bin_lbl 0"No" 1"Yes", replace
foreach var in phys_abus sexu_abus emot_abus emot_neg bully viol_parent subs_hshld prnt_mntlhlth prnt_convict prnt_sep {
	recode `var' (1=0) (2=1)
	label values `var' ace_bin_lbl
}

replace score_classic=. if _mi_m>0
forvalues j = 1/50 {
	replace score_classic=phys_abus + sexu_abus + emot_abus + emot_neg + bully + viol_parent + subs_hshld + prnt_mntlhlth + prnt_convict + prnt_sep if _mi_m==`j'
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/Post_Imp_Checks_PubOut", replace
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

*IPW Vars that haven't been imputed 
foreach var in findiff_org ethnicity  {
	tab `var' imputed, row col
}
*Means
foreach var in mat_age findiff_ipw fai_long {
	summ `var' if imputed==0, det
	summ `var' if imputed==1, det
}

log close

*Distributions 
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI"

foreach var in epds_org score_classic {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck_pub.png, replace 
}

**FMI and Mcerror checks - loop through main adjusted models 
*Rename/restore to 0/1 coding
foreach var in pain_both heavy_both days_both {
	rename `var'_org `var'
}

*Main outcomes
label define pain_both_lbl 0"No pain" 1"Pain", replace
label define heavy_both_lbl 0"Not heavy" 1"Heavy", replace
label define days_both_lbl 0"6 days or less" 1"7 days or more", replace
foreach var in pain_both heavy_both days_both {
	recode `var' (1=0) (2=1)
	label values `var' `var'_lbl
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/Post_Imp_Checks_PubOut", append

*Loop through models               
foreach exp in phys_abus sexu_abus emot_abus emot_neg bully viol_parent subs_hshld prnt_mntlhlth prnt_convict prnt_sep {
	foreach out in pain heavy days_both  {
		mi estimate, mcerror : logistic `out' `exp' ethnicity_org highed_org sclass_org findiff_org
	}
}
log close

*Highest FMI = 0.3346
*Any mcerror concerns? No (one where t was 0.11 but only just over and don't think this justifies doing a different approach to the main analysis - days and bully)

**#Deriving weights
*Restore names and 0/1 coding in outcomes
foreach var in has_pub highed sclass findiff ethnicity mat_age phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long {
	rename `var'_org `var'
}
label define has_outcome 0"Data missing" 1"Data available", replace
recode has_pub (1=0) (2=1)
label values has_pub has_outcome

**IPW for each imputed dataset 
forvalues j = 1/50 {
	logistic has_pub mat_age phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if _mi_m==`j'
	predict pubp`j' if _mi_m==`j'
	}
	
**Consistent name across datasets
gen pubp = pubp1
forvalues j = 2/50 {
	replace pubp = pubp`j' if pubp==.
}

**Create probability and weights
gen prob_pub=pubp if has_pub==1
replace prob_pub=1-pubp if has_pub==0
gen ipw_pub=1/prob_pub


*Tidy up (remove original p per dataset variables as have overall one with all information)
forvalues j = 1/50 {
	drop pubp`j'
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/Post_Imp_Checks_PubOut", append
*Summary of weights
summ ipw_pub, det
log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI"
histogram ipw_pub
graph export pub_weights.png, replace 
	
 
*Replace dataset now weights derived
save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/G1_ACEs_PubOutcomes.dta", replace

**#Weighted analysis
*Open if needed
use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/G1_ACEs_PubOutcomes.dta", clear

gen number_aces = 0
replace number_aces=1 if score_classic==1
replace number_aces=2 if score_classic==2
replace number_aces=3 if score_classic==3
replace number_aces=4 if inrange(score_classic,4,10)
label define score_lbl 0"0" 1"1" 2"2" 3"3" 4"4 or more", replace
label values number_aces score_lbl

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/Pub_MainResults", replace

*Individual ACEs (binary) 
foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep {
	foreach out in pain_both heavy_both days_both {
		mi estimate, or post: logistic `out' `exp' [pw=ipw_pub] if has_pub==1
		mi estimate, or post: logistic `out' `exp' ethnicity highed sclass findiff [pw=ipw_pub] if has_pub==1
	}
}

*Score as categorical (four plus)
foreach out in pain_both heavy_both days_both {
		mi estimate, or post: logistic `out' i.number_aces [pw=ipw_pub] if has_pub==1
		testparm i.number_aces
		mi estimate, or post: logistic `out' i.number_aces ethnicity highed sclass findiff [pw=ipw_pub] if has_pub==1
		testparm i.number_aces
	}

*Score as continuous (four plus)
foreach out in pain_both heavy_both days_both {
		mi estimate, or post: logistic `out' number_aces [pw=ipw_pub] if has_pub==1
		mi estimate, or post: logistic `out' number_aces ethnicity highed sclass findiff [pw=ipw_pub] if has_pub==1
	}
	
log close

**#*LENGTH OUTCOME**************************************************************
*Imputation conducted in R 

use "/Volumes/Studies/ALSPAC Menstrual PhD/alspacKids_ACE_0_10ALSPAC_length.dta", clear

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

save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/G1_ACEs_LengthOutcome.dta", replace

*Register as imputed
mi import flong, m(_imp) id(_id) imputed (kz030_org dw002_org dw042_org a006_org a525_org c645a_org c666a_org pb325a_org pb342a_org c472_org a600_org b587 sc_18wgest pb098_org t3336 t5404 fa5404 t3316 ypa5050 h731_org h733_org h734_org highed_org sclass_org phone_org car_org housing_org crowding_org dbl_glaze_org first_preg_org smoke_preg_org smoke_ever_org epds_org parity_org mated_org bfed_dur_org phys_abus sexu_abus emot_abus emot_neg bully viol_parent subs_hshld prnt_mntlhlth prnt_convict prnt_sep social_class fin_diffs neighbourhood support_child support_prnt physill_child physill_prnt prntchild_bond score_ext score_classic)

*Not imputed 
mi register regular (_imp _id c804_org kz029_org mz028b_org kz021_org c522_org b370_org ypa5005_dup_org ypa5007_dup ypa5009_dup_org ypa5011_dup ypa5013_dup_org ypa5017_dup_org ypa5015_dup c521_org c523_org c525_org has_length_org length_both_org findiff_org ethnicity_org mat_age_org findiff_ipw_org fai_long_org)

*Check all variables registered
mi describe

*Save
save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/G1_ACEs_LengthOutcome.dta", replace

**#Imputation checks
label define imputed 0"Observed" 1"Imputed", replace
gen imputed=0 if _mi_m==0
replace imputed=1 if _mi_m>0
label values imputed imputed

*Passively derive score variable
label define ace_bin_lbl 0"No" 1"Yes", replace
foreach var in phys_abus sexu_abus emot_abus emot_neg bully viol_parent subs_hshld prnt_mntlhlth prnt_convict prnt_sep {
	recode `var' (1=0) (2=1)
	label values `var' ace_bin_lbl
}

replace score_classic=. if _mi_m>0
forvalues j = 1/50 {
	replace score_classic=phys_abus + sexu_abus + emot_abus + emot_neg + bully + viol_parent + subs_hshld + prnt_mntlhlth + prnt_convict + prnt_sep if _mi_m==`j'
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/Post_Imp_Checks_LengthOut", replace
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

*IPW Vars that haven't been imputed 
foreach var in findiff_org ethnicity  {
	tab `var' imputed, row col
}
*Means
foreach var in mat_age findiff_ipw fai_long {
	summ `var' if imputed==0, det
	summ `var' if imputed==1, det
}

log close

*Distributions 
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI"

foreach var in epds_org score_classic {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck_length.png, replace 
}

**FMI and Mcerror checks - loop through main adjusted models 
*Rename/restore to 0/1 coding
rename length_both_org length_both
label define length_both_lbl 0"Normal (24-38)" 1"Freq. or Infreq.", replace
recode length_both (1=0) (2=1)
label values length_both length_both_lbl

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/Post_Imp_Checks_LengthOut", append

*Loop through models               
foreach exp in phys_abus sexu_abus emot_abus emot_neg bully viol_parent subs_hshld prnt_mntlhlth prnt_convict prnt_sep {
		mi estimate, mcerror : logistic length_both `exp' ethnicity_org highed_org sclass_org findiff_org
}
log close

*Highest FMI = 0.3987
*Any mcerror concerns? No, all good

**#Deriving weights
*Restore names and 0/1 coding in outcomes
foreach var in has_length highed sclass findiff ethnicity mat_age phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long {
	rename `var'_org `var'
}
label define has_outcome 0"Data missing" 1"Data available", replace
recode has_length (1=0) (2=1)
label values has_length has_outcome

**IPW for each imputed dataset 
forvalues j = 1/50 {
	logistic has_length mat_age phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if _mi_m==`j'
	predict lengthp`j' if _mi_m==`j'
	}
	
**Consistent name across datasets
gen lengthp = lengthp1
forvalues j = 2/50 {
	replace lengthp = lengthp`j' if lengthp==.
}

**Create probability and weights
gen prob_length=lengthp if has_length==1
replace prob_length=1-lengthp if has_length==0
gen ipw_length=1/prob_length


*Tidy up (remove original p per dataset variables as have overall one with all information)
forvalues j = 1/50 {
	drop lengthp`j'
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/Post_Imp_Checks_LengthOut", append
*Summary of weights
summ ipw_length, det
log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI"
histogram ipw_length
graph export length_weights.png, replace 
	
 
*Replace dataset now weights derived
save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/G1_ACEs_LengthOutcome.dta", replace

**#Weighted analysis
*Open if needed
use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/G1_ACEs_LengthOutcome.dta", clear

gen number_aces = 0
replace number_aces=1 if score_classic==1
replace number_aces=2 if score_classic==2
replace number_aces=3 if score_classic==3
replace number_aces=4 if inrange(score_classic,4,10)
label define score_lbl 0"0" 1"1" 2"2" 3"3" 4"4 or more", replace
label values number_aces score_lbl

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/Length_MainResults", replace

*Individual ACEs (binary) 
foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep {
	mi estimate, or post: logistic length_both `exp' [pw=ipw_length] if has_length==1
	mi estimate, or post: logistic length_both `exp' ethnicity highed sclass findiff [pw=ipw_length] if has_length==1
}

*Score as categorical (four plus)
mi estimate, or post: logistic length_both i.number_aces [pw=ipw_length] if has_length==1
testparm i.number_aces
mi estimate, or post: logistic length_both i.number_aces ethnicity highed sclass findiff [pw=ipw_length] if has_length==1
testparm i.number_aces

*Score as continuous (four plus)
mi estimate, or post: logistic length_both number_aces [pw=ipw_length] if has_length==1
mi estimate, or post: logistic length_both number_aces ethnicity highed sclass findiff [pw=ipw_length] if has_length==1
	
log close

**#*IRREG OUTCOME***************************************************************
*Imputation conducted in R 

use "/Volumes/Studies/ALSPAC Menstrual PhD/alspacKids_ACE_0_10ALSPAC_irreg.dta", clear

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

save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/G1_ACEs_IrregOutcome.dta", replace

*Register as imputed
mi import flong, m(_imp) id(_id) imputed (kz030_org dw002_org dw042_org a006_org a525_org c645a_org c666a_org pb325a_org pb342a_org c472_org a600_org b587 sc_18wgest pb098_org t3336 t5404 fa5404 t3316 ypa5050 h731_org h733_org h734_org highed_org sclass_org phone_org car_org housing_org crowding_org dbl_glaze_org first_preg_org smoke_preg_org smoke_ever_org epds_org parity_org mated_org bfed_dur_org phys_abus sexu_abus emot_abus emot_neg bully viol_parent subs_hshld prnt_mntlhlth prnt_convict prnt_sep social_class fin_diffs neighbourhood support_child support_prnt physill_child physill_prnt prntchild_bond score_ext score_classic)

*Not imputed 
mi register regular (_imp _id c804_org kz029_org mz028b_org kz021_org c522_org b370_org ypa5005_dup_org ypa5007_dup ypa5009_dup_org ypa5011_dup ypa5013_dup_org ypa5017_dup_org ypa5015_dup c521_org c523_org c525_org has_irreg_org irreg_org findiff_org ethnicity_org mat_age_org findiff_ipw_org fai_long_org)

*Check all variables registered
mi describe

*Save
save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/G1_ACEs_IrregOutcome.dta", replace

**#Imputation checks
label define imputed 0"Observed" 1"Imputed", replace
gen imputed=0 if _mi_m==0
replace imputed=1 if _mi_m>0
label values imputed imputed

*Passively derive score variable
label define ace_bin_lbl 0"No" 1"Yes", replace
foreach var in phys_abus sexu_abus emot_abus emot_neg bully viol_parent subs_hshld prnt_mntlhlth prnt_convict prnt_sep {
	recode `var' (1=0) (2=1)
	label values `var' ace_bin_lbl
}

replace score_classic=. if _mi_m>0
forvalues j = 1/50 {
	replace score_classic=phys_abus + sexu_abus + emot_abus + emot_neg + bully + viol_parent + subs_hshld + prnt_mntlhlth + prnt_convict + prnt_sep if _mi_m==`j'
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/Post_Imp_Checks_IrregOut", replace
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

*IPW Vars that haven't been imputed 
foreach var in findiff_org ethnicity  {
	tab `var' imputed, row col
}
*Means
foreach var in mat_age findiff_ipw fai_long {
	summ `var' if imputed==0, det
	summ `var' if imputed==1, det
}

log close

*Distributions 
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI"

foreach var in epds_org score_classic {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck_irreg.png, replace 
}

**FMI and Mcerror checks - loop through main adjusted models 
*Rename/restore to 0/1 coding
rename irreg_org irreg
label define irreg_lbl 0"Regular" 1"Irregular", replace
recode irreg (1=0) (2=1)
label values irreg irreg_lbl

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/Post_Imp_Checks_IrregOut", append

*Loop through models               
foreach exp in phys_abus sexu_abus emot_abus emot_neg bully viol_parent subs_hshld prnt_mntlhlth prnt_convict prnt_sep {
		mi estimate, mcerror : logistic irreg `exp' ethnicity_org highed_org sclass_org findiff_org
}
log close

*Highest FMI = 0.3778
*Any mcerror concerns? No, all good.

**#Deriving weights
*Restore names and 0/1 coding in outcomes
foreach var in has_irreg highed sclass findiff ethnicity mat_age phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long {
	rename `var'_org `var'
}
label define has_outcome 0"Data missing" 1"Data available", replace
recode has_irreg (1=0) (2=1)
label values has_irreg has_outcome

**IPW for each imputed dataset 
forvalues j = 1/50 {
	logistic has_irreg mat_age phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if _mi_m==`j'
	predict irregp`j' if _mi_m==`j'
	}
	
**Consistent name across datasets
gen irregp = irregp1
forvalues j = 2/50 {
	replace irregp = irregp`j' if irregp==.
}

**Create probability and weights
gen prob_irreg=irregp if has_irreg==1
replace prob_irreg=1-irregp if has_irreg==0
gen ipw_irreg=1/prob_irreg


*Tidy up (remove original p per dataset variables as have overall one with all information)
forvalues j = 1/50 {
	drop irregp`j'
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/Post_Imp_Checks_IrregOut", append
*Summary of weights
summ ipw_irreg, det
log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI"
histogram ipw_irreg
graph export irreg_weights.png, replace 
	
 
*Replace dataset now weights derived
save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/G1_ACEs_IrregOutcome.dta", replace

**#Weighted analysis
*Open if needed
*use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/G1_ACEs_IrregOutcome.dta", clear

gen number_aces = 0
replace number_aces=1 if score_classic==1
replace number_aces=2 if score_classic==2
replace number_aces=3 if score_classic==3
replace number_aces=4 if inrange(score_classic,4,10)
label define score_lbl 0"0" 1"1" 2"2" 3"3" 4"4 or more", replace
label values number_aces score_lbl

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/Irreg_MainResults", replace

*Individual ACEs (binary) 
foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep {
	mi estimate, or post: logistic irreg `exp' [pw=ipw_irreg] if has_irreg==1
	mi estimate, or post: logistic irreg `exp' ethnicity highed sclass findiff [pw=ipw_irreg] if has_irreg==1
}

*Score as categorical (four plus)
mi estimate, or post: logistic irreg i.number_aces [pw=ipw_irreg] if has_irreg==1
testparm i.number_aces
mi estimate, or post: logistic irreg i.number_aces ethnicity highed sclass findiff [pw=ipw_irreg] if has_irreg==1
testparm i.number_aces

*Score as continuous (four plus)
mi estimate, or post: logistic irreg number_aces [pw=ipw_irreg] if has_irreg==1
mi estimate, or post: logistic irreg number_aces ethnicity highed sclass findiff [pw=ipw_irreg] if has_irreg==1
	
log close

**#*PMS OUTCOME*****************************************************************
*Imputation conducted in R 

use "/Volumes/Studies/ALSPAC Menstrual PhD/alspacKids_ACE_0_10ALSPAC_pms.dta", clear

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

save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/G1_ACEs_PMSOutcome.dta", replace

*Register as imputed
mi import flong, m(_imp) id(_id) imputed (kz030_org dw002_org dw042_org a006_org a525_org c645a_org c666a_org pb325a_org pb342a_org c472_org a600_org b587 sc_18wgest pb098_org t3336 t5404 fa5404 t3316 ypa5050 h731_org h733_org h734_org highed_org sclass_org phone_org car_org housing_org crowding_org dbl_glaze_org first_preg_org smoke_preg_org smoke_ever_org epds_org parity_org mated_org bfed_dur_org phys_abus sexu_abus emot_abus emot_neg bully viol_parent subs_hshld prnt_mntlhlth prnt_convict prnt_sep social_class fin_diffs neighbourhood support_child support_prnt physill_child physill_prnt prntchild_bond score_ext score_classic)

*Not imputed 
mi register regular (_imp _id c804_org kz029_org mz028b_org kz021_org c522_org b370_org ypa5005_dup_org ypa5007_dup ypa5009_dup_org ypa5011_dup ypa5013_dup_org ypa5017_dup_org ypa5015_dup c521_org c523_org c525_org has_pms_org pms_bin_org findiff_org ethnicity_org mat_age_org findiff_ipw_org fai_long_org)

*Check all variables registered
mi describe

*Save
save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/G1_ACEs_PMSOutcome.dta", replace

**#Imputation checks
label define imputed 0"Observed" 1"Imputed", replace
gen imputed=0 if _mi_m==0
replace imputed=1 if _mi_m>0
label values imputed imputed

*Passively derive score variable
label define ace_bin_lbl 0"No" 1"Yes", replace
foreach var in phys_abus sexu_abus emot_abus emot_neg bully viol_parent subs_hshld prnt_mntlhlth prnt_convict prnt_sep {
	recode `var' (1=0) (2=1)
	label values `var' ace_bin_lbl
}

replace score_classic=. if _mi_m>0
forvalues j = 1/50 {
	replace score_classic=phys_abus + sexu_abus + emot_abus + emot_neg + bully + viol_parent + subs_hshld + prnt_mntlhlth + prnt_convict + prnt_sep if _mi_m==`j'
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/Post_Imp_Checks_PMSOut", replace
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

*IPW Vars that haven't been imputed 
foreach var in findiff_org ethnicity  {
	tab `var' imputed, row col
}
*Means
foreach var in mat_age findiff_ipw fai_long {
	summ `var' if imputed==0, det
	summ `var' if imputed==1, det
}

log close

*Distributions 
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI"

foreach var in epds_org score_classic {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck_pms.png, replace 
}

**FMI and Mcerror checks - loop through main adjusted models 
*Rename/restore to 0/1 coding
rename pms_bin_org pms_bin
label define pms_bin_lbl 0"No symptoms" 1"Any symptoms", replace
recode pms_bin (1=0) (2=1)
label values pms_bin pms_bin_lbl

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/Post_Imp_Checks_PMSOut", append

*Loop through models               
foreach exp in phys_abus sexu_abus emot_abus emot_neg bully viol_parent subs_hshld prnt_mntlhlth prnt_convict prnt_sep {
		mi estimate, mcerror : logistic pms_bin `exp' ethnicity_org highed_org sclass_org findiff_org
}
log close

*Highest FMI = 0.2412
*Any mcerror concerns? No, all good. 

**#Deriving weights
*Restore names and 0/1 coding in outcomes
foreach var in has_pms highed sclass findiff ethnicity mat_age phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long {
	rename `var'_org `var'
}
label define has_outcome 0"Data missing" 1"Data available", replace
recode has_pms (1=0) (2=1)
label values has_pms has_outcome

**IPW for each imputed dataset 
forvalues j = 1/50 {
	logistic has_pms mat_age phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if _mi_m==`j'
	predict pmsp`j' if _mi_m==`j'
	}
	
**Consistent name across datasets
gen pmsp = pmsp1
forvalues j = 2/50 {
	replace pmsp = pmsp`j' if pmsp==.
}

**Create probability and weights
gen prob_pms=pmsp if has_pms==1
replace prob_pms=1-pmsp if has_pms==0
gen ipw_pms=1/prob_pms


*Tidy up (remove original p per dataset variables as have overall one with all information)
forvalues j = 1/50 {
	drop pmsp`j'
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/Post_Imp_Checks_PMSOut", append
*Summary of weights
summ ipw_pms, det
log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI"
histogram ipw_pms
graph export pms_weights.png, replace 
	
 
*Replace dataset now weights derived
save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/G1_ACEs_PMSOutcome.dta", replace

**#Weighted analysis
*Open if needed
use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/G1_ACEs_PMSOutcome.dta", clear

gen number_aces = 0
replace number_aces=1 if score_classic==1
replace number_aces=2 if score_classic==2
replace number_aces=3 if score_classic==3
replace number_aces=4 if inrange(score_classic,4,10)
label define score_lbl 0"0" 1"1" 2"2" 3"3" 4"4 or more", replace
label values number_aces score_lbl

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G1/Results/Alternative MI/PMS_MainResults", replace

*Individual ACEs (binary) 
foreach exp in phys_abus emot_abus emot_neg sexu_abus bully viol_parent prnt_mntlhlth subs_hshld prnt_convict prnt_sep {
	mi estimate, or post: logistic pms_bin `exp' [pw=ipw_pms] if has_pms==1
	mi estimate, or post: logistic pms_bin `exp' ethnicity highed sclass findiff [pw=ipw_pms] if has_pms==1
}

*Score as categorical (four plus)
mi estimate, or post: logistic pms_bin i.number_aces [pw=ipw_pms] if has_pms==1
testparm i.number_aces
mi estimate, or post: logistic pms_bin i.number_aces ethnicity highed sclass findiff [pw=ipw_pms] if has_pms==1
testparm i.number_aces

*Score as continuous (four plus)
mi estimate, or post: logistic pms_bin number_aces [pw=ipw_pms] if has_pms==1
mi estimate, or post: logistic pms_bin number_aces ethnicity highed sclass findiff [pw=ipw_pms] if has_pms==1
	
log close


