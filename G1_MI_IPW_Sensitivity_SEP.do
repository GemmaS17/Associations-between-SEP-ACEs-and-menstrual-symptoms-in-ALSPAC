**G1 SEP to Symptoms - MI/IPW sensitivity
*Run main script for general data prep

********************************************************************************
***OUTCOME MISSING AND IMPUTE TO VARIABLES**************************************
********************************************************************************

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
drop if impute==0

********************************************************************************
********************************************************************************
********************************************************************************
**#PUB OUTCOMES SENSITIVITY SPECIFIC ANALYSIS***********************************
********************************************************************************
********************************************************************************
********************************************************************************
count if has_pub==1		//2757

*Focus dataset to relevant variables 
keep highed sclass findiff ///
	 pain_both heavy_both days_bin_both ///
	 ethnicity ///
	 has_pub  ///
	 mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long ///
	 aln 

rename pain_both pain
rename heavy_both heavy 
rename days_bin_both days_bin

/*Make sure all outcome variables are missing where data won't be used in analysis
foreach var in pain heavy days_bin {
	replace `var'=. if has_pub==0
}*/

*Drop obs with missing outcome data and missing any IPW variable 
egen ipw_vars=rmiss2(mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass bfed_dur fai_long)
count if has_pub==0 & ipw_vars>0 //expecting 915 to be dropped (leaving 4979 obs)
drop if has_pub==0 & ipw_vars>0

**Missing data patterns
*Complete 
mdesc has_pub mat_age
*Substantive variables (missing)
mdesc highed sclass findiff pain heavy days_bin ethnicity  
*IPW variables (missing)
mdesc marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long

**Set up imputation*************************************************************
mi set flong
mi register imputed highed sclass findiff ethnicity marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long
mi register regular has_pub mat_age pain heavy days_bin

**Dryrun
mi impute chained (ologit, omit(i.mated)) highed ///
				  (ologit) housing crowding first_preg parity bfed_dur ///
				  (ologit, omit(i.highed)) mated ///
				  (logit) sclass ethnicity marital_stat phone car dbl_glaze smoke_preg smoke_ever ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (pmm, knn(5)) epds fai_long ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_pub mat_age pain heavy days_bin, dryrun

**Trace plot 
mi impute chained (ologit, omit(i.mated)) highed ///
				  (ologit) housing crowding first_preg parity bfed_dur ///
				  (ologit, omit(i.highed)) mated ///
				  (logit) sclass ethnicity marital_stat phone car dbl_glaze smoke_preg smoke_ever ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (pmm, knn(5)) epds fai_long ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_pub mat_age pain heavy days_bin, force burnin(100) rseed(69703326) dots chainonly noisily showcommand savetrace("/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Pub/MI_Trace.dta", replace)

*Trace checks
use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Pub/MI_Trace.dta", clear
			 
describe 
tsset iter

cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Pub/Trace"

foreach cov in highed sclass findiff ethnicity marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long {
	tsline `cov'_mean, title(Mean imputed values of `cov') legend(off) 
	graph export `cov'_mean.png, replace
	tsline `cov'_sd, title(Standard deviation imputed values of `cov') legend(off)
	graph export `cov'_sd.png, replace
}			

*All good by 50 iterations. NB only one observation missing FAI so there is no SD observations to plot. 	  

**Run imputation model
mi impute chained (ologit, omit(i.mated)) highed ///
				  (ologit) housing crowding first_preg parity bfed_dur ///
				  (ologit, omit(i.highed)) mated ///
				  (logit) sclass ethnicity marital_stat phone car dbl_glaze smoke_preg smoke_ever ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (pmm, knn(5)) epds fai_long ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_pub mat_age pain heavy days_bin, force add(50) burnin(50) rseed(69703326) dots

*Save
save "/Volumes/157/working/data/G1_SEP_PubSens.dta", replace
*Open
use "/Volumes/157/working/data/G1_SEP_PubSens.dta", clear	

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Pub/Post_Imp_Checks", replace

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
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Pub"

foreach var in epds findiff_ipw fai_long {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck.png, replace 
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Pub/Post_Imp_Checks", append
**FMI and Mcerror checks before removing outcomes - loop through main adjusted models 
*1-mcerror of coefficient should be 10% or less of effect SE
*2-mcerror of tstat should be 0.1 or less
*3-mcerror of p should be 0.01 or less if p is 0.05 or 0.02 if p is 0.1
foreach exp in highed sclass findiff {
	foreach out in pain heavy days_bin {
		mi estimate, mcerror: logistic `out' `exp' ethnicity
	}
} 

log close

*Highest FMI = 0.0703
*Any mcerror concerns? No all looks good!

**#PUB Deriving Weights

**Get IPW for each imputed dataset and give same throughout datasets
forvalues j = 1/50 {
	logistic has_pub mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if _mi_m==`j'
	predict pubp`j' if _mi_m==`j'
	}
*Consistent name across datasets
gen pubp = pubp1
forvalues j = 2/50 {
	replace pubp = pubp`j' if pubp==.
}
*Create probability and weights
gen prob_pub=pubp if has_pub==1
replace prob_pub=1-pubp if has_pub==0
gen ipw_pub=1/prob_pub

*Tidy up
forvalues j = 1/50 {
	drop pubp`j'
}

*Summary of weights
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Pub/Weights_and_Descriptives", replace 

summ ipw_pub, det

log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Pub"

histogram ipw_pub
graph export pub_weights.png, replace

*Re-save with weights
save "/Volumes/157/working/data/G1_SEP_PubSens.dta", replace
	
**#PUB Analysis
*Descriptives
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Pub/Weights_and_Descriptives", append 
	
**Simple descriptives 
*Exposures and ethnicity
foreach var in highed sclass findiff ethnicity {
		mi estimate: proportion `var' if has_pub==1
	}

**Cross tabs 
foreach cov in highed sclass findiff ethnicity {
	foreach out in pain heavy days_bin {
		mi estimate: proportion `cov' if has_pub==1, over(`out') 
	}
}

log close	
	
*Main models 	
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Pub/Analysis", replace 	
	
foreach out in pain heavy days_bin {
	foreach exp in highed sclass findiff {
		eststo `exp'_`out'_crude: mi estimate, or: logistic `out' i.`exp' [pw=ipw_pub] if has_pub==1
		eststo `exp'_`out'_adj: mi estimate, or: logistic `out' i.`exp' ethnicity [pw=ipw_pub] if has_pub==1
	}
}	
	
log close

*Output to excel
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Pub"
capture erase "pub_results.xls"
	
*Highest education
esttab highed_painc highed_paina ///
		highed_heavyc highed_heavya ///
		highed_days_binc highed_days_bina ///
		using "pub_results.xls", append cells("b(fmt(2))ci_l(fmt(2))ci_u(fmt(2))p(fmt(3))") eform title(HIGHEST EDUCATION) 
*Social class  
estout sclass_painc sclass_paina ///  
		sclass_heavyc sclass_heavya ///  
		sclass_days_binc sclass_days_bina ///  
		using "pub_results.xls", append cells("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity) eform title(SOCIAL CLASS) 
*Financial difficulties	  
estout findiff_painc findiff_paina ///  
		findiff_heavyc findiff_heavya ///  
		findiff_days_binc findiff_days_bina ///  
		using "pub_results.xls", append cells("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity) eform title(FINANCIAL DIFFICULTIES) 	
	
	
********************************************************************************
********************************************************************************
********************************************************************************
**#LENGTH SENSITIVITY SPECIFIC ANALYSIS*****************************************
********************************************************************************
********************************************************************************
********************************************************************************
count if has_length==1		//1345

*Focus dataset to relevant variables 
keep highed sclass findiff ///
	 length_bin_both ///
	 ethnicity ///
	 has_length  ///
	 mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long ///
	 aln 

rename length_bin_both length

*Drop obs with missing outcome data and missing any IPW variable 
egen ipw_vars=rmiss2(mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass bfed_dur fai_long)
count if has_length==0 & ipw_vars>0 //expecting 1183 to be dropped (leaving 4711 obs)
drop if has_length==0 & ipw_vars>0

**Missing data patterns
*Complete 
mdesc has_length mat_age
*Substantive variables (missing)
mdesc highed sclass findiff length ethnicity  
*IPW variables (missing)
mdesc marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long

**Set up imputation*************************************************************
mi set flong
mi register imputed highed sclass findiff ethnicity marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long
mi register regular has_length mat_age length

**Dryrun
mi impute chained (ologit, omit(i.mated)) highed ///
				  (ologit) housing crowding first_preg parity bfed_dur ///
				  (ologit, omit(i.highed)) mated ///
				  (logit) sclass ethnicity marital_stat phone car dbl_glaze ///
				  (logit, omit(i.smoke_preg)) smoke_ever ///
				  (logit, omit(i.smoke_ever)) smoke_preg ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (pmm, knn(5)) epds fai_long ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_length mat_age length, dryrun

**Trace plot 
mi impute chained (ologit, omit(i.mated)) highed ///
				  (ologit) housing crowding first_preg parity bfed_dur ///
				  (ologit, omit(i.highed)) mated ///
				  (logit) sclass ethnicity marital_stat phone car dbl_glaze ///
				  (logit, omit(i.smoke_preg)) smoke_ever ///
				  (logit, omit(i.smoke_ever)) smoke_preg ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (pmm, knn(5)) epds fai_long ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_length mat_age length, force burnin(100) rseed(260637455) dots chainonly noisily showcommand savetrace("/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Length/MI_Trace.dta", replace)

*Trace checks
use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Length/MI_Trace.dta", clear
			 
describe 
tsset iter

cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Length/Trace"

foreach cov in highed sclass findiff ethnicity marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long {
	tsline `cov'_mean, title(Mean imputed values of `cov') legend(off) 
	graph export `cov'_mean.png, replace
	tsline `cov'_sd, title(Standard deviation imputed values of `cov') legend(off)
	graph export `cov'_sd.png, replace
}				  

*All looks good - stable way before 50 (but keep 50 for consistency with original imputation and other outcomes)

**Run imputation model
mi impute chained (ologit, omit(i.mated)) highed ///
				  (ologit) housing crowding first_preg parity bfed_dur ///
				  (ologit, omit(i.highed)) mated ///
				  (logit) sclass ethnicity marital_stat phone car dbl_glaze ///
				  (logit, omit(i.smoke_preg)) smoke_ever ///
				  (logit, omit(i.smoke_ever)) smoke_preg ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (pmm, knn(5)) epds fai_long ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_length mat_age length, force add(50) burnin(50) rseed(260637455) dots

*Save
save "/Volumes/157/working/data/G1_SEP_LengthSens.dta", replace
*Open
use "/Volumes/157/working/data/G1_SEP_LengthSens.dta", clear	

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Length/Post_Imp_Checks", replace

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
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Length"

foreach var in epds findiff_ipw fai_long {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck.png, replace 
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Length/Post_Imp_Checks", append
**FMI and Mcerror checks before removing outcomes - loop through main adjusted models 
*1-mcerror of coefficient should be 10% or less of effect SE
*2-mcerror of tstat should be 0.1 or less
*3-mcerror of p should be 0.01 or less if p is 0.05 or 0.02 if p is 0.1
foreach exp in highed sclass findiff {
		mi estimate, mcerror: logistic length `exp' ethnicity
	}

log close	

*Highest FMI = 0.0500
*Any mcerror concerns? No all looks good. 

**#LENGTH Deriving Weights

**Get IPW for each imputed dataset and give same throughout datasets
forvalues j = 1/50 {
	logistic has_length mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if _mi_m==`j'
	predict lengthp`j' if _mi_m==`j'
	}
*Consistent name across datasets
gen lengthp = lengthp1
forvalues j = 2/50 {
	replace lengthp = lengthp`j' if lengthp==.
}
*Create probability and weights
gen prob_length=lengthp if has_length==1
replace prob_length=1-lengthp if has_length==0
gen ipw_length=1/prob_length

*Tidy up
forvalues j = 1/50 {
	drop lengthp`j'
}

*Summary of weights
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Length/Weights_and_Descriptives", replace 

summ ipw_length, det

log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Length"

histogram ipw_length
graph export length_weights.png, replace

*Re-save with weights
save "/Volumes/157/working/data/G1_SEP_LengthSens.dta", replace
	
**#LENGTH Analysis
*Descriptives
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Length/Weights_and_Descriptives", append 
	
**Simple descriptives 
*Exposures and ethnicity
foreach var in highed sclass findiff ethnicity {
		mi estimate: proportion `var' if has_length==1
	}

**Cross tabs 
foreach cov in highed sclass findiff ethnicity {
		mi estimate: proportion `cov' if has_length==1, over(length)	
	}

log close	
	
*Main models 	
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Length/Analysis", replace 	

foreach exp in highed sclass findiff {
	eststo `exp'_lengthc: mi estimate, or: logistic length i.`exp' [pw=ipw_length] if has_length==1
	eststo `exp'_lengtha: mi estimate, or: logistic length i.`exp' ethnicity [pw=ipw_length] if has_length==1
}

	
log close

*Output to excel
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Length"
capture erase "length_results.xls"
	
*Highest education
estout highed_lengthc highed_lengtha ///
		using "length_results.xls", append cells("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity) eform title(HIGHEST EDUCATION) 
*Social class  
estout sclass_lengthc sclass_lengtha ///  
		using "length_results.xls", append cells("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity) eform title(SOCIAL CLASS) 
*Financial difficulties	  
estout findiff_lengthc findiff_lengtha ///  
		using "length_results.xls", append cells("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity) eform title(FINANCIAL DIFFICULTIES) 	
		

********************************************************************************
********************************************************************************
********************************************************************************
**#IRREGULAR SENSITIVITY SPECIFIC ANALYSIS**************************************
********************************************************************************
********************************************************************************
********************************************************************************
count if has_irreg==1		//2247

*Focus dataset to relevant variables 
keep highed sclass findiff ///
	 irreg ///
	 ethnicity ///
	 has_irreg  ///
	 mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long ///
	 aln 

*Drop obs with missing outcome data and missing any IPW variable 
egen ipw_vars=rmiss2(mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass bfed_dur fai_long)
count if has_irreg==0 & ipw_vars>0 //expecting 1053 to be dropped (leaving 4841 obs)
drop if has_irreg==0 & ipw_vars>0

**Missing data patterns
*Complete 
mdesc has_irreg mat_age
*Substantive variables (missing)
mdesc highed sclass findiff irreg ethnicity  
*IPW variables (missing)
mdesc marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long

**Set up imputation*************************************************************
mi set flong
mi register imputed highed sclass findiff ethnicity marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long
mi register regular has_irreg mat_age irreg

**Dryrun
mi impute chained (ologit, omit(i.mated)) highed ///
				  (ologit) housing crowding first_preg parity bfed_dur ///
				  (ologit, omit(i.highed)) mated ///
				  (logit) sclass ethnicity marital_stat phone car dbl_glaze smoke_preg smoke_ever ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (pmm, knn(5)) epds fai_long ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_irreg mat_age irreg, dryrun

**Trace plot 
mi impute chained (ologit, omit(i.mated)) highed ///
				  (ologit) housing crowding first_preg parity bfed_dur ///
				  (ologit, omit(i.highed)) mated ///
				  (logit) sclass ethnicity marital_stat phone car dbl_glaze smoke_preg smoke_ever ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (pmm, knn(5)) epds fai_long ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_irreg mat_age irreg, force burnin(100) rseed(28653752) dots chainonly noisily showcommand savetrace("/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Irreg/MI_Trace.dta", replace)

*Trace checks
use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Irreg/MI_Trace.dta", clear
			 
describe 
tsset iter

cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Irreg/Trace"

foreach cov in highed sclass findiff ethnicity marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long {
	tsline `cov'_mean, title(Mean imputed values of `cov') legend(off) 
	graph export `cov'_mean.png, replace
	tsline `cov'_sd, title(Standard deviation imputed values of `cov') legend(off)
	graph export `cov'_sd.png, replace
}				  

*Trace plots all look stable by 50 iterations. 

**Run imputation model
mi impute chained (ologit, omit(i.mated)) highed ///
				  (ologit) housing crowding first_preg parity bfed_dur ///
				  (ologit, omit(i.highed)) mated ///
				  (logit) sclass ethnicity marital_stat phone car dbl_glaze smoke_preg smoke_ever ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (pmm, knn(5)) epds fai_long ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_irreg mat_age irreg, force add(50) burnin(50) rseed(28653752) dots

*Save
save "/Volumes/157/working/data/G1_SEP_IrregSens.dta", replace
*Open
use "/Volumes/157/working/data/G1_SEP_IrregSens.dta", clear	

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Irreg/Post_Imp_Checks", replace

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
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Irreg"

foreach var in epds findiff_ipw fai_long {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck.png, replace 
}


log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Irreg/Post_Imp_Checks", append
**FMI and Mcerror checks before removing outcomes - loop through main adjusted models 
*1-mcerror of coefficient should be 10% or less of effect SE
*2-mcerror of tstat should be 0.1 or less
*3-mcerror of p should be 0.01 or less if p is 0.05 or 0.02 if p is 0.1
foreach exp in highed sclass findiff {
		mi estimate, mcerror: logistic irreg `exp' ethnicity
	}

log close

*Highest FMI = 0.0431
*Any mcerror concerns? All good. 

**#IRREGULAR Deriving Weights

**Get IPW for each imputed dataset and give same throughout datasets
forvalues j = 1/50 {
	logistic has_irreg mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if _mi_m==`j'
	predict irregp`j' if _mi_m==`j'
	}
*Consistent name across datasets
gen irregp = irregp1
forvalues j = 2/50 {
	replace irregp = irregp`j' if irregp==.
}
*Create probability and weights
gen prob_irreg=irregp if has_irreg==1
replace prob_irreg=1-irregp if has_irreg==0
gen ipw_irreg=1/prob_irreg

*Tidy up
forvalues j = 1/50 {
	drop irregp`j'
}

*Summary of weights
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Irreg/Weights_and_Descriptives", replace 

summ ipw_irreg, det

log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Irreg"

histogram ipw_irreg
graph export irreg_weights.png, replace

*Re-save with weights
save "/Volumes/157/working/data/G1_SEP_IrregSens.dta", replace
	
**#IRREGULAR Analysis
*Descriptives
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Irreg/Weights_and_Descriptives", append 
	
**Simple descriptives 
*Exposures and ethnicity
foreach var in highed sclass findiff ethnicity {
		mi estimate: proportion `var' if has_irreg==1
	}

**Cross tabs 
foreach cov in highed sclass findiff ethnicity {
		mi estimate: proportion `cov' if has_irreg==1, over(irreg)	
	}

log close	
	
*Main models 	
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Irreg/Analysis", replace 	

foreach exp in highed sclass findiff {
	eststo `exp'_irregc: mi estimate, or: logistic irreg i.`exp' [pw=ipw_irreg] if has_irreg==1
	eststo `exp'_irrega: mi estimate, or: logistic irreg i.`exp' ethnicity [pw=ipw_irreg] if has_irreg==1
}

	
log close

*Output to excel
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/Irreg"
capture erase "irreg_results.xls"
	
*Highest education
estout highed_irregc highed_irrega ///
		using "irreg_results.xls", append cells("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity) eform title(HIGHEST EDUCATION) 
*Social class  
estout sclass_irregc sclass_irrega ///  
		using "irreg_results.xls", append cells("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity) eform title(SOCIAL CLASS) 
*Financial difficulties	  
estout findiff_irregc findiff_irrega ///  
		using "irreg_results.xls", append cells("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity) eform title(FINANCIAL DIFFICULTIES) 	


********************************************************************************
********************************************************************************
********************************************************************************
**#PMS SENSITIVITY SPECIFIC ANALYSIS**************************************
********************************************************************************
********************************************************************************
********************************************************************************
count if has_pms==1		//1421

*Focus dataset to relevant variables 
keep highed sclass findiff ///
	 pms ///
	 ethnicity ///
	 has_pms  ///
	 mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur fai_long ///
	 aln 

*Drop obs with missing outcome data and missing any IPW variable 
egen ipw_vars=rmiss2(mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw sclass bfed_dur fai_long)
count if has_pms==0 & ipw_vars>0 //expecting 1210 to be dropped (leaving 4684 obs)
drop if has_pms==0 & ipw_vars>0

**Missing data patterns
*Complete 
mdesc has_pms mat_age fai_long
*Substantive variables (missing)
mdesc highed sclass findiff pms ethnicity  
*IPW variables (missing)
mdesc marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur 

**Set up imputation*************************************************************
mi set flong
mi register imputed highed sclass findiff ethnicity marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur 
mi register regular has_pms mat_age pms fai_long

**Dryrun
mi impute chained (ologit, omit(i.mated)) highed ///
				  (ologit) housing crowding first_preg parity bfed_dur ///
				  (ologit, omit(i.highed)) mated ///
				  (logit) sclass ethnicity marital_stat phone car dbl_glaze ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (logit, omit(i.smoke_preg)) smoke_ever ///
				  (logit, omit(i.smoke_ever)) smoke_preg ///
				  (pmm, knn(5)) epds ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_pms mat_age pms fai_long, dryrun

**Trace plot 
mi impute chained (ologit, omit(i.mated)) highed ///
				  (ologit) housing crowding first_preg parity bfed_dur ///
				  (ologit, omit(i.highed)) mated ///
				  (logit) sclass ethnicity marital_stat phone car dbl_glaze ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (logit, omit(i.smoke_preg)) smoke_ever ///
				  (logit, omit(i.smoke_ever)) smoke_preg ///
				  (pmm, knn(5)) epds ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_pms mat_age pms fai_long, force burnin(100) rseed(1974390) dots chainonly noisily showcommand savetrace("/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/PMS/MI_Trace.dta", replace)

*Trace checks
use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/PMS/MI_Trace.dta", clear
			 
describe 
tsset iter

cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/PMS/Trace"

foreach cov in highed sclass findiff ethnicity marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg smoke_ever epds parity mated findiff_ipw bfed_dur {
	tsline `cov'_mean, title(Mean imputed values of `cov') legend(off) 
	graph export `cov'_mean.png, replace
	tsline `cov'_sd, title(Standard deviation imputed values of `cov') legend(off)
	graph export `cov'_sd.png, replace
}				  

*Plots all look stable by 50 iterations. 

**Run imputation model
mi impute chained (ologit, omit(i.mated)) highed ///
				  (ologit) housing crowding first_preg parity bfed_dur ///
				  (ologit, omit(i.highed)) mated ///
				  (logit) sclass ethnicity marital_stat phone car dbl_glaze ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (logit, omit(i.smoke_preg)) smoke_ever ///
				  (logit, omit(i.smoke_ever)) smoke_preg ///
				  (pmm, knn(5)) epds ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_pms mat_age pms fai_long, force add(50) burnin(50) rseed(1974390) dots

*Save
save "/Volumes/157/working/data/G1_SEP_PMSSens.dta", replace
*Open
use "/Volumes/157/working/data/G1_SEP_PMSSens.dta", clear	

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/PMS/Post_Imp_Checks", replace

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
foreach var in epds findiff_ipw {
	summ `var' if imputed==0, det
	summ `var' if imputed==1, det
}

log close

*Distributions 
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/PMS"

foreach var in epds findiff_ipw {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck.png, replace 
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/PMS/Post_Imp_Checks", append
**FMI and Mcerror checks before removing outcomes - loop through main adjusted models 
*1-mcerror of coefficient should be 10% or less of effect SE
*2-mcerror of tstat should be 0.1 or less
*3-mcerror of p should be 0.01 or less if p is 0.05 or 0.02 if p is 0.1
foreach exp in highed sclass findiff {
		mi estimate, mcerror: logistic pms `exp' ethnicity
	}
	
log close

*Highest FMI = 0.0229
*Any mcerror concerns? No, all good. 

**#PMS Deriving Weights

**Get IPW for each imputed dataset and give same throughout datasets
forvalues j = 1/50 {
	logistic has_pms mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg smoke_ever epds i.parity i.mated findiff_ipw sclass i.bfed_dur fai_long if _mi_m==`j'
	predict pmsp`j' if _mi_m==`j'
	}
*Consistent name across datasets
gen pmsp = pmsp1
forvalues j = 2/50 {
	replace pmsp = pmsp`j' if pmsp==.
}
*Create probability and weights
gen prob_pms=pmsp if has_pms==1
replace prob_pms=1-pmsp if has_pms==0
gen ipw_pms=1/prob_pms

*Tidy up
forvalues j = 1/50 {
	drop pmsp`j'
}

*Summary of weights
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/PMS/Weights_and_Descriptives", replace 

summ ipw_pms, det

log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/PMS"

histogram ipw_pms
graph export pms_weights.png, replace

*Re-save with weights
save "/Volumes/157/working/data/G1_SEP_PMSSens.dta", replace
	
**#PMS Analysis
rename pms pms_org
gen pms=0 if pms_org==0
replace pms=1 if inrange(pms_org,1,5)

*Descriptives
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/PMS/Weights_and_Descriptives", append 
	
**Simple descriptives 
*Exposures and ethnicity
foreach var in highed sclass findiff ethnicity {
		mi estimate: proportion `var' if has_pms==1
	}

**Cross tabs 
foreach cov in highed sclass findiff ethnicity {
		mi estimate: proportion `cov' if has_pms==1, over(pms)	
	}

log close	
	
*Main models 	
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/PMS/Analysis", replace 	

foreach exp in highed sclass findiff {
	eststo `exp'_pmsc: mi estimate, or: logistic pms i.`exp' [pw=ipw_pms] if has_pms==1
	eststo `exp'_pmsa: mi estimate, or: logistic pms i.`exp' ethnicity [pw=ipw_pms] if has_pms==1
}

	
log close

*Output to excel
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/MI_IPW_Sensitivity/PMS"
capture erase "pms_results.xls"
	
*Highest education
estout highed_pmsc highed_pmsa ///
		using "pms_results.xls", append cells("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity) eform title(HIGHEST EDUCATION) 
*Social class  
estout sclass_pmsc sclass_pmsa ///  
		using "pms_results.xls", append cells("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity) eform title(SOCIAL CLASS) 
*Financial difficulties	  
estout findiff_pmsc findiff_pmsa ///  
		using "pms_results.xls", append cells("b(fmt(2)) ci_l(fmt(2)) ci_u(fmt(2)) p(fmt(3))") keep(*:) drop(_cons ethnicity) eform title(FINANCIAL DIFFICULTIES) 	
		
		
**#CONTINUOUS EDUCATION EXPOSURE ALTERNATIVE 

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Education_ContorCat_Analysis_MainMI", append
**ALTERNATIVE MIs

*Pub outcome
use "/Volumes/157/working/data/G1_SEP_PubSens.dta", clear

foreach out in pain heavy days_bin {
	mi estimate, or: logistic `out' highed [pw=ipw_pub] if has_pub==1
	mi estimate, or: logistic `out' highed ethnicity [pw=ipw_pub] if has_pub==1
}

clear 

*Length
use "/Volumes/157/working/data/G1_SEP_LengthSens.dta", clear

mi estimate, or: logistic length highed [pw=ipw_length] if has_length==1
mi estimate, or: logistic length highed ethnicity [pw=ipw_length] if has_length==1

clear

*Irreg
use "/Volumes/157/working/data/G1_SEP_IrregSens.dta", clear

mi estimate, or: logistic irreg highed [pw=ipw_irreg] if has_irreg==1
mi estimate, or: logistic irreg highed ethnicity [pw=ipw_irreg] if has_irreg==1

clear

*PMS
use "/Volumes/157/working/data/G1_SEP_PMSSens.dta", clear
rename pms pms_org
gen pms=0 if pms_org==0
replace pms=1 if inrange(pms_org,1,5)

mi estimate, or: logistic pms highed [pw=ipw_pms] if has_pms==1
mi estimate, or: logistic pms highed ethnicity [pw=ipw_pms] if has_pms==1

log close


**#Categorical with one overall p value

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Education_Categorical_OverallP_MI", append
**ALTERNATIVE MIs

*Pub outcome
use "/Volumes/157/working/data/G1_SEP_PubSens.dta", clear

foreach out in pain heavy days_bin {
	quietly mi estimate, or post: logistic `out' i.highed [pw=ipw_pub] if has_pub==1
	di "CRUDE"
	testparm i.highed
	quietly mi estimate, or post: logistic `out' i.highed ethnicity [pw=ipw_pub] if has_pub==1
	di "ADJUSTED"
	testparm i.highed
}

clear 

*Irreg
use "/Volumes/157/working/data/G1_SEP_IrregSens.dta", clear

quietly mi estimate, or post: logistic irreg i.highed [pw=ipw_irreg] if has_irreg==1
di "CRUDE"
testparm i.highed
quietly mi estimate, or post: logistic irreg i.highed ethnicity [pw=ipw_irreg] if has_irreg==1
di "ADJUSTED"
testparm i.highed

clear

*PMS
use "/Volumes/157/working/data/G1_SEP_PMSSens.dta", clear
rename pms pms_org
gen pms=0 if pms_org==0
replace pms=1 if inrange(pms_org,1,5)

quietly mi estimate, or post: logistic pms i.highed [pw=ipw_pms] if has_pms==1
di "CRUDE"
testparm i.highed
quietly mi estimate, or post: logistic pms i.highed ethnicity [pw=ipw_pms] if has_pms==1
di "ADJUSTED"
testparm i.highed

log close

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G1/Results/Education_Categorical_OverallP_MI", append
*Length
*use "/Volumes/157/working/data/G1_SEP_LengthSens.dta", clear

quietly mi estimate, or post: logistic length i.highed [pw=ipw_length] if has_length==1
di "CRUDE"
testparm i.highed
quietly mi estimate, or post: logistic length i.highed ethnicity [pw=ipw_length] if has_length==1
di "ADJUSTED"
testparm i.highed

log close

