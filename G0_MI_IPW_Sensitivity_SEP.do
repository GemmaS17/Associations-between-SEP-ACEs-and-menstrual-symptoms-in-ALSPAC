**Sensitivity MI/IPW Analysis - G0 SEP to Symptoms

********************************************************************************
********************************************************************************
**#RUN DATA PREP FROM MAIN ANALYSIS DO FILE*************************************
********************************************************************************
********************************************************************************

**TO IMPUTE ORIGINAL / APPLY TO ALL OUTCOMES

*To impute - at least one SEP exposure and half of IPW variables
egen sep=rmiss2(parentedu parentsclass edu sclass findiff)
tab sep
drop if sep==5 
*with age data
drop if mat_age==.
*Remove those with less han half IPW variables (all except rooms and smoke_ever = 17 variables - need 9 available so max 8 mising)
egen ipw_vars=rmiss2(mat_age marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity mat_ethnicity edu_ipw findiff_ipw highsclass bfed_dur fai_long)
drop if inrange(ipw_vars,9,17) 

********************************************************************************
********************************************************************************
**#MULTIPLE OUTCOMES************************************************************
********************************************************************************
********************************************************************************

*What is the expected N once those missing outcome data AND missing any weigthing variable have been dropped?
count if has_mult==0 & inrange(ipw_vars,1,8) //1184 to be dropped (overall sample of 10,890)
drop if has_mult==0 & inrange(ipw_vars,1,8)

*Check analysis sample = 9,168
tab has_mult

**#*Imputation
*Focus dataset to relevant variables 
keep parentedu parentsclass edu sclass findiff ///
	 pain heavy days irreg ///
	 ethnicity mat_age menarche ///
	 has_mult  ///
	 marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity findiff_ipw highsclass bfed_dur fai_long ///
	 aln 

**Missing data patterns
*Complete 
mdesc has_mult mat_age 
*Substantive variables (missing)
mdesc parentedu parentsclass edu sclass findiff pain heavy days irreg ethnicity menarche
*IPW variables (missing)
mdesc marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity findiff_ipw highsclass bfed_dur fai_long

**Set up imputation
mi set flong
mi register imputed parentedu parentsclass edu sclass findiff ethnicity menarche marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity findiff_ipw highsclass bfed_dur fai_long
mi register regular has_mult aln mat_age pain heavy days irreg

**Dryrun
mi impute chained (ologit) parentedu edu housing first_preg parity bfed_dur crowding ///
				  (logit, omit(i.highsclass)) sclass ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (logit) parentsclass ethnicity marital_stat phone car dbl_glaze smoke_preg ///
				  (logit, omit(i.sclass)) highsclass ///
				  (pmm, knn(5)) epds fai_long menarche ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_mult mat_age pain heavy days irreg,  dryrun
				  
				  
**Trace plot version
mi impute chained (ologit) parentedu edu housing first_preg parity bfed_dur crowding ///
				  (logit, omit(i.highsclass)) sclass ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (logit) parentsclass ethnicity marital_stat phone car dbl_glaze smoke_preg ///
				  (logit, omit(i.sclass)) highsclass ///
				  (pmm, knn(5)) epds fai_long menarche ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_mult mat_age pain heavy days irreg,  force burnin(100) rseed(63556094) dots chainonly noisily showcommand ///
				  savetrace ("/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Mult/Trace/MI_TraceData.dta", replace)			  
				  
				  
*Trace checks
use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Mult/Trace/MI_TraceData.dta", clear
			 
describe 
tsset iter

cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Mult/Trace"

foreach cov in parentedu parentsclass edu sclass findiff ethnicity menarche marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity findiff_ipw highsclass bfed_dur fai_long {
	tsline `cov'_mean, title(Mean imputed values of `cov') legend(off) 
	graph export `cov'_mean.png, replace
	tsline `cov'_sd, title(Standard deviation imputed values of `cov') legend(off)
	graph export `cov'_sd.png, replace
}				  
			  
*All good for 50 iterations 				  		  			  

**Run imputation model				  
mi impute chained (ologit) parentedu edu housing first_preg parity bfed_dur crowding ///
				  (logit, omit(i.highsclass)) sclass ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (logit) parentsclass ethnicity marital_stat phone car dbl_glaze smoke_preg ///
				  (logit, omit(i.sclass)) highsclass ///
				  (pmm, knn(5)) epds fai_long menarche ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_mult mat_age pain heavy days irreg,  force add(75) burnin(50) rseed(63556094) dots			  

*Save
save "/Volumes/157/working/data/G0_SEP_MultSens.dta", replace				

**#*Checks
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Mult/Post_Imp_Checks", replace
		  
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
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Mult"

foreach var in epds findiff_ipw fai_long {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck.png, replace 
}


log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Mult/Post_Imp_Checks", append
**FMI and Mcerror checks 
foreach exp in parentedu parentsclass edu sclass findiff {
	foreach out in pain heavy days irreg {
		mi estimate, mcerror: logistic `out' `exp' ethnicity mat_age menarche
	}
}

log close

*Largest FMI = 0.2342
*Any mcerror concerns? No, all good. 

**#*Deriving weights
forvalues j = 1/75 {
	logistic has_mult mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg epds i.parity ethnicity i.edu findiff_ipw highsclass i.bfed_dur fai_long if _mi_m==`j'
	predict multp`j' if _mi_m==`j'
}				  
  
**Consistent name across datasets
gen multp = multp1
forvalues j = 2/75 {
	replace multp=multp`j' if multp==.
}
			  				  
**Create probability and weights
gen prob_mult=multp if has_mult==1
replace prob_mult=1-multp if has_mult==0
gen ipw_mult=1/prob_mult


**Tidy up 
forvalues j = 1/75 {
	drop multp`j'
}				  
				  
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Mult/Weights_and_Descriptives", replace
*Summary of weights
summ ipw_mult, det

log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Mult"

histogram ipw_mult
graph export mult_weights.png, replace 
	
 
*Replace dataset now outcomes deleted and weights derived
save "/Volumes/157/working/data/G0_SEP_MultSens.dta", replace

**#*Descriptives

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Mult/Weights_and_Descriptives", append

**Simple 
*Exposures
foreach var in parentedu parentsclass edu sclass findiff {
	mi estimate: proportion `var' if has_mult==1
}

*Confounders
mi estimate: proportion ethnicity if has_mult==1
foreach var in mat_age menarche {
	summ `var' if imputed==0 & has_mult==1, det
}

**Cross tabs 
foreach out in pain heavy days irreg {
	foreach var in parentedu parentsclass edu sclass findiff ethnicity {
		mi estimate: proportion `var' if has_mult==1, over(`out')	
	}
	foreach var in mat_age menarche {
		bysort `out': summ `var' if imputed==0 & has_mult==1, det
	}
}

log close	

**#*Analysis
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Mult/Analysis", replace

foreach exp in parentedu parentsclass edu sclass findiff {
	foreach out in pain heavy days irreg {
		eststo `exp'_`out'c: mi estimate, or post: logistic `out' i.`exp' [pw=ipw_mult] if has_mult==1
		eststo `exp'_`out'age: mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age [pw=ipw_mult] if has_mult==1
		eststo `exp'_`out'men: mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age menarche [pw=ipw_mult] if has_mult==1
	}
}	
	
log close

********************************************************************************
********************************************************************************
**#LENGTH OUTCOME***************************************************************
********************************************************************************
********************************************************************************

*What is the expected N once those missing outcome data AND missing any weigthing variable have been dropped?
count if has_length==0 & inrange(ipw_vars,1,8) //2140 to be dropped (overall sample of 9,934)
drop if has_length==0 & inrange(ipw_vars,1,8) 

*Check analysis sample = 4,828
tab has_length

**#*Imputation
*Focus dataset to relevant variables 
keep parentedu parentsclass edu sclass findiff ///
	 length ///
	 ethnicity mat_age menarche ///
	 has_length  ///
	 marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity findiff_ipw highsclass bfed_dur fai_long ///
	 aln 

**Missing data patterns
*Complete 
mdesc has_length mat_age 
*Substantive variables (missing)
mdesc parentedu parentsclass edu sclass findiff length ethnicity menarche
*IPW variables (missing)
mdesc marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity findiff_ipw highsclass bfed_dur fai_long

**Set up imputation
mi set flong
mi register imputed parentedu parentsclass edu sclass findiff ethnicity menarche marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity findiff_ipw highsclass bfed_dur fai_long
mi register regular has_length aln mat_age length

**Dryrun
mi impute chained (ologit) parentedu edu housing first_preg parity bfed_dur crowding ///
				  (logit, omit(i.highsclass)) sclass ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (logit) parentsclass ethnicity marital_stat phone car dbl_glaze smoke_preg ///
				  (logit, omit(i.sclass)) highsclass ///
				  (pmm, knn(5)) epds fai_long menarche ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_length mat_age length,  dryrun
				  
				  
**Trace plot version
mi impute chained (ologit) parentedu edu housing first_preg parity bfed_dur crowding ///
				  (logit, omit(i.highsclass)) sclass ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (logit) parentsclass ethnicity marital_stat phone car dbl_glaze smoke_preg ///
				  (logit, omit(i.sclass)) highsclass ///
				  (pmm, knn(5)) epds fai_long menarche ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_length mat_age length, force burnin(100) rseed(53785494) dots chainonly noisily showcommand ///
				  savetrace ("/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Length/Trace/MI_TraceData.dta", replace)			  
				  
				  
*Trace checks
use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Length/Trace/MI_TraceData.dta", clear
			 
describe 
tsset iter

cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Length/Trace"

foreach cov in parentedu parentsclass edu sclass findiff ethnicity menarche marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity findiff_ipw highsclass bfed_dur fai_long {
	tsline `cov'_mean, title(Mean imputed values of `cov') legend(off) 
	graph export `cov'_mean.png, replace
	tsline `cov'_sd, title(Standard deviation imputed values of `cov') legend(off)
	graph export `cov'_sd.png, replace
}				  
			  
*All good - stable by 50 iterations. 				  		  			  

**Run imputation model				  
mi impute chained (ologit) parentedu edu housing first_preg parity bfed_dur crowding ///
				  (logit, omit(i.highsclass)) sclass ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (logit) parentsclass ethnicity marital_stat phone car dbl_glaze smoke_preg ///
				  (logit, omit(i.sclass)) highsclass ///
				  (pmm, knn(5)) epds fai_long menarche ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_length mat_age length, force add(75) burnin(50) rseed(53785494) dots			  

*Save
save "/Volumes/157/working/data/G0_SEP_LengthSens.dta", replace				

**#*Checks
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Length/Post_Imp_Checks", replace
		  
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
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Length"

foreach var in epds findiff_ipw fai_long {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck.png, replace 
}


log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Length/Post_Imp_Checks", append
**FMI and Mcerror checks 
foreach exp in parentedu parentsclass edu sclass findiff {
		mi estimate, mcerror: logistic length `exp' ethnicity mat_age menarche
	}

log close

*Largest FMI = 0.2082
*Any mcerror concerns? No, all good.

**#*Deriving weights
forvalues j = 1/75 {
	logistic has_length mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg epds i.parity ethnicity i.edu findiff_ipw highsclass i.bfed_dur fai_long if _mi_m==`j'
	predict lengthp`j' if _mi_m==`j'
}				  
  
**Consistent name across datasets
gen lengthp = lengthp1
forvalues j = 2/75 {
	replace lengthp=lengthp`j' if lengthp==.
}
			  				  
**Create probability and weights
gen prob_length=lengthp if has_length==1
replace prob_length=1-lengthp if has_length==0
gen ipw_length=1/prob_length


**Tidy up 
forvalues j = 1/75 {
	drop lengthp`j'
}				  
				  
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Length/Weights_and_Descriptives", replace
*Summary of weights
summ ipw_length, det

log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Length"

histogram ipw_length
graph export length_weights.png, replace 
	
 
*Replace dataset now outcomes deleted and weights derived
save "/Volumes/157/working/data/G0_SEP_LengthSens.dta", replace

**#*Descriptives

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Length/Weights_and_Descriptives", append

**Simple 
*Exposures
foreach var in parentedu parentsclass edu sclass findiff {
	mi estimate: proportion `var' if has_length==1
}

*Confounders
mi estimate: proportion ethnicity if has_length==1
foreach var in mat_age menarche {
	summ `var' if imputed==0 & has_length==1, det
}

**Cross tabs 
foreach var in parentedu parentsclass edu sclass findiff ethnicity {
	mi estimate: proportion `var' if has_length==1, over(length)	
}
foreach var in mat_age menarche {
	bysort length: summ `var' if imputed==0 & has_length==1, det
}


log close	

**#*Analysis
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/Length/Analysis", replace

foreach exp in parentedu parentsclass edu sclass findiff {
	eststo `exp'_lengthc: mi estimate, or post: logistic length i.`exp' [pw=ipw_length] if has_length==1
	eststo `exp'_lengthage: mi estimate, or post: logistic length i.`exp' ethnicity mat_age [pw=ipw_length] if has_length==1
	eststo `exp'_lengthmen: mi estimate, or post: logistic length i.`exp' ethnicity mat_age menarche [pw=ipw_length] if has_length==1
}

	
log close

********************************************************************************
********************************************************************************
**#PMS OUTCOME******************************************************************
********************************************************************************
********************************************************************************

*What is the expected N once those missing outcome data AND missing any weigthing variable have been dropped?
count if has_pms==0 & inrange(ipw_vars,1,8) //1555 to be dropped (overall sample of 10,519)
drop if has_pms==0 & inrange(ipw_vars,1,8)

*Check analysis sample = 7,769
tab has_pms


**#*Imputation
*Focus dataset to relevant variables 
keep parentedu parentsclass edu sclass findiff ///
	 pms ///
	 ethnicity mat_age menarche ///
	 has_pms  ///
	 marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity findiff_ipw highsclass bfed_dur fai_long ///
	 aln 

**Missing data patterns
*Complete 
mdesc has_pms mat_age 
*Substantive variables (missing)
mdesc parentedu parentsclass edu sclass findiff pms ethnicity menarche
*IPW variables (missing)
mdesc marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity findiff_ipw highsclass bfed_dur fai_long

**Set up imputation
mi set flong
mi register imputed parentedu parentsclass edu sclass findiff ethnicity menarche marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity findiff_ipw highsclass bfed_dur fai_long
mi register regular has_pms aln mat_age pms

**Dryrun
mi impute chained (ologit) parentedu edu housing first_preg parity bfed_dur crowding ///
				  (logit, omit(i.highsclass)) sclass ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (logit) parentsclass ethnicity marital_stat phone car dbl_glaze smoke_preg ///
				  (logit, omit(i.sclass)) highsclass ///
				  (pmm, knn(5)) epds fai_long menarche ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_pms mat_age pms,  dryrun
				  
				  
**Trace plot version
mi impute chained (ologit) parentedu edu housing first_preg parity bfed_dur crowding ///
				  (logit, omit(i.highsclass)) sclass ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (logit) parentsclass ethnicity marital_stat phone car dbl_glaze smoke_preg ///
				  (logit, omit(i.sclass)) highsclass ///
				  (pmm, knn(5)) epds fai_long menarche ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_pms mat_age pms, force burnin(100) rseed(3608399) dots chainonly noisily showcommand ///
				  savetrace ("/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/PMS/Trace/MI_TraceData.dta", replace)			  
				  
				  
*Trace checks
use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/PMS/Trace/MI_TraceData.dta", clear
			 
describe 
tsset iter

cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/PMS/Trace"

foreach cov in parentedu parentsclass edu sclass findiff ethnicity menarche marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity findiff_ipw highsclass bfed_dur fai_long {
	tsline `cov'_mean, title(Mean imputed values of `cov') legend(off) 
	graph export `cov'_mean.png, replace
	tsline `cov'_sd, title(Standard deviation imputed values of `cov') legend(off)
	graph export `cov'_sd.png, replace
}				  
			  
				  		  			  

**Run imputation model				  
mi impute chained (ologit) parentedu edu housing first_preg parity bfed_dur crowding ///
				  (logit, omit(i.highsclass)) sclass ///
				  (logit, omit(findiff_ipw)) findiff ///
				  (logit) parentsclass ethnicity marital_stat phone car dbl_glaze smoke_preg ///
				  (logit, omit(i.sclass)) highsclass ///
				  (pmm, knn(5)) epds fai_long menarche ///
				  (pmm, knn(5) omit(i.findiff)) findiff_ipw ///
				  = has_pms mat_age pms, force add(75) burnin(50) rseed(3608399) dots			  

*Save
save "/Volumes/157/working/data/G0_SEP_PMSSens.dta", replace				

**#*Checks
use "/Volumes/157/working/data/G0_SEP_PMSSens.dta", clear	

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/PMS/Post_Imp_Checks", replace
		  
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
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/PMS"

foreach var in epds findiff_ipw fai_long {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck.png, replace 
}


log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/PMS/Post_Imp_Checks", append
**FMI and Mcerror checks 
foreach exp in parentedu parentsclass edu sclass findiff {
		mi estimate, mcerror: logistic pms `exp' ethnicity mat_age menarche
	}

log close

*Largest FMI = 0.1852
*Any mcerror concerns? No, all good. 

**#*Deriving weights
forvalues j = 1/75 {
	logistic has_pms mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg epds i.parity ethnicity i.edu findiff_ipw highsclass i.bfed_dur fai_long if _mi_m==`j'
	predict pmsp`j' if _mi_m==`j'
}				  
  
**Consistent name across datasets
gen pmsp = pmsp1
forvalues j = 2/75 {
	replace pmsp=pmsp`j' if pmsp==.
}
			  				  
**Create probability and weights
gen prob_pms=pmsp if has_pms==1
replace prob_pms=1-pmsp if has_pms==0
gen ipw_pms=1/prob_pms


**Tidy up 
forvalues j = 1/75 {
	drop pmsp`j'
}				  
				  
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/PMS/Weights_and_Descriptives", replace
*Summary of weights
summ ipw_pms, det

log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/PMS"

histogram ipw_pms
graph export pms_weights.png, replace 
	
 
*Replace dataset now outcomes deleted and weights derived
save "/Volumes/157/working/data/G0_SEP_PMSSens.dta", replace

**#*Descriptives

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/PMS/Weights_and_Descriptives", append

**Simple 
*Exposures
foreach var in parentedu parentsclass edu sclass findiff {
	mi estimate: proportion `var' if has_pms==1
}

*Confounders
mi estimate: proportion ethnicity if has_pms==1
foreach var in mat_age menarche {
	summ `var' if imputed==0 & has_pms==1, det
}

**Cross tabs 
foreach var in parentedu parentsclass edu sclass findiff ethnicity {
	mi estimate: proportion `var' if has_pms==1, over(pms)	
}
foreach var in mat_age menarche {
	bysort pms: summ `var' if imputed==0 & has_pms==1, det
}


log close	

**#*Analysis
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/MI_IPW_Sens/PMS/Analysis", replace

foreach exp in parentedu parentsclass edu sclass findiff {
	eststo `exp'_pmsc: mi estimate, or post: logistic pms i.`exp' [pw=ipw_pms] if has_pms==1
	eststo `exp'_pmsage: mi estimate, or post: logistic pms i.`exp' ethnicity mat_age [pw=ipw_pms] if has_pms==1
	eststo `exp'_pmsmen: mi estimate, or post: logistic pms i.`exp' ethnicity mat_age menarche [pw=ipw_pms] if has_pms==1
}

	
log close


**#Continuous or categorical education exposures
log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/Education_ContorCat_MI", replace

*Multiple outcomes
use "/Volumes/157/working/data/G0_SEP_MultSens.dta", clear

foreach exp in parentedu edu {
	foreach out in pain heavy days irreg {
		mi estimate, or post: logistic `out' `exp' [pw=ipw_mult] if has_mult==1
		mi estimate, or post: logistic `out' `exp' ethnicity mat_age [pw=ipw_mult] if has_mult==1
		mi estimate, or post: logistic `out' `exp' ethnicity mat_age menarche [pw=ipw_mult] if has_mult==1
	}
}

clear 

*Length
use "/Volumes/157/working/data/G0_SEP_LengthSens.dta", clear
	
foreach exp in parentedu edu {
	mi estimate, or post: logistic length `exp' [pw=ipw_length] if has_length==1
	mi estimate, or post: logistic length `exp' ethnicity mat_age [pw=ipw_length] if has_length==1
	mi estimate, or post: logistic length `exp' ethnicity mat_age menarche [pw=ipw_length] if has_length==1
}

clear

*PMS
use "/Volumes/157/working/data/G0_SEP_PMSSens.dta", clear

*Continuous
foreach exp in parentedu edu {
	mi estimate, or post: logistic pms `exp' [pw=ipw_pms] if has_pms==1
	mi estimate, or post: logistic pms `exp' ethnicity mat_age [pw=ipw_pms] if has_pms==1
	mi estimate, or post: logistic pms `exp' ethnicity mat_age menarche [pw=ipw_pms] if has_pms==1
}

*Categorical with overall p value (own education)

mi estimate, or post: logistic pms i.edu [pw=ipw_pms] if has_pms==1
testparm i.edu
mi estimate, or post: logistic pms i.edu ethnicity mat_age [pw=ipw_pms] if has_pms==1
testparm i.edu
mi estimate, or post: logistic pms i.edu ethnicity mat_age menarche [pw=ipw_pms] if has_pms==1
testparm i.edu

log close


***Categorical with overall p value for all

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/SEP to Symptoms Chapter/ALSPAC G0/Results/Education_Cat_OverallP_MI", append

**ALTERNATIVE MI

*Multiple outcomes
use "/Volumes/157/working/data/G0_SEP_MultSens.dta", clear

foreach exp in parentedu edu {
	foreach out in pain heavy days irreg {
		quietly mi estimate, or post: logistic `out' i.`exp' [pw=ipw_mult] if has_mult==1
		di "CRUDE"
		testparm i.`exp'
		quietly mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age [pw=ipw_mult] if has_mult==1
		di "ETH AND AGE ADJ"
		testparm i.`exp'
		quietly mi estimate, or post: logistic `out' i.`exp' ethnicity mat_age menarche [pw=ipw_mult] if has_mult==1
		di "PLUS MENARCHE"
		testparm i.`exp'
	}
}

clear 

*Length
use "/Volumes/157/working/data/G0_SEP_LengthSens.dta", clear
	
foreach exp in parentedu edu {
	quietly mi estimate, or post: logistic length i.`exp' [pw=ipw_length] if has_length==1
	di "CRUDE"
	testparm i.`exp'
	quietly mi estimate, or post: logistic length i.`exp' ethnicity mat_age [pw=ipw_length] if has_length==1
	di "ETH AND AGE ADJ"
	testparm i.`exp'
	quietly mi estimate, or post: logistic length i.`exp' ethnicity mat_age menarche [pw=ipw_length] if has_length==1
	di "PLUS MENARCHE"
	testparm i.`exp'
}

clear

*PMS
use "/Volumes/157/working/data/G0_SEP_PMSSens.dta", clear

*Continuous
foreach exp in parentedu edu {
	quietly mi estimate, or post: logistic pms i.`exp' [pw=ipw_pms] if has_pms==1
	di "CRUDE"
	testparm i.`exp'
	quietly mi estimate, or post: logistic pms i.`exp' ethnicity mat_age [pw=ipw_pms] if has_pms==1
	di "ETH AND AGE ADJ"
	testparm i.`exp'
	quietly mi estimate, or post: logistic pms i.`exp' ethnicity mat_age menarche [pw=ipw_pms] if has_pms==1
	di "PLUS MENARCHE"
	testparm i.`exp'
}
	
log close

