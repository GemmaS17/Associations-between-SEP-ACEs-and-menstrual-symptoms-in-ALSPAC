**G0 ACEs ALTERNATIVE MI ANALYSIS

**#Run data prep from main do file

**IMPUTATION PREP***************************************************************

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

**12,537 ppts to be imputed

********************************************************************************
**#*MULTIPLE OUTCOME SAMPLE*****************************************************
********************************************************************************
*1,460 to drop (weight to sample of 11,077)
count if inrange(ipw_vars,1,8) & has_mult==0
drop if inrange(ipw_vars,1,8) & has_mult==0

*Focus dataset to relevant variables 
keep phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ///
	 pain heavy days irreg has_mult ///
	 parentedu parentsclass ethnicity mat_age menarche ///
	 marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity edu findiff_ipw highsclass bfed_dur fai_long ///
	 fam_poorer control priv_invade felt_unwanted local_authority m_absence p_absence prnt_rltn_fright prnt_rltn_remote happy matsm alc_use ///
	 aln 
	 
**#Set up imputation
mi set flong
mi register imputed phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd parentedu parentsclass ethnicity menarche marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity edu findiff_ipw highsclass bfed_dur fai_long fam_poorer control priv_invade felt_unwanted local_authority m_absence p_absence prnt_rltn_fright prnt_rltn_remote happy matsm alc_use
mi register regular pain heavy days irreg has_mult aln mat_age

**Dryrun
mi impute chained (logit) phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ///
				  (logit) ethnicity parentsclass marital_stat phone car dbl_glaze highsclass fam_poorer local_authority m_absence p_absence happy ///
				  (logit, omit(i.matsm)) smoke_preg ///
				  (ologit, omit(i.smoke_preg)) matsm ///
				  (ologit) parentedu crowding first_preg parity edu bfed_dur control priv_invade felt_unwanted prnt_rltn_fright prnt_rltn_remote alc_use housing ///
				  (pmm, knn(5)) menarche epds findiff_ipw fai_long ///
				  = pain heavy days irreg has_mult mat_age, dryrun

**Trace plot		
mi impute chained (logit) phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ///
				  (logit) ethnicity parentsclass marital_stat phone car dbl_glaze highsclass fam_poorer local_authority m_absence p_absence happy ///
				  (logit, omit(i.matsm)) smoke_preg ///
				  (ologit, omit(i.smoke_preg)) matsm ///
				  (ologit) parentedu crowding first_preg parity edu bfed_dur control priv_invade felt_unwanted prnt_rltn_fright prnt_rltn_remote alc_use housing ///
				  (pmm, knn(5)) menarche epds findiff_ipw fai_long ///
				  = pain heavy days irreg has_mult mat_age, force burnin(100) rseed(5755419) dots chainonly noisily showcommand ///
				  savetrace ("/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Mult/Trace/MI_TraceData.dta", replace) 
				  
**#Trace checks 
use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Mult/Trace/MI_TraceData.dta", clear			 
describe 
tsset iter

cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Mult/Trace"
foreach cov in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd parentedu parentsclass ethnicity marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity edu findiff_ipw highsclass bfed_dur fai_long {
	tsline `cov'_mean, title(Mean imputed values of `cov') legend(off) 
	graph export `cov'_mean.png, replace
	tsline `cov'_sd, title(Standard deviation imputed values of `cov') legend(off)
	graph export `cov'_sd.png, replace
}	
*60 iterations (same as main MI) okay? Yes, all looks good by 60 iterations. 

**#Run imputation	
mi impute chained (logit) phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ///
				  (logit) ethnicity parentsclass marital_stat phone car dbl_glaze highsclass fam_poorer local_authority m_absence p_absence happy ///
				  (logit, omit(i.matsm)) smoke_preg ///
				  (ologit, omit(i.smoke_preg)) matsm ///
				  (ologit) parentedu crowding first_preg parity edu bfed_dur control priv_invade felt_unwanted prnt_rltn_fright prnt_rltn_remote alc_use housing ///
				  (pmm, knn(5)) menarche epds findiff_ipw fai_long ///
				  = pain heavy days irreg has_mult mat_age, force add(60) burnin(60) rseed(5755419) dots	
				  
save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Mult/Mult_ACEs_MISensitivity.dta", replace

**#Post-imputation checks			
use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Mult/Mult_ACEs_MISensitivity.dta", clear

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Mult/Post_Imp_Checks", replace 
				  
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
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Mult"

foreach var in epds findiff_ipw fai_long {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck.png, replace 
}				  

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Mult/Post_Imp_Checks", append	
			  
**FMI and Mcerror checks 
foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd {
	foreach out in pain heavy days irreg {
		mi estimate, mcerror: logistic `out' `exp' parentedu parentsclass ethnicity mat_age menarche
	}
}

log close

*Highest FMI = 0.2512
*Any mcerror concerns? No, all good. 
				  
**#Deriving weights
forvalues j = 1/60 {
	logistic has_mult mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg epds i.parity ethnicity i.edu findiff_ipw highsclass i.bfed_dur fai_long if _mi_m==`j'
	predict multp`j' if _mi_m==`j'
}				  
 
*Consistent name across datasets
gen multp = multp1
forvalues j = 2/60 {
	replace multp=multp`j' if multp==.
}
			  				  
*Create probability and weights
gen prob_mult=multp if has_mult==1
replace prob_mult=1-multp if has_mult==0
gen ipw_mult=1/prob_mult

**Tidy up 
forvalues j = 1/60 {
	drop multp`j'
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Mult/Post_Imp_Checks", append

**Summary of weights
summ ipw_mult, det
log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Mult"

histogram ipw_mult
graph export mult_weights.png, replace 

 
*Replace dataset now outcomes deleted and weights derived
save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Mult/Mult_ACEs_MISensitivity.dta", replace			  
				  
**#Analysis
*Passively derive ace score
egen ace_score=rowtotal(phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd)

gen ace_four = ace_score
recode ace_four (0=0) (1=1) (2=2) (3=3) (4/8=4)
tab ace_four
label define scores 0"none" 1"1" 2"2" 3"3" 4"4 or more", replace
label values ace_four scores
label variable ace_four "Number of ACEs"

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Mult/Weighted_Analysis_log", replace

*Mult outcomes
foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four {
	foreach out in pain heavy days irreg {
		mi estimate, or post: logistic `out' i.`exp' [pw=ipw_mult] if has_mult==1
		mi estimate, or post: logistic `out' i.`exp' parentedu parentsclass ethnicity mat_age [pw=ipw_mult] if has_mult==1
		mi estimate, or post: logistic `out' i.`exp' parentedu parentsclass ethnicity mat_age menarche [pw=ipw_mult] if has_mult==1
	}
}	
	
*One p value for ace_four and continuous 	
foreach out in pain heavy days irreg {
		di "CATEGORICAL WITH ONE P VALUE:"
		quietly mi estimate, or post: logistic `out' i.ace_four [pw=ipw_mult] if has_mult==1
		di "Crude"
		testparm i.ace_four
		quietly mi estimate, or post: logistic `out' i.ace_four parentedu parentsclass ethnicity mat_age [pw=ipw_mult] if has_mult==1
		di "Model 2"
		testparm i.ace_four
		quietly mi estimate, or post: logistic `out' i.ace_four parentedu parentsclass ethnicity mat_age menarche [pw=ipw_mult] if has_mult==1
		di "Model 3"
		testparm i.ace_four
		di "CONTINUOUS"
		mi estimate, or post: logistic `out' ace_four [pw=ipw_mult] if has_mult==1
		mi estimate, or post: logistic `out' ace_four parentedu parentsclass ethnicity mat_age [pw=ipw_mult] if has_mult==1
		mi estimate, or post: logistic `out' ace_four parentedu parentsclass ethnicity mat_age menarche [pw=ipw_mult] if has_mult==1
}
	
log close			  
				  
********************************************************************************
**#*LENGTH OUTCOME SAMPLE*******************************************************
********************************************************************************
*2,543 to drop (weight to sample of 9,994)
count if inrange(ipw_vars,1,8) & has_length==0
drop if inrange(ipw_vars,1,8) & has_length==0

*Focus dataset to relevant variables 
keep phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ///
	 length has_length  ///
	 parentedu parentsclass ethnicity mat_age menarche ///
	 marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity edu findiff_ipw highsclass bfed_dur fai_long ///
	 fam_poorer control priv_invade felt_unwanted local_authority m_absence p_absence prnt_rltn_fright prnt_rltn_remote happy matsm alc_use ///
	 aln 

*Recode auxiliar vars to binary
recode felt_unwanted (0=0) (1/2=1)	 

foreach var in control priv_invade prnt_rltn_fright prnt_rltn_remote matsm alc_use {
	gen `var'_cat=`var'
	recode `var' (0=0) (1/2=1)
}
	
*What might be problematic?
foreach var in fam_poorer control priv_invade felt_unwanted local_authority m_absence p_absence prnt_rltn_fright prnt_rltn_remote happy matsm alc_use {
	foreach vars in fam_poorer control priv_invade felt_unwanted local_authority m_absence p_absence prnt_rltn_fright prnt_rltn_remote happy matsm alc_use {
		tab `var' `vars', m
	}
}
	
	
**#Set up imputation
mi set flong
mi register imputed phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd parentedu parentsclass ethnicity menarche marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity edu findiff_ipw highsclass bfed_dur fai_long fam_poorer control priv_invade felt_unwanted local_authority m_absence p_absence prnt_rltn_fright prnt_rltn_remote happy matsm alc_use
mi register regular length has_length aln mat_age

**Dryrun
*Aux to binary and omit problematic substantive ologits from each other (failed m=10)
mi impute chained (logit) phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ///
				  (logit) ethnicity parentsclass marital_stat phone car dbl_glaze highsclass fam_poorer local_authority m_absence p_absence happy felt_unwanted control priv_invade prnt_rltn_fright prnt_rltn_remote alc_use ///
				  (logit, omit(i.matsm i.first_preg i.parity)) smoke_preg ///
				  (logit, omit(i.smoke_preg i.first_preg)) matsm  ///
				  (ologit, omit(i.smoke_preg i.matsm i.parity i.parentedu)) first_preg ///
				  (ologit, omit(i.first_preg i.smoke_preg)) parity ///
				  (ologit, omit(i.edu i.bfed_dur i.first_preg)) parentedu ///
				  (ologit, omit(i.bfed_dur)) crowding ///
				  (ologit, omit(i.crowding i.parentedu)) bfed_dur ///
				  (ologit, omit(i.parentedu)) edu ///
				  (ologit) housing ///
				  (pmm, knn(5)) menarche epds findiff_ipw fai_long ///
				  = length has_length mat_age, dryrun
*Ascontinuous attempt (failed m=55)				  
mi impute chained (logit) phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ///
				  (logit) ethnicity parentsclass marital_stat phone car dbl_glaze highsclass fam_poorer local_authority m_absence p_absence happy felt_unwanted ///
				  (logit, omit(i.matsm i.first_preg i.parity)) smoke_preg ///
				  (ologit, omit(i.smoke_preg i.matsm i.parity)) first_preg ///
				  (ologit, omit(i.smoke_preg i.first_preg)) matsm  ///
				  (ologit, omit(i.first_preg i.smoke_preg)) parity ///
				  (ologit, ascontinuous) parentedu crowding edu bfed_dur control priv_invade prnt_rltn_fright prnt_rltn_remote alc_use housing ///
				  (pmm, knn(5)) menarche epds findiff_ipw fai_long ///
				  = length has_length mat_age, dryrun
				  
*Ascontinuous for substantive ologits plus auxiliary to binary and omit problematic ones from each other 				  
mi impute chained (logit) phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ///
				  (logit) ethnicity parentsclass marital_stat phone car dbl_glaze highsclass fam_poorer happy prnt_rltn_fright prnt_rltn_remote alc_use housing ///
				  (logit, omit(i.local_authority i.m_absence i.p_absence)) control ///
				  (logit, omit(i.m_absence i.p_absence)) priv_invade ///
				  (logit, omit(i.control i.m_absence i.p_absence i.felt_unwanted)) local_authority ///
				  (logit, omit(i.m_absence i.p_absence i.local_authority)) felt_unwanted ///
				  (logit, omit(i.control i.priv_invade i.felt_unwanted i.local_authority i.p_absence)) m_absence ///
				  (logit, omit(i.control i.priv_invade i.felt_unwanted i.local_authority i.m_absence)) p_absence ///
				  (logit, omit(matsm first_preg parity)) smoke_preg ///
				  (ologit, ascontinuous omit(i.smoke_preg matsm parity)) first_preg ///
				  (ologit, ascontinuous omit(i.smoke_preg first_preg)) matsm parity ///
				  (ologit, ascontinuous) parentedu crowding edu bfed_dur  ///
				  (pmm, knn(5)) menarche epds findiff_ipw fai_long ///
				  = length has_length mat_age, dryrun				  
				  

**Trace plot		
mi impute chained (logit) phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ///
				  (logit) ethnicity parentsclass marital_stat phone car dbl_glaze highsclass fam_poorer local_authority m_absence p_absence happy felt_unwanted control priv_invade prnt_rltn_fright prnt_rltn_remote alc_use ///
				  (logit, omit(i.matsm i.first_preg i.parity)) smoke_preg ///
				  (logit, omit(i.smoke_preg i.first_preg)) matsm  ///
				  (ologit, omit(i.smoke_preg i.matsm i.parity i.parentedu)) first_preg ///
				  (ologit, omit(i.first_preg i.smoke_preg)) parity ///
				  (ologit, omit(i.edu i.bfed_dur i.first_preg)) parentedu ///
				  (ologit, omit(i.bfed_dur)) crowding ///
				  (ologit, omit(i.crowding i.parentedu)) bfed_dur ///
				  (ologit, omit(i.parentedu)) edu ///
				  (ologit) housing ///
				  (pmm, knn(5)) menarche epds findiff_ipw fai_long ///
				  = length has_length mat_age, force burnin(100) rseed(19453547) dots chainonly noisily showcommand ///
				  savetrace ("/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Length/Trace/MI_TraceData.dta", replace) 
				  
*Perfect prediction figure out		
foreach var in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd parentedu parentsclass ethnicity  marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg edu highsclass bfed_dur fam_poorer control priv_invade  parity local_authority m_absence p_absence prnt_rltn_fright prnt_rltn_remote happy matsm alc_use {
	tab felt_unwanted `var' , m
}		

*Perfect prediction problem again - need to look at all the ologit vars 
foreach var in first_preg matsm	parity parentedu crowding edu bfed_dur control {
	foreach vars in first_preg matsm parity parentedu crowding edu bfed_dur control priv_invade prnt_rltn_fright prnt_rltn_remote alc_use housing {
		tab `var' `vars', m
	}
}   
foreach var in priv_invade prnt_rltn_fright prnt_rltn_remote alc_use housing {
	foreach vars in first_preg matsm parity parentedu crowding edu bfed_dur control priv_invade prnt_rltn_fright prnt_rltn_remote alc_use housing {
		tab `var' `vars', m
	}
}   
				  
**#Trace checks 
use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Length/Trace/MI_TraceData.dta", clear			 
describe 
tsset iter

cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Length/Trace"
foreach cov in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd parentedu parentsclass ethnicity marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity edu findiff_ipw highsclass bfed_dur fai_long {
	tsline `cov'_mean, title(Mean imputed values of `cov') legend(off) 
	graph export `cov'_mean.png, replace
	tsline `cov'_sd, title(Standard deviation imputed values of `cov') legend(off)
	graph export `cov'_sd.png, replace
}	
*60 iterations (same as main MI) okay? Yes, all looks good by 60 iterations. 

**#Run imputation	
mi impute chained (logit) phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ///
				  (logit) ethnicity parentsclass marital_stat phone car dbl_glaze highsclass fam_poorer happy prnt_rltn_fright prnt_rltn_remote alc_use housing ///
				  (logit, omit(i.local_authority i.m_absence i.p_absence)) control ///
				  (logit, omit(i.m_absence i.p_absence)) priv_invade ///
				  (logit, omit(i.control i.m_absence i.p_absence i.felt_unwanted)) local_authority ///
				  (logit, omit(i.m_absence i.p_absence i.local_authority)) felt_unwanted ///
				  (logit, omit(i.control i.priv_invade i.felt_unwanted i.local_authority i.p_absence)) m_absence ///
				  (logit, omit(i.control i.priv_invade i.felt_unwanted i.local_authority i.m_absence)) p_absence ///
				  (logit, omit(matsm first_preg parity)) smoke_preg ///
				  (ologit, ascontinuous omit(i.smoke_preg matsm parity)) first_preg ///
				  (ologit, ascontinuous omit(i.smoke_preg first_preg)) matsm parity ///
				  (ologit, ascontinuous) parentedu crowding edu bfed_dur  ///
				  (pmm, knn(5)) menarche epds findiff_ipw fai_long ///
				  = length has_length mat_age, force add(60) burnin(60) rseed(19453547) dots	
				  
save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Length/Length_ACEs_MISensitivity.dta", replace

**#Post-imputation checks			
*use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Length/Length_ACEs_MISensitivity.dta", clear

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Length/Post_Imp_Checks", replace 
				  
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
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Length"

foreach var in epds findiff_ipw fai_long {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck.png, replace 
}				  

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Length/Post_Imp_Checks", append	
			  
**FMI and Mcerror checks 
foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd {
	mi estimate, mcerror: logistic length `exp' parentedu parentsclass ethnicity mat_age menarche
}

log close

*Highest FMI = 0.2014
*Any mcerror concerns? No all good. 
				  
**#Deriving weights
forvalues j = 1/60 {
	logistic has_length mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg epds i.parity ethnicity i.edu findiff_ipw highsclass i.bfed_dur fai_long if _mi_m==`j'
	predict lengthp`j' if _mi_m==`j'
}				  
 
*Consistent name across datasets
gen lengthp = lengthp1
forvalues j = 2/60 {
	replace lengthp=lengthp`j' if lengthp==.
}
			  				  
*Create probability and weights
gen prob_length=lengthp if has_length==1
replace prob_length=1-lengthp if has_length==0
gen ipw_length=1/prob_length

**Tidy up 
forvalues j = 1/60 {
	drop lengthp`j'
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Length/Post_Imp_Checks", append

**Summary of weights
summ ipw_length, det
log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Length"

histogram ipw_length
graph export length_weights.png, replace 

 
*Replace dataset now outcomes deleted and weights derived
save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Length/Length_ACEs_MISensitivity.dta", replace			  
				  
**#Analysis
*Passively derive ace score
egen ace_score=rowtotal(phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd)

gen ace_four = ace_score
recode ace_four (0=0) (1=1) (2=2) (3=3) (4/8=4)
tab ace_four
label define scores 0"none" 1"1" 2"2" 3"3" 4"4 or more", replace
label values ace_four scores
label variable ace_four "Number of ACEs"

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/Length/Weighted_Analysis_log", replace

*Length outcome
foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four {
	mi estimate, or post: logistic length i.`exp' [pw=ipw_length] if has_length==1
	mi estimate, or post: logistic length i.`exp' parentedu parentsclass ethnicity mat_age [pw=ipw_length] if has_length==1
	mi estimate, or post: logistic length i.`exp' parentedu parentsclass ethnicity mat_age menarche [pw=ipw_length] if has_length==1
}
	
*One p value for ace_four and continuous 	
di "CATEGORICAL WITH ONE P VALUE:"
quietly mi estimate, or post: logistic length i.ace_four [pw=ipw_length] if has_length==1
di "Crude"
testparm i.ace_four
quietly mi estimate, or post: logistic length i.ace_four parentedu parentsclass ethnicity mat_age [pw=ipw_length] if has_length==1
di "Model 2"
testparm i.ace_four
quietly mi estimate, or post: logistic length i.ace_four parentedu parentsclass ethnicity mat_age menarche [pw=ipw_length] if has_length==1
di "Model 3"
testparm i.ace_four
di "CONTINUOUS"
mi estimate, or post: logistic length ace_four [pw=ipw_length] if has_length==1
mi estimate, or post: logistic length ace_four parentedu parentsclass ethnicity mat_age [pw=ipw_length] if has_length==1
mi estimate, or post: logistic length ace_four parentedu parentsclass ethnicity mat_age menarche [pw=ipw_length] if has_length==1

	
log close	

********************************************************************************
**#*PMS OUTCOME SAMPLE**********************************************************
********************************************************************************
*1,901 to drop (weight to sample of 10,636)
count if inrange(ipw_vars,1,8) & has_pms==0
drop if inrange(ipw_vars,1,8) & has_pms==0

*Focus dataset to relevant variables 
keep phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ///
	 pms has_pms ///
	 parentedu parentsclass ethnicity mat_age menarche ///
	 marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity edu findiff_ipw highsclass bfed_dur fai_long ///
	 fam_poorer control priv_invade felt_unwanted local_authority m_absence p_absence prnt_rltn_fright prnt_rltn_remote happy matsm alc_use ///
	 aln 

**#Set up imputation
*Recode auxiliar vars to binary
recode felt_unwanted (0=0) (1/2=1)	 

foreach var in control priv_invade prnt_rltn_fright prnt_rltn_remote matsm alc_use {
	gen `var'_cat=`var'
	recode `var' (0=0) (1/2=1)
}


mi set flong
mi register imputed phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd parentedu parentsclass ethnicity menarche marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity edu findiff_ipw highsclass bfed_dur fai_long fam_poorer control priv_invade felt_unwanted local_authority m_absence p_absence prnt_rltn_fright prnt_rltn_remote happy matsm alc_use
mi register regular pms has_pms aln mat_age

**Dryrun
mi impute chained (logit) phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ///
				  (logit) ethnicity parentsclass marital_stat phone car dbl_glaze highsclass fam_poorer local_authority m_absence p_absence happy ///
				  (logit, omit(i.matsm)) smoke_preg ///
				  (ologit, omit(i.smoke_preg)) matsm ///
				  (ologit) parentedu crowding first_preg parity edu bfed_dur control priv_invade felt_unwanted prnt_rltn_fright prnt_rltn_remote alc_use housing ///
				  (pmm, knn(5)) menarche epds findiff_ipw fai_long ///
				  = pms has_pms mat_age, dryrun

**Trace plot		
mi impute chained (logit) phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ///
				  (logit) ethnicity parentsclass marital_stat phone car dbl_glaze highsclass fam_poorer local_authority m_absence p_absence happy ///
				  (logit, omit(i.matsm)) smoke_preg ///
				  (ologit, omit(i.smoke_preg)) matsm ///
				  (ologit) parentedu crowding first_preg parity edu bfed_dur control priv_invade felt_unwanted prnt_rltn_fright prnt_rltn_remote alc_use housing ///
				  (pmm, knn(5)) menarche epds findiff_ipw fai_long ///
				  = pms has_pms mat_age, force burnin(100) rseed(29846126) dots chainonly noisily showcommand ///
				  savetrace ("/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/PMS/Trace/MI_TraceData.dta", replace) 
				  
**#Trace checks 
use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/PMS/Trace/MI_TraceData.dta", clear			 
describe 
tsset iter

cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/PMS/Trace"
foreach cov in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd parentedu parentsclass ethnicity marital_stat phone car housing crowding dbl_glaze first_preg smoke_preg epds parity edu findiff_ipw highsclass bfed_dur fai_long {
	tsline `cov'_mean, title(Mean imputed values of `cov') legend(off) 
	graph export `cov'_mean.png, replace
	tsline `cov'_sd, title(Standard deviation imputed values of `cov') legend(off)
	graph export `cov'_sd.png, replace
}	
*60 iterations (same as main MI) okay? Yes, all looks good by 60 iterations. 

**#Run imputation	
mi impute chained (logit) phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ///
				  (logit) ethnicity parentsclass marital_stat phone car dbl_glaze highsclass fam_poorer happy prnt_rltn_fright prnt_rltn_remote alc_use housing ///
				  (logit, omit(i.local_authority i.m_absence i.p_absence)) control ///
				  (logit, omit(i.m_absence i.p_absence)) priv_invade ///
				  (logit, omit(i.control i.m_absence i.p_absence i.felt_unwanted)) local_authority ///
				  (logit, omit(i.m_absence i.p_absence i.local_authority)) felt_unwanted ///
				  (logit, omit(i.control i.priv_invade i.felt_unwanted i.local_authority i.p_absence)) m_absence ///
				  (logit, omit(i.control i.priv_invade i.felt_unwanted i.local_authority i.m_absence)) p_absence ///
				  (logit, omit(matsm first_preg parity)) smoke_preg ///
				  (ologit, ascontinuous omit(i.smoke_preg matsm parity)) first_preg ///
				  (ologit, ascontinuous omit(i.smoke_preg first_preg)) matsm parity ///
				  (ologit, ascontinuous) parentedu crowding edu bfed_dur  ///
				  (pmm, knn(5)) menarche epds findiff_ipw fai_long ///
				  = pms has_pms mat_age, force add(60) burnin(60) rseed(29846126) dots			  
				  
save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/PMS/PMS_ACEs_MISensitivity.dta", replace

**#Post-imputation checks			
*use "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/PMS/PMS_ACEs_MISensitivity.dta", clear

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/PMS/Post_Imp_Checks", replace 
				  
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
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/PMS"

foreach var in epds findiff_ipw fai_long {
	twoway (histogram `var' if imputed==0, color(gray%30)) ///
		(histogram `var' if imputed==1, fcolor(none) lcolor(black)), ///
		legend(order(1 "Observed" 2 "Imputed")) 
		graph export `var'_MIcheck.png, replace 
}				  

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/PMS/Post_Imp_Checks", append	
			  
**FMI and Mcerror checks 
foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd {
	mi estimate, mcerror: logistic pms `exp' parentedu parentsclass ethnicity mat_age menarche
}

log close

*Highest FMI = 0.1950
*Any mcerror concerns? No, all good. 
				  
**#Deriving weights
forvalues j = 1/60 {
	logistic has_pms mat_age marital_stat phone car i.housing i.crowding dbl_glaze i.first_preg smoke_preg epds i.parity ethnicity i.edu findiff_ipw highsclass i.bfed_dur fai_long if _mi_m==`j'
	predict pmsp`j' if _mi_m==`j'
}				  
 
*Consistent name across datasets
gen pmsp = pmsp1
forvalues j = 2/60 {
	replace pmsp=pmsp`j' if pmsp==.
}
			  				  
*Create probability and weights
gen prob_pms=pmsp if has_pms==1
replace prob_pms=1-pmsp if has_pms==0
gen ipw_pms=1/prob_pms

**Tidy up 
forvalues j = 1/60 {
	drop pmsp`j'
}

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/PMS/Post_Imp_Checks", append

**Summary of weights
summ ipw_pms, det
log close

*Graphs
cd "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/PMS"

histogram ipw_pms
graph export pms_weights.png, replace 

 
*Replace dataset now outcomes deleted and weights derived
save "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/PMS/PMS_ACEs_MISensitivity.dta", replace			  
				  
**#Analysis
*Passively derive ace score
egen ace_score=rowtotal(phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd)

gen ace_four = ace_score
recode ace_four (0=0) (1=1) (2=2) (3=3) (4/8=4)
tab ace_four
label define scores 0"none" 1"1" 2"2" 3"3" 4"4 or more", replace
label values ace_four scores
label variable ace_four "Number of ACEs"

log using "/Users/qf20534/Library/CloudStorage/OneDrive-UniversityofBristol/PhD/Main Project/ACEs to Symptoms Chapter/ALSPAC G0/Results/Alternative MI/PMS/Weighted_Analysis_log", replace

*PMS outcome
foreach exp in phys_abuse sex_abuse emot_abuse emot_negl substance viol_prnt prnt_mh separatd ace_four {
	mi estimate, or post: logistic pms i.`exp' [pw=ipw_pms] if has_pms==1
	mi estimate, or post: logistic pms i.`exp' parentedu parentsclass ethnicity mat_age [pw=ipw_pms] if has_pms==1
	mi estimate, or post: logistic pms i.`exp' parentedu parentsclass ethnicity mat_age menarche [pw=ipw_pms] if has_pms==1
}
	
*One p value for ace_four and continuous 	
di "CATEGORICAL WITH ONE P VALUE:"
quietly mi estimate, or post: logistic pms i.ace_four [pw=ipw_pms] if has_pms==1
di "Crude"
testparm i.ace_four
quietly mi estimate, or post: logistic pms i.ace_four parentedu parentsclass ethnicity mat_age [pw=ipw_pms] if has_pms==1
di "Model 2"
testparm i.ace_four
quietly mi estimate, or post: logistic pms i.ace_four parentedu parentsclass ethnicity mat_age menarche [pw=ipw_pms] if has_pms==1
di "Model 3"
testparm i.ace_four
di "CONTINUOUS"
mi estimate, or post: logistic pms ace_four [pw=ipw_pms] if has_pms==1
mi estimate, or post: logistic pms ace_four parentedu parentsclass ethnicity mat_age [pw=ipw_pms] if has_pms==1
mi estimate, or post: logistic pms ace_four parentedu parentsclass ethnicity mat_age menarche [pw=ipw_pms] if has_pms==1

	
log close
