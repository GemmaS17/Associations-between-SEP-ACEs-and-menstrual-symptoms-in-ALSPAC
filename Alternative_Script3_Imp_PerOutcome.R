###Alternative multiple imputation/inverse probability weighting approach

# From Script 3 (STANDARD) ----

rm(list=ls())#clear global R environment

## Load files and packages ----
#  change to location where input files are stored:
loc_inp= '/Volumes/Studies/ALSPAC Menstrual PhD/'
#   change to location output should be stored (make sure this folder exists):
loc_out= '/Volumes/Studies/ALSPAC Menstrual PhD/'
setwd(loc_out)

#1. id used in previous script, also added to new output here
timeperiod<-c(0,10)
fileid_out<-paste0('alspacKids_ACE_',timeperiod[1],'_',timeperiod[2])
#2. load data
#a. excel file with description variables
adv_description<-data.frame(readxl::read_excel(
  paste0(loc_inp,'SI_data1_ACE_definitions_overtime.xlsx'),
  sheet = 'ACE variables',col_names=TRUE))
#b. Exact spelling for name of the data file for imputation 
#load(paste0(loc_inp,fileid_out,"_imputation.RData")) #THIS DOESN'T RUN (No such file or directory)
install.packages("haven")
library(haven)
#alspacKids_ACE_data_2018=read.dta("ALSPAC_mi_SLCMA_v12.dta") 
#alspacKids_ACE_data_2018=read_dta("alspacKids_ACE_0_10ALL2.dta") ##Added this in
load(file = "alspacKids_ACE_0_10.RData")
#3 Packages
install.packages("mice")
install.packages("foreign")
install.packages("readxl")
install.packages("matrixStats")
install.packages("tableone")
#library("foreign", "readxl", "matrixStats", "tableone")
library(foreign)
#install.packages("readxl")
library(readxl)
#install.packages("matrixStats")
library(matrixStats)
#install.packages("tableone")
library(tableone)
#library('foreign' ,'readxl' ,'matrixStats' ,'tableone')
library("mice")
library("foreign")
library("readxl")
library("matrixStats")
library("tableone")
library(dplyr)
#source("http://bioconductor.org/biocLite.R")
#for(pkg in packages){
# if(!require(pkg,character.only=T)){
# biocLite(pkg,suppressUpdates=TRUE)
#library(pkg,character.only=T)

#rm(pkg,packages)

##Universal dropping / recoding  ----
#Drop ppts with missing data on certain variables that are collinear in imputaiton procedure and can't be imputed
#Ethnicity 
table(alspacKids_ACE_data_2018$ethnicity_org, useNA = "ifany") #count - 151 NA (obs should go down to 5416)
alspacKids_ACE_data_2018<-alspacKids_ACE_data_2018[!is.na(alspacKids_ACE_data_2018$ethnicity_org), ]
#Findiff IPW
table(alspacKids_ACE_data_2018$findiff_ipw_org, useNA = "ifany") #count - 149 NA (obs should go down to 5267)
alspacKids_ACE_data_2018<-alspacKids_ACE_data_2018[!is.na(alspacKids_ACE_data_2018$findiff_ipw_org), ]
#FAI?
table(alspacKids_ACE_data_2018$fai_long_org, useNA = "ifany") #count - 14 NA (obs should go down to 5253)
alspacKids_ACE_data_2018<-alspacKids_ACE_data_2018[!is.na(alspacKids_ACE_data_2018$fai_long_org), ]

# Frequency (N)
# Include "useNA = "ifany"" to see the number of missing values
table(alspacKids_ACE_data_2018$ethnicity_org, useNA = "ifany")

#Need to remove observations with less than half IPW vars (8 or more NAs); down to 5245
ipw_vars<-c("mat_age_org", "phone_org", "car_org", "housing_org", "crowding_org", "dbl_glaze_org", "first_preg_org", "smoke_preg_org", "smoke_ever_org",
            "mated_org", "findiff_ipw_org", "sclass_org", "bfed_dur_org", "parity_org", "epds_org", "fai_long_org")
alspacKids_ACE_data_2018<-alspacKids_ACE_data_2018[rowSums(is.na(alspacKids_ACE_data_2018[ipw_vars])) < 8, ]
#Also need to remove sensitivity variables that shouldn't be involved in imputaiton procedure at all (23 vars to drop; should go down to 281 variables)
alspacKids_ACE_data_2018 = subset(alspacKids_ACE_data_2018, select = -c(marital_stat_org, rooms_org, mated_org.1, mat_ethnicity_org, menarche_org, pated_org, mat_sclass_org, pat_sclass_org, 
                                                                        pain_cont_org, heavy_cont_org, days_bin_cont_org, length_cont_org, irreg_cont_org, pms_cont_org, pain_tsm_org, heavy_tsm_org, days_bin_tsm_org, 
                                                                        length_tsm_org, irreg_tsm_org, pms_tsm_org, pain_doc_org, heavy_doc_org, heavy_days_cat_org) )

#Also some collinear variables (BF originals) that meant the first test imputation did not converge - remove these as well (11 to drop; 270 variables)
alspacKids_ACE_data_2018 = subset(alspacKids_ACE_data_2018, select = -c(b032_org, c600_org, pb260_org, t3255_org, t5510_org, fa5510_org, matsmok_tri1_org, matsmok_tri2_org,
                                                                        matsmok_tri3_c_org, matsmok_tri3_e_org, t5412_org) )

#Assumes parity is an numerical value but is actually ordered categorical - change
alspacKids_ACE_data_2018$parity_org <- factor(alspacKids_ACE_data_2018$parity_org, levels = c(0, 1, 2), labels = c("No previous children", "1 previous child", "2 or more previous children"), ordered = TRUE)
str(alspacKids_ACE_data_2018$parity_org)

#PUB SAMPLE ----

##Drop ppts missing outcome data AND missing any IPW variables (expect 4691)
pub_alspacKids_ACE_data_2018 <- alspacKids_ACE_data_2018 %>%
  filter(!(has_pub_org == "Data missing" & rowSums(is.na(select(., all_of(ipw_vars)))) > 0))
##Drop variables relating to the other outcomes 
pub_alspacKids_ACE_data_2018 = subset(pub_alspacKids_ACE_data_2018, select = -c(has_length_org, length_both_org, has_irreg_org, irreg_org, has_pms_org, pms_bin_org) )

catf <- function(..., file=paste0(loc_out,fileid_out,'_imputation_log_pub.txt'), append=TRUE){
  cat(..., file=file, append=append)
}

## Determine variables for imputation----

catf('\n\n',paste0(Sys.time()),
     'Code block 2: Determine variables for imputation\n')

imp_standard<-grep('org',names(pub_alspacKids_ACE_data_2018) ,value=T)

#For imputation to work, auxiliary ACE variables need to have at least 50 people in all levels.
#check for factor variables
table(!sapply(pub_alspacKids_ACE_data_2018[,imp_standard],function(x) is.numeric(x)|is.integer(x)))
imp_factor<-
  imp_standard[!sapply(pub_alspacKids_ACE_data_2018[,imp_standard],function(x) is.numeric(x)|is.integer(x))] 
catf('\nThere are',length(imp_standard),'imputation variables,',
     'of which',length(imp_factor),'are categorical.\n')
#i. identify ACE imputation variables with less than 50 people in any factor level:
imp_ACE <- imp_factor[gsub('_org', '', imp_factor) %in% adv_description$variable_unique]
imp_ACE_less_n50 <- imp_ACE[sapply(pub_alspacKids_ACE_data_2018[, imp_ACE], function(x) any(table(factor(x)) < 50))] 

catf('\n', length(imp_ACE_less_n50), 'of the', length(imp_ACE), 'categorical ACE variables,',
     'have <50 people in all factor levels in either boys or girls.\n')

# Identify which ACE imputation variables fulfill criteria for binary version
imp_ACE_use_binary <- imp_ACE_less_n50[
  colSums(pub_alspacKids_ACE_data_2018[, gsub('_org', '', imp_ACE_less_n50)], na.rm = TRUE) >= 50
]


#iii. If applicable remove imp_ACE_less_n50 in both original and binary version
rm_imp<-imp_ACE_less_n50[!imp_ACE_less_n50%in%imp_ACE_use_binary]
length(imp_standard)
catf(length(rm_imp),'of these',length(imp_ACE_less_n50),'variables need to be removed')
if(length(rm_imp)>0){imp_standard<-imp_standard[!imp_standard%in%rm_imp]}
length(imp_standard)

#iv. If applicable, make sure binary version is used imp_ACE_use_binary
sum(grepl('_org',imp_standard))
catf(', but',length(imp_ACE_use_binary),'variables can be included in their dichotomised version:\n',
     imp_ACE_use_binary,'\n')
if(length(imp_ACE_use_binary)>0){
  imp_standard[imp_standard%in%imp_ACE_use_binary]<-gsub('_org','',imp_standard[imp_standard%in%imp_ACE_use_binary])}
sum(grepl('_org',imp_standard))

catf('\nSo the total number of imputation variables is',length(imp_standard),'of which',
     sum(grepl('_org',imp_standard)),'are included in their original non-dichotomised format.\n')

#clean up
rm(imp_ACE,imp_ACE_less_n50,imp_ACE_use_binary,imp_factor,rm_imp,ipw_vars)

##Predictor matrix ----

catf('\n\n',paste0(Sys.time()),
     'Code block 3: Create predictor matrix\n')
ACEmeasures<-#ACEs in correct order
  c("ACEscore_classic", "ACEcat_classic",
    "physical_abuse", "sexual_abuse", "emotional_abuse", "emotional_neglect", 
    "bullying","violence_between_parents", "substance_household", 
    "mental_health_problems_or_suicide","parent_convicted_offence",
    "parental_separation", 
    "ACEscore_extended", "ACEcat_extended",
    "social_class", "financial_difficulties", "neighbourhood", 
    "social_support_child", "social_support_parent", 
    "violence_between_child_and_partner","physical_illness_child", 
    "physical_illness_parent","parent_child_bond")
ACEs<-ACEmeasures[c(3:12,15:23)]
classicACEs<-ACEs[1:10]#ACEs in correct order

ACEmeasures<-
  grep(paste0(ACEmeasures,collapse='|'),names(pub_alspacKids_ACE_data_2018),value=T)
ACEs<-
  grep(paste0(ACEs,collapse='|'),names(pub_alspacKids_ACE_data_2018),value=T)
classicACEs<-
  grep(paste0(classicACEs,collapse='|'),names(pub_alspacKids_ACE_data_2018),value=T)

#Extract default predicator matrix for imputation (everything used, matrix filled with 1)
require(mice)
table(unique(c(imp_standard,ACEmeasures))%in%names(pub_alspacKids_ACE_data_2018))
ini <- mice(pub_alspacKids_ACE_data_2018[
  ,unique(c(imp_standard,ACEmeasures))], maxit=0, pri=F)

#GS addition - change method
ini[["method"]][["parity_org"]]<-"polr"
ini[["method"]][["epds_org"]]<-"midastouch"

#Get predictor matrix
pred_mat <- ini$pred#prediction matrix
pred_mat[,grep('aln',colnames(pred_mat))] <- 0
pred_mat[grep('aln',rownames(pred_mat)),] <- 0
pred_mat[,grep('qlet',colnames(pred_mat))] <- 0
pred_mat[grep('qlet',rownames(pred_mat)),] <- 0
meth <- ini$meth#formula to be used
length(meth);dim(pred_mat)
#keep gender variable in but do not use for imputation (want to use in modelling but nothing else)
pred_mat[,"kz021_org"] <- 0
meth["kz021_org"] <- ""
length(meth);dim(pred_mat)
## removing this var coz model won't work due to 0 obs in one of the levels of this var
#pred_mat[,"a525_org"] <- 0
#meth["a525_org"] <- ""
length(meth);dim(pred_mat)
#Want to passively impute calculation sumscores
#BUT not use for imputation others so:
pred_mat[,grep('^ACE',colnames(pred_mat))] <- 0
pred_mat[grep('^ACE',rownames(pred_mat)),] <- 0
length(meth);dim(pred_mat)
#passive imputation for each score
meth[grep('ACEscore_extended_',names(meth),value=T)] <- 
  paste0('~I(',paste0('as.integer(',ACEs,')',collapse = '+'),')')
#meth[grep('ACEcat_extended_',names(meth),value=T)] <-
#  paste0('~I(',paste0('as.integer(',ACEs,')',collapse = '+'),')')
meth[grep('ACEscore_classic_',names(meth),value=T)] <- 
  paste0('~I(',paste0('as.integer(',classicACEs,')',collapse = '+'),')')
#meth[grep('ACEcat_classic_',names(meth),value=T)] <-
#  paste0('~I(',paste0('as.integer(',classicACEs,')',collapse = '+'),')')
#post-process to create categorical ACE variables
#post <- ini$post
#post[grep('ACEcat_extended_',names(post),value=T)] <- 
#  "imp[[j]][, i] <- cut(imp[[j]][, i], breaks=c(-1,1,2,5,22),labels=c('0_1low','2lomid','3_5midhi','6+high'))"
#post[grep('ACEcat_classic_',names(post),value=T)] <- 
#  "imp[[j]][, i] <- cut(imp[[j]][, i], breaks=c(-1,0,1,3,22),labels=c('0','1','2_3','4+'))"
length(meth);dim(pred_mat)
table(rowSums(pred_mat))#nr predictors per variable
names(which(rowSums(pred_mat)==0))#complete variables

#Empty method for outcome(s) to avoid imputing it
ini[["method"]][["pain_both_org"]]<-""
ini[["method"]][["heavy_both_org"]]<-""
ini[["method"]][["days_both_org"]]<-""

meth["pain_both_org"] <- ""
meth["heavy_both_org"] <- ""
meth["days_both_org"] <- ""

## Save everything needed for imputation
save (fileid_out,timeperiod,catf,meth,pred_mat,ini,pub_alspacKids_ACE_data_2018, 
      file=paste0(loc_out, fileid_out, '_imputation_data.RData'))
## Save stata version
#Save
write.dta(pub_alspacKids_ACE_data_2018, 
          paste0(loc_out, fileid_out, ".dta"))

catf('\n\n',paste0(Sys.time()),
     'Code block 4: Imputation-RECOMMEND HPC\n')

#load(file = "alspacKids_ACE_0_10_imputation_data.RData") #read in if needed


##Imputation ----
require(mice)

## function that ensures 'dat' and 'pred_mat' have matching columns
## and changes all logicals to factors as logicals with missing values
## generate errors in the 'complete' function when include=TRUE
construct.dat.for.mice <- function(dat, pred_mat) { 
  idx <- match(rownames(pred_mat),names(dat))
  dat <- dat[,idx]
  for (i in which(sapply(dat, is.logical))) {
    dat[[i]] <- as.factor(dat[[i]])
  }
  dat
}

## Trial run to see if there are no convergence issues, running 1 imputation & 1 iteration
dat <- construct.dat.for.mice(pub_alspacKids_ACE_data_2018, pred_mat)

#Trial run - "Warning messages: Number of logged events: 60" 
imp_test<-mice(dat, m = 1,maxit=1,print=TRUE, method=meth,
               predictorMatrix=pred_mat, stringsAsFactor = TRUE, seed=4343728)

## Do the imputation.Recommend doing 50 imputations with 30 iterations 

m <- 50 #50 imputations
maxit <- 30 #30 iterations

dat <- construct.dat.for.mice(pub_alspacKids_ACE_data_2018, pred_mat)
imp_all <- mice(dat, m = m,maxit=maxit,print=TRUE, method=meth,
                predictorMatrix=pred_mat, stringsAsFactor = TRUE, seed=140817)


##Combine genders
#imp_all<-rbind(imp_boys,imp_girls)
save(imp_all,file=paste0(loc_inp,'pub_imp_all_ACE_0_10.RData'))

com_all <- complete(imp_all, "long", include = TRUE)
table(is.na(com_all$ACEcat_extended_0_10))
nrow(com_all[com_all$.imp==0,]);colSums(is.na(com_all[com_all$.imp==0,]))
#needed? write.csv(com_all,file=paste0(loc_inp,'com_all.csv'))

## Save data in STATA
library(foreign)
write.dta(com_all,
          file=paste0(loc_out,fileid_out,"ALSPAC_pub.dta"))

#LENGTH SAMPLE ----
##Drop ppts missing outcome data AND missing any IPW variables (expect 4480)
length_alspacKids_ACE_data_2018 <- alspacKids_ACE_data_2018 %>%
  filter(!(has_length_org == "Data missing" & rowSums(is.na(select(., all_of(ipw_vars)))) > 0))
##Drop variables relating to the other outcomes 
length_alspacKids_ACE_data_2018 = subset(length_alspacKids_ACE_data_2018, select = -c(has_pub_org, pain_both_org, heavy_both_org, days_both_org,
                                                                                      has_irreg_org, irreg_org, has_pms_org, pms_bin_org) )

catf <- function(..., file=paste0(loc_out,fileid_out,'_imputation_log_length.txt'), append=TRUE){
  cat(..., file=file, append=append)
}

## Determine variables for imputation----

catf('\n\n',paste0(Sys.time()),
     'Code block 2: Determine variables for imputation\n')

imp_standard<-grep('org',names(length_alspacKids_ACE_data_2018) ,value=T)

#For imputation to work, auxiliary ACE variables need to have at least 50 people in all levels.
#check for factor variables
table(!sapply(length_alspacKids_ACE_data_2018[,imp_standard],function(x) is.numeric(x)|is.integer(x)))
imp_factor<-
  imp_standard[!sapply(length_alspacKids_ACE_data_2018[,imp_standard],function(x) is.numeric(x)|is.integer(x))] 
catf('\nThere are',length(imp_standard),'imputation variables,',
     'of which',length(imp_factor),'are categorical.\n')
#i. identify ACE imputation variables with less than 50 people in any factor level:
imp_ACE <- imp_factor[gsub('_org', '', imp_factor) %in% adv_description$variable_unique]
imp_ACE_less_n50 <- imp_ACE[sapply(length_alspacKids_ACE_data_2018[, imp_ACE], function(x) any(table(factor(x)) < 50))] 

catf('\n', length(imp_ACE_less_n50), 'of the', length(imp_ACE), 'categorical ACE variables,',
     'have <50 people in all factor levels in either boys or girls.\n')

# Identify which ACE imputation variables fulfill criteria for binary version
imp_ACE_use_binary <- imp_ACE_less_n50[
  colSums(length_alspacKids_ACE_data_2018[, gsub('_org', '', imp_ACE_less_n50)], na.rm = TRUE) >= 50
]


#iii. If applicable remove imp_ACE_less_n50 in both original and binary version
rm_imp<-imp_ACE_less_n50[!imp_ACE_less_n50%in%imp_ACE_use_binary]
length(imp_standard)
catf(length(rm_imp),'of these',length(imp_ACE_less_n50),'variables need to be removed')
if(length(rm_imp)>0){imp_standard<-imp_standard[!imp_standard%in%rm_imp]}
length(imp_standard)

#iv. If applicable, make sure binary version is used imp_ACE_use_binary
sum(grepl('_org',imp_standard))
catf(', but',length(imp_ACE_use_binary),'variables can be included in their dichotomised version:\n',
     imp_ACE_use_binary,'\n')
if(length(imp_ACE_use_binary)>0){
  imp_standard[imp_standard%in%imp_ACE_use_binary]<-gsub('_org','',imp_standard[imp_standard%in%imp_ACE_use_binary])}
sum(grepl('_org',imp_standard))

catf('\nSo the total number of imputation variables is',length(imp_standard),'of which',
     sum(grepl('_org',imp_standard)),'are included in their original non-dichotomised format.\n')

#clean up
rm(imp_ACE,imp_ACE_less_n50,imp_ACE_use_binary,imp_factor,rm_imp,ipw_vars)

##Predictor matrix ----

catf('\n\n',paste0(Sys.time()),
     'Code block 3: Create predictor matrix\n')
ACEmeasures<-#ACEs in correct order
  c("ACEscore_classic", "ACEcat_classic",
    "physical_abuse", "sexual_abuse", "emotional_abuse", "emotional_neglect", 
    "bullying","violence_between_parents", "substance_household", 
    "mental_health_problems_or_suicide","parent_convicted_offence",
    "parental_separation", 
    "ACEscore_extended", "ACEcat_extended",
    "social_class", "financial_difficulties", "neighbourhood", 
    "social_support_child", "social_support_parent", 
    "violence_between_child_and_partner","physical_illness_child", 
    "physical_illness_parent","parent_child_bond")
ACEs<-ACEmeasures[c(3:12,15:23)]
classicACEs<-ACEs[1:10]#ACEs in correct order

ACEmeasures<-
  grep(paste0(ACEmeasures,collapse='|'),names(length_alspacKids_ACE_data_2018),value=T)
ACEs<-
  grep(paste0(ACEs,collapse='|'),names(length_alspacKids_ACE_data_2018),value=T)
classicACEs<-
  grep(paste0(classicACEs,collapse='|'),names(length_alspacKids_ACE_data_2018),value=T)

#Extract default predicator matrix for imputation (everything used, matrix filled with 1)
require(mice)
table(unique(c(imp_standard,ACEmeasures))%in%names(length_alspacKids_ACE_data_2018))
ini <- mice(length_alspacKids_ACE_data_2018[
  ,unique(c(imp_standard,ACEmeasures))], maxit=0, pri=F)

#GS addition - change method
ini[["method"]][["parity_org"]]<-"polr"
ini[["method"]][["epds_org"]]<-"midastouch"

#Get predictor matrix
pred_mat <- ini$pred#prediction matrix
pred_mat[,grep('aln',colnames(pred_mat))] <- 0
pred_mat[grep('aln',rownames(pred_mat)),] <- 0
pred_mat[,grep('qlet',colnames(pred_mat))] <- 0
pred_mat[grep('qlet',rownames(pred_mat)),] <- 0
meth <- ini$meth#formula to be used
length(meth);dim(pred_mat)
#keep gender variable in but do not use for imputation (want to use in modelling but nothing else)
pred_mat[,"kz021_org"] <- 0
meth["kz021_org"] <- ""
length(meth);dim(pred_mat)
## removing this var coz model won't work due to 0 obs in one of the levels of this var
#pred_mat[,"a525_org"] <- 0
#meth["a525_org"] <- ""
length(meth);dim(pred_mat)
#Want to passively impute calculation sumscores
#BUT not use for imputation others so:
pred_mat[,grep('^ACE',colnames(pred_mat))] <- 0
pred_mat[grep('^ACE',rownames(pred_mat)),] <- 0
length(meth);dim(pred_mat)
#passive imputation for each score
meth[grep('ACEscore_extended_',names(meth),value=T)] <- 
  paste0('~I(',paste0('as.integer(',ACEs,')',collapse = '+'),')')
#meth[grep('ACEcat_extended_',names(meth),value=T)] <-
#  paste0('~I(',paste0('as.integer(',ACEs,')',collapse = '+'),')')
meth[grep('ACEscore_classic_',names(meth),value=T)] <- 
  paste0('~I(',paste0('as.integer(',classicACEs,')',collapse = '+'),')')
#meth[grep('ACEcat_classic_',names(meth),value=T)] <-
#  paste0('~I(',paste0('as.integer(',classicACEs,')',collapse = '+'),')')
#post-process to create categorical ACE variables
#post <- ini$post
#post[grep('ACEcat_extended_',names(post),value=T)] <- 
#  "imp[[j]][, i] <- cut(imp[[j]][, i], breaks=c(-1,1,2,5,22),labels=c('0_1low','2lomid','3_5midhi','6+high'))"
#post[grep('ACEcat_classic_',names(post),value=T)] <- 
#  "imp[[j]][, i] <- cut(imp[[j]][, i], breaks=c(-1,0,1,3,22),labels=c('0','1','2_3','4+'))"
length(meth);dim(pred_mat)
table(rowSums(pred_mat))#nr predictors per variable
names(which(rowSums(pred_mat)==0))#complete variables

#Empty method for outcome(s) to avoid imputing it
ini[["method"]][["length_both_org"]]<-""
meth["length_both_org"] <- ""

## Save everything needed for imputation
save (fileid_out,timeperiod,catf,meth,pred_mat,ini,length_alspacKids_ACE_data_2018, 
      file=paste0(loc_out, fileid_out, '_imputation_data.RData'))
## Save stata version
#Save
write.dta(length_alspacKids_ACE_data_2018, 
          paste0(loc_out, fileid_out, ".dta"))

catf('\n\n',paste0(Sys.time()),
     'Code block 4: Imputation-RECOMMEND HPC\n')

#load(file = "alspacKids_ACE_0_10_imputation_data.RData") #read in if needed


##Imputation ----
require(mice)

## function that ensures 'dat' and 'pred_mat' have matching columns
## and changes all logicals to factors as logicals with missing values
## generate errors in the 'complete' function when include=TRUE
construct.dat.for.mice <- function(dat, pred_mat) { 
  idx <- match(rownames(pred_mat),names(dat))
  dat <- dat[,idx]
  for (i in which(sapply(dat, is.logical))) {
    dat[[i]] <- as.factor(dat[[i]])
  }
  dat
}

## Trial run to see if there are no convergence issues, running 1 imputation & 1 iteration
dat <- construct.dat.for.mice(length_alspacKids_ACE_data_2018, pred_mat)

#Trial run - "Warning messages: Number of logged events: 60" 
imp_test<-mice(dat, m = 1,maxit=1,print=TRUE, method=meth,
               predictorMatrix=pred_mat, stringsAsFactor = TRUE, seed=4343728)

## Do the imputation.Recommend doing 50 imputations with 30 iterations 

m <- 50 #50 imputations
maxit <- 30 #30 iterations

dat <- construct.dat.for.mice(length_alspacKids_ACE_data_2018, pred_mat)
imp_all <- mice(dat, m = m,maxit=maxit,print=TRUE, method=meth,
                predictorMatrix=pred_mat, stringsAsFactor = TRUE, seed=140817)


##Combine genders
#imp_all<-rbind(imp_boys,imp_girls)
save(imp_all,file=paste0(loc_inp,'length_imp_all_ACE_0_10.RData'))

com_all <- complete(imp_all, "long", include = TRUE)
table(is.na(com_all$ACEcat_extended_0_10))
nrow(com_all[com_all$.imp==0,]);colSums(is.na(com_all[com_all$.imp==0,]))
#needed? write.csv(com_all,file=paste0(loc_inp,'com_all.csv'))

## Save data in STATA
write.dta(com_all,
          file=paste0(loc_out,fileid_out,"ALSPAC_length.dta"))

#IRREG SAMPLE ----
##Drop ppts missing outcome data AND missing any IPW variables (expect 4588)
irreg_alspacKids_ACE_data_2018 <- alspacKids_ACE_data_2018 %>%
  filter(!(has_irreg_org == "Data missing" & rowSums(is.na(select(., all_of(ipw_vars)))) > 0))
##Drop variables relating to the other outcomes 
irreg_alspacKids_ACE_data_2018 = subset(irreg_alspacKids_ACE_data_2018, select = -c(has_pub_org, pain_both_org, heavy_both_org, days_both_org,
                                                                                      has_length_org, length_both_org, has_pms_org, pms_bin_org) )

catf <- function(..., file=paste0(loc_out,fileid_out,'_imputation_log_irreg.txt'), append=TRUE){
  cat(..., file=file, append=append)
}

## Determine variables for imputation----

catf('\n\n',paste0(Sys.time()),
     'Code block 2: Determine variables for imputation\n')

imp_standard<-grep('org',names(irreg_alspacKids_ACE_data_2018) ,value=T)

#For imputation to work, auxiliary ACE variables need to have at least 50 people in all levels.
#check for factor variables
table(!sapply(irreg_alspacKids_ACE_data_2018[,imp_standard],function(x) is.numeric(x)|is.integer(x)))
imp_factor<-
  imp_standard[!sapply(irreg_alspacKids_ACE_data_2018[,imp_standard],function(x) is.numeric(x)|is.integer(x))] 
catf('\nThere are',length(imp_standard),'imputation variables,',
     'of which',length(imp_factor),'are categorical.\n')
#i. identify ACE imputation variables with less than 50 people in any factor level:
imp_ACE <- imp_factor[gsub('_org', '', imp_factor) %in% adv_description$variable_unique]
imp_ACE_less_n50 <- imp_ACE[sapply(irreg_alspacKids_ACE_data_2018[, imp_ACE], function(x) any(table(factor(x)) < 50))] 

catf('\n', length(imp_ACE_less_n50), 'of the', length(imp_ACE), 'categorical ACE variables,',
     'have <50 people in all factor levels in either boys or girls.\n')

# Identify which ACE imputation variables fulfill criteria for binary version
imp_ACE_use_binary <- imp_ACE_less_n50[
  colSums(irreg_alspacKids_ACE_data_2018[, gsub('_org', '', imp_ACE_less_n50)], na.rm = TRUE) >= 50
]


#iii. If applicable remove imp_ACE_less_n50 in both original and binary version
rm_imp<-imp_ACE_less_n50[!imp_ACE_less_n50%in%imp_ACE_use_binary]
length(imp_standard)
catf(length(rm_imp),'of these',length(imp_ACE_less_n50),'variables need to be removed')
if(length(rm_imp)>0){imp_standard<-imp_standard[!imp_standard%in%rm_imp]}
length(imp_standard)

#iv. If applicable, make sure binary version is used imp_ACE_use_binary
sum(grepl('_org',imp_standard))
catf(', but',length(imp_ACE_use_binary),'variables can be included in their dichotomised version:\n',
     imp_ACE_use_binary,'\n')
if(length(imp_ACE_use_binary)>0){
  imp_standard[imp_standard%in%imp_ACE_use_binary]<-gsub('_org','',imp_standard[imp_standard%in%imp_ACE_use_binary])}
sum(grepl('_org',imp_standard))

catf('\nSo the total number of imputation variables is',length(imp_standard),'of which',
     sum(grepl('_org',imp_standard)),'are included in their original non-dichotomised format.\n')

#clean up
rm(imp_ACE,imp_ACE_less_n50,imp_ACE_use_binary,imp_factor,rm_imp,ipw_vars)

##Predictor matrix ----

catf('\n\n',paste0(Sys.time()),
     'Code block 3: Create predictor matrix\n')
ACEmeasures<-#ACEs in correct order
  c("ACEscore_classic", "ACEcat_classic",
    "physical_abuse", "sexual_abuse", "emotional_abuse", "emotional_neglect", 
    "bullying","violence_between_parents", "substance_household", 
    "mental_health_problems_or_suicide","parent_convicted_offence",
    "parental_separation", 
    "ACEscore_extended", "ACEcat_extended",
    "social_class", "financial_difficulties", "neighbourhood", 
    "social_support_child", "social_support_parent", 
    "violence_between_child_and_partner","physical_illness_child", 
    "physical_illness_parent","parent_child_bond")
ACEs<-ACEmeasures[c(3:12,15:23)]
classicACEs<-ACEs[1:10]#ACEs in correct order

ACEmeasures<-
  grep(paste0(ACEmeasures,collapse='|'),names(irreg_alspacKids_ACE_data_2018),value=T)
ACEs<-
  grep(paste0(ACEs,collapse='|'),names(irreg_alspacKids_ACE_data_2018),value=T)
classicACEs<-
  grep(paste0(classicACEs,collapse='|'),names(irreg_alspacKids_ACE_data_2018),value=T)

#Extract default predicator matrix for imputation (everything used, matrix filled with 1)
require(mice)
table(unique(c(imp_standard,ACEmeasures))%in%names(irreg_alspacKids_ACE_data_2018))
ini <- mice(irreg_alspacKids_ACE_data_2018[
  ,unique(c(imp_standard,ACEmeasures))], maxit=0, pri=F)

#GS addition - change method
ini[["method"]][["parity_org"]]<-"polr"
ini[["method"]][["epds_org"]]<-"midastouch"

#Get predictor matrix
pred_mat <- ini$pred#prediction matrix
pred_mat[,grep('aln',colnames(pred_mat))] <- 0
pred_mat[grep('aln',rownames(pred_mat)),] <- 0
pred_mat[,grep('qlet',colnames(pred_mat))] <- 0
pred_mat[grep('qlet',rownames(pred_mat)),] <- 0
meth <- ini$meth#formula to be used
length(meth);dim(pred_mat)
#keep gender variable in but do not use for imputation (want to use in modelling but nothing else)
pred_mat[,"kz021_org"] <- 0
meth["kz021_org"] <- ""
length(meth);dim(pred_mat)
## removing this var coz model won't work due to 0 obs in one of the levels of this var
#pred_mat[,"a525_org"] <- 0
#meth["a525_org"] <- ""
length(meth);dim(pred_mat)
#Want to passively impute calculation sumscores
#BUT not use for imputation others so:
pred_mat[,grep('^ACE',colnames(pred_mat))] <- 0
pred_mat[grep('^ACE',rownames(pred_mat)),] <- 0
length(meth);dim(pred_mat)
#passive imputation for each score
meth[grep('ACEscore_extended_',names(meth),value=T)] <- 
  paste0('~I(',paste0('as.integer(',ACEs,')',collapse = '+'),')')
#meth[grep('ACEcat_extended_',names(meth),value=T)] <-
#  paste0('~I(',paste0('as.integer(',ACEs,')',collapse = '+'),')')
meth[grep('ACEscore_classic_',names(meth),value=T)] <- 
  paste0('~I(',paste0('as.integer(',classicACEs,')',collapse = '+'),')')
#meth[grep('ACEcat_classic_',names(meth),value=T)] <-
#  paste0('~I(',paste0('as.integer(',classicACEs,')',collapse = '+'),')')
#post-process to create categorical ACE variables
#post <- ini$post
#post[grep('ACEcat_extended_',names(post),value=T)] <- 
#  "imp[[j]][, i] <- cut(imp[[j]][, i], breaks=c(-1,1,2,5,22),labels=c('0_1low','2lomid','3_5midhi','6+high'))"
#post[grep('ACEcat_classic_',names(post),value=T)] <- 
#  "imp[[j]][, i] <- cut(imp[[j]][, i], breaks=c(-1,0,1,3,22),labels=c('0','1','2_3','4+'))"
length(meth);dim(pred_mat)
table(rowSums(pred_mat))#nr predictors per variable
names(which(rowSums(pred_mat)==0))#complete variables

#Empty method for outcome(s) to avoid imputing it
ini[["method"]][["irreg_org"]]<-""
meth["irreg_org"] <- ""

## Save everything needed for imputation
#save (fileid_out,timeperiod,catf,meth,pred_mat,ini,irreg_alspacKids_ACE_data_2018, 
 #     file=paste0(loc_out, fileid_out, '_imputation_data.RData'))
## Save stata version
#Save
#write.dta(irreg_alspacKids_ACE_data_2018, 
 #         paste0(loc_out, fileid_out, ".dta"))

catf('\n\n',paste0(Sys.time()),
     'Code block 4: Imputation-RECOMMEND HPC\n')

#load(file = "alspacKids_ACE_0_10_imputation_data.RData") #read in if needed


##Imputation ----
require(mice)

## function that ensures 'dat' and 'pred_mat' have matching columns
## and changes all logicals to factors as logicals with missing values
## generate errors in the 'complete' function when include=TRUE
construct.dat.for.mice <- function(dat, pred_mat) { 
  idx <- match(rownames(pred_mat),names(dat))
  dat <- dat[,idx]
  for (i in which(sapply(dat, is.logical))) {
    dat[[i]] <- as.factor(dat[[i]])
  }
  dat
}

## Trial run to see if there are no convergence issues, running 1 imputation & 1 iteration
dat <- construct.dat.for.mice(irreg_alspacKids_ACE_data_2018, pred_mat)

#Trial run - "Warning messages: Number of logged events: 60" 
imp_test<-mice(dat, m = 1,maxit=1,print=TRUE, method=meth,
               predictorMatrix=pred_mat, stringsAsFactor = TRUE, seed=4343728)

## Do the imputation.Recommend doing 50 imputations with 30 iterations 

m <- 50 #50 imputations
maxit <- 30 #30 iterations

dat <- construct.dat.for.mice(irreg_alspacKids_ACE_data_2018, pred_mat)
imp_all <- mice(dat, m = m,maxit=maxit,print=TRUE, method=meth,
                predictorMatrix=pred_mat, stringsAsFactor = TRUE, seed=140817)


##Combine genders
#imp_all<-rbind(imp_boys,imp_girls)
save(imp_all,file=paste0(loc_inp,'irreg_imp_all_ACE_0_10.RData'))

com_all <- complete(imp_all, "long", include = TRUE)
table(is.na(com_all$ACEcat_extended_0_10))
nrow(com_all[com_all$.imp==0,]);colSums(is.na(com_all[com_all$.imp==0,]))
#needed? write.csv(com_all,file=paste0(loc_inp,'com_all.csv'))

## Save data in STATA
write.dta(com_all,
          file=paste0(loc_out,fileid_out,"ALSPAC_irreg.dta"))

#PMS SAMPLE ----
##Drop ppts missing outcome data AND missing any IPW variables (expect 4464)
pms_alspacKids_ACE_data_2018 <- alspacKids_ACE_data_2018 %>%
  filter(!(has_pms_org == "Data missing" & rowSums(is.na(select(., all_of(ipw_vars)))) > 0))
##Drop variables relating to the other outcomes 
pms_alspacKids_ACE_data_2018 = subset(pms_alspacKids_ACE_data_2018, select = -c(has_pub_org, pain_both_org, heavy_both_org, days_both_org,
                                                                                    has_length_org, length_both_org, has_irreg_org, irreg_org) )

catf <- function(..., file=paste0(loc_out,fileid_out,'_imputation_log_pms.txt'), append=TRUE){
  cat(..., file=file, append=append)
}

## Determine variables for imputation----

catf('\n\n',paste0(Sys.time()),
     'Code block 2: Determine variables for imputation\n')

imp_standard<-grep('org',names(pms_alspacKids_ACE_data_2018) ,value=T)

#For imputation to work, auxiliary ACE variables need to have at least 50 people in all levels.
#check for factor variables
table(!sapply(pms_alspacKids_ACE_data_2018[,imp_standard],function(x) is.numeric(x)|is.integer(x)))
imp_factor<-
  imp_standard[!sapply(pms_alspacKids_ACE_data_2018[,imp_standard],function(x) is.numeric(x)|is.integer(x))] 
catf('\nThere are',length(imp_standard),'imputation variables,',
     'of which',length(imp_factor),'are categorical.\n')
#i. identify ACE imputation variables with less than 50 people in any factor level:
imp_ACE <- imp_factor[gsub('_org', '', imp_factor) %in% adv_description$variable_unique]
imp_ACE_less_n50 <- imp_ACE[sapply(pms_alspacKids_ACE_data_2018[, imp_ACE], function(x) any(table(factor(x)) < 50))] 

catf('\n', length(imp_ACE_less_n50), 'of the', length(imp_ACE), 'categorical ACE variables,',
     'have <50 people in all factor levels in either boys or girls.\n')

# Identify which ACE imputation variables fulfill criteria for binary version
imp_ACE_use_binary <- imp_ACE_less_n50[
  colSums(pms_alspacKids_ACE_data_2018[, gsub('_org', '', imp_ACE_less_n50)], na.rm = TRUE) >= 50
]


#iii. If applicable remove imp_ACE_less_n50 in both original and binary version
rm_imp<-imp_ACE_less_n50[!imp_ACE_less_n50%in%imp_ACE_use_binary]
length(imp_standard)
catf(length(rm_imp),'of these',length(imp_ACE_less_n50),'variables need to be removed')
if(length(rm_imp)>0){imp_standard<-imp_standard[!imp_standard%in%rm_imp]}
length(imp_standard)

#iv. If applicable, make sure binary version is used imp_ACE_use_binary
sum(grepl('_org',imp_standard))
catf(', but',length(imp_ACE_use_binary),'variables can be included in their dichotomised version:\n',
     imp_ACE_use_binary,'\n')
if(length(imp_ACE_use_binary)>0){
  imp_standard[imp_standard%in%imp_ACE_use_binary]<-gsub('_org','',imp_standard[imp_standard%in%imp_ACE_use_binary])}
sum(grepl('_org',imp_standard))

catf('\nSo the total number of imputation variables is',length(imp_standard),'of which',
     sum(grepl('_org',imp_standard)),'are included in their original non-dichotomised format.\n')

#clean up
rm(imp_ACE,imp_ACE_less_n50,imp_ACE_use_binary,imp_factor,rm_imp,ipw_vars)

##Predictor matrix ----

catf('\n\n',paste0(Sys.time()),
     'Code block 3: Create predictor matrix\n')
ACEmeasures<-#ACEs in correct order
  c("ACEscore_classic", "ACEcat_classic",
    "physical_abuse", "sexual_abuse", "emotional_abuse", "emotional_neglect", 
    "bullying","violence_between_parents", "substance_household", 
    "mental_health_problems_or_suicide","parent_convicted_offence",
    "parental_separation", 
    "ACEscore_extended", "ACEcat_extended",
    "social_class", "financial_difficulties", "neighbourhood", 
    "social_support_child", "social_support_parent", 
    "violence_between_child_and_partner","physical_illness_child", 
    "physical_illness_parent","parent_child_bond")
ACEs<-ACEmeasures[c(3:12,15:23)]
classicACEs<-ACEs[1:10]#ACEs in correct order

ACEmeasures<-
  grep(paste0(ACEmeasures,collapse='|'),names(pms_alspacKids_ACE_data_2018),value=T)
ACEs<-
  grep(paste0(ACEs,collapse='|'),names(pms_alspacKids_ACE_data_2018),value=T)
classicACEs<-
  grep(paste0(classicACEs,collapse='|'),names(pms_alspacKids_ACE_data_2018),value=T)

#Extract default predicator matrix for imputation (everything used, matrix filled with 1)
require(mice)
table(unique(c(imp_standard,ACEmeasures))%in%names(pms_alspacKids_ACE_data_2018))
ini <- mice(pms_alspacKids_ACE_data_2018[
  ,unique(c(imp_standard,ACEmeasures))], maxit=0, pri=F)

#GS addition - change method
ini[["method"]][["parity_org"]]<-"polr"
ini[["method"]][["epds_org"]]<-"midastouch"

#Get predictor matrix
pred_mat <- ini$pred#prediction matrix
pred_mat[,grep('aln',colnames(pred_mat))] <- 0
pred_mat[grep('aln',rownames(pred_mat)),] <- 0
pred_mat[,grep('qlet',colnames(pred_mat))] <- 0
pred_mat[grep('qlet',rownames(pred_mat)),] <- 0
meth <- ini$meth#formula to be used
length(meth);dim(pred_mat)
#keep gender variable in but do not use for imputation (want to use in modelling but nothing else)
pred_mat[,"kz021_org"] <- 0
meth["kz021_org"] <- ""
length(meth);dim(pred_mat)
## removing this var coz model won't work due to 0 obs in one of the levels of this var
#pred_mat[,"a525_org"] <- 0
#meth["a525_org"] <- ""
length(meth);dim(pred_mat)
#Want to passively impute calculation sumscores
#BUT not use for imputation others so:
pred_mat[,grep('^ACE',colnames(pred_mat))] <- 0
pred_mat[grep('^ACE',rownames(pred_mat)),] <- 0
length(meth);dim(pred_mat)
#passive imputation for each score
meth[grep('ACEscore_extended_',names(meth),value=T)] <- 
  paste0('~I(',paste0('as.integer(',ACEs,')',collapse = '+'),')')
#meth[grep('ACEcat_extended_',names(meth),value=T)] <-
#  paste0('~I(',paste0('as.integer(',ACEs,')',collapse = '+'),')')
meth[grep('ACEscore_classic_',names(meth),value=T)] <- 
  paste0('~I(',paste0('as.integer(',classicACEs,')',collapse = '+'),')')
#meth[grep('ACEcat_classic_',names(meth),value=T)] <-
#  paste0('~I(',paste0('as.integer(',classicACEs,')',collapse = '+'),')')
#post-process to create categorical ACE variables
#post <- ini$post
#post[grep('ACEcat_extended_',names(post),value=T)] <- 
#  "imp[[j]][, i] <- cut(imp[[j]][, i], breaks=c(-1,1,2,5,22),labels=c('0_1low','2lomid','3_5midhi','6+high'))"
#post[grep('ACEcat_classic_',names(post),value=T)] <- 
#  "imp[[j]][, i] <- cut(imp[[j]][, i], breaks=c(-1,0,1,3,22),labels=c('0','1','2_3','4+'))"
length(meth);dim(pred_mat)
table(rowSums(pred_mat))#nr predictors per variable
names(which(rowSums(pred_mat)==0))#complete variables

#Empty method for outcome(s) to avoid imputing it
ini[["method"]][["pms_bin_org"]]<-""
meth["pms_bin_org"] <- ""

## Save everything needed for imputation
#save (fileid_out,timeperiod,catf,meth,pred_mat,ini,pms_alspacKids_ACE_data_2018, 
#      file=paste0(loc_out, fileid_out, '_imputation_data.RData'))
## Save stata version
#Save
#write.dta(pms_alspacKids_ACE_data_2018, 
#          paste0(loc_out, fileid_out, ".dta"))

catf('\n\n',paste0(Sys.time()),
     'Code block 4: Imputation-RECOMMEND HPC\n')

#load(file = "alspacKids_ACE_0_10_imputation_data.RData") #read in if needed


##Imputation ----
require(mice)

## function that ensures 'dat' and 'pred_mat' have matching columns
## and changes all logicals to factors as logicals with missing values
## generate errors in the 'complete' function when include=TRUE
construct.dat.for.mice <- function(dat, pred_mat) { 
  idx <- match(rownames(pred_mat),names(dat))
  dat <- dat[,idx]
  for (i in which(sapply(dat, is.logical))) {
    dat[[i]] <- as.factor(dat[[i]])
  }
  dat
}

## Trial run to see if there are no convergence issues, running 1 imputation & 1 iteration
dat <- construct.dat.for.mice(pms_alspacKids_ACE_data_2018, pred_mat)

#Trial run - "Warning messages: Number of logged events: 60" 
imp_test<-mice(dat, m = 1,maxit=1,print=TRUE, method=meth,
               predictorMatrix=pred_mat, stringsAsFactor = TRUE, seed=4343728)

## Do the imputation.Recommend doing 50 imputations with 30 iterations 

m <- 50 #50 imputations
maxit <- 30 #30 iterations

dat <- construct.dat.for.mice(pms_alspacKids_ACE_data_2018, pred_mat)
imp_all <- mice(dat, m = m,maxit=maxit,print=TRUE, method=meth,
                predictorMatrix=pred_mat, stringsAsFactor = TRUE, seed=140817)


##Combine genders
#imp_all<-rbind(imp_boys,imp_girls)
save(imp_all,file=paste0(loc_inp,'pms_imp_all_ACE_0_10.RData'))

com_all <- complete(imp_all, "long", include = TRUE)
table(is.na(com_all$ACEcat_extended_0_10))
nrow(com_all[com_all$.imp==0,]);colSums(is.na(com_all[com_all$.imp==0,]))
#needed? write.csv(com_all,file=paste0(loc_inp,'com_all.csv'))

## Save data in STATA
write.dta(com_all,
          file=paste0(loc_out,fileid_out,"ALSPAC_pms.dta"))


