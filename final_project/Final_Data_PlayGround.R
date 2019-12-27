library(e1071)
library(varhandle)

rm(list=ls())

## Set Up Working Directory
DATA_LOCATION <- "/Users/Pende/Desktop/us-perm-visas"
setwd(DATA_LOCATION)

dataF <- read.csv('us_perm_visas_SMALL.csv')

fullFields = c("case_no",
                 "case_status",
                 "agent_state",
                 "case_received_date_EPOCH",
                 "case_received_date_YEAR",
                 "class_of_admission", 
                 "country_of_citizenship", 
                 "employer_state",
                 "application_type",
                 "decision_date",
                 "employer_postal_code",
                 "foreign_worker_ownership_interest",
                 "foreign_worker_info_education",
                 "job_info_major",
                 "pw_amount_9089",
                 "pw_unit_of_pay_9089",
                 "us_economic_sector",
                 "wage_offer_from_9089",
                 "wage_offer_unit_of_pay_9089",
                 "Accepted")

fields1 <- c("case_no",
             "Accepted",
             "wage_offer_from_9089",
             "wage_offer_unit_of_pay_9089")


cleanDataSet <- function(df, nameVec) {
  keepIdx = 1:nrow(df)
  for(ii in 1:length(nameVec))
  {
    if(nameVec[ii] == 'pw_amount_9089' || nameVec[ii] == 'wage_offer_from_9089') {
      # Only let numbers through
      keepIdx = intersect(keepIdx, which(check.numeric(df[,nameVec[ii]]) & df[,nameVec[ii]] != ''))
    }
    #else if(nameVec[ii] == 'case_status') {
    #  keepIdx = intersect(keepIdx, which(df[,nameVec[ii]] != ''))
    #  keepIdx = intersect(keepIdx, which(df[,nameVec[ii]] != 'Withdrawn'))
    #}
    else {
      keepIdx = intersect(keepIdx, which(df[,nameVec[ii]] != ''))
    }
  }
  
  return(df[keepIdx,nameVec])
}

sampleData <- function(df, sampleNum) {
# sampleNum is the max number of rows per class
# df_sampled <- sampleData(df, 3000)
# so 3000 rows of Accepted and 3000 of Denied
  idxCertified <- which(df$Accepted == TRUE)
  #maxCertified_nonExpired <- which(df$case_status == "Certified")
  #maxCertified_Expired <- which(df$case_status == "Certified-Expired")
  #idxCertified <- union(maxCertified_nonExpired, maxCertified_Expired)
  #idxDenied <- which(df$case_status == "Denied")
  idxDenied <- which(df$Accepted == FALSE)
  
  maxCertified <- length(idxCertified)
  maxDenied <- length(idxDenied)
  
  maxSampleSize <- min(maxCertified, maxDenied)
  
  if(sampleNum > maxSampleSize) {
    message(sprintf("Max Number of Samples Used:\t%d", maxSampleSize))
    sampleNum <- maxSampleSize
  }
  
  finalIdxCertified <- sample(idxCertified, sampleNum, replace = FALSE, prob = NULL)
  finalIdxDenied <- sample(idxDenied, sampleNum, replace = FALSE, prob = NULL)
  
  finalIdx <- union(finalIdxCertified, finalIdxDenied)
  
  return(df[finalIdx,])
  
}

convertPWWage <- function(new, wage, unit) {
# Assumes the data has already been cleaned and no empty data exists for
# the wage and unit fields in the new dataSet
  new$yearly=0;

  new[,wage] <- as.numeric(new[,wage])
  
  idxBi <- new[,unit] =='bi' | new[,unit] == 'Bi-Weekly'
  idxHr <- new[,unit] =='Hour' | new[,unit] == 'hr'
  idxMnth <- new[,unit] =='Month' | new[,unit] =='mth'
  idxWeek <- new[,unit] =='week' | new[,unit] =='wk'
  idxYear <- new[,unit] =='Year' | new[,unit] =='yr'
  
  new$yearly[idxBi] <- 365/14*new[idxBi,wage]
  new$yearly[idxHr] <- 8*365*new[idxHr,wage]
  new$yearly[idxMnth] <- 12*new[idxMnth,wage]
  new$yearly[idxWeek] <- 365/7**new[idxWeek,wage]
  new$yearly[idxYear] <- new[idxYear,wage]
  
  return(new)
}

# Quick Stats:
barplot(prop.table(table(dataF$case_status)))
barplot(prop.table(table(dataF$case_status))*100, names.arg = sort(c(as.character(unique(dataF$case_status)))), main="Case Status", col=c("darkblue"))

idxKeep <- which(dataF$application_type != '');
temp <- dataF$application_type[idxKeep]
barplot(prop.table(table(temp))*100, main="Application Type", col=c("darkblue"))

# ========= CLASSIFIER WORK =================================================================
# Get only fields we care about that are not empty
dataF_1 <- cleanDataSet(dataF, fields1)
# sample the data with 3000 sample points
dataF_1_Sampled <- sampleData(dataF_1, 3000)
dataF_1_Sampled <- convertPWWage(dataF_1_Sampled, "wage_offer_from_9089", "wage_offer_unit_of_pay_9089")

fields2 = c("case_no",
            "case_status",
            #"agent_state",
            "case_received_date_EPOCH",
            #"application_type", 
            "country_of_citizenship", 
            "employer_postal_code",
            "foreign_worker_ownership_interest",
            "foreign_worker_info_education",
            "job_info_major",
            #"pw_amount_9089",
            #"pw_unit_of_pay_9089",
            #"us_economic_sector",
            #"wage_offer_from_9089",
            #"wage_offer_unit_of_pay_9089",
            "Accepted")

dataF_1 <- cleanDataSet(dataF, fields2)
dataF_1$employer_postal_code <- as.numeric(dataF_1$employer_postal_code)
dataF_1_Sampled <- sampleData(dataF_1, 3000)
dataF_1_Sampled <- convertPWWage(dataF_1_Sampled, "wage_offer_from_9089", "wage_offer_unit_of_pay_9089")
dataF_1_Sampled <- convertPWWage(dataF_1_Sampled, "wage_offer_from_9089", "wage_offer_unit_of_pay_9089")

library(rattle)
rattle()
