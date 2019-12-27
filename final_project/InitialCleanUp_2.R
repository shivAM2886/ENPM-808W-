# This script further cleans the data after the InitialCleanUp.py script

rm(list=ls())

## Set Up Working Directory
DATA_LOCATION <- "/Users/Pende/Desktop/us-perm-visas"
setwd(DATA_LOCATION)

dataF <- read.csv('us_perm_visas_UPDATED.csv') # Load updated Dataset with some fixed variables (afer output of InitialCleanUp.py)
prop.table(summary(dataF$case_status))
length(dataF)

# Keep these fields
fieldsToKeep = c("case_no",
                 "case_status",
                 "agent_state",
                 "case_received_date_EPOCH",
                 "case_received_date_YEAR",
                 "class_of_admission", 
                 "country_of_citizenship", 
                 "employer_name",
                 "employer_state",
                 "application_type",
                 "decision_date_YEAR",
                 "employer_postal_code",
                 "foreign_worker_ownership_interest",
                 "foreign_worker_info_education",
                 "job_info_major",
                 "pw_amount_9089",
                 "pw_unit_of_pay_9089",
                 "us_economic_sector",
                 "wage_offer_from_9089",
                 "wage_offer_unit_of_pay_9089")

dataF = dataF[,fieldsToKeep]
dataF$Accepted = TRUE
# Certified Certified-Expired Denied Withdrawn
idxDenied = which(dataF$case_status == "Denied") # Definitly bad
idxWithdrawn = which(dataF$case_status == "Withdrawn") # Treat these as Bad as well
idxFalse = union(idxDenied, idxWithdrawn)
dataF$Accepted[idxFalse] = FALSE
write.csv(dataF,'us_perm_visas_SMALL.csv', row.names = FALSE)