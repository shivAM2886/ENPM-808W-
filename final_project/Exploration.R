
# Check out the Dataset
#

library(e1071)


rm(list=ls())

## Set Up Working Directory
DATA_LOCATION <- "/Users/Pende/Desktop/us-perm-visas"
setwd(DATA_LOCATION)

#dataF <- read.csv('us_perm_visas.csv') # Load initial LARGE data set
dataF <- read.csv('us_perm_visas_UPDATED.csv') # Load updated Dataset with some fixed variables (afer output of InitialCleanUp.py)
#dataF <- read.csv('small_Sample.csv')
#dataF <- read.csv('small_Sample_UPDATED.csv')
prop.table(summary(dataF$case_status))
length(dataF)

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



# THE REST OF THIS WAS MOSTLY FOR THE MIDTERM ========================================================

length(which("" != dataF$job_info_work_postal_code))/length(dataF$job_info_work_postal_code)
head(dataF$job_info_work_postal_code, 5)

length(which("" != dataF$case_status))
accepted <- (length(which("Certified" == dataF$case_status)) + length(which("Certified-Expired" == dataF$case_status)))
(length(which("Certified" == dataF$case_status)) + length(which("Certified-Expired" == dataF$case_status)))/length(dataF$case_status)
denied <- (length(which("Denied" == dataF$case_status)) + length(which("Withdrawn" == dataF$case_status)))
(length(which("Denied" == dataF$case_status)) + length(which("Withdrawn" == dataF$case_status)))/length(dataF$case_status)


length(unique(tolower(dataF$job_info_major)))
unique(dataF$job_info_major)

#dataF_Temp <- data.frame(dataF$case_status, dataF$employer_postal_code, dataF$decision_date, dataF$case_received_date, )

# Create smaller dataSet for US VISA APPLICANTS (only keep Certified/Certified-expired and Denied)
fieldsToKeep = c("case_no", "case_status", "agent_state", "case_received_date_EPOCH", "class_of_admission", "country_of_citizenship", "employer_state")
keepIdx_Case_Number <- which("" != dataF$case_number)
keepIdx_case_Status <- which("Withdrawn" != dataF$case_status)
# ("Certified-Expired" == dataF$case_status), ("Denied" == dataF$case_status)
keepIdx_agent_state <- which(dataF$agent_state != '')
keepIdx_case_received_date <- which(dataF$case_received_date_EPOCH != '')
keepIdx_ClassAdmission <- which(dataF$class_of_admission != '')
keepIdx_CountryOfCitizenship <- which(dataF$country_of_citizenship != '')
keepIdx_employerState <- which(dataF$employer_state != '')

keepIdx = 1:nrow(dataF)
keepIdx = intersect(keepIdx, keepIdx_Case_Number)
keepIdx = intersect(keepIdx, keepIdx_case_Status)
keepIdx = intersect(keepIdx, keepIdx_agent_state)
keepIdx = intersect(keepIdx, keepIdx_case_received_date)
keepIdx = intersect(keepIdx, keepIdx_ClassAdmission)
keepIdx = intersect(keepIdx, keepIdx_CountryOfCitizenship)
keepIdx = intersect(keepIdx, keepIdx_employerState)

dataF_Small = dataF[keepIdx,fieldsToKeep]

dataF_Small$Final <- rep(TRUE, nrow(dataF_Small))
dataF_Small$Final[which(dataF_Small$case_status == "Denied")] <- FALSE
trueIdx <- which(dataF_Small$case_status != "Denied")
falseIdx <- which(dataF_Small$case_status == "Denied")

maxSamples <- min(length(trueIdx), length(falseIdx))

finalTrueIdx_train <- sample(1:length(trueIdx), maxSamples, replace = FALSE, prob = NULL)
finalFalseIdx_train <- sample(1:length(falseIdx), maxSamples, replace = FALSE, prob = NULL)
finalIdx_train <- union(finalTrueIdx_train, finalFalseIdx_train)
trainData <- dataF_Small[finalIdx_train,]

finalTrueIdx_test <- sample(1:length(trueIdx), maxSamples, replace = FALSE, prob = NULL)
finalFalseIdx_test <- sample(1:length(falseIdx), maxSamples, replace = FALSE, prob = NULL)
finalIdx_test <- union(finalTrueIdx_test, finalFalseIdx_test)
testData <- dataF_Small[finalIdx_test,]

# Test out trainign a classifier... it crashes almost everytime for me
svmFit_linear <- svm(Final ~ case_status+agent_state+case_received_date_EPOCH+class_of_admission+employer_state, kernel="linear", data=trainData, type = 'C-classification')
designResponseBool <- predict(svmFit_linear, newdata = dataF_Small)
trueIdx <- which(designResponseBool == TRUE)
falseIdx <- which(designResponseBool == FALSE)
tempFinal <- rep(TRUE, length(designResponseBool))
tempFinal[falseIdx] <- FALSE
designResponseBool <- tempFinal
TP <- sum(designResponseBool == TRUE & dataF_Small$Final == TRUE)
TP_SVM_Linear_Idx <- which(designResponseBool == TRUE & dataF_Small$Final == TRUE)
FN <- sum(designResponseBool == FALSE & dataF_Small$Final == TRUE)
FN_SVM_Linear_Idx <- which(designResponseBool == FALSE & dataF_Small$Final == TRUE)
TN <- sum(designResponseBool == FALSE & dataF_Small$Final == FALSE)
TN_SVM_Linear_Idx <- which(designResponseBool == FALSE & dataF_Small$Final == FALSE)
FP <- sum(designResponseBool == TRUE & dataF_Small$Final == FALSE)
FP_SVM_Linear_Idx <- which(designResponseBool == TRUE & dataF_Small$Final == FALSE)
Accuracy_SVM_Linear <- (TP + TN)/(TP+FN+TN+FP)


######################### USED TO RENAME THE TOP TEN COMPANIES TO SMALLER NAMES #############################
levels(dataF$employer_name)[levels(dataF$employer_name)== "COGNIZANT TECHNOLOGY SOLUTIONS US CORPORATION"] <- "COGNIZANT"
levels(dataF$employer_name)[levels(dataF$employer_name)== "MICROSOFT CORPORATION"] <- "MICROSOFT"
levels(dataF$employer_name)[levels(dataF$employer_name)== "INTEL CORPORATION"] <- "INTEL"
levels(dataF$employer_name)[levels(dataF$employer_name)== "GOOGLE INC."] <- "GOOGLE"
levels(dataF$employer_name)[levels(dataF$employer_name)== "AMAZON CORPORATE LLC"] <- "AMAZON"
levels(dataF$employer_name)[levels(dataF$employer_name)== "ORACLE AMERICA, INC."] <- "ORACLE"
levels(dataF$employer_name)[levels(dataF$employer_name)== "CISCO SYSTEMS, INC."] <- "CISCO"
levels(dataF$employer_name)[levels(dataF$employer_name)== "QUALCOMM TECHNOLOGIES INC."] <- "QUALCOMM"
levels(dataF$employer_name)[levels(dataF$employer_name)== "APPLE INC."] <- "APPLE"
levels(dataF$employer_name)[levels(dataF$employer_name)== "INFOSYS LTD."] <- "INFOSYS"

temp <- prop.table(table(dataF$employer_name))*100
tempSorted <- sort(temp, decreasing=TRUE)
company <- tempSorted[1:10]
company <- names(company)
company <- as.character(company)
idx <- which(dataF$employer_name %in% company)

keepCompany <- tempSorted[1:100] # How many do we want to keep?
length(which(dataF$employer_name %in% (names(keepCompany[1:100])))) # Takes into account this much of the data
levels(dataF$employer_name)[!(levels(dataF$employer_name) %in% names(keepCompany))] <- "OTHER"

# Plot the data per employer name (by denied and accepted
library(ggplot2)
p<-ggplot(data=dataF[idx,], aes(x=employer_name, fill=case_status)) +
  geom_bar(stat="count")
p <- p + labs(title="Case Status By Employer", y="Total", x="Employer Name")
p <- p + coord_flip()
p

# Used to compute the accepted vs. denied ratios per company name
idxCompany <- which(dataF$employer_name == "AMAZON")
idxGood <- which(dataF$case_status == "Certified")
idxGood <- union(which(dataF$case_status == "Certified-Expired"), idxGood)
idxBad <- which(dataF$case_status == "Denied")
length(intersect(idxCompany, idxGood))/length(intersect(idxCompany, idxBad))

# Used to compute the accepted vs. denied ratios per Country
idxCountry <- which(dataF$country_of_citizenship == "UNITED KINGDOM")
idxGood <- which(dataF$case_status == "Certified")
idxGood <- union(which(dataF$case_status == "Certified-Expired"), idxGood)
idxBad <- which(dataF$case_status == "Denied")
length(intersect(idxCountry, idxGood))/length(intersect(idxCountry, idxBad))

# Used to compute the accepted vs. denied ratios per US Economic Sector
idxUSec <- which(dataF$us_economic_sector == "Transportation")
idxGood <- which(dataF$case_status == "Certified")
idxGood <- union(which(dataF$case_status == "Certified-Expired"), idxGood)
idxBad <- which(dataF$case_status == "Denied")
length(intersect(idxUSec, idxGood))/length(intersect(idxUSec, idxBad))
################## USED FOR INITIAL CLASSIFIER TESTS ####################

library(rattle)
rattle()
