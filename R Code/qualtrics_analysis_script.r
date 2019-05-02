##Reading in data of test batch, not to be used in actual research as the kinks are worked out.
library(tidyverse)


##Reading in first 50 responses, checking for legitimacy
first_50 = read_csv("batch_1_results.csv")
first_50$Answer.surveycode = as.character(substr(first_50$Answer.surveycode, start = 1, stop = 4))
first_50_merge = inner_join(first_50, survey_200, "Answer.surveycode")

##These ID's appeared twice and will need to be dropped from the data set, unfortunately there was overlap between two numbers
##Drop non-unique values
#A2MKXI4KCRRI7Y 6119
#A1YSYI926BBOHW 6277
##"A1XO6ONCCTBMKW" needs to be dropped due to a failure to properly register his number
#first_50[20,]
first_50_mergeClean = first_50_merge %>% filter(Answer.surveycode != "6119" & Answer.surveycode != "6277" &
                                                WorkerId != "A1XO6ONCCTBMKW")
first_50_mergeClean = first_50_mergeClean %>% filter()

##Second Data Set
next_150 = read_csv("batch_2_results.csv")
next_150$Answer.surveycode = as.character(substr(next_150$Answer.surveycode, start = 1, stop = 4))
next_150_merge = inner_join(next_150, survey_200, "Answer.surveycode")

##Reading in next 150 responses, checking for legitimacy
##Will need to drop worker ID A1CFPKUOCGJIM6, AMG9Y1YLBTKIV, A1XLGIFFGB01EU, A2PXJTMWGUE5DC
next_150_mergeClean = next_150_merge %>% filter(WorkerId != 'A1CFPKUOCGJIM6' & WorkerId != 'AMG9Y1YLBTKIV' & WorkerId != 'A2PXJTMWGUE5DC' &
                                                WorkerId != 'A1XLGIFFGB01EU')

##Merging cleaned data files
finalClean_1 = bind_rows(first_50_mergeClean, next_150_mergeClean)

##Dropping observations that had any previous exposure to the survey, then this data set is complete!
##As well as those that were rejected and were not paying attention
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

finalClean_1 =  finalClean_1 %>% filter(!is.na(ApprovalTime) & is.na(attention) & substrRight(LifetimeApprovalRate, 5) == '(1/1)')

##Third and final 250 responses Processing
last_250 = read_csv("batch_3_results.csv")
last_250$Answer.surveycode = as.character(substr(last_250$Answer.surveycode, start = 1, stop = 7))

##Final Survey data:
survey_Results_Final = read_csv("Survey_Results_final.csv")
colnames(survey_Results_Final)[41] = 'Answer.surveycode'
survey_Results_Final = survey_Results_Final[3:dim(survey_Results_Final)[1],]
##Cleaning out NAs, people who did not complete or were not paying attention.
sum(!is.na(survey_Results_Final$attention))
survey_Results_Final = survey_Results_Final %>% filter(!is.na(Answer.surveycode))

##Merging the data
last_250_merge = inner_join(last_250, survey_Results_Final, "Answer.surveycode")
last_250_mergeClean = last_250_merge %>% filter(substrRight(LifetimeApprovalRate, 5) == '(1/1)' & is.na(attention))


finalClean = bind_rows(finalClean_1, last_250_mergeClean)

##Filtering those that did not take more than 8 seconds on the first question for robustness
#finalClean = finalClean %>% filter(as.numeric(`QTime_Page Submit`) > 15.0)
##Space for loading additional data sets

##Isolating different treatment options
AWNA = finalClean$AWNA
AWNA = AWNA[!is.na(AWNA)]
PLNA = finalClean$PLNA
PLNA = PLNA[!is.na(PLNA)]
AWA = finalClean$AWA
AWA = AWA[!is.na(AWA)]
PLA = finalClean$PLA
PLA = PLA[!is.na(PLA)]

##Statistics of Interest
##bootstrapped assessment
successes = c()
lengths = c()
##AWNA
successes = c(successes, sum(substr(AWNA, start = 1, stop = 4) == 'Inve'))
lengths = c(lengths, length(AWNA))
# ##AWA
successes = c(successes, sum(substr(AWA, start = 1, stop = 4) == 'Inve'))
lengths = c(lengths, length(AWA))
##PLNA
successes = c(successes, sum(substr(PLNA, start = 1, stop = 4) == 'Inve'))
lengths = c(lengths, length(PLNA))
##PLA
successes = c(successes, sum(substr(PLA, start = 1, stop = 4) == 'Inve'))
lengths = c(lengths, length(PLA))

barplot(successes / lengths, ylab = "Investment Rate", xlab = "Treatment Option", main = "'Investment' Frequency Distribution",
        names.arg = c("AWNA", "AWA", "PLNA", "PLA"))

prop.test(successes, lengths)

##Bootstrap difference between PLNA and PLA
set.seed(3242019)
bootResult = c()
for (i in 1:10000) {
  bootResult = c(bootResult,
                 sum(substr(sample(x = AWNA, size = length(PLNA), replace = TRUE), start = 1, stop = 4) == 'Inve') /
                     length(AWNA) >
                 sum(substr(sample(x = AWA, size = length(PLA), replace = TRUE), start = 1, stop = 4) == 'Inve') /
                     length(AWA)   
                   )
}

##Statistic
1 - sum(!bootResult) / 10000

###Combining the similiar treatments:
nonAmb = c(AWNA, PLNA)
amb = c(AWA, PLA)

set.seed(3242019)
bootResult = c()
for (i in 1:10000) {
  bootResult = c(bootResult,
                 sum(substr(sample(x = nonAmb, size = length(nonAmb), replace = TRUE), start = 1, stop = 4) == 'Inve') /
                   length(AWNA) >
                   sum(substr(sample(x = amb, size = length(amb), replace = TRUE), start = 1, stop = 4) == 'Inve') /
                   length(AWA)   
  )
}

##Statistic
1 - sum(!bootResult) / 10000

###Preparing for Regression Analysis:
##Code a variable for investment, ## 1 is invest, 0 is work
finalClean_reg = finalClean %>% mutate(invest = ifelse(substr(AWNA, start = 1, stop = 4) == 'Inve' |
                                                       substr(AWA, start = 1, stop = 4) == 'Inve' |
                                                       substr(PLNA, start = 1, stop = 4) == 'Inve' |
                                                      substr(PLA, start = 1, stop = 4) == 'Inve', 1, 0))
finalClean_reg$invest[is.na(finalClean_reg$invest)] = 0

##Code a variable for Treatment
finalClean_reg = finalClean_reg %>% mutate(treatment = case_when(!is.na(AWNA) ~ 'AWNA',
                                                                 !is.na(AWA) ~ 'AWA',
                                                                 !is.na(PLNA) ~ 'PLNA',
                                                                 !is.na(PLA) ~ 'PLA'))

##Logistic regression model
library(stargazer)
finalClean_reg$`QTime_Page Submit` = as.numeric(finalClean_reg$`QTime_Page Submit`)
finalClean_reg$treatment = as.factor(finalClean_reg$treatment)
finalClean_reg = finalClean_reg %>% mutate(interaction = paste(treatment, Sex, sep = ''))

out_1 <- lm(invest ~ treatment, data = finalClean_reg)
summary(out_1)
out_2 <- lm(invest ~ treatment + Sex, data = finalClean_reg)
summary(out_2)
#out_4 <- lm(invest ~ interaction, data = finalClean_reg)
#summary(out_4)
out_3 <- lm(invest ~ treatment + Sex + lifestyle + Employment + HHI, data = finalClean_reg)
summary(out_3)

stargazer(out_1, out_2, out_3, dep.var.caption = "Investment Rate", dep.var.labels = rep("", 3),
          omit = c("lifestyle", "Employment", 'HHI'),  add.lines = list(
  c("Controls", "No", "No", "Yes")
))

##Gender distribution by treatment
male = c()
lengths = c()

AWNA = finalClean$AWNA
AWNA_Sex = finalClean$Sex[!is.na(AWNA)]
male = c(male, sum(AWNA_Sex == 'Male'))
lengths = c(lengths, length(AWNA_Sex))

PLNA = finalClean$PLNA
PLNA_Sex = finalClean$Sex[!is.na(PLNA)]
male = c(male, sum(PLNA_Sex == 'Male'))
lengths = c(lengths, length(PLNA_Sex))

AWA = finalClean$AWA
AWA_Sex = finalClean$Sex[!is.na(AWA)]
male = c(male, sum(AWA_Sex == 'Male'))
lengths = c(lengths, length(AWA_Sex))

PLA = finalClean$PLA
PLA_Sex = finalClean$Sex[!is.na(PLA)]
male = c(male, sum(PLA_Sex == 'Male'))
lengths = c(lengths, length(PLA_Sex))

barplot(male / lengths, ylab = "Percentage of Treatment - Male", xlab = "Treatment Option", main = "Male Percentage Distribution",
        names.arg = c("AWNA", "AWA", "PLNA", "PLA"))
####################################################################################################################
#############################THIS CONCLUDES ACTUAL EMPIRICAL ANALYSIS###############################################
##Function which automates the payout process!
invest = function(treatment) {
  decision = runif(n = 1, 0, 1) > 0.5
    
  if (treatment == 'AWNA') {
    if (decision) {
      return("12.00")
    } else {
      return("10.00")
    }
    
  } else if (treatment  == 'AWA') {
    return(toString(round(rnorm(1, 11, 0.40), digits = 2)))
   
  } else if (treatment == 'PLNA') {
    if (decision) {
      return("13.00")
    } else {
      return("9.00")
    }
    
  } else if (treatment == 'PLA') {
    return(toString(sample(round(c(rnorm(5, 9, 0.5), rnorm(15, 11.66, 0.5)), digits = 2), 20)[1]))
    
  }
}

###Compiling worker bonuses!
##Loading test batch results
trial_batch = read_csv("trial_50_batch_results.csv")
trial_batch$Answer.surveycode = substr(as.character(trial_batch$Answer.surveycode), start = 1, stop = 4)
#Getting survey data for test people
survey_trial = read_csv("trial_survey_results.csv")
survey_trial = survey_trial[3:dim(survey_trial)[1],]

##Cleaning Survey Results, must have finished the survey, must not be survey preview,
##and must not have failed the attention question
survey_trial = survey_trial %>% filter(Status != "Survey Preview" & Finished != "False")
colnames(survey_trial)[37] = 'Answer.surveycode'
survey_trial$Answer.surveycode = substr(as.character(survey_trial$Answer.surveycode), start = 1, stop = 4)
##Merging the trial results together
trial_batch = left_join(trial_batch, survey_trial, by = "Answer.surveycode")

##Determining the payout for workers who won using appropriate random process
##Trial batch
trial_batch = trial_batch %>% filter(is.na(lose))
##Work payments
trial_batch = trial_batch %>% mutate(payment = ifelse(!is.na(win_work), "10.00", "0"))
##Investment payments
trial_batch = trial_batch %>% mutate(payment = ifelse(!is.na(win_invest) & !is.na(AWNA), invest("AWNA"), payment)) %>%
                mutate(payment = ifelse(!is.na(win_invest) & !is.na(AWA), invest("AWA"), payment)) %>%
                mutate(payment = ifelse(!is.na(win_invest) & !is.na(PLNA), invest("PLNA"), payment)) %>%
                mutate(payment = ifelse(!is.na(win_invest) & !is.na(PLA), invest("PLA"), payment))

##Final_Clean Payout Allocation
finalClean_cash = finalClean %>% filter(is.na(lose))  
##Work payments
finalClean_cash = finalClean_cash %>% mutate(payment = ifelse(!is.na(win_work), "10.00", "0"))

##Investment payments
finalClean_cash = finalClean_cash %>% mutate(payment = ifelse(!is.na(win_invest) & !is.na(AWNA), "AWNA", payment)) %>%
  mutate(payment = ifelse(!is.na(win_invest) & !is.na(AWA), "AWA", payment)) %>%
  mutate(payment = ifelse(!is.na(win_invest) & !is.na(PLNA), "PLNA", payment)) %>%
  mutate(payment = ifelse(!is.na(win_invest) & !is.na(PLA), "PLA", payment))

for (i in 1:length(finalClean_cash$payment)) {
  if (nchar(finalClean_cash[i, 71]) < 5) {
    finalClean_cash[i, 71] = invest(finalClean_cash[i, 71])
  }
}

##Building payment vectors
workerIDs = c(trial_batch$WorkerId, finalClean_cash$WorkerId)
assignmentIDs = c(trial_batch$AssignmentId, finalClean_cash$AssignmentId)
payments = c(trial_batch$payment, finalClean_cash$payment)
sum(as.numeric(payments))
reasonsGiven = rep(paste("Thank you for participating in our study. As noted at the end of the study, you were randomly selected to have your investment/work ",
              "decision payed out. This is that payment.", sep = ""), length(workerIDs))

##Using MTurkR API to pay workers super easily
##Setting System variables for account
Sys.setenv(AWS_ACCESS_KEY_ID = "AKIAJ3UXBXXHIQLEBI5A",
           AWS_SECRET_ACCESS_KEY = "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")
library(MTurkR)
write_csv(finalClean_cash, "BonusPaymentPlan.csv")
write_csv(trial_batch, "BonusPaymentPlan2.csv")
#####Bonus Payment dispatchment was completed using the MTurk Python API
###Final Demographic Balance Analysis
##Getting column denoting treatment
finalCleanGraph = finalClean %>% mutate(treatment = ifelse(!is.na(AWNA), "AWNA", NA))
finalCleanGraph = finalCleanGraph %>% mutate(treatment = ifelse(!is.na(AWA), "AWA", treatment))
finalCleanGraph = finalCleanGraph %>% mutate(treatment = ifelse(!is.na(PLA), "PLA", treatment))
finalCleanGraph = finalCleanGraph %>% mutate(treatment = ifelse(!is.na(PLNA), "PLNA", treatment))

##Grouping by treatment and get percentage distribution

##Function which takes in a variable, a relevant data set, and a label.
##Writes out a .csv file labeled with the designated label. File sumarizes distribution of demographic data across treatments
library(tidyverse)
demAnalyzer = function(vector, data, basePath, label) {
  ##Format vector as factor, get levels
  vector = as.factor(vector)
  levels = levels(vector)
  levLength = length(levels)
  ##Initilize dataframe
  curLev = levels[1]
  demData = data %>% group_by(treatment) %>% summarize(temp = mean(!! sym(label) == curLev))
  colnames(demData)[2] = curLev
  demData[,2] = signif(demData[,2], digits = 2)
  #print(demData[,2])
  ##Iterate through level array, beefing out data frame
  for (i in 2:levLength) {
    curLev = levels[i]
    #print(curLev)
    curData = data %>% group_by(treatment) %>% summarize(temp = mean(!! sym(label) == curLev))
    colnames(curData)[2] = curLev
    #print(curData)
    demData = bind_cols(demData, signif(curData[,2], digits = 2))
  }
  ##build file path, write frame to .csv file
  filePath = paste(basePath, label, ".csv", sep = "")
  write_csv(x = demData, path = filePath)
  return("All Set!")
}

##Using the function demAnalyzer to generate cross-treatment summary statistics!
basePath = "C:/Users/goodm/Desktop/UPenn/UPenn Senior Year/ECON 300/data/demographics/"
##Age
demAnalyzer(vector = finalCleanGraph$Age, data = finalCleanGraph, basePath = basePath, label = "Age")
##Income
demAnalyzer(vector = finalCleanGraph$HHI, data  = finalCleanGraph, basePath = basePath, label = "HHI")
##Industry
demAnalyzer(vector = finalCleanGraph$Industry, data  = finalCleanGraph, basePath = basePath, label = "Industry")
##MTurk Freq, MTurk hours, MTurk need
demAnalyzer(vector = finalCleanGraph$per_week, data  = finalCleanGraph, basePath = basePath, label = "per_week")
demAnalyzer(vector = finalCleanGraph$time_spent, data  = finalCleanGraph, basePath = basePath, label = "time_spent")
demAnalyzer(vector = finalCleanGraph$lifestyle, data  = finalCleanGraph, basePath = basePath, label = "lifestyle")
##Schooling
demAnalyzer(vector = finalCleanGraph$School, data  = finalCleanGraph, basePath = basePath, label = "School")
##Country
demAnalyzer(vector = finalCleanGraph$country, data  = finalCleanGraph, basePath = basePath, label = "country")
##Employment
demAnalyzer(vector = finalCleanGraph$Employment, data  = finalCleanGraph, basePath = basePath, label = "Employment")
##This concludes data manipulation and analysis completed for my Senior Thesis!