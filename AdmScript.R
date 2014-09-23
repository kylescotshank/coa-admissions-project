#------------------------
# Kyle Scot Shank
# 9/18/14
#
# The purpose of this code is to perform a logistic regression on data
# obtained from two years worth of records regarding matriculant deposits at College of the Atlantic.
#
# We hope to use the admissions records provided from 2011,2012, and 2013 to see what key variables are present in the data
# which may explain the decision to deposit or not deposit. Then, utilizing the estimated coefficients, we will
# see how the regression equation predicts the outcomes in 2014. All of the individuals recorded
# in this dataset were admitted to College of the Atlantic. 
#
# Since this project is being uploaded to Github and therefore will be in the public domain, all information 
# containing potentially private information (names, ss number, etc.) have been "scrubbed" from he dataset.
#
# Notes on Data Scrubbing:
#
# Original dataset contained 1015 unique entries
# Ages were originally represented as decimals: they've been rounded down to the nearest whole number.
# 1 entry was removed for suspected data entry errors 
# 4 entries were removed for data errors with a_rank and p_rank (no rankings entered)
# 
#
# Thus, our final data set contains 1010 unique entries.
#------------------------

#------------------------
# Preamble
#------------------------

library(car)
library(aod)
library(plyr)

oldData<-read.csv("C:/Users/Kyle Shank/Desktop/SCHOOL/COA/ECONOMETRICS/github project/coa_admit_data_with_ed.csv")

## ----------------------------------------------------------------------
## ----------------------------------------------------------------------
#------------------------
# Summary Statistics
#------------------------
## ----------------------------------------------------------------------
## ----------------------------------------------------------------------

total.admits<-length(oldData$outcome)
total.deposits<-sum(oldData$outcome==1)
percent.total.deposits<-total.deposits/total.admits
total.admits
total.deposits
percent.total.deposits
## ----------------------------------------------------------------------
## Our data set contains 1010 total applicants, of which 358 deposited. 
## This gives us a net deposit rate of 35.44%. we face a question, then: 
## should we keep ED students in our analysis? (i.e.; do all ed students
## deposit?)
## ----------------------------------------------------------------------

total.ed.admits<-sum(oldData$ed==1)
total.ed.deposits<-sum(oldData$ed==1 & oldData$outcome==1)
percent.total.admits<-total.ed.admits/total.admits
percent.total.deposits<-total.ed.deposits/total.deposits
ed.deposit.rate<-total.ed.deposits/total.ed.admits
total.ed.admits
total.ed.deposits
percent.total.admits
percent.total.deposits
ed.deposit.rate

## ----------------------------------------------------------------------
## Of 1010 total admitted students, 96 of these were ED applicants. Of 
## Of the 358 total deposits, 89 of those were from ED students. Thus, ED
## applicants comprise approximately 9.5% of the total admitted students from
## 2011-2013, but comprise nearly 25% of total deposits over the same period of time.
## The deposit rate of ED students is approximately 93%. 
##
## Interesting finding: of our ED admits, the mean and median COA award for
## those that deposited was $19,199 and $2,860, respectively. 
## For those that did not deposit, these values are $22,750 and 0, respectively.
## of the 7 ED admits that did not deposit, six were given award amounts of 0. This
## particular admitted student, id# 132035, was also an international student. 
## ----------------------------------------------------------------------

mean(oldData$age)
median(oldData$age)
sd(oldData$age)
## ----------------------------------------------------------------------
## the average applicant was approximately 19 years old, with a standard
## deviation of 2.26 years
## ----------------------------------------------------------------------

number.freshman.applicants<-sum(oldData$freshman==1)
number.transfer.applicants<-sum(oldData$freshman==0)
percent.freshman.applicants<-number.freshman.applicants/total.admitted
percent.transfer.applicants<-number.transfer.applicants/total.admitted
freshman.deposits<-sum(oldData$freshman==1 & oldData$outcome==1)
percent.freshman.deposits<-freshman.deposits/total.deposits
transfer.deposits<-sum(oldData$freshman==0 & oldData$outcome==1)
percent.transfer.deposits<-1-percent.freshman.deposits
number.freshman.applicants
number.transfer.applicants
percent.freshman.applicants
percent.transfer.applicants
freshman.deposits
transfer.deposits
percent.freshman.deposits
percent.transfer.deposits
## ----------------------------------------------------------------------
## 742 of 913 applicants were freshman, 171 were transfers. This
## gives us a freshman and applicant percentage of 81% and 19%, respectively.
## Of our 292 deposits, 190 of those were from freshman applicants.
## Thus, of our total deposits, approximately 65% of those were from freshman
## and approximately 35% were from transfer applicants. Since 190 of 742 freshman deposited,
## this gives us a deposit rate of approximately 26%. Since 102 of 171 transfer applicants
## deposited, this gives us a deposit rate of nearly 60%.
## ----------------------------------------------------------------------

females.admitted<-sum(oldData$female==1)
percent.females.admitted<-females.admitted/total.total.admitted
females.deposited<-sum(oldData$female==1 & oldData$outcome==1)
percent.females.deposited<-females.deposited/total.deposits
females.admitted
percent.females.admitted
females.deposited
percent.females.deposited
## ----------------------------------------------------------------------
## Of our 913 records, 636 of the admitted students were female, comprising roughly
## 70% of he entire admitted student body. 
## Of 292 deposits, 200 of those recorded were female, which shows that roughly
## 68% of deposits came from female admitted students. Of 636 admitted female students,
## 200 deposited, giving us a deposit rate of 31.44%. This is slightly lower than the
## deposit rate for males (92 deposited, 277 admitted), which is 33.2%
##
## ----------------------------------------------------------------------

whites.admitted<-sum(oldData$white==1,na.rm=TRUE)
nonwhites.admitted<-sum(oldData$white==0, na.rm=TRUE)
percent.whites.admitted<-whites.admitted/total.admitted
whites.deposited<-sum(oldData$white==1 & oldData$outcome==1,na.rm=TRUE)
nonwhites.deposited<-sum(oldData$white==0 & oldData$outcome==1, na.rm=TRUE)
percent.whites.deposited<-whites.deposited/total.deposits
missing.race<-sum(is.na(oldData$white))
whites.admitted
nonwhites.admitted
percent.whites.admitted
whites.deposited
percent.whites.deposited
missing.race
## ----------------------------------------------------------------------
## Of our 913, 648 of the admitted students self-identified as being caucasian.
## This means that nearly 71% of our admitted students were white. 
## Of 292 deposits, 222 of those recorded were from people identifying as white, meaning
## roughly 76% of the depositing students were white. This means that
## roughly 34% (222/648) of admitted white students will deposit. This also holds true for admitted students
## that do not identify as white (124) of which 43 deposited, for a deposit rateof 34.67%.
## NOTE: We are missing racial data for 141 admitted students. Of those 141 admitted,
## 27 deposited, giving a "no-race-given" deposit rate of 19%. 
## ----------------------------------------------------------------------

intl.admitted<-sum(oldData$intl==1)
percent.intl.admitted<-intl.admitted/total.admitted
intl.deposits<-sum(oldData$intl==1 & oldData$outcome==1)
percent.intl.deposits<-intl.deposits/total.deposits
intl.admitted
percent.intl.admitted
intl.deposits
percent.intl.deposits
## ----------------------------------------------------------------------
## Of our 913, 79 of the admitted students were international students,
## meaning approximately 9% of our admitted students were international.
## Of 292 deposits, 28 were international students, giving us a deposit ratio
## of nearly 10%. This means that roughly 35% (28/79) of international students that are admitted, deposit.
## ----------------------------------------------------------------------

married.admitted<-sum(oldData$married==1, na.rm=TRUE)
percent.married.admitted<-married.admitted/total.admitted
married.deposits<-sum(oldData$married==1 & oldData$outcome==1, na.rm=TRUE)
percent.married.deposits<-married.deposits/total.deposits
married.deposit.rate<-married.deposits/married.admitted
missing.married<-sum(is.na(oldData$married))
married.admitted
percent.married.admitted
married.deposits
percent.married.deposits
## ----------------------------------------------------------------------
## Of 913 admitted students, 600 had parents who were married, for a total
## of approximately 66% of admitted students. Of students who deposited, 
## 181 had parents that were married, comprising ~62% of depositing students.
## This gives us a deposit ratio of 30%. This is slightly worse than the deposit ratio
## for students who do not have married parents (302 admitted / 107 accepted = 35.4% deposit ratio).
## ----------------------------------------------------------------------

hsgpa.admitted<-sum(!is.na(oldData$hsgpa))
no.hsgpa.admitted<-sum(is.na(oldData$hsgpa))
percent.hsgpa.admitted<-hsgpa.admitted/total.admitted
percent.no.hsgpa.admitted<-no.hsgpa.admitted/total.admitted
hsgpa.deposits<-sum(!is.na(oldData$hsgpa) & oldData$outcome==1)
no.hsgpa.deposits<-sum(is.na(oldData$hsgpa)& oldData$outcome==1)
percent.hsgpa.deposits<-hsgpa.deposits/total.deposits
percent.no.hsgpa.deposits<-no.hsgpa.deposits/total.deposits

hsgpa.admitted
corrected.hsgpa<-oldData$hsgpa[-714]
## This is because there is one entry which has a non-standard GPA (94.92). 
## We've suppressed this entry for calculated mean, median, and sd.
mean(corrected.hsgpa, na.rm=TRUE)
median(corrected.hsgpa, na.rm=TRUE)
sd(corrected.hsgpa, na.rm=TRUE)
no.hsgpa.admitted
percent.hsgpa.admitted
percent.no.hsgpa.admitted
hsgpa.deposits
no.hsgpa.deposits
percent.hsgpa.deposits
percent.no.hsgpa.deposits
## ----------------------------------------------------------------------
## Of 913 admitted students, 534 submitted high school GPAs for review.
## The mean GPA was 3.65, with a median of 3.7 and a standard deviation of 0.385
## 379 students did not submit GPAs for review. Thus, approximately 58% of our
## admitted students submitted GPAs and 42% did not.
## 158 of the 292 depositing students submitted GPAs, comprising 54% of total deposits.
## 46% of depositing students did not. Thus, roughly 30% of students that submit GPAs
## deposit (158/534). 35% of students that do not submit GPAs and are admitted deposit (134/379)
## ----------------------------------------------------------------------



## ----------------------------------------------------------------------
## Of 
#------------------------
# Run Regression / Summary
#------------------------

glm.out<-glm(outcome ~ freshman+age+female+a_rank+p_rank+
               white+int+log(coaaid),
           family= binomial(logit),data = OldData)
summary(glm.out)

# CI using profiled log-likelihood
confint(glm.out)
# CI using standard errors
confint.default(glm.out)
# wald test
# wald.test(b = coef(glm.out), Sigma = vcov(glm.out), Terms = 4:6)
#
#odds ratios and 95% CI
exp(cbind(OR = coef(glm.out), confint(glm.out)))

# model fit testing
# The difference in deviance between the two models
with(glm.out, null.deviance - deviance)
#The number of predictor variables in the model
with(glm.out, df.null - df.residual)
#And thus the p-value is 
with(glm.out, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))


#------------------------
# PREDICTIVE ACCURACY TESTS
#------------------------

#----------------------------
# PREDICTIVE WITH CV = 0.5
#----------------------------

# Makes a matrix
ymat<-matrix(numeric(0),ncol=2,nrow=length(OldData$outcome))
# Puts the Data OUTCOME into column 1
ymat[,1]<-OldData$outcome
# Puts the predicted outcomes in column 2
prediction<-round(predict(glm.out,OldData,type="response"))
ymat[,2]<-prediction
# Data Deposit Outcome
count1<-count(ymat[,1]==1 & ymat[,2]==1)[2,2]
# Predicted Deposit Outcome
count0<-count(ymat[,1]==0 & ymat[,2]==0)[2,2]
# Data Non-Deposit Outcome
# Accuracy of Yield
yield.accuracy<-(count1+count0)/length(OldData$outcome)
# Accuracy of Deposits
total1<-sum(ymat[,1])
deposits.accuracy<-count1/total1
# Accuracy of Non-Deposits
total0<-count(ymat[,1]==0)[2,2]
nondeposits.accuracy<-count0/total0

# Accuracy of Yield
print(yield.accuracy)
# Accuracy of Deposits
print(deposits.accuracy)
# Accuracy of Non-Deposits
print(nondeposits.accuracy)


#----------------------------------
# PREDICTIVE WITH CV not equal 0.5
#----------------------------------

# Makes a matrix
ymat<-matrix(numeric(0),ncol=2,nrow=length(OldData$outcome))
# Puts the Data OUTCOME into column 1
ymat[,1]<-OldData$outcome
# Puts the predicted outcomes in column 2
prediction<-predict(glm.out,OldData,type="response")
ymat[,2]<-prediction
# resets the predicted outcomes to critical value
ymat[,2]<-ifelse(ymat[,2]>=0.425,1,0)
# Data Deposit Outcome
count1<-count(ymat[,1]==1 & ymat[,2]==1)[2,2]
# Predicted Deposit Outcome
count0<-count(ymat[,1]==0 & ymat[,2]==0)[2,2]
# Data Non-Deposit Outcome
# Accuracy of Yield
yield.accuracy<-(count1+count0)/length(OldData$outcome)
# Accuracy of Deposits
total1<-sum(ymat[,1])
deposits.accuracy<-count1/total1
# Accuracy of Non-Deposits
total0<-count(ymat[,1]==0)[2,2]
nondeposits.accuracy<-count0/total0


# Accuracy of Yield
print(yield.accuracy)
# Accuracy of Deposits
print(deposits.accuracy)
# Accuracy of Non-Deposits
print(nondeposits.accuracy)

#----------------------------------
# Predict for Future Year
#----------------------------------
# Makes a matrix
ymat<-matrix(numeric(0),ncol=2,nrow=length(NewData$age))
colnames(ymat)<-c("id","decision")
# Puts the Student ID into column 1
ymat[,1]<-NewData$id
# Puts the predicted outcomes in column 2
prediction<-predict(glm.out,NewData,type="response")
ymat[,2]<-prediction
# resets the predicted outcomes to critical value
ymat[,2]<-ifelse(ymat[,2]>=0.4,1,0)
write.table(ymat, file = "lastlist.csv", sep = ",")
# Number of Deposits Predicted
count1<-count(ymat[,2]==1)[2,2]
# Number of Non-Deposits Predicted
count0<-count(ymat[,2]==0)[2,2]
# Data Non-Deposit Outcome
# Accuracy of Yield
yield.accuracy<-(count1+count0)/length(NewData$age)
# Accuracy of Deposits
total1<-sum(ymat[,1])
deposits.accuracy<-count1/total1
# Accuracy of Non-Deposits
total0<-count(ymat[,1]==0)[2,2]
nondeposits.accuracy<-count0/total0

# Accuracy of Yield
print(yield.accuracy)
# Accuracy of Deposits
print(deposits.accuracy)
# Accuracy of Non-Deposits
print(nondeposits.accuracy)

  