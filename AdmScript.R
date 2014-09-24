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
## just.ed<- oldData[oldData$ed==1,]
## just.ed.nr<-oldData[oldData$ed==1 & oldData&outcome==0,]
## mean(just.ed$award)
## median(just.ed$award)
## mean(just.ed.nr$award)
## median(just.ed.nr$award)
##
## Interesting finding (from above): of our ED admits, the mean and median COA award for
## those that deposited was $19,199 and $22,750, respectively. (st. div = 11,781.15)
## For those that did not deposit, these values are $2,860 and 0, respectively. (st.div = 7567.98)
## of the 7 ED admits that did not deposit, six were given award amounts of 0. This
## particular admitted student, id# 132035, was also an international student. 
## This reinforces the commonsense notion that ED students are highly sensitive to aid amounts
## ----------------------------------------------------------------------

mean(oldData$age)
median(oldData$age)
sd(oldData$age)

## ----------------------------------------------------------------------
## the average applicant was approximately 19 years old (mean 18.85 / median 18), with a standard
## deviation of 2.24 years
## ----------------------------------------------------------------------

freshman.admits<-sum(oldData$freshman==1)
transfer.admits<-sum(oldData$freshman==0)
percent.freshman.admits<-freshman.admits/total.admits
percent.transfer.admits<-transfer.admits/total.admits
freshman.deposits<-sum(oldData$freshman==1 & oldData$outcome==1)
percent.freshman.deposits<-freshman.deposits/total.deposits
transfer.deposits<-sum(oldData$freshman==0 & oldData$outcome==1)
percent.transfer.deposits<-transfer.deposits/total.deposits
freshman.deposit.rate<-freshman.deposits/freshman.admits
transfer.deposit.rate<-transfer.deposits/transfer.admits
freshman.admits
transfer.admits
percent.freshman.admits
percent.transfer.admits
freshman.deposits
transfer.deposits
percent.freshman.deposits
percent.transfer.deposits
freshman.deposit.rate
transfer.deposit.rate

## ----------------------------------------------------------------------
## 834 of 1010 admitted students were freshman, 176 were transfers. This
## gives us an admitted student pool comprised of of approximately 
## 82.6% freshmen and 17.4% transfers.
## Of our 358 deposits, 259 of those were from freshman applicants.
## Thus, of our total deposits, approximately 72.3% of those were from freshman
## applicants and approximately 27.6% were from transfer applicants. 
## Since 259 of 834 freshman deposited, this gives us a deposit rate of 
## approximately 31%. Since 99 of 176 transfer applicants
## deposited, this gives us a deposit rate of 56.25%.
## ----------------------------------------------------------------------

female.admits<-sum(oldData$female==1)
male.admits<-sum(oldData$female==0)
percent.female.admits<-female.admits/total.admits
percent.male.admits<-male.admits/total.admits
female.deposits<-sum(oldData$female==1 & oldData$outcome==1)
male.deposits<-sum(oldData$female==0 & oldData$outcome==1)
percent.female.deposits<-female.deposits/total.deposits
percent.male.deposits<-male.deposits/total.deposits
female.deposit.rate<-female.deposits/female.admits
male.deposit.rate<-male.deposits/male.admits
female.admits
male.admits
percent.female.admits
percent.male.admits
female.deposits
male.deposits
percent.female.deposits
percent.male.deposits
female.deposit.rate
male.deposit.rate
## ----------------------------------------------------------------------
## Of our 1010 admits, 702 of the admitted students were female, comprising roughly
## 69.5% of the entire admitted student body (308 were male, comprising 30.5%).
## Of 358 deposits, 246 of those  were female, which shows that roughly
## 68.7% of deposits came from female admitted students (112 were male, comprising 31.3%).
## Of 702 admitted female students, 246 deposited, giving us a deposit rate of 35.04%. 
## Of 308 admitted male students, 112 deposited, for a deposit rate of 36.36%.
## ----------------------------------------------------------------------

white.admits<-sum(oldData$white==1,na.rm=TRUE)
nonwhite.admits<-sum(oldData$white==0, na.rm=TRUE)
percent.white.admits<-white.admits/total.admits
percent.nonwhite.admits<-nonwhite.admits/total.admits
white.deposits<-sum(oldData$white==1 & oldData$outcome==1,na.rm=TRUE)
nonwhite.deposits<-sum(oldData$white==0 & oldData$outcome==1, na.rm=TRUE)
percent.white.deposits<-white.deposits/total.deposits
percent.nonwhite.deposits<-nonwhite.deposits/total.deposits
white.deposit.rate<-white.deposits/white.admits
nonwhite.deposit.rate<-nonwhite.deposits/nonwhite.admits
missing.race<-sum(is.na(oldData$white))
missing.race.deposits<-sum(is.na(oldData$white & oldData$outcome==1))
missing.race.deposit.rate<-missing.race.deposits/missing.race
white.admits
nonwhite.admits
percent.white.admits
percent.nonwhite.admits
white.deposits
nonwhite.deposits
percent.white.deposits
percent.nonwhite.deposits
white.deposit.rate
nonwhite.deposit.rate
missing.race
## ----------------------------------------------------------------------
## Of our 1010 admits, 733 of the admitted students self-identified as being white,
## giving us a total admitted student composition of roughly 72.5% caucasian (with 13.2% identify
## as a different race and 14.3% not responding).
## Of 358 deposits, 289 of those recorded were from people identifying as white, meaning
## roughly 80.7% of the depositing students were white (with 13.1% identifying as another race
## and 6.2% not responding). This means that the deposit rate for white admitted students
## was 39.4%, and the deposit rate for nonwhite students was 35.07%. The deposit rate for
## students who did not identify their race (143 admitted students) was 15.38% (22 deposits/143 admits)
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

  