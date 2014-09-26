## ----------------------------------------------------------------------
# Kyle Scot Shank
# 9/26/2014
#
# The purpose of this code is to perform a logistic regression on data
# obtained from three years worth of records regarding matriculant deposits at College of the Atlantic.
#
# We hope to use the admissions records provided from 2011,2012, and 2013 to see what, if any, key variables 
# may explain the decision to deposit or not deposit. Then, utilizing the estimated coefficients, we will
# see how the regression equation predicts the outcomes in 2014. All of the individuals recorded
# in this dataset were admitted to College of the Atlantic. 
#
# Since this project is being uploaded to Github and therefore will be in the public domain, all information 
# containing potentially private information (names, ss number, etc.) have been "scrubbed" from he dataset.
# Additionally, further privacy was enforced by encoding things such as parental marital status and home state
# in a binary format (this also allowed for increased ease of stasticial manipulation).
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
#
# 
# Explanation of variables:
#
# "id" = unique numeric ID assigned to applicants 
# "outcome" = binary variable; 1 if the student deposited, 0 if the student did not.
# "freshman" = binary variable; 1 if the student was a freshman applicants, 0 if not (if the student was labelled a "READMIT", they were assigned a 0)
# "ed" = binary variable; 1 if the student applied Early Decision (ED1 or ED2), 0 if applied Regular Decision
# "age" = age of the applicant, computed from data of birth and expected date of entry
# "female" = binary variable; 1 if the student was female, 0 if male
# "white" = binary variable; 1 if the student responded to their racial identification question with "White", 0 if any other response (N/A if student did not respond/data error)
# ("white" was a forced binary: original data contained other optional responses, i.e. "Black", "Asian", etc. Other responses were condensed.)
# "newengland" = binary variable; 1 if the student was from a New England state (ME, MA, VT, CT, RI, NH), 0 if otherwise (N/A if student did not respond/data error)
# "intl" = binary variable; 1 if student was an international applicant, 0 otherwise 
# "married" = binary variable; 1 if student responded to questions regarding parental relationship status with "married", 0 otherwise (N/A if student did not respond/data error).
# ("married" was a forced binary: original data contained other optional responses, i.e. "Widowed", "Single", etc. Other responses were condensed.)
# "hsgpa" = student high school gpa at time of graduation, self-reported. (N/A if student did not respond/data error)
# "hsrank" = student high school ranking, computed from self-reported rank and self-reported student body size. This is a percentile ranking. (N/A if student did not respond/data error)
# "sat" & "act" = student SAT and ACT scores, self-reported (N/A if student did not respond/data error)
# "a_rank" & "p_rank" = College of the Atlantic internal grading scheme; a_rank measures academic potential, p_rank measures personality/personal potential. Rank is descending (1-6, 1 the highest, 6 the lower).
# "interview" = binary variable; 1 if student had an interview with an admissions counselor, 0 otherwise. (Computed from interview score codes; only students with interview scores were counted as having received an interview; scores were repressed and not represented here.)
# "award" = Reported amount of COA financial aid award. Students with no award listed in master file had variable forced to 0. 
#
## ----------------------------------------------------------------------

## ----------------------------------------------------------------------
# Preamble
## ----------------------------------------------------------------------

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
## This gives us a net deposit rate of 35.44%. 
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
## just.ed.nr<-oldData[oldData$ed==1 & oldData$outcome==0,]
## summary(just.ed$award)
## summary(just.ed.nr$award)
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

newengland.admits<-sum(oldData$newengland==1, na.rm=TRUE)
not.newengland.admits<-sum(oldData$newengland==0, na.rm=TRUE)
percent.newengland.admits<-newengland.admits/total.admits
percent.not.newengland.admits<-not.newengland.admits/total.admits
newengland.deposits<-sum(oldData$newengland==1 & oldData$outcome==1, na.rm=TRUE)
not.newengland.deposits<-sum(oldData$newengland==0 & oldData$outcome==1, na.rm=TRUE)
percent.newengland.deposits<-newengland.deposits/total.deposits
percent.not.newengland.deposits<-not.newengland.deposits/total.deposits
newengland.deposit.rate<-newengland.deposits/newengland.admits
missing.newengland<-sum(is.na(oldData$newengland))
missing.newengland.deposits<-sum(is.na(oldData$newengland & oldData$outcome==1))
missing.newengland.deposit.rate<-missing.married.deposits/missing.married
newengland.admits
not.newengland.admits
percent.newengland.admits
percent.not.newengland.admits
newengland.deposits
not.newengland.deposits
percent.newengland.deposits
percent.not.newengland.deposits
missing.newengland
missing.newengland.deposits
missing.newengland.deposit.rate

## ----------------------------------------------------------------------
## Of our 1010 admits, 348 of the admitted students were from New England states, with
## 654 being from states outside of New England. Thus 34.45% of the admitted student body
## was New Englanders, and 64.75% "from away". (8 students did not submit information regarding their
## home state and/or data was incorrectly entered, comprising 0.79% of the student body)
## 134 students from New England deposited, whereas 223 students not from New England deposited,
## comprising 37.43% and 62.29% of the applicant pool, respectively. Of the 8 students which
## did not submit state information and/or had data entry errors, only 1 deposited (interestingly enough,
## this student had the "worst" A and P scores and received no award.)
## Due to the way the data was cleaned, international students were groupd with those students not from
## New England (both had the variable "newengland" = 0. If we remove international students from the "not.newengland" grouping,
## the findings change somewhat. 553 non-New Englanders admitted, comprising 54.75% of the admitted student body.
## 181 non-New Englanders deposited, comprising 50.55% of the depositing student body, for a deposit rate
## of 32.73%. None of the 8 students above missing state values were international students. 
# 
# not.newengland.admits.corrected<-sum(oldData$newengland==0 & oldData$intl==0, na.rm=TRUE)
# percent.not.newengland.admits.corrected<-not.newengland.admits.corrected/total.admits
# not.newengland.deposits.corrected<-sum(oldData$outcome==1 & oldData$newengland==0 & oldData$intl==0, na.rm=TRUE)
# percent.not.newengland.deposits.corrected<-not.newengland.deposits.corrected/total.deposits
# not.newengland.deposit.rate.corrected<-not.newengland.deposits.corrected/not.newengland.admits.corrected
# not.newengland.admits.corrected
# percent.not.newengland.admits.corrected
# not.newengland.deposits.corrected
# percent.not.newengland.deposits.corrected
# not.newengland.deposit.rate.corrected
## ----------------------------------------------------------------------

intl.admits<-sum(oldData$intl==1)
percent.intl.admits<-intl.admits/total.admits
intl.deposits<-sum(oldData$intl==1 & oldData$outcome==1)
percent.intl.deposits<-intl.deposits/total.deposits
intl.deposit.rate<-intl.deposits/intl.admits
intl.admits
percent.intl.admits
intl.deposits
percent.intl.deposits
intl.deposit.rate
## ----------------------------------------------------------------------
## Of the 1010 admitted students, 101 of the admitted students were international students.
## Therefore, 10% of the admitted students were international student.
## Of 358 deposits, 42 were international students, giving us a deposit ratio
## of 11.73%. We therefore have a deposit ratio of 41.58% (42 deposits / 101 admitted).
##
## just.intl<- oldData[oldData$intl==1 & oldData$outcome==1,]
## just.intl.nr<-oldData[oldData$intl==1 & oldData$outcome==0,]
## summary(just.intl$award)
## summary(just.intl.nr$award)
##
## Interestingly, the international student aid compositions seem to be relatively similar.
## International students that deposit have generally higher mean ($24,060) and median ($26,360)
## award values than those international students that did not deposit ($18,610 and $22,950), but
## not significantly so.
## ----------------------------------------------------------------------

married.admits<-sum(oldData$married==1, na.rm=TRUE)
not.married.admits<-sum(oldData$married==0, na.rm=TRUE)
percent.married.admits<-married.admits/total.admits
percent.not.married.admits<-not.married.admits/total.admits
married.deposits<-sum(oldData$married==1 & oldData$outcome==1, na.rm=TRUE)
not.married.deposits<-sum(oldData$married==0 & oldData$outcome==1, na.rm=TRUE)
percent.married.deposits<-married.deposits/total.deposits
percent.not.married.deposits<-not.married.deposits/total.deposits
married.deposit.rate<-married.deposits/married.admits
missing.married<-sum(is.na(oldData$married))
missing.married.deposits<-sum(is.na(oldData$married & oldData$outcome==1))
missing.married.deposit.rate<-missing.married.deposits/missing.married
married.admits
not.married.admits
percent.married.admits
percent.not.married.admits
married.deposits
not.married.deposits
percent.married.deposits
percent.not.married.deposits
missing.married
missing.married.deposits
missing.married.deposit.rate

## ----------------------------------------------------------------------
## Of 1010 admitted students, 652 had parents who were married for a total
## of approximately 64.5% of admitted students. 346 students did not, comprising
## 34.25% of admitted students. 12 students did not respond, comprising 1.1% of the admitted student body.
## 215 of 358 deposiing students had married parents, comprising approximately 60% of depositing students.
## 140 depositing students did not, comprising 39.1% of depositing students. 
## 3 depositing students did not provide information for this variable, comprising 0.8% of the
## depositing student body. 
## ----------------------------------------------------------------------

hsgpa.admits<-sum(!is.na(oldData$hsgpa))
no.hsgpa.admits<-sum(is.na(oldData$hsgpa))
percent.hsgpa.admits<-hsgpa.admits/total.admits
percent.no.hsgpa.admits<-no.hsgpa.admits/total.admits
hsgpa.deposits<-sum(!is.na(oldData$hsgpa) & oldData$outcome==1)
no.hsgpa.deposits<-sum(is.na(oldData$hsgpa)& oldData$outcome==1)
percent.hsgpa.deposits<-hsgpa.deposits/total.deposits
percent.no.hsgpa.deposits<-no.hsgpa.deposits/total.deposits
hsgpa.deposit.rate<-hsgpa.deposits/hsgpa.admits
no.hsgpa.deposit.rate<-no.hsgpa.deposits/no.hsgpa.admits
hsgpa.admits
no.hsgpa.admits
percent.hsgpa.admits
percent.no.hsgpa.admits
hsgpa.deposits
no.hsgpa.deposits
percent.hsgpa.deposits
percent.no.hsgpa.deposits
hsgpa.deposit.rate
no.hsgpa.deposit.rate

## We run into obvious problems, howver, when we begin to try and compute
## averages for GPAs that have beeen admitted. This is primarily due to the differing
## scales and methodologies utilized by different institutions. For example, we have two
## entries [211,][890,] which have GPA values >5 [5.6] and [94.92] respectively (interestingly
## enough, these students did not deposit). We have 43 entries where GPAs were greater than 4.0  (of which
## only 5 deposited).
## This is an acknowledged problem in both general statistics and in higer education admissions. 
## Normalizing all of the GPA scores provided would not alleviate
## this problem nor allow for more accurate comparative statistics. Therefore, it's most accurate to simply
## find the summary values for those GPAs submitted on a 4.0 scale (ignoring, then, the 45 values which are
## outside of this scale).

corrected.hsgpa.admits<-oldData[oldData$hsgpa<=4.0 & !is.na(oldData$hsgpa),]
corrected.hsgpa.deposits<-oldData[oldData$outcome==1 & !is.na(oldData$hsgpa) & oldData$hsgpa<=4.0,]
mean(corrected.hsgpa.admits$hsgpa)
median(corrected.hsgpa.admits$hsgpa)
sd(corrected.hsgpa.admits$hsgpa)
mean(corrected.hsgpa.deposits$hsgpa)
median(corrected.hsgpa.deposits$hsgpa)
sd(corrected.hsgpa.deposits$hsgpa)

## ----------------------------------------------------------------------
## Of 1010 admitted students, 588 submitted high school GPAs for review.
## The mean GPA of those submitted on a 4.0 scale was 3.59, with a median of 3.66 and a standard deviation of 0.329.
## 422 students did not submit GPAs for review. Thus, approximately 58.22% of our
## admitted students submitted GPAs and 41.78% did not.
## 198 of the 358 depositing students submitted GPAs, comprising 55.3% of total deposits.
## The mean GPA (4.0 scale) of depositing students was also 3.59, with a median of 3.7 and a standard deviation of 0.324.
## 44.7% of depositing students did not. Thus, roughly 33.7% of students that submit GPAs
## deposit (198/588). Students that do not submit GPAs have a deposit rate of 37.9% (160/422)
## ----------------------------------------------------------------------

hsrank.admits<-sum(!is.na(oldData$hsrank))
no.hsrank.admits<-sum(is.na(oldData$hsrank))
percent.hsrank.admits<-hsrank.admits/total.admits
percent.no.hsrank.admits<-no.hsrank.admits/total.admits
hsrank.deposits<-sum(!is.na(oldData$hsrank) & oldData$outcome==1)
no.hsrank.deposits<-sum(is.na(oldData$hsrank)& oldData$outcome==1)
percent.hsrank.deposits<-hsrank.deposits/total.deposits
percent.no.hsrank.deposits<-no.hsrank.deposits/total.deposits
hsrank.deposit.rate<-hsrank.deposits/hsrank.admits
no.hsrank.deposit.rate<-no.hsrank.deposits/no.hsrank.admits
hsrank.admits
no.hsrank.admits
percent.hsrank.admits
percent.no.hsrank.admits
hsrank.deposits
no.hsrank.deposits
percent.hsrank.deposits
percent.no.hsrank.deposits
hsrank.deposit.rate
no.hsrank.deposit.rate
mean(oldData$hsrank, na.rm=TRUE)
median(oldData$hsrank, na.rm=TRUE)
sd(oldData$hsrank, na.rm=TRUE)
just.deposits.hsrank<-oldData[oldData$outcome==1 & !is.na(oldData$hsrank),]
mean(just.deposits.hsrank$hsrank)
median(just.deposits.hsrank$hsrank)
sd(just.deposits.hsrank$hsrank)

## ----------------------------------------------------------------------
## ----------------------------------------------------------------------
## Of 1010 admitted students, 316 submitted high school rankings for review. 694
## did not submit high school rankings. Thus, 31.28% of admitted students supplied rankings,
## whereas 68.71% did not. Of 358 total deposits, 103 of those submitted high school rankings
## for review, whereas 255 did not, comprising 28.77% and 71.23% of the depositing student body, respectively. 
## The mean high school rank of an admitted student was the 82% percentile, with a median of 
## the 86% percentile and a standard deviation of 15%. The mean high school rank of a depositing student
## was the 79% percentile, with a median of the 83% and a standard deviation of 16%. 
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

  