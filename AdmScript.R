## ----------------------------------------------------------------------
## Kyle Scot Shank
## 9/26/2014
## 
## The purpose of this code is to perform a logistic regression on data
## obtained from three years worth of records regarding admitted student deposits at College of the Atlantic.
## 
## We hope to use the admissions records provided from 2011,2012, and 2013 to see what, if any, key variables 
## may explain the decision to deposit or not deposit. Then, utilizing the estimated coefficients, we will
## see how well this regression equation predicts the deposit outcomes for 2014. All of the individuals recorded
## in this dataset were admitted to College of the Atlantic. 
## 
## Since this project is being uploaded to Github and therefore will be in the public domain, all information 
## containing potentially private information (names, ss number, etc.) have been "scrubbed" from the dataset.
## 
## 
## Notes on Data Scrubbing:
## 
## Original dataset contained 1015 unique entries
## Ages were originally represented as decimals: they've been rounded down to the nearest whole number.
## 1 entry was removed for suspected data entry errors 
## 4 entries were removed for data errors with a_rank and p_rank (no rankings entered)
## 1 entry has a data error for "age", accounted for and corrected below.
## 
## 
## Thus, our final data set contains 1010 unique entries.
## 
## 
## Explanation of variables:
## 
## "id" = unique numeric ID assigned to applicants 
## "outcome" = binary variable; 1 if the student deposited, 0 if the student did not.
## "freshman" = binary variable; 1 if the student was a freshman applicants, 0 if not. 
## "ed" = binary variable; 1 if the student applied Early Decision (ED1 or ED2), 0 if applied Regular Decision.
## "age" = age of the applicant, computed from data of birth and expected date of entry
## "female" = binary variable; 1 if the student was female, 0 if male
## "white" = binary variable; 1 if the student responded to their racial identification question with "White", 
## 0 if any other response (N/A if student did not respond/data error)
## ("white" was a forced binary: original data contained other optional responses, i.e. "Black", "Asian", etc.)
## "newengland" = binary variable; 1 if the student was from a New England state (ME, MA, VT, CT, RI, NH), 
## 0 if otherwise (N/A if student did not respond/data error)
## "intl" = binary variable; 1 if student was an international applicant, 0 otherwise 
## "married" = binary variable; 1 if student responded to questions regarding parental relationship status 
## with "married", 0 otherwise (N/A if student did not respond/data error).
## ("married" was a forced binary: original data contained other optional responses, i.e. "Widowed", "Single", etc.)
## "hsgpa" = student high school gpa at time of graduation, self-reported. (N/A if student did not respond/data error)
## "hsrank" = student high school ranking, computed from self-reported rank and self-reported student body size. 
## (This is a percentile ranking. (N/A if student did not respond/data error))
## "sat" & "act" = student SAT and ACT scores, self-reported (N/A if student did not respond/data error)
## "a_rank" & "p_rank" = College of the Atlantic internal grading scheme; a_rank measures academic potential, 
## p_rank measures personality/personal potential. Rank is descending (1-6, 1 the highest, 6 the lower).
## "interview" = binary variable; 1 if student had an interview with an admissions counselor, 0 otherwise. 
## (Computed from internal interview score codes; only students with interview scores were counted as having received an interview. 
## Scores were repressed and are not represented here.)
## "award" = Reported amount of COA financial aid award. Students with no award listed in master file had variable forced to 0. 
##
## ----------------------------------------------------------------------

## ----------------------------------------------------------------------
## Preamble / Load
## ----------------------------------------------------------------------

library(aod)
library(MASS)
library(ggplot2)
library(ggthemes)
library(arm)
library(plyr)
library(Epi)


oldData<-read.csv("coa_admit_data_with_ed.csv")
newData<-read.csv("coa_admit_data_2014.csv")
## Both of these files need to be obtained from the Github repository
## and put into your working directory
## oldData<-https://github.com/kylescotshank/coa-admissions-project/blob/master/coa_admit_data_with_ed.csv
## newData<-https://github.com/kylescotshank/coa-admissions-project/blob/master/coa_admit_data_2014.csv

## ----------------------------------------------------------------------
## ----------------------------------------------------------------------
#------------------------
# Descriptive Statistics
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
## Our data set contains 1010 total admitted students, of which 358 deposited. 
## This gives us a net deposit rate of 35.44%. 
## ----------------------------------------------------------------------

total.ed.admits<-sum(oldData$ed==1)
total.ed.deposits<-sum(oldData$ed==1 & oldData$outcome==1)
percent.ed.total.admits<-total.ed.admits/total.admits
percent.ed.total.deposits<-total.ed.deposits/total.deposits
ed.deposit.rate<-total.ed.deposits/total.ed.admits
total.ed.admits
total.ed.deposits
percent.ed.total.admits
percent.ed.total.deposits
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
## mean(oldData$award[oldData$ed==0])
## median(oldData$award[oldDAta$ed==0])
## summary(just.ed.nr$award)
##
## Interesting finding (from above): of our ED admits, the mean and median COA award for
## those that deposited was $19,19 and $22,750, respectively. (st. div = 11,781.15). This is significantly 
## higher than the mean ($15822.11) and median ($15,000) for non-ED students. (st.div = 12,544.73)
## For those that did not deposit, these values are $2,860 and 0, respectively. (st.div = 7567.98)
## of the 7 ED admits that did not deposit, six were given award amounts of 0. The student 
## which did receive an award ($20,023), and not deposit, id# 132035 [26,], was incidentally an international student. 
## This reinforces the commonsense notion that ED students are highly sensitive to aid amounts
## ----------------------------------------------------------------------

correct.age<-oldData[-628,]
mean(correct.age$age,na.omit=T)
median(correct.age$age)
sd(correct.age$age)


plot1<- ggplot(correct.age, aes(x = age)) + geom_histogram(binwidth=1, aes(fill=..count..)) + theme_few() +
    scale_color_few("dark") + labs(x="Age of Applicant", y = "Count", title= "Histogram of Ages") + 
    xlim(c(10,50)) 
plot1
## Histogram plot of ages


plot2 <- ggplot(correct.age, aes(x = age, y = award)) + geom_point(aes(color = award)) + theme_few() +
    scale_color_gradient(low = "bisque3", high = "chartreuse4") + labs(x =" Age of Applicant", 
                                                                         y =" Financial Aid Award",
                                                                         title = "Age v. Award Amount")
plot2
## Scatterplot of ages v. award amounts

## ----------------------------------------------------------------------
## One student (id# 216822, [628,]) has an incorrect data entry for age, who is therefore supressed for the age
## calculation. The average applicant was approximately 19 years old (mean 18.87 / median 18), with a standard
## deviation of 2.16 years.
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
##
## summary(oldData[oldData$freshman==1,]$award)
## summary(oldData[oldData$freshman==1 & oldData$outcome==1,]$award)
## summary(oldData[oldData$freshman==0,]$award)
## summary(oldData[oldData$freshman==0 & oldData$outcome==1,]$award)
##
## The mean award for an admitted freshman student was $16,750, with a median of $17,490.
## The mean award for a depositing freshman student was $22,930, with a median of $25,940.
## The mean award for an admited transfer student was $13,200, with a median of $10,000.
## The mean award for a depositing transfer student was $15,310, with a median of $18,000. 
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
##
## summary(oldData[oldData$female==1,]$award)
## summary(oldData[oldData$female==1 & oldData$outcome==1,]$award)
## summary(oldData[oldData$female==0,]$award)
## summary(oldData[oldData$female==0 & oldData$outcome==1,]$award)
##
## The mean award for an admitted female student was $16,460, with a median of $17,050.
## The mean award for a depositing female student was $21,190, with a median of $24,490
## The mean award for an admitted male student was $15,400, with a median of $13,560.
## The mean award for a depositing male student was $20,030, with a median of $24,490.
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
## giving us a total admitted student composition of roughly 72.5% caucasian (with 13.2% identifying
## as a different race and 14.3% not responding).
## Of 358 deposits, 289 of those recorded were from people identifying as white, meaning
## roughly 80.7% of the depositing students were white (with 13.1% identifying as another race
## and 6.2% not responding). This means that the deposit rate for white admitted students
## was 39.4%, and the deposit rate for nonwhite students was 35.07%. The deposit rate for
## students who did not identify their race (143 admitted students) was 15.38% (22 deposits/143 admits)
##
## summary(oldData[oldData$white==1,]$award)
## summary(oldData[oldData$white==1 & oldData$outcome==1,]$award)
## summary(oldData[oldData$white==0,]$award)
## summary(oldData[oldData$white==0 & oldData$outcome==1,]$award)
##
## The mean award for an admitted white student was $16,580, with a median of $17,130.
## The mean award for a depositing white student was $21,120, with a meedian of $24,330.
## The mean award for an admitted non-white student was $19,130, with a median of $23,940.
## The mean award for a depositing non-white student was $25,190, with a median of $26,250.
##
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
missing.newengland.deposit.rate<-missing.newengland.deposits/missing.newengland
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
## home state and/or data was incorrectly entered, comprising 0.79% of the population)
## 134 students from New England deposited, whereas 223 students not from New England deposited,
## comprising 37.43% and 62.29% of the deposit pool, respectively. Of the 8 students which
## did not submit state information and/or had data entry errors, only 1 deposited (Note:
## this student had the "worst" A and P scores and received no financial aid award. oldData[363,])
## Due to the way the data was cleaned, international students were groupd with those students not from
## New England (both had the variable "newengland" = 0. If we remove international students from the "not.newengland" grouping,
## the findings change somewhat. 553 non-New Englanders admitted, comprising 54.75% of the admitted student body.
## 181 non-New Englanders deposited, comprising 50.55% of the depositing student body, for a deposit rate
## of 32.73%. None of the 8 students above missing state values were international students. 
# 
## not.newengland.admits.corrected<-sum(oldData$newengland==0 & oldData$intl==0, na.rm=TRUE)
## percent.not.newengland.admits.corrected<-not.newengland.admits.corrected/total.admits
## not.newengland.deposits.corrected<-sum(oldData$outcome==1 & oldData$newengland==0 & oldData$intl==0, na.rm=TRUE)
## percent.not.newengland.deposits.corrected<-not.newengland.deposits.corrected/total.deposits
## not.newengland.deposit.rate.corrected<-not.newengland.deposits.corrected/not.newengland.admits.corrected
## not.newengland.admits.corrected
## percent.not.newengland.admits.corrected
## not.newengland.deposits.corrected
## percent.not.newengland.deposits.corrected
## not.newengland.deposit.rate.corrected
##
## summary(oldData[oldData$newengland==1,]$award)
## summary(oldData[oldData$newengland==1 & oldData$outcome==1,]$award)
## summary(oldData[oldData$newengland==0,]$award)
## summary(oldData[oldData$newengland==0 & oldData$outcome==1,]$award)
##
## The mean award for an admitted New England student was $16,370, with a median of $16,480.
## The mean award for a depositing New England student was $19,910, with a meedian of $23,050.
## The mean award for an admitted non-New England student was $16,100, with a median of $16,200.
## The mean award for a depositing non-New England student was $21,470, with a median of $25,920.
##
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
## Of 358 deposits, 42 were international students, accounting for 
## of 11.73% of depositing students. We therefore have a deposit ratio of 41.58% (42 deposits / 101 admitted).
##
## summary(oldData[oldData$intl==1,]$award)
## summary(oldData[oldData$intl==1 & oldData$outcome==1,]$award)
## summary(oldData[oldData$intl==0,]$award)
## summary(oldData[oldData$intl==0 & oldData$outcome==1,]$award)
##
## The mean award for an admitted international student was $20,870, with a median of $25,500.
## The mean award for a depositing international student was $24,060, with a meedian of $26,360.
## The mean award for an admitted non-international student was $15,610, with a median of $13,620.
## The mean award for a depositing non-international student was $20,390, with a median of $24,000.
##
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
##
## summary(oldData[oldData$married==1,]$award)
## summary(oldData[oldData$married==1 & oldData$outcome==1,]$award)
## summary(oldData[oldData$married==0,]$award)
## summary(oldData[oldData$married==0 & oldData$outcome==1,]$award)
##
## The mean award for an admitted student with married parents was $16,330, with a median of $15,080.
## The mean award for a depositing student with married parents was $19,360, with a median of $22,950.
## The mean award for an admitted student without married parents was $15,730, with a median of $18,260.
## The mean award for a depositing student without married parents was $23,140, with a median of $25,960.
##
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
corrected.no.hsgpa.admits<-oldData[oldData$hsgpa>4.0 & is.na(oldData$hsgpa),]
corrected.hsgpa.deposits<-oldData[oldData$outcome==1 & !is.na(oldData$hsgpa) & oldData$hsgpa<=4.0,]
mean(corrected.hsgpa.admits$hsgpa)
median(corrected.hsgpa.admits$hsgpa)
sd(corrected.hsgpa.admits$hsgpa)
mean(corrected.hsgpa.deposits$hsgpa)
median(corrected.hsgpa.deposits$hsgpa)
sd(corrected.hsgpa.deposits$hsgpa)


plot3 <- ggplot(corrected.hsgpa.admits, aes(x = hsgpa, y = award)) + geom_point(aes(color = award)) + theme_few() +
    scale_color_gradient(low = "chocolate", high = "darkgreen") + labs(x = "High School GPA",
                                                                       y = "Financial Aid Award",
                                                                       title = "H.S. GPA v. Award Amount")
plot3
## Scatterplot of hsgpa v. award amounts

## ----------------------------------------------------------------------
## Of 1010 admitted students, 588 submitted high school GPAs on a 4.0 scale for review.
## The mean GPA of those submitted on a 4.0 scale was 3.59, with a median of 3.66 and a standard deviation of 0.329.
## 422 students did not submit GPAs on a 4.0 scale for review. Thus, approximately 58.22% of our
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

plot4 <- ggplot(oldData, aes(x = hsrank, y = award)) + geom_point(aes(color = award)) + theme_few() +
    scale_color_gradient(low = "chocolate", high = "darkgreen") + labs(x = "High School Rank",
                                                                       y = "Financial Aid Award",
                                                                       title = "H.S. Rank v. Award Amount")
plot4

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

sat.admits<-sum(!is.na(oldData$sat))
no.sat.admits<-sum(is.na(oldData$sat))
percent.sat.admits<-sat.admits/total.admits
percent.no.sat.admits<-no.sat.admits/total.admits
sat.deposits<-sum(!is.na(oldData$sat) & oldData$outcome==1)
no.sat.deposits<-sum(is.na(oldData$sat)& oldData$outcome==1)
percent.sat.deposits<-sat.deposits/total.deposits
percent.no.sat.deposits<-no.sat.deposits/total.deposits
sat.deposit.rate<-sat.deposits/sat.admits
no.sat.deposit.rate<-no.sat.deposits/no.sat.admits
sat.admits
no.sat.admits
percent.sat.admits
percent.no.sat.admits
sat.deposits
no.sat.deposits
percent.sat.deposits
percent.no.sat.deposits
sat.deposit.rate
no.sat.deposit.rate
mean(oldData$sat, na.rm=TRUE)
median(oldData$sat, na.rm=TRUE)
sd(oldData$sat, na.rm=TRUE)
just.deposits.sat<-oldData[oldData$outcome==1 & !is.na(oldData$sat),]
mean(just.deposits.sat$sat)
median(just.deposits.sat$sat)
sd(just.deposits.sat$sat)


plot5 <- ggplot(oldData, aes(x = sat, y = award)) + geom_point(aes(color = award)) + theme_few() +
    scale_color_gradient(low = "slategray4", high = "firebrick") + labs(x = "SAT Score",
                                                                       y = "Financial Aid Award",
                                                                       title = "SAT Score v. Award Amount") +
    xlim(c(1000,2400))
plot5


## ----------------------------------------------------------------------
## Of 1010 admitted students, 524 students reported SAT scores and 486 did not, comprising
## 51.88% and 48.12% of the admitted student population, respectively.
## of 358 depositing students, 150 students reported SAT scores and 208 did not, comprising 
## 41.89% and 58.1% of the depositing student population, respectively. Thus, we have a
## deposit rate of 28.63% for students who supply SAT scores and 42.8% for students that do not.
## The mean SAT score for admitted students was 1898, with a median of 1910 and a standard deviation of 195 points.
## The mean SAT score for depositing students was 1894, with a median of 1900 and a stand deviation of 171 points.
## ----------------------------------------------------------------------

act.admits<-sum(!is.na(oldData$act))
no.act.admits<-sum(is.na(oldData$act))
percent.act.admits<-act.admits/total.admits
percent.no.act.admits<-no.act.admits/total.admits
act.deposits<-sum(!is.na(oldData$act) & oldData$outcome==1)
no.act.deposits<-sum(is.na(oldData$act)& oldData$outcome==1)
percent.act.deposits<-act.deposits/total.deposits
percent.no.act.deposits<-no.act.deposits/total.deposits
act.deposit.rate<-act.deposits/act.admits
no.act.deposit.rate<-no.act.deposits/no.act.admits
act.admits
no.act.admits
percent.act.admits
percent.no.act.admits
act.deposits
no.act.deposits
percent.act.deposits
percent.no.act.deposits
act.deposit.rate
no.act.deposit.rate
mean(oldData$act, na.rm=TRUE)
median(oldData$act, na.rm=TRUE)
sd(oldData$act, na.rm=TRUE)
just.deposits.act<-oldData[oldData$outcome==1 & !is.na(oldData$act),]
mean(just.deposits.act$act)
median(just.deposits.act$act)
sd(just.deposits.act$act)


plot6 <- ggplot(oldData, aes(x = act, y = award)) + geom_point(aes(color = award)) + theme_few() +
    scale_color_gradient(low = "plum4", high = "aquamarine") + labs(x = "ACT Score",
                                                                        y = "Financial Aid Award",
                                                                        title = "ACT Score v. Award Amount") +
    xlim(c(10,35))
plot6

## ----------------------------------------------------------------------
## Of 1010 admitted students, 202 students submitted ACT scores and 808 did not,
## comprising 20% and 80% of the admitted student population. 
## Of 358 total deposits, 64 students submitted ACT scores and 294 did not,
## comprising 17.88% and 82.12% of the depositing student population. The deposit rate
## for those that provided ACT scores was 31.68% and 36.37% for those that did not.
## The average ACT score for an admitted student was 28.26, with a median of 28 and a standard deviation of 3.17.
## The average ACT score for a depositing student was 28.31, with a median of 29 and a standard deviation of 2.9.
## ----------------------------------------------------------------------

summary(oldData$p_rank)

deposit.prank<-oldData[oldData$outcome==1,]
summary(deposit.prank$p_rank)
sd(deposit.prank$p_rank)

just.female.prank<-oldData[oldData$female==1,]
summary(just.female.prank$p_rank)
sd(just.female.prank$p_rank)
just.female.deposit.prank<-oldData[oldData$female==1 & oldData$outcome==1,]
summary(just.female.deposit.prank$p_rank)
sd(just.female.deposit.prank$p_rank)

just.male.prank<-oldData[oldData$female==0,]
summary(just.male.prank$p_rank)
sd(just.male.prank$p_rank)
just.male.deposit.prank<-oldData[oldData$female==0 & oldData$outcome==1,]
summary(just.male.deposit.prank$p_rank)
sd(just.male.deposit.prank$p_rank)

just.white.prank<-oldData[oldData$white==1,]
summary(just.white.prank$p_rank, na.rm=TRUE)
sd(just.white.prank$p_rank, na.rm=T)
just.white.prank.deposits<-oldData[oldData$white==1 & oldData$outcome==1,]
summary(just.white.prank.deposits$p_rank, na.rm=TRUE)
sd(just.white.prank.deposits$p_rank, na.rm=T)

just.notwhite.prank<-oldData[oldData$white==0,]
summary(just.notwhite.prank$p_rank)
sd(just.notwhite.prank$p_rank, na.rm=T)
just.notwhite.prank.deposits<-oldData[oldData$white==0 & oldData$outcome==0,]
summary(just.notwhite.prank.deposits$p_rank)
sd(just.notwhite.prank.deposits$p_rank, na.rm=T)

just.freshman.prank<-oldData[oldData$freshman==1,]
summary(just.freshman.prank$p_rank)
sd(just.freshman.prank$p_rank)
just.freshman.prank.deposits<-oldData[oldData$freshman==1 & oldData$outcome==1,]
summary(just.freshman.prank.deposits$p_rank)
sd(just.freshman.prank.deposits$p_rank)

just.transfer.prank<-oldData[oldData$freshman==0,]
summary(just.transfer.prank$p_rank)
sd(just.transfer.prank$p_rank)
just.transfer.prank.deposits<-oldData[oldData$freshman==0 & oldData$outcome==1,]
summary(just.transfer.prank.deposits$p_rank)
sd(just.transfer.prank.deposits$p_rank)

just.intl.prank<-oldData[oldData$intl==1,]
summary(just.intl.prank$p_rank)
sd(just.intl.prank$p_rank)
just.intl.prank.deposits<-oldData[oldData$intl==1 & oldData$outcome==1,]
summary(just.intl.prank.deposits$p_rank)
sd(just.intl.prank.deposits$p_rank)

hsgpa.prank.admits<-oldData[!is.na(oldData$hsgpa),]
summary(hsgpa.prank.admits$p_rank)
sd(hsgpa.prank.admits$p_rank)
hsgpa.prank.deposits<-oldData[!is.na(oldData$hsgpa) & oldData$outcome==1,]
summary(hsgpa.prank.deposits$p_rank)
sd(hsgpa.prank.deposits$p_rank)
no.hsgpa.prank.admits<-oldData[is.na(oldData$hsgpa),]
summary(no.hsgpa.prank.admits$p_rank)
sd(no.hsgpa.prank.admits$p_rank)
no.hsgpa.prank.deposits<-oldData[is.na(oldData$hsgpa) & oldData$outcome==1,]
summary(no.hsgpa.prank.deposits$p_rank)
sd(no.hsgpa.prank.deposits$p_rank)

sat.prank.admits<-oldData[!is.na(oldData$sat),]
summary(sat.prank.admits$p_rank)
sd(sat.prank.admits$p_rank)
sat.prank.deposits<-oldData[!is.na(oldData$sat) & oldData$outcome==1,]
summary(sat.prank.deposits$p_rank)
sd(sat.prank.deposits$p_rank)
no.sat.prank.admits<-oldData[is.na(oldData$sat),]
summary(no.sat.prank.admits$p_rank)
sd(no.sat.prank.admits$p_rank)
no.sat.prank.deposits<-oldData[is.na(oldData$sat) & oldData$outcome==1,]
summary(no.sat.prank.deposits$p_rank)
sd(no.sat.prank.deposits$p_rank)

act.prank.admits<-oldData[!is.na(oldData$act),]
summary(act.prank.admits$p_rank)
sd(act.prank.admits$p_rank)
act.prank.deposits<-oldData[!is.na(oldData$act) & oldData$outcome==1,]
summary(act.prank.deposits$p_rank)
sd(act.prank.deposits$p_rank)
no.act.prank.admits<-oldData[is.na(oldData$act),]
summary(no.act.prank.admits$p_rank)
sd(no.act.prank.admits$p_rank)
no.act.prank.deposits<-oldData[is.na(oldData$act) & oldData$outcome==1,]
summary(no.act.prank.deposits$p_rank)
sd(no.act.prank.deposits$p_rank)

interview.prank<-oldData[oldData$interview==1,]
summary(interview.prank$p_rank)
sd(interview.prank$p_rank)
interview.prank.deposits<-oldData[oldData$interview==1 & oldData$outcome==1,]
summary(interview.prank.deposits$p_rank)
sd(interview.prank.deposits$p_rank)
no.interview.prank<-oldData[oldData$interview==0,]
summary(no.interview.prank$p_rank)
sd(no.interview.prank$p_rank)
no.interview.prank.deposits<-oldData[oldData$interview==0 & oldData$outcome==1,]
summary(no.interview.prank.deposits$p_rank)
sd(no.interview.prank.deposits$p_rank)


## ----------------------------------------------------------------------
## Of 1010 admitted students, the mean p_rank score was 2.525, with the median of 3 and standard deviation of 0.654
## Of 358 depositing students, the mean p_rank score was 2.489, with a median of 2 and standard deviation of 0.634
## Of 702 female admitted students, the mean p_rank score was 2.491, with a median of 3 and standard deviation of 0.631
## Of 246 female depositing students, the mean p_rank score was 2.484, with a median of 3 and standard deviation of 0.617
## Of 208 male admitted students, the  mean p_rank was 2.568, with a median of 3 and a standard deviation of 0.703 
## Of 112 male depositing students, the mean p_rank was 2.5, with a median of 2 and a standard deviation of 0.671
## Of 876 white admitted students, the mean p_rank was 2.518, with a median of 3 and a standard deviation of 0.646
## Of 311 white depositing students, the mean p_rank was 2.464, with a median of 2 and a standard deviation of 0.645
## Of 227 non-white admitted students, the mean p_rank was 2.507, with a median of 3 and a standard deviation of 0.68
## Of 208 non-white depositing students, the mean p_rank was 2.46, with a median of 3 and a standard deviation of 0.712
## Of 834 admitted freshman students, the mean p_rank was 2.5, with a median of 3 and a standard deviation of 0.656
## Of 259 depositing freshman students, the mean p_rank was 2.467, with a median of 2 and a standard deviation of 0.642
## Of 176 admitted transfer students, the mean p_rank was 2.591, with a median of 3 and a standard deviation of 0.644
## Of 99 depositing transfer students, the mean p_rank was 2.545, with a median of 3 and a standard deviation of 0.611
## Of 101 admitted international students, the mean p_rank was 2.356 with a median of 2 and a standard deviation of 0.61
## Of 42 depositing international students, the mean p_rank was 2.357 with a median of 2 and a standard deviation of 0.533
## Of 588 admitted students that submitted HS GPAs, the mean p_rank was 2.524 with a median of 3 and a standard deviation of 0.643
## Of 198 depositing students that submitted HS GPAs, the mean p_rank was 2.5 with a median of 3 and a standard deviation of 0.627
## Of 422 admitted students that did not submit HS GPAs, the mean p_rank was 2.502 with a median of 3 and a standard deviation of 0.671
## Of 160 depositing students that did not submit HS GPAs, the mean p_rank was 2.475 with a median of 2 and a standard deviation of 0.644
## Of 524 admitted students that submitted SAT scores, the mean p_rank was 2.544 with a median of 3 and a standard deviation of 0.666
## Of 150 depositing students that submitted SAT scores, the mean p_rank was 2.547 with a median of 3 and a standard deviation of 0.651
## Of 486 admitted students that did not submit SAT scores, the mean p_rank was 2.484 with a median of 3 and a standard deviation of 0.641
## Of 208 depositing studnets that did not submit SAT scores, the mean p_rank was 2.447 with a median of 3 and a standard deviation of 0.619
## Of 202 admitted students that submitted ACT scores, the mean p_rank was 2.416 with a median of 2 and a standard deviation of 0.7
## Of 64 depositing students that submitted ACT scores, the mean p_rank was 2.438 with a median of 2 and a standard deviation of 0.64
## Of 319 admitted students that had interviews, the mean p_rank was 2.357 with a median of 2 and a standard deviation of 0.681
## Of 182 depositing students that had interviews, the mean p_rank was 2.412 with a median of 2 and a standard deviation of 0.665
## Of 691 admitted students that did not have interviews, the mean p_rank was 2.588 wih a median of 3 and a standard deviation of 0.63
## Of 176 depositing students that did not have interviews, the mean p_rank was 2.568 with a median of 3 and a standard deviation of 0.59
## ----------------------------------------------------------------------

summary(oldData$a_rank)

deposit.arank<-oldData[oldData$outcome==1,]
summary(deposit.arank$a_rank)

just.female.arank<-oldData[oldData$female==1,]
summary(just.female.arank$a_rank)
just.female.deposit.arank<-oldData[oldData$female==1 & oldData$outcome==1,]
summary(just.female.deposit.arank$a_rank)

just.male.arank<-oldData[oldData$female==0,]
summary(just.male.arank$a_rank)
just.male.deposit.arank<-oldData[oldData$female==0 & oldData$outcome==1,]
summary(just.male.deposit.arank$a_rank)

just.white.arank<-oldData[oldData$white==1,]
summary(just.white.arank$a_rank, na.rm=TRUE)
just.white.arank.deposits<-oldData[oldData$white==1 & oldData$outcome==1,]
summary(just.white.arank.deposits$a_rank, na.rm=TRUE)

just.notwhite.arank<-oldData[oldData$white==0,]
summary(just.notwhite.arank$a_rank)
just.notwhite.arank.deposits<-oldData[oldData$white==0 & oldData$outcome==0,]
summary(just.notwhite.arank.deposits$a_rank)

just.freshman.arank<-oldData[oldData$freshman==1,]
summary(just.freshman.arank$a_rank)
just.freshman.arank.deposits<-oldData[oldData$freshman==1 & oldData$outcome==1,]
summary(just.freshman.arank.deposits$a_rank)

just.transfer.arank<-oldData[oldData$freshman==0,]
summary(just.transfer.arank$a_rank)
just.transfer.arank.deposits<-oldData[oldData$freshman==0 & oldData$outcome==1,]
summary(just.transfer.arank.deposits$a_rank)

just.intl.arank<-oldData[oldData$intl==1,]
summary(just.intl.arank$a_rank)
just.intl.arank.deposits<-oldData[oldData$intl==1 & oldData$outcome==1,]
summary(just.intl.arank.deposits$a_rank)

hsgpa.arank.admits<-oldData[!is.na(oldData$hsgpa),]
summary(hsgpa.arank.admits$a_rank)
sd(hsgpa.arank.admits$a_rank)
hsgpa.arank.deposits<-oldData[!is.na(oldData$hsgpa) & oldData$outcome==1,]
summary(hsgpa.arank.deposits$a_rank)
sd(hsgpa.arank.deposits$a_rank)
no.hsgpa.arank.admits<-oldData[is.na(oldData$hsgpa),]
summary(no.hsgpa.arank.admits$a_rank)
sd(no.hsgpa.arank.admits$a_rank)
no.hsgpa.arank.deposits<-oldData[is.na(oldData$hsgpa) & oldData$outcome==1,]
summary(no.hsgpa.arank.deposits$a_rank)
sd(no.hsgpa.arank.deposits$a_rank)

sat.arank.admits<-oldData[!is.na(oldData$sat),]
summary(sat.arank.admits$a_rank)
sd(sat.arank.admits$a_rank)
sat.arank.deposits<-oldData[!is.na(oldData$sat) & oldData$outcome==1,]
summary(sat.arank.deposits$a_rank)
sd(sat.arank.deposits$a_rank)
no.sat.arank.admits<-oldData[is.na(oldData$sat),]
summary(no.sat.arank.admits$a_rank)
sd(no.sat.arank.admits$a_rank)
no.sat.arank.deposits<-oldData[is.na(oldData$sat) & oldData$outcome==1,]
summary(no.sat.arank.deposits$a_rank)
sd(no.sat.arank.deposits$a_rank)

act.arank.admits<-oldData[!is.na(oldData$act),]
summary(act.arank.admits$a_rank)
sd(act.arank.admits$a_rank)
act.arank.deposits<-oldData[!is.na(oldData$act) & oldData$outcome==1,]
summary(act.arank.deposits$a_rank)
sd(act.arank.deposits$a_rank)
no.act.arank.admits<-oldData[is.na(oldData$act),]
summary(no.act.arank.admits$a_rank)
sd(no.act.arank.admits$a_rank)
no.act.arank.deposits<-oldData[is.na(oldData$act) & oldData$outcome==1,]
summary(no.act.arank.deposits$a_rank)
sd(no.act.arank.deposits$a_rank)

interview.arank<-oldData[oldData$interview==1,]
summary(interview.arank$a_rank)
sd(interview.arank$a_rank)
interview.arank.deposits<-oldData[oldData$interview==1 & oldData$outcome==1,]
summary(interview.arank.deposits$a_rank)
sd(interview.arank.deposits$a_rank)
no.interview.arank<-oldData[oldData$interview==0,]
summary(no.interview.arank$a_rank)
sd(no.interview.arank$a_rank)
no.interview.arank.deposits<-oldData[oldData$interview==0 & oldData$outcome==1,]
summary(no.interview.arank.deposits$a_rank)
sd(no.interview.arank.deposits$a_rank)


oldData2 <- oldData
oldData2$female[oldData2$female==1]<- "Female"
oldData2$female[oldData2$female==0]   <- "Male"
plot7<- ggplot(oldData2, aes(x = p_rank, y = award)) + geom_point(aes(color = a_rank)) + theme_few() +
    xlim(c(6,1)) + geom_jitter(alpha=0.7, aes(color= a_rank),position = position_jitter(width = .2)) +
    scale_color_gradient(low = "chocolate", high = "seagreen4")
plot7 + facet_grid(.~female)
## Scatter facet plot showing p_rank (x-axis) v. award (y-axis) with a_rank being the color-fill density
## of each individual data point. Faceting is done via respect to "Male" v. "Female"

## ----------------------------------------------------------------------
## Of 1010 admitted students, the mean a_rank score was 2.714, with the median of 3 and standard deviation of 0.819
## Of 358 depositing students, the mean a_rank score was 2.832, with a median of 3 and standard deviation of 0.816
## Of 702 female admitted students, the mean a_rank score was 2.67, with a median of 3 and standard deviation of 0.789
## Of 246 female depositing students, the mean a_rank score was 2.756, with a median of 3 and standard deviation of 0.780
## Of 208 male admitted students, the  mean a_rank was 2.815, with a median of 3 and a standard deviation of 0.877 
## (interesting, the only a_rank of 6 is [524,], a white male who, in the end, deposited.)
## Of 112 male depositing students, the mean a_rank was 3, with a median of 3 and a standard deviation of 0.869
## Of 876 white admitted students, the mean a_rank was 2.708, with a median of 3 and a standard deviation of 0.816
## Of 311 white depositing students, the mean a_rank was 2.799, with a median of 3 and a standard deviation of 0.804
## Of 227 non-white admitted students, the mean a_rank was 2.724, with a median of 3 and a standard deviation of 0.836
## Of 208 non-white depositing students, the mean a_rank was 2.632, with a median of 3 and a standard deviation of 0.851
## Of 834 admitted freshman students, the mean a_rank was 2.694, with a median of 3 and a standard deviation of 0.824
## Of 259 depositing freshman students, the mean a_rank was 2.819, with a median of 3 and a standard deviation of 0.813
## Of 176 admitted transfer students, the mean a_rank was 2.807, with a median of 3 and a standard deviation of 0.798
## Of 99 depositing transfer students, the mean a_rank was 2.869, with a median of 3 and a standard deviation of 0.829
## Of 101 admitted international students, the mean a_rank was 2.545 with a median of 2 and a standard deviation of 0.794
## Of 42 depositing international students, the mean a_rank was 2.643 with a median of 2.5 and a standard deviation of 0.727
## Of 588 admitted students that submitted HS GPAs, the mean a_rank was 2.716 with a median of 3 and a standard deviation of 0.821
## Of 198 depositing students that submitted HS GPAs, the mean a_rank was 2.813 with a median of 3 and a standard deviation of 0.794
## Of 422 admitted students that did not submit HS GPAs, the mean a_rank was 2.711 with a median of 3 and a standard deviation of 0.82
## Of 160 depositing students that did not submit HS GPAs, the mean a_rank was 2.856 with a median of 3 and a standard deviation of 0.846
## Of 524 admitted students that submitted SAT scores, the mean a_rank was 2.567 with a median of 3 and a standard deviation of 0.802
## Of 150 depositing students that submitted SAT scores, the mean a_rank was 2.653 with a median of 3 and a standard deviation of 0.794
## Of 486 admitted students that did not submit SAT scores, the mean a_rank was 2.872 with a median of 3 and a standard deviation of 0.81
## Of 208 depositing studnets that did not submit SAT scores, the mean a_rank was 2.962 with a median of 3 and a standard deviation of 0.81
## Of 202 admitted students that submitted ACT scores, the mean a_rank was 2.515 with a median of 2 and a standard deviation of 0.83
## Of 64 depositing students that submitted ACT scores, the mean a_rank was 2.547 with a median of 3 and a standard deviation of 0.733
## Of 319 admitted students that had interviews, the mean a_rank was 2.671 with a median of 3 and a standard deviation of 0.794
## Of 182 depositing students that had interviews, the mean a_rank was 2.819 with a median of 3 and a standard deviation of 0.762
## Of 691 admitted students that did not have interviews, the mean a_rank was 2.734 wih a median of 3 and a standard deviation of 0.831
## Of 176 depositing students that did not have interviews, the mean a_rank was 2.847 with a median of 3 and a standard deviation of 0.871
## ----------------------------------------------------------------------

interview.admits<-sum(oldData$interview==1)
interview.deposits<-sum(oldData$interview==1 & oldData$outcome==1)
percent.interview.admits<-interview.admits/total.admits
percent.interview.deposits<-interview.deposits/total.deposits
interview.deposit.rate<-interview.deposits/interview.admits
interview.admits
interview.deposits
percent.interview.admits
percent.interview.deposits
interview.deposit.rate


## ----------------------------------------------------------------------
## Of 1010 admitted students, 319 of these has interviews, comprising 31.58% of the admitted
## student population.
## Of the 358 depositing students, 182 had interviews, comprising 50.84% of the depositing student population.
## Students that interviewed had a deposit rate of 57.05%. 
##
## summary(oldData[oldData$interview==1,]$award)
## summary(oldData[oldData$interview==1 & oldData$outcome==1,]$award)
## summary(oldData[oldData$interview==0,]$award)
## summary(oldData[oldData$interview==0 & oldData$outcome==1,]$award)
##
## The mean award for an admitted student with an interview was $17,580, with a median of $19,740.
## The mean award for a depositing student with an interview as $19,900, with a median of $24,000.
## The mean award for an admitted student without an interview was $15,470, with a median of $15,000.
## The mean award for a depositing student without an interview was $21,780, with a median of $25,690.
## ----------------------------------------------------------------------

summary(oldData$award)
sd(oldData$award)
deposit.awards<-oldData[oldData$outcome==1,]
summary(deposit.awards$award)
sd(deposit.awards$award)
length(which(oldData$award>=27440))
length(which(deposit.awards$award>=30000))
length(which(oldData$award==0 & oldData$outcome==1))
no.award.admit<-oldData[oldData$award==0,]
no.award.deposit<-oldData[oldData$award==0 & oldData$outcome==1,]


plot8<- ggplot(oldData2, aes(x = award)) + 
    geom_histogram(aes(fill = ..count..), binwidth = 5000) + theme_few() 
plot8 + facet_grid(.~female)
## Facet plot, awards v. gender

## ----------------------------------------------------------------------
## The minimum award package for admitted students was was $0. This award was given to 221 admitted students, 
## comprising approximately 21.88% of admitted students. 
## The minimum award package for depositing students was $0. This award was given to 49 depositing students, 
## comprising 13.69% of depositing students.
## The maximum award for admitted students was $38,390. The third quartile award was $27,440. 253 admitted students
## were offered awards greater than or equal to this amount, comprising approximately 25% of admitted student awards.
## The maximum award for depositing students was $36,590. The third quartile award was $30,000. 104 depositing students
## were offered awards greater than or equal to this amount, comprising 29% of the depositing student population.
## The mean award for admitted students was $16,140, the median $15,980, with a standard deviation of $12,506.07
## The mean award for depositing students was $20,820, the median $24,490, with a standard deviation of $11,857.65
## Thus, the mean award for depositing students is approximately 29% higher than the award for admitted students,
## with the median award for depositing students being 53% higher than the median award for admitted students. 
## ----------------------------------------------------------------------

## ----------------------------------------------------------------------
## ----------------------------------------------------------------------
#------------------------
# Correlations
#------------------------
## ----------------------------------------------------------------------
## ----------------------------------------------------------------------

corMat<-cor(oldData, use="pairwise.complete.obs", method="pearson")
corMat
## ----------------------------------------------------------------------
## Computes a matrix of correlation coefficients between all variables. 
## Note: the error message "Warning message:
##  In cor(oldData, use = "pairwise.complete.obs", method = "pearson") :
##   the standard deviation is zero"
## Stems from the relationship between "hsrank" and "intl", as no international students
## submitted high school rankings. 
##
## Only correlationc coefficients >=|0.3| are discussed. 
## The strongest correlations for "outcome" are for "ed" (0.388) and "interview" (0.307). Both are weakly positive.
## The strongest correlation for "freshman" is "age" (-0.467). This is weakly negative.
## The strongest correlation for "white" is "intl" (-0.346). This is weakly negative.
## The strongest correlation for "hsgpa" is "act" (0.418). This is weakly positive. 
## The strongest correlations for "hsrank" are "sat" (0.4), "act" (0.564), and "a_rank" (-0.583). "sat" is weakly positive.
## "act" is moderatley positive. "a_rank" is moderately negative. 
## The strongest correlation for "sat" is "act" (0.805). This is strongly positive.
## The strongest correlations for "a_rank" are "hsrank" (-0.583), "sat" (-0.545), "act" (-0.532), and "p_rank" (0.375)
## The first three are moderately negative. "p_rank" is weakly positive. 
## The strongest correlation for "interview" is "outcome" (0.307). This is weakly positive.
## ----------------------------------------------------------------------


## ----------------------------------------------------------------------
## ----------------------------------------------------------------------
#------------------------
# Regression
#------------------------
## ----------------------------------------------------------------------
## ----------------------------------------------------------------------

## ----------------------------------------------------------------------
## We want to analyze the choice that admitted students make at College of the Atlantic when they choose to
## either deposit or not deposit with the institution. To do so, we will utilize a binomial logit model with a
## dichotomous dependent variable ("outcome") and both continuous and dichotomous independent variables.
## 
## The general form of binomial logits is as such:
## 
## D_i = (1)/(1+EXP(-[B_0 + B_1*X_1i + B_2*X_2i ... B_N*X_Ni + e_i]))
## or
## ln(D_i/(1-D_i)) = B_0 + B_1*X_1i + B_2*X_2i ... B_N*X_Ni + e_i
##
## where ln(D_i/(1-D_i)) = the "log odds", or ln(Pr(D_i = 1))
##
## This model is useful as it provides both an upper (1) and lower (0) bound to our estimated D_i. 
## The glm() algorithm used by R will compute this regression using MLE methods. 
##
## After several exploratory regression, a new dataframe has been constructed below upon which to test
## our logistic regression. 
##
## ----------------------------------------------------------------------

bad.logit.output<-glm(outcome ~ freshman + ed + age + female + white + newengland + intl + married + 
                      a_rank + p_rank + interview + award + sat + act + hsgpa + hsrank,
                  family= binomial(logit),data = na.omit(oldData), maxit=100)

summary(bad.logit.output)

## ----------------------------------------------------------------------
## This model specification outputs errors that are directly related to the "hsrank" variable.
## Additionally, after several model specification changes, it's been discovered that "sat", "act", 
## "hsgpa", and "hsrank" are never significant in and of their own accord. We therefore move forward
## with a changed dummy variable below.
## ----------------------------------------------------------------------

oldData.reg<-oldData
oldData.reg[628,]$age<-NA
hs_dummy<-as.numeric(oldData$hsgpa>0 & !is.na(oldData$hsgpa) | oldData$hsrank>0 & !is.na(oldData$hsrank))
#("hs_dummy" is now just a dichotomous variable representing whether or not an admitted student self-reported high school data.)
oldData.reg$hs_dummy<-hs_dummy
oldData.reg<-subset(oldData.reg,select=-hsgpa)
oldData.reg<-subset(oldData.reg,select=-hsrank)
test_dummy<-as.numeric(oldData$sat>0 & !is.na(oldData$sat) | oldData$act>0 & !is.na(oldData$act))
#("test_dummy" is now just a dichotomous variable representing whether or not an admitted student self-reported their standardized test scores.)
oldData.reg$test_dummy<-test_dummy
oldData.reg<-subset(oldData.reg,select=-sat)
oldData.reg<-subset(oldData.reg,select=-act)

## ----------------------------------------------------------------------

logit.output<-glm(outcome ~ freshman + ed + age + female + white + newengland + intl + married + 
                      a_rank + p_rank + interview + award + hs_dummy + test_dummy,
                  family= binomial(logit),data = na.omit(oldData.reg),maxit=100)

summary(logit.output)

## ----------------------------------------------------------------------
## In this regression model, "freshman","ed","age","a_rank","interview","award", and "hs_dummy" are statistically significant
## at a p-level of 0.05. 
##
## Thus, our model becomes:
## Ln(Pr(Outcome=1)) = -8.502 - 1.348(freshman) + 3.873(ed) + 0.309(age) + 0.193(female) + 0.241(white) + 0.07(newengland)
## + 0.395(intl) - 0.24(married) + 0.386(a_rank) - 0.131(p_rank) + 1.04(interview) + 7.61*10^{-5}(award) + 0.467(hs_dummy)
## + 0.056(test_dummy)
## ----------------------------------------------------------------------


exp(cbind(OR=coef(logit.output),confint.default(logit.output)))
## ----------------------------------------------------------------------
## This computes the odds ratios and confidence intervals utilizing standard errors
## For example, we can see from this model specification that being an early decision applicant ("ed"=1)
## means that an admitted student is roughly 48 times more likely to deposit than a student that is not early decision.
## EXP(3.873*(1-0)) = 48.08. We also see that international students ("intl"=1) are about 1.5 times as likely
## to deposit as non-international students, as are students that submit high school rankings/gpas. 
##
## Going further: a 25 year old student is 8.7 times more likely to deposit than an 18 year old.
## (EXP(B_"age"*(x_i - x_j)) = exp(0.309*(25-18))
## 
## Or, perhaps more interestingly:
## A student awarded an a_rank of "5" is 4.5 times more likely to deposit than an a_rank of "1". 
## exp(0.386*(5-1)) = 4.68
## ----------------------------------------------------------------------


anova(logit.output,test="Chisq")
stepAIC(logit.output)
## ----------------------------------------------------------------------
## We want to test model fit. To test model fit on logit models, one method is to minimize both deviance and 
## the Aikake information critera (AIC). Note: AIC tells us nothing about how our model compares to a null
## model in the absolute sense. However, given a set of candidate models for the data, the preferred model is the 
## one with the minimum AIC value. Hence AIC not only rewards goodness of fit, but also includes a penalty 
## that is an increasing function of the number of estimated parameters. 
## This penalty discourages overfitting (increasing the number of parameters in the model almost always improves the goodness of the fit).
##
## To do this, we will utilize the stepAIC() function from the MASS library, which will systematically
## work through each independent variable to find which model specification minimizes the deviance and AIC.
##
## Thus, our "optimized" regression equation is:
## Outcome_i = -8.354 - 1.314(freshman) + 3.917(ed) + + 0.3039(age) + 0.347(a_rank) + 1.041(interview) + 
## 7.6868e-05(award) + 0.4023(hs_dummy). We will call this new regression bestfit.logit.output.
## ----------------------------------------------------------------------

bestfit.logit<-glm(outcome ~ freshman + ed + age +  a_rank  + interview + award + hs_dummy ,
                  family= binomial(logit), data = na.omit(oldData.reg), maxit=100)
summary(bestfit.logit)
exp(cbind(OR=coef(bestfit.logit),confint.default(bestfit.logit)))

## ----------------------------------------------------------------------
## ----------------------------------------------------------------------
## ------------------------
## Predictive Accuracy
## ------------------------
## ----------------------------------------------------------------------
## ----------------------------------------------------------------------



prediction.matrix<-matrix(0,ncol=4,nrow=length(oldData.reg$outcome))
colnames(prediction.matrix)<-c("id", "outcome", "fitted probability","predicted outcome")
## Construct a matrix with four columns: "id", "outcome", "fitted probability", and "predictd outcomes"
prediction.matrix[,1]<-oldData.reg$id
## Put the ids into column 1
prediction.matrix[,2]<-oldData.reg$outcome
## Puts the predicted outcomes in column 2
fitted.values <- predict(bestfit.logit,oldData.reg,type="response")
## Creates a vector of fitted values from bestfit.logit
prediction.matrix[,3]<- fitted.values
## Puts this vector into column 3
predicted.outcomes<-ifelse(prediction.matrix[,3]>=0.5,1,0)
## Creates a vector that rounds the fitted probabilities up or down from 0.5,
## simluating a positive (1) and negative (0) response
predicted.outcomes[628]=0
## Fixes the mistake called by the NA in age in oldData.reg[628]
prediction.matrix[,4]<-predicted.outcomes
## Puts these predicted outcomes into column 4


true.positives<-count(prediction.matrix[,2]==1 & prediction.matrix[,4]==1)[2,2]
true.positives
## True positives where actual outcome matches predicted outcome
## 218 true positives
false.positives<-count(prediction.matrix[,2]==0 & prediction.matrix[,4]==1)[2,2]
false.positives
## False positives where actual outcomes do not match predicted outcomes
## 78 false positives
true.negatives<-count(prediction.matrix[,2]==0 & prediction.matrix[,4]==0)[2,2]
true.negatives
## True negatives where actual outcome matches predicted outcome
## 574 true negatives
false.negatives<-count(prediction.matrix[,2]==1 & prediction.matrix[,4]==0)[2,2]
false.negatives
## 140 false negatives.


confusion.table<-matrix(nrow=2,ncol=2)
colnames(confusion.table)<-c("Condition Positive","Condition Negative")
rownames(confusion.table)<-c("Test Outcome Positive","Test Outcome Negative")
confusion.table[,1]<-c(true.positives,false.positives)
confusion.table[,2]<-c(false.negatives,true.negatives)
confusion.table

sensitivity<-true.positives/total.deposits
## Sensitivity is the proportion of of corretly identified positives,
## or TPR = TP/P = TP/(TP+FN)
## The logit regression correctly identifies 60.89% of the depositing students.
specificity<-true.negatives/(total.admits-total.deposits)
## Specificty is the proportion of correclty identified negatives,
## or TPN = TN/N = TN/(FP+TN)
## The logit regression correclty identifies non-depositing students 88.03% of the time.
accuracy<-(true.negatives+true.positives)/(total.admits)
## Accuracy is the proportion of true results in the population.
## ACC = (TP+TN)/(TP+FP+TN+FN)
## The logit regression is 78.41% accurate
precision<-(true.positives)/(true.positives+false.positives)
## Precision is the proportion of true positives against all positive results.
## PPV (precision = positive predicted value) = TP/(TP+FP)
## The logit regression is 73.64% precise

## As we can see, the logit model has relatively high accuracy and precision.
## has high specifity, but has relatively low sensitivity. We want to maximize both
## specificty and sensitivity. This will be dependent upon what value we assign to the round()
## function above (the cut-off condition, or eta): the fitted.value at which or above we assign a value of 1 (deposit) 
## and below which we assign a value of 0 (non-deposit). To find this optimal value,
## we will find the ROC (receiver operating characteristic) curve, which measures
## sensitivity against fall-out, or 1-specificty.) 

ROC(form=outcome ~ freshman + ed + age +  a_rank  + interview + award + hs_dummy, data=oldData.reg)

## Thus, specificity and sensitivity are maximized at an eta of 0.301

best.prediction.matrix<-matrix(0,ncol=4,nrow=length(oldData.reg$outcome))
colnames(best.prediction.matrix)<-c("id", "outcome", "fitted probability","predicted outcome")
## Construct a matrix with four columns: "id", "outcome", "fitted probability", and "predictd outcomes"
best.prediction.matrix[,1]<-oldData.reg$id
## Put the ids into column 1
best.prediction.matrix[,2]<-oldData.reg$outcome
## Puts the predicted outcomes in column 2
best.fitted.values <- predict(bestfit.logit,oldData.reg,type="response")
## Creates a vector of fitted values from bestfit.logit
best.prediction.matrix[,3]<- best.fitted.values
## Puts this vector into column 3
best.predicted.outcomes<-ifelse(best.prediction.matrix[,3]>=0.301,1,0)
## Creates a vector that rounds the fitted probabilities up or down from 0.5,
## simluating a positive (1) and negative (0) response
best.predicted.outcomes[628]=0
## Fixes the mistake called by the NA in age in oldData.reg[628]
best.prediction.matrix[,4]<-best.predicted.outcomes
## Puts these predicted outcomes into column 4

best.true.positives<-count(best.prediction.matrix[,2]==1 & best.prediction.matrix[,4]==1)[2,2]
best.true.positives
## True positives where actual outcome matches predicted outcome
## 295 true positives
best.false.positives<-count(best.prediction.matrix[,2]==0 & best.prediction.matrix[,4]==1)[2,2]
best.false.positives
## False positives where actual outcomes do not match predicted outcomes
## 210 false positives
best.true.negatives<-count(best.prediction.matrix[,2]==0 & best.prediction.matrix[,4]==0)[2,2]
best.true.negatives
## True negatives where actual outcome matches predicted outcome
## 442 true negatives
best.false.negatives<-count(best.prediction.matrix[,2]==1 & best.prediction.matrix[,4]==0)[2,2]
best.false.negatives
## 63 false negatives.

best.sensitivity<-best.true.positives/total.deposits
## The optimal cut-off correctly identifies 82.4% of the depositing students.
best.specificity<-best.true.negatives/(total.admits-total.deposits)
## The optimal cut-off correclty identifies non-depositing students 67,79% of the time.
best.accuracy<-(best.true.negatives+best.true.positives)/(total.admits)
## The optimal cut-off is 72.97% accurate
best.precision<-(best.true.positives)/(best.true.positives+best.false.positives)
## The optimal cut-off is 58.41% precise

## ----------------------------------------------------------------------
## Does this outperform the admissions-process "Null Model", which is that a random third of all
## admissions candidates will deposit?
## ----------------------------------------------------------------------

null.prediction.matrix<-matrix(0,ncol=4,nrow=length(oldData.reg$outcome))
colnames(null.prediction.matrix)<-c("id", "outcome", "fitted probability","predicted outcome")
## Construct a matrix with four columns: "id", "outcome", "fitted probability", and "predictd outcomes"
null.prediction.matrix[,1]<-oldData.reg$id
## Put the ids into column 1
null.prediction.matrix[,2]<-oldData.reg$outcome
## Puts the predicted outcomes in column 2
null.fitted.values <- predict(bestfit.logit,oldData.reg,type="response")
## Creates a vector of fitted values from bestfit.logit
null.prediction.matrix[,3]<- null.fitted.values
## Puts this vector into column 3
set.seed(1234)
## For reproduction
null.predicted.outcomes<-rbinom(length(oldData.reg$outcome),1,0.3)
## Creates a vector of random 1s and 0s
## simluating a positive (1) and negative (0) response by creating a binomial 
## distribution with rbinom(). The probability of success (1) is 30%
null.prediction.matrix[,4]<-null.predicted.outcomes
## Puts this predicted outcomes into column 4


## ----------------------------------------------------------------------
## Null model performance
## ----------------------------------------------------------------------

null.true.positives<-count(null.prediction.matrix[,2]==1 & null.prediction.matrix[,4]==1)[2,2]
null.true.positives
## True positives where actual outcome matches predicted outcome
## 104 true positives
null.false.positives<-count(null.prediction.matrix[,2]==0 & null.prediction.matrix[,4]==1)[2,2]
null.false.positives
## False positives where actual outcomes do not match predicted outcomes
## 202 false positives
null.true.negatives<-count(null.prediction.matrix[,2]==0 & null.prediction.matrix[,4]==0)[2,2]
null.true.negatives
## True negatives where actual outcome matches predicted outcome
## 450 true negatives
null.false.negatives<-count(null.prediction.matrix[,2]==1 & null.prediction.matrix[,4]==0)[2,2]
null.false.negatives
## 254 false negatives.

null.sensitivity<-null.true.positives/total.deposits
## The null hypothesis identifies 29.05% of the depositing students.
null.specificity<-null.true.negatives/(total.admits-total.deposits)
## The null hypothesis identifies non-depositing students 69.01% of the time.
null.accuracy<-(null.true.negatives+null.true.positives)/(total.admits)
## The null hypothesis is 54.85% accurate
null.precision<-(null.true.positives)/(null.true.positives+null.false.positives)
## The null hypothesis is 33.98% precise

## ----------------------------------------------------------------------
## We see that the best.fit model (with an eta value of 0.301) provides
## the highest degree of sensitivity, or true-poisitve rate. (79.05%, as opposed
## to 57.26% for a cut-off value of 0.5 and 29.05% for the null hypothesis). 
## The best.fit model has a sensitivity that is 38.05% better than the model with a cut-off
## value of 0.5 and 172.11% better than the null model.
##
## It does not, however, have the highest specificity, or true-negative rate: the cut-off value of 0.5 
## has a specificity of 89.87, which is 24.9% better than the best.fit model and 30.2% better than
## the null model.
##
## The best.fit model also underperforms by measures of accuracy: the cut-off value of 0.5
## has an accuracy of 78.31%, the highest of the three. However, it is only 5.18% more accurate
## than the best-fit model (74.45%). It is 42.77% more accurate than the null model.
##
## The best.fit model, however, does maximize both sensitivity and specificity underneath 
## the constraints of the current dataset. We will therefore move forward and use the ROC point of
## 0.301 to work on the data from 2014.
## ----------------------------------------------------------------------


## ----------------------------------------------------------------------
## ----------------------------------------------------------------------
## ------------------------
## 2014 Data
## ------------------------
## ----------------------------------------------------------------------
## ----------------------------------------------------------------------

## The dataset of 2014 does not have the information required to construct a "hs_dummy" variable.
## We therefore must create a new logistic regression equation

bestfit.logit.2014<-glm(outcome ~ freshman + ed + age +  a_rank  + interview + award,
                   family= binomial(logit), data = na.omit(oldData.reg), maxit = 100)
summary(bestfit.logit.2014)
exp(cbind(OR=coef(bestfit.logit.2014),confint.default(bestfit.logit.2014)))

## We must also find a new eta
ROC(form=outcome ~ freshman + ed + age +  a_rank  + interview + award, data=newData)

## Utilizing an eta = 0.216
prediction.matrix.2014<-matrix(0,ncol=4,nrow=length(newData$outcome))
colnames(prediction.matrix.2014)<-c("id", "outcome", "fitted probability","predicted outcome")
## Construct a matrix with four columns: "id", "outcome", "fitted probability", and "predictd outcomes"
prediction.matrix.2014[,1]<-newData$id
## Put the ids into column 1
prediction.matrix.2014[,2]<-newData$outcome
## Puts the predicted outcomes in column 2
fitted.values.2014 <- predict(bestfit.logit.2014,newData,type="response")
## Creates a vector of fitted values from bestfit.logit
prediction.matrix.2014[,3]<- fitted.values.2014
## Puts this vector into column 3
predicted.outcomes.2014<-ifelse(prediction.matrix.2014[,3]>=0.216,1,0)
## Creates a vector that rounds the fitted probabilities up or down from 0.5,
## simluating a positive (1) and negative (0) response
prediction.matrix.2014[,4]<-predicted.outcomes.2014
## Puts these predicted outcomes into column 4


## ----------------------------------------------------------------------
## Testing bestfit.logit.2014 performance
## ----------------------------------------------------------------------

true.positives.2014<-count(prediction.matrix.2014[,2]==1 & prediction.matrix.2014[,4]==1)[2,2]
true.positives.2014
## True positives where actual outcome matches predicted outcome
## 74 true positives
false.positives.2014<-count(prediction.matrix.2014[,2]==0 & prediction.matrix.2014[,4]==1)[2,2]
false.positives.2014
## False positives where actual outcomes do not match predicted outcomes
## 103 false positives
true.negatives.2014<-count(prediction.matrix.2014[,2]==0 & prediction.matrix.2014[,4]==0)[2,2]
true.negatives.2014
## True negatives where actual outcome matches predicted outcome
## 155 true negatives
false.negatives.2014<-count(prediction.matrix.2014[,2]==1 & prediction.matrix.2014[,4]==0)[2,2]
false.negatives.2014
## 22 false negatives.

sensitivity.2014<-true.positives.2014/sum(newData$outcome)
sensitivity.2014
## The optimal cut-off correctly identifies 77.08% of depositing students.
specificity.2014<-true.negatives.2014/(length(newData$outcome)-sum(newData$outcome))
specificity.2014
## The optimal cut-off correclty identifies non-depositing students 60.07% of the time.
accuracy.2014<-(true.negatives.2014 + true.positives.2014)/(length(newData$outcome))
accuracy.2014
## The optimal cut-off is 64.68% accurate
precision.2014<-(true.positives.2014)/(true.positives.2014 + false.positives.2014)
precision.2014
## The optimal cut-off is 41.81% precise


## ----------------------------------------------------------------------
## Does this outperform the Null Hypothesis as applied to the 2014 data?
## ----------------------------------------------------------------------

null.prediction.matrix.2014<-matrix(0,ncol=4,nrow=length(newData$outcome))
colnames(null.prediction.matrix.2014)<-c("id", "outcome", "fitted probability","predicted outcome")
## Construct a matrix with four columns: "id", "outcome", "fitted probability", and "predictd outcomes"
null.prediction.matrix.2014[,1]<-newData$id
## Put the ids into column 1
null.prediction.matrix.2014[,2]<-newData$outcome
## Puts the predicted outcomes in column 2
null.fitted.values.2014 <- predict(bestfit.logit.2014,newData,type="response")
## Creates a vector of fitted values from bestfit.logit
null.prediction.matrix.2014[,3]<- null.fitted.values.2014
## Puts this vector into column 3
set.seed(1234)
## For reproduction
null.predicted.outcomes.2014<-rbinom(length(newData$outcome),1,0.3)
## Creates a vector of random 1s and 0s
## simluating a positive (1) and negative (0) response by creating a binomial 
## distribution with rbinom(). The probability of success (1) is 30%
null.prediction.matrix.2014[,4]<-null.predicted.outcomes.2014
## Puts this predicted outcomes into column 4


null.true.positives.2014<-count(null.prediction.matrix.2014[,2]==1 & null.prediction.matrix.2014[,4]==1)[2,2]
null.true.positives.2014
## True positives where actual outcome matches predicted outcome
## 24 true positives
null.false.positives.2014<-count(null.prediction.matrix.2014[,2]==0 & null.prediction.matrix.2014[,4]==1)[2,2]
null.false.positives.2014
## False positives where actual outcomes do not match predicted outcomes
## 76 false positives
null.true.negatives.2014<-count(null.prediction.matrix.2014[,2]==0 & null.prediction.matrix.2014[,4]==0)[2,2]
null.true.negatives.2014
## True negatives where actual outcome matches predicted outcome
## 182 true negatives
null.false.negatives.2014<-count(null.prediction.matrix.2014[,2]==1 & null.prediction.matrix.2014[,4]==0)[2,2]
null.false.negatives.2014
## 72 false negatives.

null.sensitivity.2014<-null.true.positives.2014/length(newData$outcome)
null.sensitivity.2014
## The null hypothesis for 2014 data correctly identifies 6.77% of the depositing students.
null.specificity.2014<-null.true.negatives.2014/(length(newData$outcome)-sum(newData$outcome))
null.specificity.2014
## The null hypothesis for 2014 correctly identifies 70.54 % of non-depositing students.
null.accuracy.2014<-(null.true.negatives.2014+null.true.positives.2014)/(length(newData$outcome))
null.accuracy.2014
## The null hypothesis for 2014 is 58.19% accurate
null.precision.2014<-(null.true.positives.2014)/(null.true.positives.2014 + null.false.positives.2014)
null.precision.2014
## The null hypothesis for 2014 is 24% precise



## ----------------------------------------------------------------------
## ----------------------------------------------------------------------
## ------------------------
## Outcome
## ------------------------
## ----------------------------------------------------------------------
## ----------------------------------------------------------------------
##
## As measured by maximized sensitivity (correctly identifying true positives),
## The bestfit.logit.2014 model drastically outperforms the null model. It may therefore
## be of some value when attempting to forecast potential depositing students within the
## admitted student pool. 