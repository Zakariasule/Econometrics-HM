#Homework5/Lab4
#group members: Zakaria Sule, Akash Persaud, & Mohammed Almuhaymin


load("C:/Users/Zakaria Sule/Documents/acs2017_ny_data.RData")
library(tidyverse)
dim(acs2017_ny)
attach(acs2017_ny)

#Interesting how this two functions provide outline dataset, using both functions gives you a diiferent view.
# summary function basically provide the summary stats for all the 109 variables while glimpse gives the observation with the vectoe type (i.e., integer, double or even both)
summary(acs2017_ny)
glimpse(acs2017_ny)

#The code below creates a subset from the dataset of people aged 25 to 55, people in the labor force and working for wage, people who work more than 4 weeks and lastly people who worked more than 35 hours a week.
# Run a summary stats and dim on the new subsetted data and the observations drooped to 46,971.
use_varb <- (acs2017_ny$AGE >= 25) & (acs2017_ny$AGE <= 55) & (acs2017_ny$LABFORCE == 2) & (acs2017_ny$WKSWORK2 > 4) & (acs2017_ny$UHRSWORK >= 35)
dat_use <- subset(acs2017_ny,use_varb)
detach()
summary(dat_use)
xtabs(dat_use)
dim(dat_use)

# creates a linear regression model
#to predict the variable INCWAGE (income) based on several predictor variables
model_temp1 <- lm(INCWAGE ~ AGE + female + AfAm + Asian + Amindian + race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + educ_advdeg, data = dat_use)
summary(model_temp1)
#Below gives the 5-number summary of the model_temp1
#  Min      1Q  Median      3Q     Max 
#-148088  -33205  -10708   13053  625543 

#with 0.05 significance level, the p-value: < 2.2e-16, this shows the predicting variables actually has some effect or degree of correlation with the dependant variable.
# The stargazer function did indeed looked fancier than summary.
library(stargazer)
stargazer(model_temp1, type = "text")

library(AER)
# This require(AER) but thats built in this version of R.
# subset in order to plot...
NNobs <- length(dat_use$INCWAGE)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <-subset(dat_use,graph_obs)  

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = dat_graph)
# ^^ that looks like crap since Wages are soooooooo skew!  So try to find some sensible ylim = c(0, ??)
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)
# discus what you see in this plot
#In a way wages almost evenly distributed between the prime age of 25 to 55 but its a bit skewed toward high age of 55 which makes sense, moew years of working more income.

# change this line to fit your choices about explanatory variables
to_be_predicted2 <- data.frame(AGE = 25:55, female = 1, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, Hispanic = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)
lines(yhat ~ AGE, data = to_be_predicted2)





#This is expanded by the group, more credit to Akash!
# creates a new linear regression model
#to predict the variable INCWAGE (income) based on several predictor variables

model_temp2 <- lm(INCWAGE ~ AGE + female  + educ_hs + educ_somecoll + educ_college + educ_advdeg + in_NYC, data = dat_use)
summary(model_temp2)

require(stargazer)
stargazer(model_temp2, type = "text")

############ AGE #################
# Estimate: 1366.59
# Std. Error: 39.89
# t value: 34.258
# p-value: < 2e-16 (Extremely small)
# The hypothesis test for AGE:
# Null Hypothesis (H0): The coefficient of AGE is equal to zero suggesting that AGE has no effect on INCWAGE.
# Alternative Hypothesis (Ha): The coefficient of AGE is not equal to zero suggesting that AGE has an effect on INCWAGE.
# The t-value is very large (34.258), indicating that the coefficient for AGE is highly statistically significant. The extremely small p-value (< 2e-16) also confirms that AGE significantly affects INCWAGE. The confidence interval for AGE would not include zero, further supporting the alternative hypothesis.

############ FEMALE #################
# Estimate: -25,752.000
# Std. Error: 718.971
# t value: -35.818
# p-value: < 2e-16 (Extremely small)
# The hypothesis test for female:
# Null Hypothesis (H0): The coefficient of female is equal to zero suggesting female has no effect on INCWAGE.
# Alternative Hypothesis (Ha): The coefficient of female is not equal to zero suggesting female has an effect on INCWAGE.
# The t-value is very large (-35.818), indicating that the coefficient for female is highly statistically significant. The extremely small p-value (< 2e-16) confirms that female significantly affects INCWAGE.

############ EDUC_HS #################
# Estimate: 13,589.190
# Std. Error: 1,790.844
# t value: 7.588
# p-value: 3.30e-14 (Very small)
# The hypothesis test for educ_hs:
# Null Hypothesis (H0): The coefficient of educ_hs is equal to zero suggesting educ_hs has no effect on INCWAGE.
# Alternative Hypothesis (Ha): The coefficient of educ_hs is not equal to zero suggesting educ_hs has an effect on INCWAGE.
# The t-value is 7.588, and the p-value is very small (3.30e-14), indicating that educ_hs significantly affects INCWAGE.

################# educ_somecoll #################
# 
# Estimate: 26,270.440
# Std. Error: 1,827.974
# t value: 14.371
# p-value: < 2e-16 (Extremely small)
# The hypothesis test for educ_somecoll:
# 
# Null Hypothesis (H0): The coefficient of educ_somecoll is equal to zero suggesting educ_somecoll has no effect on INCWAGE).
# Alternative Hypothesis (Ha): The coefficient of educ_somecoll is not equal to zero suggesting educ_somecoll has an effect on INCWAGE.
# The t-value is 14.371, and the p-value is extremely small (< 2e-16), indicating that educ_somecoll significantly affects INCWAGE.
# 
################# educ_college #################
# Estimate: 61,305.060
# Std. Error: 1,785.734
# t value: 34.330
# p-value: < 2e-16 (Extremely small)
# The hypothesis test for educ_college:
# 
# Null Hypothesis (H0): The coefficient of educ_college is equal to zero suggesting educ_college has no effect on INCWAGE.
# Alternative Hypothesis (Ha): The coefficient of educ_college is not equal to zero suggesting educ_college has an effect on INCWAGE.
# The t-value is 34.330, and the p-value is extremely small (< 2e-16), indicating that educ_college significantly affects INCWAGE.
# 
#################educ_advdeg #################
# 
# Estimate: 87,486.440
# Std. Error: 1,828.693
# t value: 47.841
# p-value: < 2e-16 (Extremely small)
# The hypothesis test for educ_advdeg:
# 
# Null Hypothesis (H0): The coefficient of educ_advdeg is equal to zero suggesting educ_advdeg has no effect on INCWAGE.
# Alternative Hypothesis (Ha): The coefficient of educ_advdeg is not equal to zero suggesting educ_advdeg has an effect on INCWAGE.
# The t-value is 47.841, and the p-value is extremely small (< 2e-16), indicating that educ_advdeg significantly affects INCWAGE.
# 
#################in_NYC#################
# Estimate: 3,937.484
# Std. Error: 735.575
# t value: 5.353
# p-value: 8.69e-08 (Very small)
# The hypothesis test for in_NYC:
# Null Hypothesis (H0): The coefficient of in_NYC is equal to zero suggesting in_NYC has no effect on INCWAGE).
# Alternative Hypothesis (Ha): The coefficient of in_NYC is not equal to zero suggesting in_NYC has an effect on INCWAGE).
# The t-value is 5.353, and the p-value is very small (8.69e-08), indicating that in_NYC significantly affects INCWAGE.


coeftest(model_temp1,vcovHC)
coeftest(model_temp2,vcovHC)

require(AER)
# subset in order to plot...
NNobs <- length(dat_use$INCWAGE)
set.seed(12345)
graph_obs <- (runif(NNobs) < 0.1) 
dat_graph <-subset(dat_use,graph_obs)  

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = dat_graph)

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)
# change this line to fit your choices about explanatory variables
to_be_predicted2 <- data.frame(AGE = 25:55, female = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 0 ,in_NYC=1)
to_be_predicted2$yhat <- predict(model_temp2, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)
