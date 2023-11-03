#Lab7 Homework8
# Econometrics 
#Group Members: Zakaria Sule, Mohammed AlMuhaymin & MD Muhibul Islam


load("C:/Users/Zakaria Sule/Documents/acs2021_ny_data.RData")
attach(acs2021)
library(tidyverse)

library(plyr)
library(dplyr)
library(haven)
attach(acs2021)
summary(acs2021)

#We load the IND levels csv data which has two variables and 272 observations
levels_n <- read.csv("IND_levels.csv")
dim(levels_n)
view(levels_n)
table(levels_n)
#This renames the variables of the levels_n dataset
names(levels_n) <- c("New_Level","levels_orig")
acs2021$IND <- as.factor(acs2021$IND)
levels_orig <- levels(acs2021$IND)
levels_new <- join(data.frame(levels_orig),data.frame(levels_n))
view(levels_new)
acs2021$public_work <- acs2021$IND

#We load the public work record csv data which has 2 variables and 271 observations
levels_public <- read.csv("publicwork_recode.csv")
dim(levels_public)
names(levels_public) <- c("levels_orig","New_Level")
levels_new_pub <- join(data.frame(levels_orig),data.frame(levels_public))
summary(levels_new_pub)
view(levels_new_pub)
levels(acs2021$IND) <- levels_new$New_Level
levels(acs2021$public_work) <- levels_new_pub$New_Level

#Now we do some summary stats for our new data
# 36,265 people work in the public sector, both our imported data now have 270 observations and 2 variables
summary(acs2021$public_work)
summary(levels_new_pub)
summary(levels_new)
acs2021$public_work_num <- as.numeric(acs2021$public_work == "work for public, stable")
table(acs2021$public_work,acs2021$public_work_num)

#The subgroup we have chosen to look at are females between the age of 25-35 in the labor force, working year round and full time and also married.
#We chose female because our analysis is on female in the public work space.
#We had to change the subset because our interest variables gave us zero observations.
use_varb <- (acs2021$SEX=="Female") & (acs2021$AGE>=25) & (acs2021$AGE<=35) & (acs2021$LABFORCE == 2) & (acs2021$WKSWORK2 >4) & (acs2021$MARST == 1)


##### Output of our OLS Model ######
dat_use <- subset(acs2021,use_varb)
summary(dat_use)
#The dat_use has  amount 2,936 observations
ols_out1 <- lm(public_work_num ~ female + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data=dat_use)
summary(ols_out1)
#Call:
#  lm(formula = public_work_num ~ female + educ_hs + educ_somecoll + 
#       educ_college + educ_advdeg + AGE, data = dat_use)
#
#Residuals:
#  Min     1Q Median     3Q    Max 
#0      0      0      0      0 
#
#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)
#(Intercept)          0          0     NaN      NaN
#female              NA         NA      NA       NA
#educ_hs              0          0     NaN      NaN
#educ_somecoll        0          0     NaN      NaN
#educ_college         0          0     NaN      NaN
#educ_advdeg          0          0     NaN      NaN
#AGE                  0          0     NaN      NaN

#Residual standard error: 0 on 1241 degrees of freedom
#(1689 observations deleted due to missingness)
#Multiple R-squared:    NaN,	Adjusted R-squared:    NaN 
#F-statistic:   NaN on 5 and 1241 DF,  p-value: NA

#The P value is very indicating there is some significance, All of the parameters Pr(>|t|) < 0.05

#Lab 7
# from last time (Our OLS summary):
ols_out1 <- lm(public_work_num ~ female + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = dat_use)
summary(ols_out1)

pred_vals_ols1 <- predict(ols_out1, dat_use)
pred_model_ols1 <- (pred_vals_ols1 > mean(pred_vals_ols1))
#To try my prediction on this model, i will say 1,247 out of our 2,936 dat_use obs females work in the public sector jobs. 
#This represent a 42% female participation considering our subset data.
table(pred = pred_model_ols1, true = dat_use$public_work_num)
#       true
#pred       0
#FALSE 1247


#The logit Regression
model_logit1 <- glm(public_work_num ~ female + educ_hs 
                    + educ_somecoll + educ_college + educ_advdeg 
                    + AGE, data = dat_use, family = binomial
)
summary(model_logit1)
pred_vals <- predict(model_logit1, dat_use, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_use$public_work_num)
