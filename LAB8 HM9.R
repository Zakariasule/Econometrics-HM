#Lab8

library(tidyverse)
getwd()
setwd("C:/Users/Zakaria Sule/Documents/Econometrics datasets")
load("C:/Users/Zakaria Sule/Documents/Econometrics datasets/acs2021_ny_data.RData")
attach(acs2021)


library(plyr)
library(dplyr)
library(haven)
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

summary(acs2021$SEX)
view(acs2021)

#The subgroup we have chosen to look at are people between the age of 25-35 in the labor force, working year round and full time and also married..
#We had to change the subset because our interest variables gave us zero observations.
# I was curious about African American race so i created a subgroup that contain only the Negro race and run the ols for that.
use_varb <- (acs2021$RACE==2)&(acs2021$AGE>=25) & (acs2021$AGE<=35) & (acs2021$LABFORCE == 2) & (acs2021$WKSWORK2 >4) & (acs2021$MARST == 1)


##### Output of our OLS Model ######
dat_use <- subset(acs2021,use_varb)
summary(dat_use)
dim(dat_use)
# Our subset the dat_use(the negro race subgroup) has  amount 250 observations and returns the 120 variables.
# Now we run our OLS regression for female participation in the public work space
ols_out1 <- lm(public_work_num ~ female + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data=dat_use)
summary(ols_out1)

#Call:
#lm(formula = public_work_num ~ female + educ_hs + educ_somecoll + 
#     educ_college + educ_advdeg + AGE, data = dat_use)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-0.5579 -0.3602 -0.2304  0.4422  0.8885 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   1.111e-01  7.767e-02   1.431 0.152590    
#female        1.298e-01  1.212e-02  10.705  < 2e-16 ***
#  educ_hs       6.962e-02  3.719e-02   1.872 0.061215 .  
#educ_somecoll 1.346e-01  3.711e-02   3.627 0.000290 ***
#  educ_college  1.188e-01  3.581e-02   3.319 0.000909 ***
#  educ_advdeg   3.165e-01  3.624e-02   8.732  < 2e-16 ***
#  AGE           1.517e-05  2.204e-03   0.007 0.994509    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.4571 on 5937 degrees of freedom
#Multiple R-squared:  0.07209,	Adjusted R-squared:  0.07115 
#F-statistic: 76.87 on 6 and 5937 DF,  p-value: < 2.2e-16




#Lab 7 Logit
# from last time (Our OLS summary):
ols_out1 <- lm(public_work_num ~ female + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = dat_use)
summary(ols_out1)

pred_vals_ols1 <- predict(ols_out1, dat_use)
pred_model_ols1 <- (pred_vals_ols1 > mean(pred_vals_ols1))
summary(pred_model_ols1)
summary(pred_vals_ols1)
#To try my prediction on this model, the  250 dat_use obs, 123 negros work in the public sector jobs. 
#This represent a 49% female participation considering our independent chosen education variables and age group.
table(pred = pred_model_ols1, true = dat_use$public_work_num)
#       true
#pred  0  1
#FALSE 84 43
#TRUE  50 73
# 73 female out of the total subgroup are predicted to be true and actually true.
# while 50 are predicted to be true and actually false. 

#The logit Regression
model_logit1 <- glm(public_work_num ~ female + educ_hs 
                    + educ_somecoll + educ_college + educ_advdeg 
                    + AGE, data = dat_use, family = binomial
)
summary(model_logit1)
pred_vals <- predict(model_logit1, dat_use, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
summary(pred_model_logit1)
#For the logit model, the females predicted to participate in the pubic work are 114, represent 45%  
table(pred = pred_model_logit1, true = dat_use$public_work_num)
# Also the 71 are predicted to be true and actually true
# While 43 are Predicted to be false and actually false.


# LAB 8 Homework 9

# our subgroup
use_varb <- (acs2021$RACE==2)&(acs2021$AGE>=25) & (acs2021$AGE<=35) & (acs2021$LABFORCE == 2) & (acs2021$WKSWORK2 >4) & (acs2021$MARST == 1)
dat_use <- subset(acs2021,use_varb)
summary(dat_use)


# for now, really simplify the education dummy
dat_use$BA_plus <- dat_use$educ_college + dat_use$educ_advdeg

# whole dataset
model_lpm_v1 <- lm(public_work_num ~ female + BA_plus + AGE + I(female*BA_plus) + I(AGE * female), data = dat_use)
summary(model_lpm_v1)

# Creating a subgroup for both males and females
dat_use_female <- subset(dat_use,as.logical(dat_use$female))
dat_use_male <- subset(dat_use,!(dat_use$female))

# now split into 2 parts
# Regressing the this education variable (BA_Plus) and age in different models to see female and male participation in the public work sector
# From the output of both models, the estimate of the coefficients in the female model is low than in the male model.
# The standard errors in the male model is low than in the male model therefore the precision of the estimate in the male is greater than in the female model. 
model_lpm_v1f <- lm(public_work_num ~ BA_plus + AGE, data = dat_use_female)
summary(model_lpm_v1f)
model_lpm_v1m <- lm(public_work_num ~ BA_plus + AGE, data = dat_use_male)
summary(model_lpm_v1m)

###Random Forest###
install.packages("randomForest")
library(randomForest)

set.seed(54321)
model_randFor <- randomForest(as.factor(pub_work) ~ ., data = sobj$data, importance=TRUE, proximity=TRUE)
print(model_randFor)
round(importance(model_randFor),2)
varImpPlot(model_randFor)

# look at confusion matrix for this too
pred_model1 <- predict(model_randFor,  s_dat_test)
table(pred = pred_model1, true = dat_test$pub_work)


install.packages("e1071")
library(e1071)
# tuned_parameters <- tune.svm(as.factor(pub_work) ~ ., data = sobj$data, gamma = 10^(-3:0), cost = 10^(-2:2)) 
# summary(tuned_parameters)
# figure best parameters and input into next
svm.model <- svm(as.factor(pub_work) ~ ., data = sobj$data, cost = 1, gamma = 0.1)
svm.pred <- predict(svm.model, s_dat_test)
table(pred = svm.pred, true = dat_test$pub_work)
