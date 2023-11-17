# Econometrics Exam2
# Zakaria Sule
# 11/16/2023



####### Q2 #######
getwd()
setwd("C:/Users/Zakaria Sule/Documents/Econometrics datasets")
load("C:/Users/Zakaria Sule/Documents/Econometrics datasets/acs2021_recoded.RData")
library(tidyverse)
library(haven)
# i use the dim and summary to check my data the dataset very huge.
dim(acs2021)
summary(acs2021)
attach(acs2021)

# I created A subgroup of the data of both male and female gender between the age of 18 to 64 since 18 is the legal age of adulthood to move or migrate, included people who were born here and all sttes of the US.
# The subgroup also included people who has 2 years of college education and married, and also include people who live int he state they were born and people living in the state there were not born
# my subgroup has 1883823 obs and 112 same variables 
Use_varb <-  (acs2021$AGE >=18) & (acs2021$AGE <=64) & (acs2021$SEX == "Male") | (acs2021$SEX == "Female") & (acs2021$born_in_USstate == "born in a state in the US") & (acs2021$STATEFIP == 1 ) & (acs2021$EDUC == "2 years of college") & (acs2021$MARST == 1) & (acs2021$live_same_state_born == 1) | (acs2021$live_same_state_born == 0)
My_subgroup <- subset(acs2021, Use_varb)
summary(My_subgroup)
dim(My_subgroup)
table(My_subgroup)
detach()


####### Q3 #######
# my 2 sets of OLS regressions. One set splits the sample into men and women, the other set uses interaction terms.
# My gender splits
male_var <- (My_subgroup$SEX == "Male")
female_var <- (My_subgroup$SEX == "Female")
# Below give my ols set 1 for the male and female variables
Ols_Reg1 <- lm(live_same_state_born ~ female_var + male_var + AGE, data = My_subgroup)
summary(Ols_Reg1)

# Call:
# lm(formula = live_same_state_born ~ female_var + male_var + AGE, 
#   data = My_subgroup)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.56786 -0.38324 -0.01642  0.49169  0.62271 

# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     5.679e-01  7.271e-04   781.0   <2e-16 ***
#   female_varTRUE -4.204e-01  5.875e-04  -715.6   <2e-16 ***
#   male_varTRUE           NA         NA      NA       NA    
# AGE            -2.978e-03  1.426e-05  -208.8   <2e-16 ***
#  ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#  standard error: 0.3874 on 1883820 degrees of freedom
# Multiple R-squared:  0.2422,	Adjusted R-squared:  0.2422 
# F-statistic: 3.01e+05 on 2 and 1883820 DF,  p-value: < 2.2e-16

# The second ols
Ols_Reg2 <- lm(live_same_state_born ~ EDUC + AGE, data = My_subgroup)
summary(Ols_Reg1)

# Call:
# lm(formula = live_same_state_born ~ female_var + male_var + AGE, 
$    data = My_subgroup)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.56786 -0.38324 -0.01642  0.49169  0.62271 

# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     5.679e-01  7.271e-04   781.0   <2e-16 ***
#   female_varTRUE -4.204e-01  5.875e-04  -715.6   <2e-16 ***
#   male_varTRUE           NA         NA      NA       NA    
#  AGE            -2.978e-03  1.426e-05  -208.8   <2e-16 ***
#  ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3874 on 1883820 degrees of freedom
# Multiple R-squared:  0.2422,	Adjusted R-squared:  0.2422 
# F-statistic: 3.01e+05 on 2 and 1883820 DF,  p-value: < 2.2e-16

lm(live_same_state_born ~ AGE + I(AGE^2) + EDU + (EDU*AGE^2)  + I(AGE*2), data = My_subgroup )
pred_Ols1_Reg1 <- predict(Ols_Reg1, My_subgroup, type = "response")
pred_Ols1_Reg1_model <- (pred_Ols1_Reg1>0.5)
table(pred=pred_Ols1_Reg1_model, true =My_subgroup$live_same_state_born)


####### Q4 #######
# Now i Estimate a better OLS model for whether people live in the same state as born.
# In here my dependent or responsive variable 
Ols_Reg3 <- lm(live_same_state_born ~ born_in_USstate + EDUC + AGE + MARST + STATEFIP, data = My_subgroup)
summary(Ols_Reg1)

# Call:
# lm(formula = live_same_state_born ~ female_var + male_var + AGE, 
#    data = My_subgroup)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.56786 -0.38324 -0.01642  0.49169  0.62271 

# Coefficients: (1 not defined because of singularities)
$ Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     5.679e-01  7.271e-04   781.0   <2e-16 ***
#   female_varTRUE -4.204e-01  5.875e-04  -715.6   <2e-16 ***
#   male_varTRUE           NA         NA      NA       NA    
# AGE            -2.978e-03  1.426e-05  -208.8   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.3874 on 1883820 degrees of freedom
# Multiple R-squared:  0.2422,	Adjusted R-squared:  0.2422 
# F-statistic: 3.01e+05 on 2 and 1883820 DF,  p-value: < 2.2e-16

# table(acs2021$AGE)
# summary(acs2021$born_in_USstate )

####### Q5 #######

# Now I Estimate a simple logit model, for the outcome variable live_same_state_born, within my subsample.

Logit_model1 <- glm(live_same_state_born ~ SEX + EDUC + AGE + born_in_USstate + MARST, data = My_subgroup)
summary(Logit_model1)
# The output of my logit model gives:

# Call:
# glm(formula = live_same_state_born ~ SEX + EDUC + AGE + born_in_USstate + 
#      MARST, data = My_subgroup)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               2.788e-01  1.811e-03  153.96   <2e-16 ***
#   SEXFemale                                -3.636e-01  5.595e-04 -649.82   <2e-16 ***
#   EDUCNursery school to grade 4            -2.029e-01  2.248e-03  -90.26   <2e-16 ***
#   EDUCGrade 5, 6, 7, or 8                  -4.541e-02  2.041e-03  -22.25   <2e-16 ***
#   EDUCGrade 9                               2.918e-02  2.662e-03   10.96   <2e-16 ***
#   EDUCGrade 10                              7.494e-02  2.575e-03   29.10   <2e-16 ***
#   EDUCGrade 11                              1.375e-01  2.386e-03   57.63   <2e-16 ***
#   EDUC1 year of college                     1.206e-01  1.715e-03   70.33   <2e-16 ***
#   EDUC2 years of college                    1.379e-01  1.829e-03   75.41   <2e-16 ***
#   EDUC4 years of college                    8.104e-02  1.660e-03   48.82   <2e-16 ***
#   EDUC5+ years of college                   2.434e-02  1.718e-03   14.17   <2e-16 ***
#   AGE                                      -3.598e-03  1.526e-05 -235.71   <2e-16 ***
#   born_in_USstateborn in a state in the US  2.534e-01  6.425e-04  394.47   <2e-16 ***
$   MARST                                     1.757e-03  1.323e-04   13.29   <2e-16 ***
#  ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for gaussian family taken to be 0.1307083)

# Null deviance: 373152  on 1883822  degrees of freedom
# Residual deviance: 246229  on 1883808  degrees of freedom
# AIC: 1512897

# Number of Fisher Scoring iterations: 2

detach()  

####### Q6 #######
# Etimating other models than ols or logit
library(randomForest)

#Random Forest
data.frame(My_subgroup)
set.seed(54321)
model_randFor <- randomForest(as.factor(live_same_state_born) ~ AGE + EDUC, data = My_subgroup, importance=TRUE, proximity=TRUE)
print(model_randFor)
round(importance(model_randFor),2)
varImpPlot(model_randFor)

# Now I look confusion matrix for this too
pred_model1 <- predict(model_randFor,  s_dat_test)
table(pred = pred_model1, true = My_subgroup$live_same_state_born)

# Aother model is: Support Vector Machines
require(e1071)
# tuned_parameters <- tune.svm(as.factor(live_same_state_born) ~ AGE + EDUC, data = my_subgroup, gamma = 10^(-3:0), cost = 10^(-2:2)) 
# summary(tuned_parameters)
# figure best parameters and input into next
svm.model <- svm(as.factor(pub_work) ~ ., data = sobj$data, cost = 1, gamma = 0.1)
svm.pred <- predict(svm.model, s_dat_test)
table(pred = svm.pred, true = dat_test$pub_work)



