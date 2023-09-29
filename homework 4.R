#Homework 4 Lab3
#Group Members: Zakaria Sule & Akash


load("C:/Users/Zakaria Sule/Documents/Household_Pulse_data_w57.RData")
library(tidyverse)
library(caret)
library(class)
attach(Household_Pulse_data)

#To remind myself the number of variables and obs in the Household Pulse data i used the dim function to look it up.
dim(Household_Pulse_data)
summary(Household_Pulse_data$KIDGETVAC_12_17Y)KIDGETVAC_12_17Y
#I understood that this code below tookout all NAs in the KIDGETVAC_12_17Y variable.
dat_kidvaxx_nonmissing <- subset(Household_Pulse_data, (Household_Pulse_data$KIDGETVAC_12_17Y != "NA") )
summary(dat_kidvaxx_nonmissing)

#This basically transform all the levels in the kid who get vaccinated  between the age 12-17 into numeric(similiar to the 5 number summary) 
temp1 <- fct_recode(dat_kidvaxx_nonmissing$KIDGETVAC_12_17Y, '5' = 'kids 12-17yo definitely get vaxx',
                    '4'='kids 12-17yo probably get vaxx', '3'='unsure kids 12-17yo get vaxx',
                    '2'='kids 12-17yo probably NOT get vaxx', '1'='kids 12-17yo definitely NOT get vaxx',
                    '3'='do not know plans for vaxx for kids 12-17yo')
summary(temp1)

# I understand that this code converts factor to numeric.
kidsvax1217 <- as.numeric(levels(temp1))[temp1]
summary(kidsvax1217)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   1.000   1.000   1.618   2.000   5.000 

#It is observed from the summary stats of kidsvax1217 variable (the mean) is skewed more to 1(kids 12-17yo definitely NOT get vaxx).
# This tells us that more of the kids from 12-17 definately did not get vaccinated.

norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE)  )
}

# I understand that this code converts factors to numbers, using the order of the levels of my chosen variables, Race and Level of education
# I decided to choose Race in place of states, then my classification will be education and race which has 3 levels to see
# whats the different interest of black or white people getting their kids vaccinated
data_use_prelim <- data.frame(norm_varb(as.numeric(dat_kidvaxx_nonmissing$EEDUC)),norm_varb(as.numeric(dat_kidvaxx_nonmissing$RRACE)))

good_obs_data_use <- complete.cases(data_use_prelim,dat_kidvaxx_nonmissing$KIDGETVAC_12_17Y)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(kidsvax1217,good_obs_data_use)

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]

summary(cl_data)
summary(train_data)


for (indx in seq(1, 9, by= 2)) {
  pred_y <- knn3Train(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_y == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}

cl_data_n <- as.numeric(cl_data)
summary(as.factor(cl_data_n))
names(train_data) <- c("norm_educ","norm_race")


model_ols1 <- lm(cl_data_n ~ train_data$norm_educ + train_data$norm_race)

y_hat <- fitted.values(model_ols1)

mean(y_hat[cl_data_n == 2])
mean(y_hat[cl_data_n == 3])
mean(y_hat[cl_data_n == 4])
mean(y_hat[cl_data_n == 5])

# here try classifying the technique one at a time with OLS

cl_data_n2 <- as.numeric(cl_data_n == 2) 
# here this is now binary 1 or 0, depending whether the condition is true or false

model_ols_v2 <- lm(cl_data_n2 ~ train_data$norm_educ + train_data$norm_race)
y_hat_v2 <- fitted.values(model_ols_v2)
mean(y_hat_v2[cl_data_n2 == 1])
mean(y_hat_v2[cl_data_n2 == 0])
#The model became hard to expalin from my classification, but in general the coefficients of the models of course indicates a correlation between interest of vacinating kids between 12-17.
#  we are always reminded that modeling in regression is not cause-effect relationship, but rather correlation since a dependent variable many relation with other variables.

