# Econometrics Exam1
# Zakaria Sule
# 10/12/2023

# Q1
#table(Educ_BAplus,wfh)

#       wfh    some wfh   no
#BAplus  6838   7218    15973
#no      2640   2230    20336

# I decided to drop all no's and combine wfh and some wfh and has BA and call it firstgrouping.
# and i combined all wfh and some who wfh but has no BA and call it secondgrouping
firstgrouping <- c(6838, 7218)
secondgrouping <- c(2640,2230 )
# I run a summary stats for these two groupings and there sim to be some differences between their mean value.
# Lets do better with a hypothesis test between these groups. 
summary(firstgrouping)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6838    6933    7028    7028    7123    7218 
summary(secondgrouping)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2230    2332    2435    2435    2538    2640 

# Running one sample t-test (the hypothesis test) for each group. i just showed the output, i will the explaination in the two sample t-test.
t.test(firstgrouping)
#	One Sample t-test
#data:  firstgrouping
#t = 36.989, df = 1, p-value = 0.01721
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
#  4613.821 9442.179
#sample estimates:
#  mean of x 
#7028 

t.test(secondgrouping)
#data:  secondgrouping
#t = 11.878, df = 1, p-value = 0.05347
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
#  -169.772 5039.772
#sample estimates:
#  mean of x 
#2435 

#Now lets do welch two sample t-test for the two groups and call it test_results.
test_results <- t.test(firstgrouping, secondgrouping)
#Welch Two Sample t-test

#data:  firstgrouping and secondgrouping
#t = 16.432, df = 1.9886, p-value = 0.003775
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  3383.719 5802.281
#sample estimates:
#  mean of x mean of y 
#7028      2435 

#Now lets extract the statistics in this hypothesis individually.
p_value <- test_results$p.value
print(p_value)
#[1] 0.003774771

estimate_value <- test_results$estimate
print(estimate_value)
#mean of x mean of y 
#7028      2435 

standarderror_value <- test_results$stderr
print(standarderror_value)
#[1] 279.5085

t_stat <- test_results$statistic
print(t_stat)
#       t 
# 16.43242 

confidence_interval1 <- test_results$conf.int
print(confidence_interval1)
#[1] 3383.719 5802.281
#attr(,"conf.level")
#[1] 0.95

# So my hypothesis of the two groups was actually making camparison between all people who work from home and has bachelors(BA) and those who work from who but has no bachelors(BA).
# Between these two groups our alternative hypothesis is that "true difference in mean is not equal to 0" as oppose to our null/default hypothesis "mean equal to 0. this is a two tailed test
#With a confidence level 95 our significant level(Alpha) is 0.05. with a p-value equal of 0.003774771 which is very small as compared to the alpha it means there is a statistically significant difference between the average/mean values of the true groups therefore strong evidence to reject the null hypothesis. 
detach()



#Q2
load("C:/Users/Zakaria Sule/Documents/Household_Pulse_data_w57.RData")
library(tidyverse)
# Im running a summary and dim to glance through the overall dataset i'm going to use.
attach(Household_Pulse_data)
summary(Household_Pulse_data)
dim(Household_Pulse_data)

xtabs(~ kids_vax1[(REGION == "Northeast")] + Educ_BAplus[(REGION == "Northeast")], data = Household_Pulse_data)

xtabs(~kids)
detach()

#Q3



#Q4
load("C:/Users/Zakaria Sule/Documents/acs2017_ny_data.RData")
attach(acs2017_ny)
#running a summary to check which variables or column to work with.
summary(acs2017_ny)

#choosing a subgroup to find something interesting.
#I wanna find out how many males or female has a doctorate degree
summary(acs2017_ny$SEX)
summary(acs2017_ny$EDUCD=="Doctoral degree ")

combataX <- (acs2017_ny$SEX) & (acs2017_ny$EDUCD=="Doctoral degree ")


comb <- sum(acs2017_ny$SEX & acs2017_ny$EDUCD)


summary(combda)
acs2017_ny$


Q4
# I wanna do a regression for number of hous and rooms, how gender affect the education level.
mod1 <- lm(UHRSWORK~ROOMS)
summary(mod1)
cor_coe <- cor(UHRSWORK, ROOMS)
# Create a scatter plot
plot(UHRSWORK, ROOMS, main = "Scatter Plot of x and y", xlab = "x", ylab = "y", pch = 19, col = "blue")

# Add a regression line
abline
detach()

#Q4
# KNN 
a
library(caret)
library(class)
summary(Household_Pulse_data$KIDGETVAC_12_17Y)
dat_kidvaxx_nonmissing <- subset(Household_Pulse_data, (Household_Pulse_data$KIDGETVAC_12_17Y != "NA") )

summary(dat_kidvaxx_nonmissing)
mytemp <- fct_recode(dat_kidvaxx_nonmissing$KIDGETVAC_12_17Y, '5' = 'kids 12-17yo definitely get vaxx',
                    '4'='kids 12-17yo probably get vaxx', '3'='unsure kids 12-17yo get vaxx',
                    '2'='kids 12-17yo probably NOT get vaxx', '1'='kids 12-17yo definitely NOT get vaxx',
                    '3'='do not know plans for vaxx for kids 12-17yo')
summary(mytemp)



