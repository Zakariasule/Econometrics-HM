#Homework 3
#LAB2
#Group Members: Zakaria Sule, Mishal and Akash.

load("C:/Users/Zakaria Sule/Documents/Household_Pulse_data_w57.RData")
library(tidyverse)
attach(Household_Pulse_data)
summary(Household_Pulse_data)
#This gave me the summary stats of RECVDVACC variable,
summary(Household_Pulse_data$RECVDVACC)
#Picking a subset in order to focus on the observations we are interested in.
#this also takes of NAs in the RECVDVACC variable.
Restrict <- (Household_Pulse_data$RECVDVACC=="yes got vaxx") | (Household_Pulse_data$RECVDVACC=="no did not get vaxx")
Vaccdatanew<- subset(Household_Pulse_data, Restrict)
summary(Vaccdatanew)

#The group used the GENID_DESCRIBE variable to check that with their vacination interest, more especially among males and females.
summary(Household_Pulse_data$GENID_DESCRIBE)
# Count the number of males who got vaccinated
male_vaccinated_count <- sum(sample_data_vaxx$GENID_DESCRIBE== "male" & sample_data_vaxx$RECVDVACC == "yes got vaxx")
# The number of males who were vaccinated is 21405.
# Count the number of males who didnt vaccinated
male_unvaccinated_count <- sum(sample_data_vaxx$GENID_DESCRIBE == "male" & sample_data_vaxx$RECVDVACC == "no did not get vaxx")
# The number of males who were vaccinated is 3114.

# Count the number of females who got vaccinated.
female_vaccinated_count <- sum(sample_data_vaxx$GENID_DESCRIBE == "female" & sample_data_vaxx$RECVDVACC == "yes got vaxx")
## The number of females who were vaccinated is 28484.

# Count the number of females who didnt get vaccinated.
female_unvaccinated_count <- sum(sample_data_vaxx$GENID_DESCRIBE == "female" & sample_data_vaxx$RECVDVACC == "no did not get vaxx")
# The number of females who were did not get vaccinated was 4336.  
# what are the age ranges of men who got vaccinated?
#Filter data for men who got vaccinated
filteredmen_vax <- (Household_Pulse_data$RECVDVACC == "yes got vaxx" ) | (Household_Pulse_data$GENID_DESCRIBE == "male")
filtmensample_data_vaxx <- subset(Household_Pulse_data,filteredmen_vax)


