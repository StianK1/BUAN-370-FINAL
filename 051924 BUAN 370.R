########################## BUAN 370 Final Project ###################################

############### Load Libaries ################
library(ggplot2)

#Import Data
Customer <- read.csv(#instert cleaned data here#)

View(Customer)

#Check for NA Values
sum(is.na(Customer))

#Summary of the Data
summary(Customer)

# Identifying the class of each variable
# Using sapply()
sapply(Customer, class)

####################### Cleaning ########################
# Rename the Columns

