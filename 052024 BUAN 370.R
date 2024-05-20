########################## BUAN 370 Final Project ###################################

############### Load Libaries ################
library(ggplot2)
install.packages("readxl")
library(readxl)
install.packages("readr")
library(readr)


#Import Data
Spotify <- read_csv("https://raw.githubusercontent.com/StianK1/BUAN-370-FINAL/main/Spotify%202016-2019%20Top%20100%20Songs%20(2).csv")

View(Spotify)

#Check for NA Values
sum(is.na(Spotify))

#Summary of the Data
summary(Spotify)

# Identifying the class of each variable
# Using sapply()
sapply(Spotify, class)

####################### Cleaning ########################
# Rename the Columns

