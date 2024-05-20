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
colnames(Spotify)[colnames(Spotify)=="title"] <- "Song_Title"

colnames(Spotify)[colnames(Spotify)=="artist"] <- "Artist_Name"

colnames(Spotify)[colnames(Spotify)=="top genre"] <- "Top_Genre"

colnames(Spotify)[colnames(Spotify)=="year released"] <- "Year_Released"

colnames(Spotify)[colnames(Spotify)=="bpm"] <- "Beat_Per_Minute"

colnames(Spotify)[colnames(Spotify)=="nrgy"] <- "Energy"

colnames(Spotify)[colnames(Spotify)=="dnce"] <- "Danceability"

colnames(Spotify)[colnames(Spotify)=="dB"] <- "Decibel_Meter"

colnames(Spotify)[colnames(Spotify)=="live"] <- "Live"

colnames(Spotify)[colnames(Spotify)=="val"] <- "Positivity"

colnames(Spotify)[colnames(Spotify)=="dur"] <- "Duration"

colnames(Spotify)[colnames(Spotify)=="acous"] <- "Acoustic"

colnames(Spotify)[colnames(Spotify)=="spch"] <- "Speech"

colnames(Spotify)[colnames(Spotify)=="pop"] <- "Popularity"

colnames(Spotify)[colnames(Spotify)=="top year"] <- "Top_Year"

colnames(Spotify)[colnames(Spotify)=="artist type"] <- "Artist_Type"



