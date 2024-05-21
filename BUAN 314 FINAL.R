########################## BUAN 370 Final Project ###################################

############### Load Libaries ################
library(ggplot2)
install.packages("readxl")
library(readxl)
install.packages("readr")
library(readr)
library(sqldf)
library(tidyverse)

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

colnames(Spotify)[colnames(Spotify)=="bpm"] <- "Beats_Per_Minute"

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

#Make Sure the Column Renaming was successful
colnames(Spotify)


# Scatter Plot: Relationship between danceability and popularity
ggplot(Spotify, aes(x = Danceability, y = Popularity)) +
  geom_point(alpha = 0.5, color = "orange") +
  labs(title = "Relationship between Danceability and Popularity", x = "Danceability", y = "Popularity") +
  theme_minimal()


##Average Popularity Throught The Years##

AvgYearlyPOP <- sqldf("SELECT AVG(Popularity), Artist_Type, Top_Year
                      FROM Spotify
                      GROUP By Top_Year, Artist_Type
                      ORDER BY Top_Year")
## Coercing as factors so they work on a bar plot##
Spotify$Top_Year <- as.factor((Spotify$Top_Year))
Spotify$Artist_Type <- as.factor(Spotify$Artist_Type)

##Plotting the Average yearly Popularity By Artist Type##
ggplot(Spotify, aes(x = Top_Year, y = Popularity, fill = Artist_Type)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  xlab("Year")

##Finding Distinct Genres##

Distinct_Genres <- sqldf("SELECT DISTINCT Top_Genre
      FROM Spotify"
      )

## Finding all types of Rap##
raponly <- sqldf("SELECT Song_Title, Top_Genre as Rap_music, Popularity as Avg_Pop
                FROM Spotify
                WHERE Top_Genre LIKE '%rap%'
                OR Top_Genre LIKE '%hip hop%'
                OR Top_Genre LIKE '%drill%'")

## making Rap Genres a single record ##
Rap <- sqldf("SELECT Avg(Avg_Pop) as RapPOP
      From raponly"
      )

##Finding all Pop music##
POPonly <- sqldf("SELECT Song_Title, Top_Genre as Pop_music, Popularity as Avg_Pop
                FROM Spotify
                WHERE Top_Genre LIKE '%pop%'
                AND Top_Genre NOT LIKE '%rap%'")

##Average Popularity of Pop Music##
POP<- sqldf("SELECT Avg(Avg_POP) as POP_avg
             FROM POPonly")

##Finding Everything Except Rap and Pop Music##
EverythingElse <- sqldf("SELECT Song_Title, Top_Genre as Other_Music, Popularity AS Avg_pop
                        FROM Spotify
                        WHERE Top_Genre NOT LIKE '%pop%'
                        AND Top_Genre NOT LIKE '%rap%'
                        AND Top_Genre NOT LIKE '%hip hop%'
                        AND Top_Genre NOT LIKE '%drill%'")

##Average Popularity of Everything excet for POP and Rap##
EvryEls <- sqldf("SELECT Avg(Avg_POP) as POP_avg
             FROM EverythingElse")
##Creating a data fram from the Queried data##
AMP <- c(EvryEls$POP_avg, POP$POP_avg, Rap$RapPOP)
Music_Genres <- c("Other", "Pop_Music", "Rap")
as.factor(Music_Genres)
Avg_Music_Popularity <-data.frame(Music_Genres, AMP)

##Make a bar Plot##
ggplot(Avg_Music_Popularity, aes(x = Music_Genres, y = AMP, fill = Music_Genres)) +
  geom_bar(stat = 'identity') +
  labs(y ="Average Music Popularity")
