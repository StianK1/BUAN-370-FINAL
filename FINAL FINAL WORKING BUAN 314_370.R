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

################### exploratory analysis ########################

## VISUALIZATION 1: Artist Type Popularity Over the Years ##

# Load necessary libraries
if (!require(gridExtra)) {
  install.packages("gridExtra")
  library(gridExtra)
}

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)

data <- Spotify
# Aggregate data by artist type and year
`Artist_Type_Counts` <- data %>%
  count(`Year_Released`, `Artist_Type`)

# Create a list to store plots
plots <- list()

# Loop through each year to create pie charts
years <- c(2016, 2017, 2018, 2019)
for (year in years) {
  year_data <- Artist_Type_Counts %>%
    filter(`Year_Released` == year)
  
  p <- ggplot(year_data, aes(x = "", y = n, fill = `Artist_Type`)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = paste("Artist Type Popularity in", year),
         x = "",
         y = "",
         fill = "Artist_Type") +
    theme_void() +
    theme(legend.position = "right")
  
  # Add the plot to the list
  plots[[as.character(year)]] <- p
}
# Arrange and print all four pie charts in a single plot
grid.arrange(grobs = plots, ncol = 2)

##VISUALIZATION 2 HISTOGRAM DANCEABILITY OVER THE YEARS##

ggplot(Spotify, aes(x = `Year_Released`, y = Danceability, color = `Year_Released`)) +
  geom_jitter(width = 0.2, height = 0) +
  stat_summary(fun = mean, geom = "line", size = 1.5) +
  labs(title = "Danceability over the Years",
       x = "Year Released",
       y = "Danceability",
       color = "Year Released") +
  theme_minimal()

## VISUALIZATION 3: Scatter Plot Relationship between danceability and popularity ##

ggplot(Spotify, aes(x = Danceability, y = Popularity)) +
  geom_point(alpha = 0.5, color = "orange") +
  labs(title = "Relationship between Danceability and Popularity", x = "Danceability", y = "Popularity") +
  theme_minimal()

## VISUALIZATION 4: SCATTERPLOT POPULARITY AND DANCEABILITY ##

# Create a boxplot to visualize the relationship between artist type and bpm
ggplot(spotify_data, aes(x = `artist type`, y = bpm, fill = `artist type`)) +
  geom_boxplot() +
  labs(title = "Distribution of BPM by Artist Type",
       x = "Artist Type",
       y = "BPM") +
  theme_minimal()

## VISUALIZATION 5: BOXPLOT POPULARITY BY ARTIST TYPE ##
ggplot(Spotify, aes(x = `Artist_Type`, y = `Popularity`, fill = `Artist_Type`)) +
  geom_boxplot() +
  labs(title = "Popularity by Artist Type",
       x = "Artist Type",
       y = "Popularity") +
  theme_minimal()

## VISUALIZATION 6: Finding Distince Genres ##
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
# Read the data from the Excel file
file_path <- "Spotify 2016-2019 Top 100 Songs.xlsx"  # Ensure this path is correct
spotify_data <- Spotify_2016_2019_Top_100_Songs

# Create a boxplot to visualize the relationship between artist type and bpm
ggplot(Spotify, aes(x = `Artist_Type`, y = Beats_Per_Minute, fill = `Artist_Type`)) +
  geom_boxplot() +
  labs(title = "Distribution of BPM by Artist Type",
       x = "Artist Type",
       y = "BPM") +
  theme_minimal()

## QUERY 1 ##
View(Query1)

## QUERY2 - Who are the most popular Artists??##

Query2 <- sqldf("SELECT Artist_Name, 
                SUM(Popularity) AS Popularity_Total, 
                COUNT(Song_Title) as Number_Of_Songs, 
                AVG(Popularity) as Avg_Popularity_per_Song,
                Artist_Type
                FROM Spotify
                GROUP BY Artist_Name
                ORDER BY Popularity_Total DESC")
View(Query2)

## Query3 - Who Are the top 50 Biggest On Hit Wonders? ##

Query3 <- sqldf("SELECT Song_Title,
                Artist_Name,
                Popularity
                FROM Spotify
                GROUP BY Artist_Name
                HAVING COUNT(Song_Title) = 1
                ORDER BY Popularity DESC
                LIMIT 50;")
View(Query3)

##QUERY4 Teamwork makes the dream work!! Who are the best DUO/TRIO/Bands that make the Best Music##

Query4 <- sqldf("SELECT
                Artist_Name,
                AVG(Popularity) AS Avg_Music_Popularity,
                Artist_Type
                FROM Spotify
                WHERE Artist_Type NOT LIKE '%Solo%'
                GROUP BY Artist_Name
                ORDER BY Avg_Music_Popularity DESC")
View(Query4)

##QUERY 5 - PARTY TIME! Parties last Roughly 4 Hours Which is 14400/2 min per song = 120 songs seconds. What songs have the best Energy and Dancability combo for my Playlist!##

Query5 <- sqldf("SELECT
                DISTINCT Song_Title,
                Artist_Name,
                Top_Genre,
                Duration,
                Energy + Danceability AS Rage_Score
                FROM Spotify
                ORDER BY Rage_Score DESC
                Limit 120;")
View(Query5)

