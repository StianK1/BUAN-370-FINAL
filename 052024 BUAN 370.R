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

# Bar Chart: Distribution of songs by genre
ggplot(Spotify, aes(x = Top_Genre)) +
  geom_bar(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Distribution of Songs by Top_Genre", x = "Top_Genre", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pie Chart: Proportion of songs released each year
ggplot(Spotify, aes(x = "", fill = as.factor(Year_Released))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Songs Released Each Year", fill = "Year") +
  theme_void()

# Histogram: Distribution of song durations
ggplot(Spotify, aes(x = Beats_Per_Minute/60000)) +
  geom_histogram(binwidth = 0.5, fill = "lightcoral", color = "black") +
  labs(title = "Distribution of Song Durations", x = "Duration (minutes)", y = "Frequency") +
  theme_minimal()

# Scatter Plot: Relationship between danceability and popularity
ggplot(Spotify, aes(x = Danceability, y = Popularity)) +
  geom_point(alpha = 0.5, color = "orange") +
  labs(title = "Relationship between Danceability and Popularity", x = "Danceability", y = "Popularity") +
  theme_minimal()

# Box Plot: Distribution of tempo by genre
ggplot(Spotify, aes(x = Top_Genre, y = Beats_Per_Minute, fill = Top_Genre)) +
  geom_boxplot() +
  labs(title = "Distribution of Tempo by Genre", x = "Genre", y = "Tempo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Line Plot: Trend of energy over the years
ggplot(Spotify, aes(x = Year_Released, y = Energy)) +
  geom_line(color = "green") +
  geom_point(color = "green") +
  labs(title = "Trend of Energy Over the Years", x = "Year", y = "Mean Energy") +
  theme_minimal()

# Sort the songs by Danceability and Rnergy
top_songs <- Spotify[order(-Song_Title$Danceability, -Song_Title$Energy), ]
# Take the top 10 songs
top_10 <- top_songs[1:10, ]
# Plot the scatter plot
ggplot(top_songs, aes(x = Danceability, y = Rnergy, label = Song_Title)) +
  geom_point(color = "blue") +
  geom_text_repel() +  # Add text labels with geom_text_repel for better label placement
  labs(Song_Title = "Top 10 Songs with Highest Danceability and Energy",
       x = "Danceability", y = "Energy")

# Find the correlation of the variables
data <- data.frame("Beats_Per_Minute", "Energy", "Danceability", "Decibel_Meter", "Duration")

cm <- cor(data, use = "complete.obs")

print(cm)

# Create the heatmap (let me know if this runs for you, it wasnt running for me)
heatmap(cm, 
        Rowv = NA, 
        Colv = NA, 
        col = colorRampPalette(c("blue", "white", "red"))(20), 
        xlab = "Variables", 
        ylab = "Variables", 
        main = "Correlation Heatmap")

correlation_Area <- cor(Energy, Danceability)
correlation_Area

correlation_bath <- cor(Energy, Danceability)
correlation_bath

correlation_stories <- cor(Energy, Danceability)
correlation_stories


