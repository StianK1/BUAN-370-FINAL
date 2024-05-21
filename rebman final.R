
# Load necessary libraries
library(readxl)
library(ggplot2)


Spotify 2016 2019 Top 100 Songs2010-2019


library(readxl)
library(ggplot2)

# Read the data from the Excel file
file_path <- "Spotify 2016-2019 Top 100 Songs.xlsx"  # Ensure this path is correct
spotify_data <- Spotify_2016_2019_Top_100_Songs

# Create a boxplot to visualize the relationship between artist type and bpm
ggplot(spotify_data, aes(x = `artist type`, y = bpm, fill = `artist type`)) +
  geom_boxplot() +
  labs(title = "Distribution of BPM by Artist Type",
       x = "Artist Type",
       y = "BPM") +
  theme_minimal()
# 2. Danceability over the Years
ggplot(spotify_data, aes(x = `year released`, y = dnce, color = `year released`)) +
  geom_jitter(width = 0.2, height = 0) +
  stat_summary(fun = mean, geom = "line", size = 1.5) +
  labs(title = "Danceability over the Years",
       x = "Year Released",
       y = "Danceability",
       color = "Year Released") +
  theme_minimal()


# 4. Popularity by Artist Type
ggplot(spotify_data, aes(x = `artist type`, y = pop, fill = `artist type`)) +
  geom_boxplot() +
  labs(title = "Popularity by Artist Type",
       x = "Artist Type",
       y = "Popularity") +
  theme_minimal()


