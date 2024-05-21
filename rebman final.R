
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

if (!require(gridExtra)) {
  install.packages("gridExtra")
  library(gridExtra)
}

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)
data <- spotify_data
# Aggregate data by artist type and year
artist_type_counts <- data %>%
  count(`year released`, `artist type`)

# Create a list to store plots
plots <- list()

# Loop through each year to create pie charts
years <- c(2016, 2017, 2018, 2019)
for (year in years) {
  year_data <- artist_type_counts %>%
    filter(`year released` == year)
  
  p <- ggplot(year_data, aes(x = "", y = n, fill = `artist type`)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = paste("Artist Type Popularity in", year),
         x = "",
         y = "",
         fill = "Artist Type") +
    theme_void() +
    theme(legend.position = "right")
  
  # Add the plot to the list
  plots[[as.character(year)]] <- p
}
# Arrange and print all four pie charts in a single plot
grid.arrange(grobs = plots, ncol = 2)

  