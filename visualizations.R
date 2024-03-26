# Load packages 
library(tidyverse)
library(ggcorrplot)


# Set working directory to wherever cloned repository is located locally
setwd("... /Vinyl_Collection_Visualizations")

# Read in the data
ds <- read.csv("data/music_collection.csv")

# Histogram of album duration in minutes
ggplot(ds, aes(x = Duration)) +
  geom_histogram(bins = 28, aes(y = ..density..), colour = "black", fill = "white") +
  theme(
    axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0),
    legend.position = "none"
  ) +
  geom_vline(aes(xintercept = mean(Duration)), color = "blue", linetype = "dashed", size = 1) +
  geom_density(alpha = .2, fill = "#FF6666", outline.type = "full") +
  labs(
    x = "Duration (minutes)",
    y = "Density",
    title = "Duration Distribution"
  )

# Density of album durations split by format
ggplot(ds, aes(x = Duration, fill = Format)) +
  geom_density(alpha = .2, outline.type = "full") +
  geom_vline(aes(xintercept = mean(Duration)), color = "blue", linetype = "dashed", size = 1) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0)) +
  labs(
    x = "Duration (minutes)",
    y = "Density",
    title = "Duration Distributions by Type"
  )

# High frequency of artists
artist_counts <- table(ds$Artist)
simple_table <- as.data.frame(artist_counts)
simple_table <- filter(simple_table, rank(desc(Freq)) <= 16)
ggplot(simple_table, aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Artist",
    y = "Frequency",
    title = "Artists with three or more releases"
  )

# Histogram of years it took to produce vinyl after albums release date
ggplot(ds, aes(x = Vinyl.Release.Year - Album.Release.Year)) +
  geom_histogram(bins = 50, aes(y = ..density..), colour = "black", fill = "white") +
  theme(
    axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0),
    legend.position = "none"
  ) +
  geom_vline(aes(xintercept = mean(Vinyl.Release.Year - Album.Release.Year)),
    color = "blue", linetype = "dashed", size = 1
  ) +
  geom_density(alpha = .2, fill = "#FF6666", outline.type = "full") +
  labs(
    x = "Years to Produce",
    y = "Density",
    title = "Vinyl Release - Album Release"
  )


# Densities of album releases and vinyl releases
ds1 <- transmute(ds, year = Album.Release.Year, Release = "Album Release")
ds2 <- transmute(ds, year = Vinyl.Release.Year, Release = "Vinyl Release")
ds3 <- rbind(ds1, ds2)
ggplot(ds3, aes(x = year, fill = Release)) +
  geom_density(alpha = .2, outline.type = "full") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0)) +
  labs(
    x = "Year",
    y = "Density",
    title = "Album Release and Vinyl Release Distributions"
  )

# Plot of vinyl release year vs.nalbum release year
ggplot(ds, aes(x = Album.Release.Year, y = Vinyl.Release.Year)) +
  geom_point(alpha = 0.5) +
  theme(
    axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0),
    legend.position = "none"
  ) +
  labs(
    x = "Album Release Year",
    y = "Vinyl Release Year",
    title = "Vinyl Release Year vs. Album Release Year"
  )

# Density of album release year
ggplot(ds, aes(x = Album.Release.Year, fill = "red")) +
  geom_density(alpha = .2, outline.type = "full") +
  geom_vline(aes(xintercept = mean(Album.Release.Year)),
    color = "blue", linetype = "dashed", size = 1
  ) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0)) +
  labs(
    x = "Album Release Year",
    y = "Density",
    title = "Distribution of Album Release Year"
  ) +
  guides(fill = FALSE)

# Density of album release year by format
ggplot(ds, aes(x = Album.Release.Year, fill = Format)) +
  geom_density(alpha = .2, outline.type = "full") +
  geom_vline(aes(xintercept = mean(Album.Release.Year)),
    color = "blue", linetype = "dashed", size = 1
  ) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0)) +
  labs(
    x = "Album Release Year",
    y = "Density",
    title = "Distribution of Album Release Year by Type"
  )

# Density of album release year by format
ggplot(ds, aes(x = Vinyl.Release.Year, fill = Format)) +
  geom_density(alpha = .2, outline.type = "full") +
  geom_vline(aes(xintercept = mean(Vinyl.Release.Year)),
    color = "blue", linetype = "dashed", size = 1
  ) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0)) +
  labs(
    x = "Vinyl Release Year",
    y = "Density",
    title = "Distribution of Vinyl Release Year by Type"
  )

# Histogram of track counts
ggplot(ds, aes(x = Track.Count)) +
  geom_histogram(bins = 28, aes(y = ..density..), colour = "black", fill = "white") +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0), legend.position = "none") +
  geom_vline(aes(xintercept = mean(Track.Count)), color = "blue", linetype = "dashed", size = 1) +
  geom_density(alpha = .2, fill = "#FF6666", outline.type = "full") +
  labs(
    x = "Track Count",
    y = "Density",
    title = "Distribution of Track Count"
  )

# Density of track counts by format
ggplot(ds, aes(x = Track.Count, fill = Format)) +
  geom_density(alpha = .2, outline.type = "full") +
  geom_vline(aes(xintercept = mean(Track.Count)),
    color = "blue", linetype = "dashed", size = 1
  ) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0)) +
  labs(
    x = "Track Count",
    y = "Density",
    title = "Distribution of Track Count by Type"
  )

# Plot of track count vs. duration colored by format
ggplot(ds, aes(x = Duration, y = Track.Count, color = Format)) +
  geom_point() +
  geom_smooth(method = "loess", alpha = .15, fullrange = TRUE, color = "black") +
  labs(
    x = "Duration (minutes)",
    y = "Track Count",
    title = "Track Count vs. Duration"
  )

# Correlation matrix of numeric variables
numeric_cols <- c("Duration", "Album.Release.Year", "Vinyl.Release.Year", "Track.Count")
ds[numeric_cols] <- lapply(ds[numeric_cols], as.numeric)
corr_matrix <- cor(ds[numeric_cols])
ggcorrplot(corr_matrix)
