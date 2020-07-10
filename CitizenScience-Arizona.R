# Description of what script does
# Kathleen L Prudic
# klprudic@arizona.edu
# created 2020-07-09

# Remove wonton variables 
rm(list = ls())

# Load additional packages
library(tidyverse)
library(lubridate)

# Load data 
arizona_obs <- read_csv(file = "data/iNaturalist-Arizona.csv")

# Filter data to have months March, April, May only
arizona_spring <- arizona_obs %>%
  filter(month(observed_on) %in% c(3, 4, 5))

# Count number of unique users and observations by county for 2018, 2019, 2020
# Column names are county, year, users, observations
arizona_counts <- arizona_spring %>%
  group_by(place_county_name, year(observed_on)) %>%
  summarize(num_observations = n(), 
            num_users = length(unique(user_id)))

# Rename columns
colnames(arizona_counts)[1:2] <- c("county", "year")
  
# Plot number of observations 
obs_plot <- ggplot(data = arizona_counts, mapping = aes(x = year, 
                                                        y = num_observations,
                                                        group = county, 
                                                        color = county)) +
  geom_line()
obs_plot

# Plot number of users
users_plot <- ggplot(data = arizona_counts, mapping = aes (x = year, 
                                                           y = num_users,
                                                           group = county,
                                                           color = county)) +
  geom_line()
users_plot
