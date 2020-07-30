# Analyses of Arizona iNat data re COVID19 (%change by county)
# Kathleen L Prudic
# klprudic@arizona.edu
# created 2020-07-09

# Remove wonton variables 
rm(list = ls())

# Load additional packages
library(tidyverse)
library(lubridate)

# Load data 
arizona_obs <- read_csv(file = "data/2018-2020-iNat-Arizona.csv")

# Filter data to have months March, April, May only
arizona_spring <- arizona_obs %>%
  filter(month(observed_on) %in% c(3, 4, 5))

# Count number of unique users and observations by county for 2018, 2019, 2020
# Column names are county, year, users, observations
arizona_counts <- arizona_spring %>%
  group_by(place_county_name, year(observed_on)) %>%
  summarize(num_observations = n(), 
            num_users = length(unique(user_id)))

# Rename columns from place_county_name to county and year(observed_on) to year
arizona_counts <- arizona_counts %>% 
  rename(county = place_county_name, 
         year = "year(observed_on)")

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

# Calculate percent change in observations 2018-2019 and 2019-2020 
# by spring (Mar-May) and county
# ((observations in year t/ observations in year t-1) -1) * 100

arizona_obs_change <- arizona_counts %>% 
  mutate(prev_obs = lag(num_observations)) %>%
  filter(!is.na(prev_obs)) %>%
  mutate(per_change_obs = (((num_observations/prev_obs)-1) * 100)) %>%
  select(-prev_obs) %>% 
  mutate(comparison = if_else(year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))

# Plot percent change in observations
change_obs_plot <- ggplot(data = arizona_obs_change, 
                          mapping = aes(x = comparison, 
                                        y = per_change_obs,
                                        group = county, 
                                        color = county)) +
  geom_line()
change_obs_plot


# Calculate percent change in users 2018-2019 and 2019-2020 
# by spring (Mar-May) and county

arizona_users_change <- arizona_counts %>% 
  mutate(prev_users = lag(num_users)) %>%
  filter(!is.na(prev_users)) %>%
  mutate(per_change_users = (((num_users/prev_users)-1) * 100)) %>%
  select(-prev_users) %>% 
  mutate(comparison = if_else(year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))

# Plot percent change in users

change_users_plot <- ggplot(data = arizona_users_change, 
                          mapping = aes(x = comparison, 
                                        y = per_change_users,
                                        group = county, 
                                        color = county)) +
  geom_line()
change_users_plot

# Create dataframe for observations t test
arizona_obs_ttest <- arizona_obs_change %>% 
  select(county, year, per_change_obs) %>%
  pivot_wider(names_from = year, values_from = per_change_obs)

# t test on change in growth in observations

obs_t_test <- t.test(x = arizona_obs_ttest$`2019`, 
                     y = arizona_obs_ttest$`2020`, 
                     paired = TRUE)

# Create dataframe for users t test
arizona_users_ttest <- arizona_users_change %>% 
  select(county, year, per_change_users) %>%
  pivot_wider(names_from = year, values_from = per_change_users)

# t test on change in growth in users

users_t_test <- t.test(x = arizona_users_ttest$`2019`, 
                     y = arizona_users_ttest$`2020`, 
                     paired = TRUE)

