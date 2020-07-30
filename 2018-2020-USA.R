# Analyses of USA iNaturalist data re COVID19 (%change by state)
# Kathleen L Prudic
# klprudic@arizona.edu
# created 2020-07-09
# DOI: https://doi.org/10.15468/dl.4prgpy

# Remove wonton variables 
rm(list = ls())

# Load additional packages
library(tidyverse)
library(lubridate)
library(data.table)

# Load data large dataset have to use data.table, ugh
# Despite the file extension (.csv) it is a tab delimited file, GBIF!
global_obs <- fread(file = "data/2018-2020-iNaturalist-Global.csv")
  
# Select columns only related to analyses
cols_keep <- c("year", "month", "countryCode", "stateProvince", 
               "occurrenceID", "recordedBy")

# DT[, keep, with = FALSE]
global_obs <- global_obs[, cols_keep, with = FALSE] 

# Filter to USA data only
# DT[V2 > 5]
usa_obs <- global_obs[countryCode == "US"]

# Convert to tibble, not tribble
usa_obs_tbl <- as_tibble(usa_obs)

# Count number of unique users and observations by state for 2018, 2019, 2020
# Column names are year, month, countryCode, stateProvince, occurrenceID, recordedBy
# Remove non-USA states
usa_counts <- usa_obs_tbl %>%
  filter(nchar(stateProvince) > 0) %>%
  filter(stateProvince != "Baja California") %>%
  filter(stateProvince != "Midway") %>%
  group_by(stateProvince, year) %>%
  summarize(num_observations = n(), 
            num_users = length(unique(recordedBy)))

# Plot number of observations 
usa_obs_plot <- ggplot(data = usa_counts, mapping = aes(x = year, 
                                                        y = num_observations,
                                                        group = stateProvince, 
                                                        color = stateProvince)) +
  geom_line() + 
  scale_y_log10() +
  theme(legend.position = "none")
usa_obs_plot

# Calculate percent change in observations 2018-2019 and 2019-2020 
# by spring (Mar-May) and state
# ((observations in year t/ observations in year t-1) -1) * 100

usa_obs_change <- usa_counts %>% 
  mutate(prev_obs = lag(num_observations)) %>%
  filter(!is.na(prev_obs)) %>%
  mutate(per_change_obs = (((num_observations/prev_obs)-1) * 100)) %>%
  select(-prev_obs) %>% 
  mutate(comparison = if_else(year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))

# Plot percent change in observations
change_usa_obs_plot <- ggplot(data = usa_obs_change, 
                          mapping = aes(x = comparison, 
                                        y = per_change_obs,
                                        group = stateProvince, 
                                        color = stateProvince)) +
  geom_line() +
  theme(legend.position = "none")
change_usa_obs_plot

# Plot number of users
usa_users_plot <- ggplot(data = usa_counts, mapping = aes (x = year, 
                                                           y = num_users,
                                                           group = stateProvince,
                                                           color = stateProvince)) +
  geom_line() +
  scale_y_log10() +
  theme(legend.position = "none")
usa_users_plot

# Calculate percent change in users 2018-2019 and 2019-2020 
# by spring (Mar-May) and state
usa_users_change <- usa_counts %>% 
  mutate(prev_users = lag(num_users)) %>%
  filter(!is.na(prev_users)) %>%
  mutate(per_change_users = (((num_users/prev_users)-1) * 100)) %>%
  select(-prev_users) %>% 
  mutate(comparison = if_else(year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))

# Plot percent change in users
change_usa_users_plot <- ggplot(data = usa_users_change, 
                            mapping = aes(x = comparison, 
                                          y = per_change_users,
                                          group = stateProvince, 
                                          color = stateProvince)) +
  geom_line() +
  theme(legend.position = "none")
change_usa_users_plot

# Create dataframe for observations t test
usa_obs_ttest <- usa_obs_change %>% 
  select(stateProvince, year, per_change_obs) %>%
  pivot_wider(names_from = year, values_from = per_change_obs)

# t test on change in growth in unique observations
obs_t_test <- t.test(x = usa_obs_ttest$`2019`, 
                     y = usa_obs_ttest$`2020`, 
                     alternative = "greater",
                     paired = TRUE)

# Create dataframe for users t test
usa_users_ttest <- usa_users_change %>% 
  select(stateProvince, year, per_change_users) %>%
  pivot_wider(names_from = year, values_from = per_change_users)

# t test on change in growth in unique users
users_t_test <- t.test(x = usa_users_ttest$`2019`, 
                       y = usa_users_ttest$`2020`,
                       alternative = "greater",
                       paired = TRUE)
