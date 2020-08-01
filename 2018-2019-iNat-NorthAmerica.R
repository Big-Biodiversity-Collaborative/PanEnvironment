# Analyses of North America iNaturalist data re COVID19 (%change by state/province)
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

# Convert to a tibble, not a tribble
global_obs_tbl <- as_tibble(global_obs)

# Filter to CAN, MEX, USA data only
# DT[V2 > 5]
noram_obs <- global_obs_tbl %>%
  filter(countryCode %in% c("CA", "MX", "US"))

# Count number of unique users and observations by state/provience for 2018, 2019, 2020
# Column names are year, month, countryCode, stateProvince, occurrenceID, recordedBy
noram_counts <- noram_obs %>%
  filter(nchar(stateProvince) > 0) %>%
  group_by(stateProvince, year) %>%
  summarize(num_observations = n(), 
            num_users = length(unique(recordedBy)))

# Plot number of observations 
noram_obs_plot <- ggplot(data = noram_counts, mapping = aes(x = year, 
                                                              y = num_observations,
                                                              group = stateProvince, 
                                                              color = stateProvince)) +
  geom_line() + 
  scale_y_log10() +
  theme(legend.position = "none")
noram_obs_plot

# Calculate percent change in observations 2018-2019 and 2019-2020 
# by spring (Mar-May) and state/province
# ((observations in year t/ observations in year t-1) -1) * 100

noram_obs_change <- noram_counts %>% 
  mutate(prev_obs = lag(num_observations)) %>%
  filter(!is.na(prev_obs)) %>%
  mutate(per_change_obs = (((num_observations/prev_obs)-1) * 100)) %>%
  select(-prev_obs) %>% 
  mutate(comparison = if_else(year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))

# Plot percent change in observations
change_noram_obs_plot <- ggplot(data = noram_obs_change, 
                                 mapping = aes(x = comparison, 
                                               y = per_change_obs,
                                               group = stateProvince, 
                                               color = stateProvince)) +
  geom_line() +
  theme(legend.position = "none")
change_noram_obs_plot

# Plot number of users
noram_users_plot <- ggplot(data = noram_counts, mapping = aes (x = year, 
                                                                 y = num_users,
                                                                 group = stateProvince,
                                                                 color = stateProvince)) +
  geom_line() +
  scale_y_log10() +
  theme(legend.position = "none")
noram_users_plot

# Calculate percent change in users 2018-2019 and 2019-2020 
# by spring (Mar-May) and nation
noram_users_change <- noram_counts %>% 
  mutate(prev_users = lag(num_users)) %>%
  filter(!is.na(prev_users)) %>%
  mutate(per_change_users = (((num_users/prev_users)-1) * 100)) %>%
  select(-prev_users) %>% 
  mutate(comparison = if_else(year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))

# Plot percent change in users
change_noram_users_plot <- ggplot(data = noram_users_change, 
                                   mapping = aes(x = comparison, 
                                                 y = per_change_users,
                                                 group = stateProvince, 
                                                 color = stateProvince)) +
  geom_line() +
  theme(legend.position = "none")
change_noram_users_plot

# Create dataframe for observations t test
noram_obs_ttest <- noram_obs_change %>% 
  select(stateProvince, year, per_change_obs) %>%
  pivot_wider(names_from = year, values_from = per_change_obs) %>%
  rename(`2018-2019` = `2019`, 
         `2019-2020` = `2020`) 

# t test on change in growth in unique observations
noram_obs_ttest_list <- t.test(x = noram_obs_ttest$`2018-2019`, 
                                y = noram_obs_ttest$`2019-2020`, 
                                alternative = "greater",
                                paired = TRUE)
noram_obs_ttest_list
cat(round(noram_obs_ttest_list$estimate, digits = 6), " (",
    paste0(round(as.numeric(noram_obs_ttest_list$conf.int), digits = 4), collapse = ", "),
    ")", sep = "")

# Create dataframe for observers t test
noram_users_ttest <- noram_users_change %>% 
  select(stateProvince, year, per_change_users) %>%
  pivot_wider(names_from = year, values_from = per_change_users) %>%
  rename(`2018-2019` = `2019`, 
         `2019-2020` = `2020`) 

# t test on change in growth in unique observers
noram_users_ttest_list <- t.test(x = noram_users_ttest$`2018-2019`, 
                                  y = noram_users_ttest$`2019-2020`,
                                  alternative = "greater",
                                  paired = TRUE)
noram_users_ttest_list
cat(round(noram_users_ttest_list$estimate, digits = 5), " (",
    paste0(round(as.numeric(noram_users_ttest_list$conf.int), digits = 4), collapse = ", "),
    ")", sep = "")
