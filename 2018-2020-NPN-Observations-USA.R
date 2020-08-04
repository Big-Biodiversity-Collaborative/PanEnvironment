# Analyses of USA National Phenology Network data re COVID19 (%change by state)
# Kathleen L Prudic
# klprudic@arizona.edu
# created 2020-07-09

# Remove wonton variables 
rm(list = ls())

# Load additional packages
library(tidyverse)
library(lubridate)

# Load data 
usa_observations <- read_csv(file = "data/2018-2020-NPN-Observations-USA.csv")

# Filter for months 3-7
usa_spring <- usa_observations %>%
  filter(month %in% c(3:7)) %>%
  group_by(state, year) %>%
  summarize(total_obs = sum(count_obs))

# Identify states with data for all three years
usa_complete <- usa_spring %>%
  group_by(state) %>%
  summarize(num_year = n()) %>%
  filter(num_year == 3)

# Remove any province/states without all three years of data
usa_observations <- usa_spring %>%
  filter(state %in% usa_complete$state)

# Calculate percent change in users 2018-2019 and 2019-2020 
# by spring (Mar-May) and state
usa_obs_change <- usa_observations %>% 
  group_by(state) %>%
  mutate(prev_obs = lag(total_obs)) %>%
  filter(!is.na(prev_obs)) %>%
  mutate(per_change_obs = (((total_obs/prev_obs)-1) * 100)) %>%
  select(-prev_obs) %>% 
  mutate(comparison = if_else(year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))

# Calculate means
usa_obs_means <- usa_obs_change %>%
  group_by(comparison) %>%
  summarize(mean_obs = mean(per_change_obs))
usa_obs_means

# Plot percent change in users
change_usa_obs_plot <- ggplot(data = usa_obs_change, 
                                mapping = aes(x = comparison, 
                                              y = per_change_obs,
                                              group = state, 
                                              color = state)) +
  geom_line() +
  theme(legend.position = "none")
change_usa_obs_plot

# Create dataframe for users t test
usa_obs_ttest <- usa_obs_change %>% 
  select(state, year, per_change_obs) %>%
  pivot_wider(names_from = year, values_from = per_change_obs) %>%
  rename(`2018-2019` = `2019`, 
         `2019-2020` = `2020`) 

# t test on change in growth in unique users
usa_obs_ttest_list <- t.test(x = usa_obs_ttest$`2018-2019`, 
                               y = usa_obs_ttest$`2019-2020`,
                               alternative = "greater",
                               paired = TRUE)
usa_obs_ttest_list
cat(round(usa_obs_ttest_list$estimate, digits = 6), " (",
    paste0(round(as.numeric(usa_obs_ttest_list$conf.int), digits = 4), collapse = ", "),
    ")", sep = "")
