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
usa_obs <- read_csv(file = "data/2018-2020-NPN-Records-USA.csv")

# Identify states with data for all three years
usa_complete <- usa_obs %>%
  group_by(State) %>%
  summarize(num_rows = n()) %>%
  filter(num_rows == 9)

# Remove any province/states without all three years of data
usa_obs <- usa_obs %>%
  filter(State %in% usa_complete$State)

# Sum number of records by state and year
usa_counts <- usa_obs %>%
  group_by(State, Year) %>%
  summarize(num_records = sum(Records) )

# Plot number of records 
obs_plot <- ggplot(data = usa_counts, mapping = aes(x = Year, 
                                                        y = num_records,
                                                        group = State, 
                                                        color = State)) +
  geom_line() + 
  scale_y_log10() +
  theme(legend.position = "none")
obs_plot

# Calculate percent change in records 2018-2019 and 2019-2020 
# by spring (Mar-May) and state
usa_obs_change <- usa_counts %>% 
  group_by(State) %>%
  mutate(prev_obs = lag(num_records)) %>%
  filter(!is.na(prev_obs)) %>%
  mutate(per_change_obs = (((num_records/prev_obs)-1) * 100)) %>%
  select(-prev_obs) %>% 
  mutate(comparison = if_else(Year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))

# Calculate means
usa_obs_means <- usa_obs_change %>%
  group_by(comparison) %>%
  summarize(mean_obs = mean(per_change_obs))
usa_obs_means

# Plot percent change in records
change_usa_obs_plot <- ggplot(data = usa_obs_change, 
                              mapping = aes(x = comparison, 
                                            y = per_change_obs,
                                            group = State, 
                                            color = State)) +
  geom_line() +
  theme(legend.position = "none")
change_usa_obs_plot

# Create dataframe for records t test
usa_obs_ttest <- usa_obs_change %>% 
  select(State, Year, per_change_obs) %>%
  pivot_wider(names_from = Year, values_from = per_change_obs) %>%
  rename(`2018-2019` = `2019`, 
         `2019-2020` = `2020`) 

# t test on change in growth in records
usa_obs_ttest_list <- t.test(x = usa_obs_ttest$`2018-2019`, 
                             y = usa_obs_ttest$`2019-2020`, 
                             paired = TRUE)
usa_obs_ttest_list
cat(round(usa_obs_ttest_list$estimate, digits = 6), " (",
    paste0(round(as.numeric(usa_obs_ttest_list$conf.int), digits = 4), collapse = ", "),
    ")", sep = "")
