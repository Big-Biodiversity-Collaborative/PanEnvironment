# Analyses of Arizona eButterfly data re COVID19 (%change by county)
# Kathleen L Prudic
# klprudic@arizona.edu
# created 2020-07-30

# Remove wonton variables 
rm(list = ls())

# Load additional packages
library(tidyverse)
library(lubridate)

# Load data 
ebutt_obs <- read_csv(file = "data/2018-2020-eButt.csv")

# Remove countries with less than 15 observations and observers
ebutt_usa <- ebutt_obs %>%
  filter(Country == "United States") %>%
  mutate(Year = year(`Date Observed`))

# Count and sum number of unique observations and observers
usa_counts <- ebutt_usa %>%
  group_by(`Province/State`, Year) %>%
  summarize(num_observations = n(),
            num_users = length(unique(Observer)))

# Identify province/state with all 3 years of data
usa_complete <- usa_counts %>%
  group_by(`Province/State`) %>%
  summarize(num_year = n()) %>%
  filter(num_year == 3)

# Remove any province/states without all three years of data
usa_counts <- usa_counts %>%
  filter(`Province/State` %in% usa_complete$`Province/State`)


# Plot number of observations 
usa_obs_plot <- ggplot(data = usa_counts, mapping = aes(x = Year, 
                                                        y = num_observations,
                                                        group = `Province/State`, 
                                                        color = `Province/State`)) +
  geom_line() + 
  scale_y_log10() +
  theme(legend.position = "none")
usa_obs_plot

################################################################################
# Observations
# Calculate percent change in observations 2018-2019 and 2019-2020 
# by spring (Mar-May) and state/province
usa_obs_change <- usa_counts %>% 
  group_by(`Province/State`) %>%
  mutate(prev_observations = lag(num_observations)) %>%
  filter(!is.na(prev_observations)) %>%
  mutate(per_change_obs = (((num_observations/prev_observations)-1) * 100)) %>%
  select(-prev_observations) %>% 
  mutate(comparison = if_else(Year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))

# Calculate means
usa_obs_means <- usa_obs_change %>% 
  group_by(comparison) %>%
  summarize(mean_obs = mean(per_change_obs))
usa_obs_means

# Plot percent change in observations
change_usa_obs_plot <- ggplot(data = usa_obs_change, 
                              mapping = aes(x = comparison, 
                                            y = per_change_obs,
                                            group = `Province/State`, 
                                            color = `Province/State`)) +
  geom_line() +
  theme(legend.position = "none")
change_usa_obs_plot

# Create dataframe for observations t test
usa_obs_ttest <- usa_obs_change %>% 
  select(`Province/State`, Year, per_change_obs) %>%
  pivot_wider(names_from = Year, values_from = per_change_obs) %>%
  rename(`2018-2019` = `2019`, 
         `2019-2020` = `2020`) 

# t test on change in growth in observations
usa_obs_ttest_list <- t.test(x = usa_obs_ttest$`2018-2019`, 
                             y = usa_obs_ttest$`2019-2020`, 
                             paired = TRUE)
usa_obs_ttest_list
cat(round(usa_obs_ttest_list$estimate, digits = 6), " (",
    paste0(round(as.numeric(usa_obs_ttest_list$conf.int), digits = 4), collapse = ", "),
    ")", sep = "")

################################################################################
# Observers
# Calculate percent change in observers 2018-2019 and 2019-2020 
# by spring (Mar-May) and state/province
usa_users_change <- usa_counts %>% 
  group_by(`Province/State`) %>%
  mutate(prev_users = lag(num_users)) %>%
  filter(!is.na(prev_users)) %>%
  mutate(per_change_users = (((num_users/prev_users)-1) * 100)) %>%
  select(-prev_users) %>% 
  mutate(comparison = if_else(Year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))

# Calculate means
usa_users_means <- usa_users_change %>% 
  group_by(comparison) %>%
  summarize(mean_users = mean(per_change_users))
usa_users_means

# Plot percent change in observers
change_usa_users_plot <- ggplot(data = usa_users_change, 
                                mapping = aes(x = comparison, 
                                              y = per_change_users,
                                              group = `Province/State`, 
                                              color = `Province/State`)) +
  geom_line() +
  theme(legend.position = "none")
change_usa_users_plot

# Create dataframe for observers t test
usa_users_ttest <- usa_users_change %>% 
  select(`Province/State`, Year, per_change_users) %>%
  pivot_wider(names_from = Year, values_from = per_change_users) %>%
  rename(`2018-2019` = `2019`, 
         `2019-2020` = `2020`) 

# t test on change in growth in observers
usa_users_ttest_list <- t.test(x = usa_users_ttest$`2018-2019`, 
                               y = usa_users_ttest$`2019-2020`, 
                               paired = TRUE)
usa_users_ttest_list
cat(round(usa_users_ttest_list$estimate, digits = 5), " (",
    paste0(round(as.numeric(usa_users_ttest_list$conf.int), digits = 4), collapse = ", "),
    ")", sep = "")
