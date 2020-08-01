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
ebutt_can_usa <- ebutt_obs %>%
  filter(Country != "Mexico") %>%
  filter(Country != "Panama") %>%
  filter(Country != "Puerto Rico") %>%
  mutate(Year = year(`Date Observed`))


# Count and sum number of unique observations and observers
noram_counts <- ebutt_can_usa %>%
  group_by(`Province/State`, Year) %>%
  summarize(num_observations = n(),
            num_users = length(unique(Observer)))

# Identify province/state with all 3 years of data
noram_complete <- noram_counts %>%
  group_by(`Province/State`) %>%
  summarize(num_year = n()) %>%
  filter(num_year == 3)

# Remove any province/states without all three years of data
noram_counts <- noram_counts %>%
  filter(`Province/State` %in% noram_complete$`Province/State`)


# Plot number of observations 
noram_obs_plot <- ggplot(data = noram_counts, mapping = aes(x = Year, 
                                                        y = num_observations,
                                                        group = `Province/State`, 
                                                        color = `Province/State`)) +
  geom_line() + 
  scale_y_log10() +
  theme(legend.position = "none")
noram_obs_plot

################################################################################
# Observations
# Calculate percent change in observations 2018-2019 and 2019-2020 
# by spring (Mar-May) and state/province
noram_obs_change <- noram_counts %>% 
  group_by(`Province/State`) %>%
  mutate(prev_observations = lag(num_observations)) %>%
  filter(!is.na(prev_observations)) %>%
  mutate(per_change_obs = (((num_observations/prev_observations)-1) * 100)) %>%
  select(-prev_observations) %>% 
  mutate(comparison = if_else(Year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))

# Calculate means
noram_obs_means <- noram_obs_change %>% 
  group_by(comparison) %>%
  summarize(mean_obs = mean(per_change_obs))
noram_obs_means

# Plot percent change in observations
change_noram_obs_plot <- ggplot(data = noram_obs_change, 
                              mapping = aes(x = comparison, 
                                            y = per_change_obs,
                                            group = `Province/State`, 
                                            color = `Province/State`)) +
  geom_line() +
  theme(legend.position = "none")
change_noram_obs_plot

# Create dataframe for observations t test
noram_obs_ttest <- noram_obs_change %>% 
  select(`Province/State`, Year, per_change_obs) %>%
  pivot_wider(names_from = Year, values_from = per_change_obs) %>%
  rename(`2018-2019` = `2019`, 
         `2019-2020` = `2020`) 

# t test on change in growth in observations
noram_obs_ttest_list <- t.test(x = noram_obs_ttest$`2018-2019`, 
                             y = noram_obs_ttest$`2019-2020`, 
                             paired = TRUE)
noram_obs_ttest_list
cat(round(noram_obs_ttest_list$estimate, digits = 6), " (",
    paste0(round(as.numeric(noram_obs_ttest_list$conf.int), digits = 4), collapse = ", "),
    ")", sep = "")

################################################################################
# Observers
# Calculate percent change in observers 2018-2019 and 2019-2020 
# by spring (Mar-May) and state/province
noram_users_change <- noram_counts %>% 
  group_by(`Province/State`) %>%
  mutate(prev_users = lag(num_users)) %>%
  filter(!is.na(prev_users)) %>%
  mutate(per_change_users = (((num_users/prev_users)-1) * 100)) %>%
  select(-prev_users) %>% 
  mutate(comparison = if_else(Year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))

# Calculate means
noram_users_means <- noram_users_change %>% 
  group_by(comparison) %>%
  summarize(mean_users = mean(per_change_users))
noram_users_means

# Plot percent change in observers
change_noram_users_plot <- ggplot(data = noram_users_change, 
                                mapping = aes(x = comparison, 
                                              y = per_change_users,
                                              group = `Province/State`, 
                                              color = `Province/State`)) +
  geom_line() +
  theme(legend.position = "none")
change_noram_users_plot

# Create dataframe for observers t test
noram_users_ttest <- noram_users_change %>% 
  select(`Province/State`, Year, per_change_users) %>%
  pivot_wider(names_from = Year, values_from = per_change_users) %>%
  rename(`2018-2019` = `2019`, 
         `2019-2020` = `2020`) 

# t test on change in growth in observers
noram_users_ttest_list <- t.test(x = noram_users_ttest$`2018-2019`, 
                               y = noram_users_ttest$`2019-2020`, 
                               paired = TRUE)
noram_users_ttest_list
cat(round(noram_users_ttest_list$estimate, digits = 5), " (",
    paste0(round(as.numeric(noram_users_ttest_list$conf.int), digits = 4), collapse = ", "),
    ")", sep = "")
