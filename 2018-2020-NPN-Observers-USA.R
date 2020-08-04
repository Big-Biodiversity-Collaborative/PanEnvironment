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
usa_users <- read_csv(file = "data/2018-2020-NPN-Observers-USA.csv")

# Identify states with data for all three years
usa_complete <- usa_users %>%
  group_by(State) %>%
  summarize(num_year = n()) %>%
  filter(num_year == 3)

# Remove any province/states without all three years of data
usa_users <- usa_users %>%
  filter(State %in% usa_complete$State)

# Calculate percent change in users 2018-2019 and 2019-2020 
# by spring (Mar-May) and state
usa_users_change <- usa_users %>% 
  group_by(State) %>%
  arrange(Year) %>%
  mutate(prev_users = lag(Observers)) %>%
  filter(!is.na(prev_users)) %>%
  mutate(per_change_users = (((Observers/prev_users)-1) * 100)) %>%
  select(-prev_users) %>% 
  mutate(comparison = if_else(Year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))

# Calculate means
usa_users_means <- usa_users_change %>%
  group_by(comparison) %>%
  summarize(mean_users = mean(per_change_users))
usa_users_means

# Plot percent change in users
change_usa_users_plot <- ggplot(data = usa_users_change, 
                                mapping = aes(x = comparison, 
                                              y = per_change_users,
                                              group = State, 
                                              color = State)) +
  geom_line() +
  theme(legend.position = "none")
change_usa_users_plot

# Create dataframe for users t test
usa_users_ttest <- usa_users_change %>% 
  select(State, Year, per_change_users) %>%
  pivot_wider(names_from = Year, values_from = per_change_users) %>%
  rename(`2018-2019` = `2019`, 
         `2019-2020` = `2020`) 

# t test on change in growth in unique users
usa_users_ttest_list <- t.test(x = usa_users_ttest$`2018-2019`, 
                               y = usa_users_ttest$`2019-2020`,
                               alternative = "greater",
                               paired = TRUE)
usa_users_ttest_list
cat(round(usa_users_ttest_list$estimate, digits = 6), " (",
    paste0(round(as.numeric(usa_users_ttest_list$conf.int), digits = 4), collapse = ", "),
    ")", sep = "")

