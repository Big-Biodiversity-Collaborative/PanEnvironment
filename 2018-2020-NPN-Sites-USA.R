# Analyses of USA National Phenology Network data re COVID19 (%change by state)
# Kathleen L Prudic
# klprudic@arizona.edu
# created 2020-08-02

# Remove wonton variables 
rm(list = ls())

# Load additional packages
library(tidyverse)
library(lubridate)


# Load data 
usa_sites <- read_csv(file = "data/2018-2020-NPN-Sites-USA.csv")

# Identify states with data for all three years
usa_complete <- usa_sites %>%
  group_by(siteID) %>%
  summarize(num_year = n()) %>%
  filter(num_year == 3)

# Remove any province/states without all three years of data
usa_sites <- usa_sites %>%
  filter(siteID %in% usa_complete$siteID)

# Calculate percent change in users 2018-2019 and 2019-2020 
# by spring (Mar-May) and state
usa_sites_change <- usa_sites %>% 
  group_by(siteID) %>%
  arrange(year) %>%
  mutate(prev_sites = lag(records_ct)) %>%
  filter(!is.na(prev_sites)) %>%
  mutate(per_change_sites = (((records_ct/prev_sites)-1) * 100)) %>%
  select(-prev_sites) %>% 
  mutate(comparison = if_else(year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))

# Calculate means
usa_sites_means <- usa_sites_change %>%
  group_by(comparison) %>%
  summarize(mean_sites = mean(per_change_sites))
usa_sites_means

# Plot percent change in users
change_usa_sites_plot <- ggplot(data = usa_sites_change, 
                                mapping = aes(x = comparison, 
                                              y = per_change_sites,
                                              group = siteID, 
                                              color = as.factor(siteID))) +
  geom_line() +
  theme(legend.position = "none")
change_usa_sites_plot

# Create dataframe for users t test
usa_sites_ttest <- usa_sites_change %>% 
  select(siteID, year, per_change_sites) %>%
  pivot_wider(names_from = year, values_from = per_change_sites) %>%
  rename(`2018-2019` = `2019`, 
         `2019-2020` = `2020`)

# t test on change in growth in unique users
usa_sites_ttest_list <- t.test(x = usa_sites_ttest$`2018-2019`, 
                               y = usa_sites_ttest$`2019-2020`,
                               alternative = "greater",
                               paired = TRUE)
usa_sites_ttest_list
cat(round(usa_sites_ttest_list$estimate, digits = 6), " (",
    paste0(round(as.numeric(usa_sites_ttest_list$conf.int), digits = 4), collapse = ", "),
    ")", sep = "")
