# Analyses of USA National Phenology Network data re COVID19 (%change by state)
# Kathleen L Prudic & Theresa Crimmins
# klprudic@arizona.edu
# created 2020-07-09
#modified 8-4-20

# Remove wonton variables 
rm(list = ls())

# Load additional packages
library(tidyverse)
library(lubridate)

# Load data 
records_site <- read_csv(file = "data/records_by_site_MAM_18-20.csv")

# Identify sites with data for all three years
usa_sites <- records_site %>%
  group_by(siteID) %>%
  summarize(num_rows = n()) %>%
  filter(num_rows == 3)

# Remove any sites without all three years of data
records_site <- records_site %>%
  filter(siteID %in% usa_sites$siteID)

# Sum number of records by state and year
#usa_counts <- usa_obs %>%
#  group_by(State, Year) %>%
#  summarize(num_records = sum(Records) )

# Plot number of records by site over 2018-2020
obs_plot <- ggplot(data = records_site, mapping = aes(x = year, 
                                                        y = records_ct,
                                                        group = siteID, 
                                                        color = siteID)) +
  geom_line() + 
  scale_y_log10() +
  theme(legend.position = "none")
obs_plot

# Calculate percent change in records 2018-2019 and 2019-2020 
# by spring (Mar-May) and site
records_change <- records_site %>% 
  group_by(siteID) %>%
  mutate(prev_obs = lag(records_ct)) %>%
  filter(!is.na(prev_obs)) %>%
  mutate(per_change_obs = (((records_ct/prev_obs)-1) * 100)) %>%
  select(-prev_obs) %>% 
  mutate(comparison = if_else(year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))

# Calculate means
records_means <- records_change %>%
  group_by(comparison) %>%
  summarize(mean_obs = mean(per_change_obs))
records_means

# Plot percent change in records
change_records_plot <- ggplot(data = records_change, 
                              mapping = aes(x = comparison, 
                                            y = per_change_obs,
                                            group = siteID, 
                                            color = siteID)) +
  geom_line() +
  theme(legend.position = "none")
change_records_plot

#this part is where I stopped because things were messed up above
# Create dataframe for records t test
records_ttest <- records_change %>% 
  select(siteID, year, per_change_obs) %>%
  pivot_wider(names_from = year, values_from = per_change_obs) %>%
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
