##TMC analyzing NN site visit frequency pre & during COVID shut-down as part of Bates et al. big analysis
#pulled & prepped data using Tableau, MySQL, and JMP

#################################
#Replicating Katy's analyses using "2018-2020-Arizona.R"


library(tidyverse)
library(lubridate)
library(dplyr)
library("readxl")

# RECORDS
#Read in records*month*year*state
records_MAM <- read_excel("data/records_by_month-year-state_MAM_2018-20_only.xlsx")

#group records in M,A,M 
TotRecords <- records_MAM %>%
  group_by(State, Year) %>%
  summarize(TotRecords = sum(Records))

# Calculate percent change in observations 2018-2019 and 2019-2020 
# by spring (Mar-May) and state
# ((observations in year t/ observations in year t-1) -1) * 100

change_in_records <- TotRecords %>% 
  mutate(prev_records = lag(TotRecords)) %>%
  filter(!is.na(prev_records)) %>%
  mutate(per_change_records = (((TotRecords/prev_records)-1) * 100)) %>%
  select(-prev_records) %>% 
  mutate(comparison = if_else(Year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))


# Create dataframe for records t test
records_ttest <- change_in_records %>% 
  select(State, Year, per_change_records) %>%
  pivot_wider(names_from = Year, values_from = per_change_records)

# t test on change in growth in observations

records_t_test <- t.test(x = records_ttest$`2019`, 
                         y = records_ttest$`2020`, 
                         paired = TRUE)

##REDO for only Apr, May (exclude March)
AM_Records <- records_MAM %>%
  filter(Month == c("April","May")) %>%
  group_by(State, Year) %>%
  summarize(TotRecords = sum(Records))

AMchange_in_records <- AM_Records %>% 
  mutate(prev_records = lag(TotRecords)) %>%
  filter(!is.na(prev_records)) %>%
  mutate(per_change_records = (((TotRecords/prev_records)-1) * 100)) %>%
  select(-prev_records) %>% 
  mutate(comparison = if_else(Year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))

# Create dataframe for records t test
records_ttest <- AMchange_in_records %>% 
  select(State, Year, per_change_records) %>%
  pivot_wider(names_from = Year, values_from = per_change_records)

# t test on change in growth in observations

records_t_test <- t.test(x = records_ttest$`2019`, 
                         y = records_ttest$`2020`, 
                         paired = TRUE)



##UNIQUE PARTICIPANTS

#Read in unique particpants in MAM*year*state
people_MAM <- read_excel("data/unique_observers-MAM_2018-20.xlsx")

# Calculate percent change in #unique observers 2018-2019 and 2019-2020 
# by spring (Mar-May) and state
# ((observations in year t/ observations in year t-1) -1) * 100

change_in_people <- people_MAM %>% 
  mutate(prev_people = lag(Observers)) %>%
  filter(!is.na(prev_people)) %>%
  mutate(per_change_people = (((Observers/prev_people)-1) * 100)) %>%
  select(-prev_people) %>% 
  mutate(comparison = if_else(Year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))


# Create dataframe for records t test
people_ttest <- change_in_people %>% 
  select(State, Year, per_change_people) %>%
  pivot_wider(names_from = Year, values_from = per_change_people)

# t test on change in growth in observations

people_t_test <- t.test(x = people_ttest$`2019`, 
                         y = people_ttest$`2020`, 
                         paired = TRUE)

#REDO JUST FOR Apr-May
#Read in unique particpants in AM*year*state
people_AM <- read_excel("data/unique_observers-AM_2018-20.xlsx")


change_in_people <- people_AM %>% 
  mutate(prev_people = lag(Observers)) %>%
  filter(!is.na(prev_people)) %>%
  mutate(per_change_people = (((Observers/prev_people)-1) * 100)) %>%
  select(-prev_people) %>% 
  mutate(comparison = if_else(Year == 2019, 
                              true = "2018-2019", 
                              false = "2019-2020"))


# Create dataframe for records t test
people_ttest <- change_in_people %>% 
  select(State, Year, per_change_people) %>%
  pivot_wider(names_from = Year, values_from = per_change_people)

# t test on change in growth in observations

people_t_test <- t.test(x = people_ttest$`2019`, 
                        y = people_ttest$`2020`, 
                        paired = TRUE)



