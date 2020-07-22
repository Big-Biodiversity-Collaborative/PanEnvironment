# Analyses of Arizona iNat data re COVID19 (Temporal Autocorrelation LME)
# Kathleen L Prudic
# klprudic@arizona.edu
# created 2020-07-09

# Remove wonton variables 
rm(list = ls())

# Load additional packages
library(tidyverse)
library(lubridate)
library(nlme)

# Load data 
arizona_spring <- read_csv(file = "data/2011-2020-iNat-Arizona.csv")

# Filter data to have months March, April, May only
arizona_spring <- arizona_spring %>%
  filter(year(observed_on) %in% c(2016:2020))

# Count number of unique users and observations by county for each year
# Column names are county, year, users, observations
arizona_counts <- arizona_spring %>%
  group_by(place_county_name, year(observed_on)) %>%
  summarize(num_observations = n(), 
            num_users = length(unique(user_id)))

# Rename columns from place_county_name to county and year(observed_on) to year
arizona_counts <- arizona_counts %>% 
  rename(county = place_county_name, 
         year = "year(observed_on)")

# Log transform counts (base 10)
arizona_counts <- arizona_counts %>% 
  mutate(log_num_observations = log10(num_observations), 
         log_num_users = log10(num_users))

# Plot number of observations 
obs_plot <- ggplot(data = arizona_counts, mapping = aes(x = year, 
                                                        y = log_num_observations,
                                                        group = county, 
                                                        color = county)) +
  geom_line() 
obs_plot

# Plot number of users
users_plot <- ggplot(data = arizona_counts, mapping = aes (x = year, 
                                                           y = log_num_users,
                                                           group = county,
                                                           color = county)) +
  geom_line()
  # geom_point() +
  # geom_smooth(method = "lm", se = FALSE)
users_plot

# Create column for pre and post COVID19 assignment
arizona_counts <- arizona_counts %>%
  mutate(COVID19 = if_else(year > 2019, true = "post", false = "pre"))

# Relevel COVID19 column
arizona_counts$COVID19 <- factor(arizona_counts$COVID19,
                                 levels = c("pre", "post"))

################################################################################
# Mixed effects models testing COVID on observations and unique users

# OBSERVATIONS
# Random intercept for county
obs_model_intercept <- lme(log_num_observations ~ COVID19,
                     data = arizona_counts, 
                     corr = corARMA(form = ~1|county/year, q = 2), 
                     random = ~1|county)
obs_model_intercept
summary(obs_model_intercept)

# Random intercept and slope for county
obs_model_slope <- lme(log_num_observations ~ COVID19,
                               data = arizona_counts, 
                               corr = corARMA(form = ~1|county/year, q = 2), 
                               random = ~1 + year|county)
obs_model_slope
summary(obs_model_slope)

# Compare intercept model to intercept + slope model
anova(obs_model_intercept, obs_model_slope)
#                     Model df      AIC      BIC    logLik   Test    L.Ratio p-value
# obs_model_intercept     1  6 114.4906 128.1506 -51.24530                          
# obs_model_slope         2  8 118.4795 136.6928 -51.23974 1 vs 2 0.01110932  0.9945

# USERS
# Random intercept for county
users_model_intercept <- lme(log_num_users ~ COVID19,
                           data = arizona_counts, 
                           corr = corARMA(form = ~1|county/year, q = 2), 
                           random = ~1|county)
users_model_intercept
summary(users_model_intercept)

# Random intercept and slope for county
users_model_slope <- lme(log_num_users ~ COVID19,
                       data = arizona_counts, 
                       corr = corARMA(form = ~1|county/year, q = 2), 
                       random = ~1 + year|county, 
                       control = lmeControl(msMaxIter = 200, 
                                            msMaxEval = 500))
users_model_slope
summary(users_model_slope)

# Compare intercept model to intercept + slope model
anova(users_model_intercept, users_model_slope)
#                       Model df      AIC      BIC    logLik   Test    L.Ratio p-value
# users_model_intercept     1  6 89.83231 103.4923 -38.91615                          
# users_model_slope         2  8 93.82860 112.0419 -38.91430 1 vs 2 0.00370131  0.9982


################################################################################
# Not sure about temporal autocorrelation results. What happens when we compare
# slopes including and excluding 2020 data? If there is growth post-covid, then
# we expect model slope with 2020 data to be greater than model slope without 
# 2020 data. We think

# Linear regression on all data using time (years, Mar-May) to predict number
# of observations (log 10) and number of unique users (log 10)(mixed effects 
# model, county = random)

# Random intercept for county
obs_reality_intercept <- lme(log_num_observations ~ year,
                           data = arizona_counts, 
                           random = ~1|county)
obs_reality_intercept
summary(obs_reality_intercept) 

# Extract slope for effect of time
obs_reality_intercept$coefficients$fixed

# Create new dataframe with no 2020 data (Mar-May, 2016-2019)
arizona_counts_no2020 <- arizona_counts %>%
  filter(year %in% c(2016:2019))

# Linear regression on no 2020 data using time to predict number of observations
# and number of users (both log 10)(mixed effects model, county = random)

obs_reality_no2020_intercept <- lme(log_num_observations ~ year,
                             data = arizona_counts_no2020, 
                             random = ~1|county)
obs_reality_no2020_intercept
summary(obs_reality_no2020_intercept) 

# Extract slope for effect of time
obs_reality_no2020_intercept$coefficients$fixed

# Extract slopes for two models for eyeball comparison and reality check
obs_reality_intercept$coefficients$fixed
obs_reality_no2020_intercept$coefficients$fixed

plot(x=arizona_counts$year, y=arizona_counts$log_num_observations)
abline(a = obs_reality_intercept$coefficients$fixed[1], 
       b = obs_reality_intercept$coefficients$fixed[2])
abline(a = obs_reality_no2020_intercept$coefficients$fixed[1],
       b = obs_reality_no2020_intercept$coefficients$fixed[2], 
       lty = 2)

# Looks like slope is smaller when including 2020 data. Sigh. Not sure about
# temporal autocorrelation. Nope nope nope, not sure at all 