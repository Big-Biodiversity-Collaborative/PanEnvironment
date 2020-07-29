##TMC analyzing NN site visit frequency pre & during COVID shut-down as part of Bates et al. big analysis
#pulled & prepped data using Tableau, MySQL, and JMP

library(nlme)
library(dplyr)
library(raster)
library("readxl")
library(tidyverse)

#set working directory
setwd("c:/NPN/Manuscripts/Working/PAN-Environment_COVID")


#read in site visits just for Apr-May
site_visits_AM <- read_excel("visits_by_site-month-year-NoJun.xlsx")

#OLD, but don't want to lose
#following Amanda's model for testing for effects - AMJ
#lme(data=site_visits, Ct_visits ~ Period, random = ~1|Station_ID)

#following Amanda's model for testing for effects - AM
#lme(data=site_visits_AM, Ct_visits ~ Period, random = ~1|Station_ID)



# create ref grid
ref_grid<-extent(-180, 180, -90, 90)
ref_grid<-raster(ref_grid)
res(ref_grid)<-1
values(ref_grid)<-1#dummy values
projection(ref_grid)<-CRS("+proj=longlat +datum=WGS84 +no_defs    +ellps=WGS84 +towgs84=0,0,0")

# get cell from lat/lon
site_visits_AM$cellNum<-cellFromXY(ref_grid, cbind(site_visits_AM$Longitude, site_visits_AM$Latitude))
site_visits_AM<- cbind.data.frame(site_visits_AM, xyFromCell(ref_grid,site_visits_AM$cellNum))

# summarize by month/year/cell

summarizedCell<- site_visits_AM %>% group_by(Month,Year,cellNum) %>% summarise(sumCount=sum(Ct_visits),
                                                                     LonCell=first(x),
                                                                     LatCell=first(y),
                                                                     Period=first(Period)
                                                                     )




#following Amanda's model using data summarized by cell (processing steps below)
results <- lme(data=summarizedCell, sumCount ~ Period, random = ~1|cellNum)

summary(results)

#FROM HERE DOWN - 7-14-20, after talking with Tom - want to add in rows for 0 obs counts, try diff models

#fill in rows for cellNo*Month*Year with no obs, set sumCount = 0

#make table with rows of all combos of Month, Year, cellNum
Month = c("April", "May")
Year = c(2009:2020)
cellNum = unique(summarizedCell$cellNum)
test = expand.grid(Month,Year,cellNum)

#FYI, another way to list out all years is do it this way
#Year = seq(2009,2020,1)

#change column names in "test" to match "summarizedCell"
colnames(test) = c("Month", "Year", "cellNum")

#join full table of all combos w/original df
bigdf <- full_join(summarizedCell, test, by = c("Month","Year","cellNum"))

#fill NAs in sumCount w/0s

bigdf$sumCount[is.na(bigdf$sumCount)] <- 0

#fill in Period column
#if Year = 2020, Period = Shutdown, else Period = Before
bigdf$Period <- ifelse(bigdf$Year == 2020, "Shutdown", "Before")

hist(bigdf$sumCount, 
     breaks=600, 
     xlim=c(0,10),
     col="green"
     )

#write out data files as .csv
write.csv(bigdf, file = 'bigdf.csv')
write.csv(summarizedCell, file = 'summarizedCell.csv')

##Urban/Rural designation for sites
library(sp)
library(rgdal)
library(raster)

#read in urban census boundaries layer as spatial data frame
shape <- readOGR(dsn = ".", layer = "tl_2017_us_uac10")

#plot urban boundaries 
plot(shape)

#convert site visits df to spatial data frame
spdf<-SpatialPointsDataFrame(cbind(site_visits_AM$Longitude,site_visits_AM$Latitude),site_visits_AM)

## CRS arguments: NA
crs(shape) <- CRS("+proj=longlat +datum=WGS84")
crs(spdf) <- CRS("+proj=longlat +datum=WGS84")

#intersect points with urban boundaries
o <- over(spdf, shape)
spdf@data = cbind(spdf@data, shape[o,])
site_visits_AM-urban <-cbind.data.frame(site_visits_AM,o)


#################################
#Replicating Katy's analyses using "2018-2020-Arizona.R"


library(tidyverse)
library(lubridate)
library(dplyr)
library("readxl")

# RECORDS
#Read in records*month*year*state
records_MAM <- read_excel("records_by_month-year-state_MAM_2018-20_only.xlsx")

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




##NEED TO STILL DO ALL OF THIS FOR PARTICIPANTS