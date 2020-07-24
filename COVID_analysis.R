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




