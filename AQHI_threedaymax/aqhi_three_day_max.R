rm(list = ls())

wd = dirname(rstudioapi::getSourceEditorContext()$path)

setwd(wd)



library(lubridate)
library(stringr)
library(httr)
library(raster)
library(sf)
library(tidyverse)
library(rvest)


#### import smoke data #### 

smoke.communities = "../smoke_shp/smoke.shp"

smoke.communities = st_read(smoke.communities)

# info on the dataset: https://eccc-msc.github.io/open-data/msc-data/nwp_raqdps/readme_raqdps-datamart_en/

current.time = as.POSIXlt(Sys.time(), tz = "UTC")
current_hour <- as.numeric(format(current.time, "%H"))
formatted.date = format(current.time, "%Y%m%d")

# set the HH value for the script 
if (current_hour >= 12) {
  HH = "12"
} else {
  HH = "00"
}


################ 1. Download all pollutant files for 72 hours. ################

VAR = "NO2"

download72hr = function(VAR) {
  df <- data.frame(ID = 1:698)
  
  for (temp.hour in 0:72) {
    # this is the hour we are extracting.
    print(temp.hour)
    
    curr.hour = str_pad(temp.hour, 3, pad = "0")
    
    # # get the URL
    endpoint = paste(
      "https://dd.weather.gc.ca/model_raqdps/10km/grib2/",
      HH,
      "/",
      curr.hour,
      "/",
      formatted.date,
      "T",
      HH,
      "Z_MSC_RAQDPS_",
      VAR,
      "_Sfc_RLatLon0.09_PT",
      curr.hour,
      "H.grib2",
      sep = ""
    )
    
    # print(endpoint)
    
    # # get the file
    poll.response <- GET(endpoint)
    #
    # # give the tif a name
    poll.name = paste("forecast_",
                      VAR,
                      "_Base",
                      HH,
                      "_",
                      formatted.date,
                      "_H",
                      curr.hour,
                      ".tif",
                      sep = "")
    #
    # # save the raster
    writeBin(content(poll.response, "raw"), poll.name)
    
    # import the raster
    temp.ras = raster(poll.name) * 1000000000
    
    # extract values at smoke communities
    no2 = raster::extract(temp.ras, smoke.communities)
    
    # add to a data frame
    df[[curr.hour]] <- no2
    
    # delete the raster file
    file.remove(poll.name)
  }
  
  df
}

no2 = download72hr("NO2")
pm2.5 = download72hr("PM2.5")
o3 = download72hr("O3")

################# calculate three hour average for NO2. ################

# calculate three hour average. Skip the first column because it's the index.

ThreeHRAverage = function(pollutantDataFrame) {
  # create an empty dataframe
  df <- data.frame(ID = 1:698)
  
  ## look into these values and figure out why some of them are high.
  
  # we can only run the table up to the third last index otherwise we will get an error. This still includes the last two columns.
  
  last.col = ncol(pollutantDataFrame) - 2
  
  # this loop will calculate the average for every three columns.
  
  for (i in 2:last.col) {
    # current column
    print(i)
    
    # end of the columns to be averaged
    end_of = i + 2
    
    # filter the dataframe
    temp.df = pollutantDataFrame[, i:end_of]
    print(head(temp.df))
    
    # calculate the row mean 
    rm = rowMeans(temp.df)
    
    # print(rm)
    
    # grab the name of the column that we are calculating the three hour average for. It's the last column in the filtered dataset. 
    cname = colnames(pollutantDataFrame)[end_of]
    
    # attach the row mean to the new dataframe. 
    df[[cname]] <- rm
  }
  
  # remove the ID column 
  df$ID <- NULL
  
  df
}


no2.avg = ThreeHRAverage(no2)
pm.avg = ThreeHRAverage(pm2.5)
o3.avg = ThreeHRAverage(o3)

## do the same thing for the other pollutants. When that is done we can calculate AQHI.

# Calculate AQHI

A = (exp(0.000537 * o3.avg) - 1)
B = (exp(0.000871 * no2.avg) - 1)
C = (exp(0.000487 * pm.avg) - 1)

# calculate files
aqhi = (10 / 10.4) * 100 * (A + B + C)


# import data on smoke communities and the associated time zone 

smoke_tz = read.csv("/Users/priyapatel/Library/CloudStorage/OneDrive-UniversityofToronto/!CANUE/AQHI Work/SmoKE_ID_communities_w_TZ.csv")

# More information on Canadian time zones : https://www.timeanddate.com/time/zone/canada

combined = cbind(smoke_tz,aqhi)

combined <- subset(combined, select=-c(lat,lon))

# filter 

unique(combined$name)

####################################################################################

#### eastern and atlantic time zones: ####

##### day 1 #####
ET = combined[(combined$name == 'ET'),]

et.day1 = ET[,"006":"029"]

et.day1.max = apply(et.day1, 1, max)

##### day 2 #####

et.day2 = ET[,"030":"053"]

et.day2.max = apply(et.day2, 1, max)


##### day 3 #####

et.day3 = ET[,"054":"073"]

et.day3.max = apply(et.day3, 1, max)


######################

#### pacific time zone: ####

##### day 1 #####
PT = combined[(combined$name == 'PT'),]

pt.day1 = PT[,"009":"032"]

pt.day1.max = apply(pt.day1, 1, max)

##### day 2 #####

pt.day2 = PT[,"033":"056"]

pt.day2.max = apply(pt.day2, 1, max)


##### day 3 #####

pt.day3 = PT[,"057":"073"]

pt.day3.max = apply(pt.day3, 1, max)


#### mountain standard time: ####

##### day 1 #####
MT = combined[(combined$name == 'MT'),]

mt.day1 = MT[,"008":"031"]

mt.day1.max = apply(mt.day1, 1, max)

##### day 2 #####

mt.day2 = MT[,"032":"055"]

mt.day2.max = apply(mt.day2, 1, max)

##### day 3 #####

mt.day3 = MT[,"055":"073"]

mt.day3.max = apply(mt.day3, 1, max)


#### central standard time: ####

##### day 1 #####
CT = combined[(combined$name == 'CT'),]

ct.day1 = CT[,"007":"030"]

ct.day1.max = apply(ct.day1, 1, max)

##### day 2 #####

ct.day2 = CT[,"031":"054"]

ct.day2.max = apply(ct.day2, 1, max)

##### day 3 #####

ct.day3 = CT[,"055":"073"]

ct.day3.max = apply(ct.day3, 1, max)






#### atlantic standard time: ####

##### day 1 #####
AT = combined[(combined$name == 'AT'| combined$name =="NT"),]

at.day1 = AT[,"005":"028"]

at.day1.max = apply(at.day1, 1, max)

##### day 2 #####

at.day2 = AT[,"029":"052"]

at.day2.max = apply(at.day2, 1, max)

##### day 3 #####

at.day3 = AT[,"053":"073"]

at.day3.max = apply(at.day3, 1, max)



day1.combine = as.data.frame(c(ct.day1.max, et.day1.max, pt.day1.max, mt.day1.max,at.day1.max))
day2.combine = as.data.frame(c(ct.day2.max, et.day2.max, pt.day2.max, mt.day2.max,at.day2.max))
day3.combine = as.data.frame(c(ct.day3.max, et.day3.max, pt.day3.max, mt.day3.max,at.day3.max))


day1.combine = day1.combine[order(as.numeric(rownames(day1.combine))),,drop=FALSE]
day2.combine = day2.combine[order(as.numeric(rownames(day2.combine))),,drop=FALSE]
day3.combine = day3.combine[order(as.numeric(rownames(day3.combine))),,drop=FALSE]

#  limit decimal places 
options(digits=2)

# combine in the table 
df <- data.frame(ID = 1:698, day1.combine,day2.combine,day3.combine)

colnames(df) <- c("idcomm", "aqhi_maxtoday","aqhi_maxtomorrow","aqhi_maxovermorrow")

## send to postgres 

library(DBI)
library(RPostgres)

# Your PostgreSQL connection details
host <- '147.182.150.83'          # e.g., 'localhost' or an IP address
port <- '5432'          # usually 5432 for PostgreSQL
dbname <- 'smokedb'
user <- 'smokedb'
password <- 'smokedb'

# Create a connection
con <- dbConnect(RPostgres::Postgres(), 
                 dbname = dbname,
                 host = host, 
                 port = port,
                 user = user, 
                 password = password)

# Your data frame

## upload temp table to postgres
DBI::dbWriteTable(con, "aqhi_for_temp", df,overwrite=TRUE)

## create query 
query <- "UPDATE smoke.aqhi_for 
SET aqhi_maxtoday = aqhi_for_temp.aqhi_maxtoday,
aqhi_maxtomorrow = aqhi_for_temp.aqhi_maxtomorrow,
aqhi_maxovermorrow = aqhi_for_temp.aqhi_maxovermorrow
FROM public.aqhi_for_temp
WHERE commid = idcomm"


#### update master table ####
DBI::dbSendQuery(con, query)

# Close the connection
dbDisconnect(con)


################# UPDATE TEXT FILE ##############

# Open the file in append mode
file_path <- "update_log.txt"

con <- file(file_path, open = "a")

# Text to be added
text_to_add <- paste("The aqhi was last updated on.",current.time.end, "UTC")

# Append the text to the file
cat(text_to_add, file = con, "\n")

# Close the file connection
close(con)





