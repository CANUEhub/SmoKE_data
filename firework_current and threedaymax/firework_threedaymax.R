
rm(list = ls())

# set the working directory 
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
# https://dd.weather.gc.ca/model_raqdps-fw/10km/grib2/00/000/20240109T00Z_MSC_RAQDPS-FW_PM2.5_Sfc_RLatLon0.09_PT000H.grib2


######## get the time values #########
current.time = as.POSIXlt(Sys.time(), tz = "UTC")
current_hour <- as.numeric(format(current.time, "%H"))
formatted.date = format(current.time, "%Y%m%d")


# set the HH value for the script 
if (current_hour >= 12) {
  HH = "12"
} else {
  HH = "00"
}


################ test the status code. As long as it's 404, keep running. Stop after three attemps. ################

endpoint = paste(
  "https://dd.weather.gc.ca/model_raqdps-fw/10km/grib2/",
  HH,
  "/001/",
  formatted.date,
  "T",
  HH,
  "Z_MSC_RAQDPS-FW_PM2.5_Sfc_RLatLon0.09_PT001H.grib2",
  sep = ""
)

# check an initial file connection 
poll.response <- GET(endpoint)

# r = 404 represents an error status code 
r <- poll.response$status_code

iterator <- 1

# run the endpoint until we no longer get r = 404, which means it finally worked. 15 minute lag in between. Give up after 4 attempts.
while(r == 404 && iterator < 4) {
  print("In while loop")
  Sys.sleep(900) # this is a 15 minute lag
  poll.response <- GET(endpoint)
  r <- poll.response$status_code
  iterator <- iterator + 1
}


################ 1. Download all pollutant files for 72 hours. ################

df <- data.frame(ID = 1:698)
  
for (temp.hour in 0:72) {
  # this is the hour we are extracting.
  print(temp.hour)
  
  curr.hour = str_pad(temp.hour, 3, pad = "0")

  # # get the URL
  endpoint = paste(
    "https://dd.weather.gc.ca/model_raqdps-fw/10km/grib2/",
    HH,
    "/",
    curr.hour,
    "/",
    formatted.date,
    "T",
    HH,
    "Z_MSC_RAQDPS-FW_PM2.5_Sfc_RLatLon0.09_PT",
    curr.hour,
    "H.grib2",
    sep = ""
  )
  
  # print(endpoint)
  
  # # get the file
  poll.response <- GET(endpoint)
  #
  # # give the tif a name
  poll.name = paste("forecast_PM25_Base",
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
  pm = raster::extract(temp.ras, smoke.communities)
  
  # add to a data frame
  df[[curr.hour]] <- pm
  
  # delete the raster file
  file.remove(poll.name)
}


################# calculate three hour average for NO2. ################

# import data on smoke communities and the associated time zone 

smoke_tz = read.csv("/Users/priyapatel/Library/CloudStorage/OneDrive-UniversityofToronto/!CANUE/AQHI Work/SmoKE_ID_communities_w_TZ.csv")

# More information on Canadian time zones : https://www.timeanddate.com/time/zone/canada

combined = cbind(smoke_tz,df)

combined <- subset(combined, select=-c(lat,lon))

# filter 

unique(combined$name)

####################################################################################

#### eastern time zones: ####

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


# combine all the data 
day1.combine = as.data.frame(c(ct.day1.max, et.day1.max, pt.day1.max, mt.day1.max,at.day1.max))
day2.combine = as.data.frame(c(ct.day2.max, et.day2.max, pt.day2.max, mt.day2.max,at.day2.max))
day3.combine = as.data.frame(c(ct.day3.max, et.day3.max, pt.day3.max, mt.day3.max,at.day3.max))


day1.combine = day1.combine[order(as.numeric(rownames(day1.combine))),,drop=FALSE]
day2.combine = day2.combine[order(as.numeric(rownames(day2.combine))),,drop=FALSE]
day3.combine = day3.combine[order(as.numeric(rownames(day3.combine))),,drop=FALSE]

#  limit decimal places 
options(digits=2)

# combine in the table 
df <- data.frame(idcomm = 1:698, day1.combine,day2.combine,day3.combine)

colnames(df)[2] <- "pm25_maxtoday"
colnames(df)[3] <- "pm25_maxtomorrow"
colnames(df)[4] <- "pm25_maxovermorrow"

# write.csv(df,"maxvalues.csv",row.names = FALSE)

## table to track timestamp 

current.time.end = as.POSIXlt(Sys.time(), tz = "UTC")

texttime = format(current.time.end, "%Y-%m-%d %H:%M:%S %Z")

val = "update"

df.update = cbind(val,texttime)

colnames(df.update) <- c("lu", "var_lu")

df.update = as.data.frame(df.update)

###########


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
DBI::dbWriteTable(con, "pm25_cur_temp", df,overwrite=TRUE)

DBI::dbWriteTable(con, "temp_lu", df.update,overwrite=TRUE)


## create query 

query <- "UPDATE smoke.pm25_for
SET pm25_maxtoday = pm25_cur_temp.pm25_maxtoday,
pm25_maxtomorrow = pm25_cur_temp.pm25_maxtomorrow,
pm25_maxovermorrow = pm25_cur_temp.pm25_maxovermorrow
FROM public.pm25_cur_temp
WHERE commid = idcomm"


query.timestamp <- "UPDATE smoke.curfor_lu
SET aqhi_for_lu = temp_lu.var_lu
FROM public.temp_lu"


#### update master table ####
DBI::dbSendQuery(con, query)
DBI::dbSendQuery(con, query.timestamp)

# Close the connection
dbDisconnect(con)


################# UPDATE TEXT FILE ##############

# Open the file in append mode
file_path <- "update_log_fw.txt"

con <- file(file_path, open = "a")

current.time.est = as.POSIXlt(Sys.time(), tz = "EST")

# Text to be added
text_to_add <- paste("The FW PM2.5 was last updated on.",current.time.est, "EST")

# Append the text to the file
cat(text_to_add, file = con, "\n")

# Close the file connection
close(con)




