
wd = dirname(rstudioapi::getSourceEditorContext()$path)

setwd(wd)


## this script is to get the current firework PM2.5 


##### import libraries #####

library(httr)
library(raster)
library(sf)
library(tidyverse)
library(rvest)


##### import shapefile  #####

smoke.communities = "../smoke_shp/smoke.shp"
smoke.communities = st_read(smoke.communities)

# get the current time and date 
current.time = as.POSIXlt(Sys.time(), tz = "UTC")

hour(current.time)

##### these are the dates of interest 

# get the current hour and date 
date_nearest.hour = floor_date(current.time, "hour")

#### this is the file format 
# https://dd.weather.gc.ca/model_rdaqa/10km/00/20231006T00Z_MSC_RDAQA-FW_PM2.5_Sfc_RLatLon0.09_PT0H.grib2

########## GET THE RASTERS  ###########

month.current = str_pad(month(date_nearest.hour), 2, pad = "0")

day.current = str_pad(day(current.time), 2, pad = "0")

hour.current = str_pad((hour(current.time)-2), 2, pad = "0")

# create a URL

# https://dd.weather.gc.ca/model_rdaqa/10km/22/20240109T22Z_MSC_RDAQA-FW_PM2.5_Sfc_RLatLon0.09_PT0H.grib2
# https://dd.weather.gc.ca/model_rdaqa/10km/22/20240110T22Z_MSC_RDAQA-FW_PM2.5_Sfc_RLatLon0.09_PT0H.grib2"

endpoint.url = paste(
  "https://dd.weather.gc.ca/model_rdaqa/10km/",
  hour.current,
  "/",
  year(date_nearest.hour),
  month.current,
  day.current,
  "T",
  hour.current,
  "Z_MSC_RDAQA-FW_PM2.5_Sfc_RLatLon0.09_PT0H.grib2",
  sep = ""
)

# get the file
poll.response <- GET(endpoint.url)

# give the tif a name
poll.name = paste("FW_PM2.5", "_", date(current.time), "_H", hour(current.time), ".tif", sep =
                    "")

# save the raster
writeBin(content(poll.response, "raw"), poll.name)

pm25 = raster(poll.name)


## extract values from rasters at the smoke communities  
pm25_smoke = raster::extract(pm25,smoke.communities)*1000000000

df = cbind(smoke.communities$ID, pm25_smoke)

df = as.data.frame(df)

# write.csv(combined,"current_fw.csv")

######### POSTGRES #############



colnames(df) <- c("idcomm", "pm25_cur_temp_val")

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

## create query 
query <- "UPDATE smoke.pm25_cur
SET pm25_cur = pm25_cur_temp.pm25_cur_temp_val
FROM public.pm25_cur_temp
WHERE commid = idcomm"


#### update master table ####
DBI::dbSendQuery(con, query)

# Close the connection
dbDisconnect(con)














