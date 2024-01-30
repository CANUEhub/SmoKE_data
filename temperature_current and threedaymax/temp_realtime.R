## this script is to get the current temperature


# set working directory 
wd = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)

# Data is coming from here:  https://eccc-msc.github.io/open-data/msc-data/nwp_rdps/readme_rdps-datamart_en/

#### this is the file format : CMC_reg_Variable_LevelType_level_ps10km_YYYYMMDDHH_Phhh.grib2



##### import libraries #####

library(httr)
library(raster)
library(sf)
library(tidyverse)
library(rvest)


##### import shapefile  #####

smoke.communities = "/Users/priyapatel/Library/CloudStorage/OneDrive-UniversityofToronto/!CANUE/AQHI Work/shapefiles/smoke_shp/smoke.shp"

smoke.communities = st_read(smoke.communities)

# get the current time and date 
current.dt = as.POSIXlt(Sys.time(), tz = "UTC")

##### these are the dates of interest 

# get the current hour and date 
date_nearest.hour = floor_date(current.dt, "hour")

########## GET THE RASTERS  ###########

month.current = str_pad(month(current.dt), 2, pad = "0")
day.current = str_pad(day(current.dt), 2, pad = "0")
hour.current = str_pad((hour(date_nearest.hour)), 3, pad = "0")
HH = "00"

# create a URL

# https://dd.weather.gc.ca/model_gem_regional/10km/grib2/00/000/CMC_reg_TMP_TGL_2_ps10km_2024011200_P000.grib2

endpoint = paste(
  "https://dd.weather.gc.ca/model_gem_regional/10km/grib2/00/",
  hour.current,
  "/CMC_reg_TMP_TGL_2_ps10km_",
  year(current.dt),
  month.current,
  day.current,
  HH,
  "_P",
  hour.current,
  ".grib2",
  sep = ""
)

# get the file
poll.response <- GET(endpoint)

# give the tif a name
poll.name = paste("TMP", "_", date(current.dt), "_H", hour(current.dt), ".tif", sep =
                    "")

# save the raster
writeBin(content(poll.response, "raw"), poll.name)

tmp = raster(poll.name)


## extract values from rasters at the smoke communities  
tmp_smoke = raster::extract(tmp,smoke.communities)

df = cbind(smoke.communities$ID, tmp_smoke)

df = as.data.frame(df)


# write.csv(combined,"current_tmp.csv")






colnames(df) <- c("idcomm", "cel_cur_temp_val")

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
DBI::dbWriteTable(con, "cel_cur_temp", df,overwrite=TRUE)

## create query 

query <- "UPDATE smoke.cel_cur 
SET temp_cur = cel_cur_temp.cel_cur_temp_val
FROM public.cel_cur_temp
WHERE commid = idcomm"


#### update master table ####
DBI::dbSendQuery(con, query)

# Close the connection
dbDisconnect(con)






