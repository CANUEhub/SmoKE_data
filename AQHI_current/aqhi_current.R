## this script is for the current AQHI value 


##### import libraries #####

library(httr)
library(raster)
library(sf)
library(tidyverse)
library(rvest)


##### import shapefile  #####

smoke.communities = "/Users/priyapatel/Library/CloudStorage/OneDrive-UniversityofToronto/!CANUE/AQHI Work/shapefiles/smoke_shp/smoke.shp"
smoke.communities = st_read(smoke.communities)

# set wd 

wd = dirname(rstudioapi::getSourceEditorContext()$path)

setwd(wd)

# get the current time and date 
current.time = as.POSIXlt(Sys.time(), tz = "UTC")
hour(current.time)

##### these are the dates of interest 

# get the current hour and date 
date_nearest.hour = floor_date(current.time, "hour") - 1

# get the hour before 
date_nearest.hour_2 = date_nearest.hour - hours(1) 

# and the hour before 
date_nearest.hour_3 = date_nearest.hour - hours(2)


#### this is the file format 
# https://dd.weather.gc.ca/model_rdaqa/10km/20/20231005T20Z_MSC_RDAQA-FW_PM10_Sfc_RLatLon0.09_PT0H.grib2
# {YYYYMMDD}T{HH}Z_MSC_{SYSTEM}_{VAR}_{LVLTYPE}_{grid}{resolution}_PT0H.grib2


########## GET THE RASTERS  ###########


getRas = function(poll, date_var, level) {
  day.current = ifelse(
    day(date_var) < 10,
    paste(as.character(0), day(date_var) , sep = ""),
    day(date_var)
  )
  hour.current = ifelse(
    hour(date_var) < 10,
    paste(as.character(0), hour(date_var) , sep = ""),
    hour(date_var)
  )
  
  # create a URL
  
  endpoint.url = paste(
    "https://dd.weather.gc.ca/model_rdaqa/10km/",
    hour.current,
    "/",
    year(date_var),
    str_pad(month(date_var), 2, pad = "0"),
    day.current,
    "T",
    hour.current,
    "Z_MSC_",
    level,
    "_",
    poll,
    "_Sfc_RLatLon0.09_PT0H.grib2",
    sep = ""
  )
  
  # get the file 
  poll.response <- GET(endpoint.url)
  
  # give the tif a name 
  poll.name = paste(wd,"/",poll,"_",date(date_var),"_H",hour(date_var),".tif",sep="")
  
  # save the raster 
  writeBin(content(poll.response, "raw"), poll.name)
  
  
}

# get current hour 
getRas("NO2",date_nearest.hour,"RDAQA-Prelim")
getRas("PM2.5",date_nearest.hour,"RDAQA-Prelim")
getRas("O3",date_nearest.hour,"RDAQA-Prelim")

# get last hour 
getRas("NO2",date_nearest.hour_2,"RDAQA")
getRas("PM2.5",date_nearest.hour_2,"RDAQA-FW")
getRas("O3",date_nearest.hour_2,"RDAQA")


# get 2nd last hour 
getRas("NO2",date_nearest.hour_3,"RDAQA")
getRas("PM2.5",date_nearest.hour_3,"RDAQA-FW")
getRas("O3",date_nearest.hour_3,"RDAQA")



no2_stack = stack(list.files(path = wd, pattern = "NO2"))
o3_stack = stack(list.files(path = wd, pattern = "O3"))
pm25_stack = stack(list.files(path = wd, pattern = "PM2.5"))


no2 = calc(no2_stack, fun = mean)*1000000000
o3 = calc(o3_stack, fun = mean)*1000000000
pm25 = calc(pm25_stack, fun = mean)*1000000000



# Calculate AQHI

A = (exp(0.000537 * o3) - 1)
B = (exp(0.000871 * no2) - 1)
C = (exp(0.000487 * pm25) - 1)

# calculate files
aqhi = (10 / 10.4) * 100 * (A + B + C)


## extract values from rasters at the AQHI stations 
aqhi_smoke = raster::extract(aqhi,smoke.communities)

df_temp = cbind(smoke.communities %>% st_drop_geometry(),aqhi_smoke)

df = cbind(df_temp$ID,df_temp$aqhi_smoke)

df = as.data.frame(df)

colnames(df) <- c("idcomm", "aqhi_cur_temp_val")

## table to track timestamp 

current.time.end = as.POSIXlt(Sys.time(), tz = "UTC")

texttime = format(current.time.end, "%Y-%m-%d %H:%M:%S %Z")

val = "update"

df.update = cbind(val,texttime)

colnames(df.update) <- c("lu", "aqhi_cur_lu")

df.update = as.data.frame(df.update)






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
DBI::dbWriteTable(con, "aqhi_cur_temp", df,overwrite=TRUE)

DBI::dbWriteTable(con, "aqhi_cur_temp_lu", df.update,overwrite=TRUE)

## create query 

query <- "UPDATE smoke.aqhi_cur 
SET aqhi_cur_val = aqhi_cur_temp.aqhi_cur_temp_val
FROM public.aqhi_cur_temp
WHERE commid = idcomm"


query.timestamp <- "UPDATE smoke.curfor_lu
SET aqhi_cur_lu = aqhi_cur_temp_lu.aqhi_cur_lu
FROM public.aqhi_cur_temp_lu"



#### update master table ####
DBI::dbSendQuery(con, query)
DBI::dbSendQuery(con, query.timestamp)

# Close the connection
dbDisconnect(con)



