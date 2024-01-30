## this script is to get the current precipitation

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

endpoint = paste(
  "https://dd.weather.gc.ca/model_gem_regional/10km/grib2/00/",
  hour.current,
  "/CMC_reg_ACPCP_SFC_0_ps10km_",
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

# create a raster 
tmp = raster(poll.name)


## extract values from rasters at the smoke communities  
tmp_smoke = raster::extract(tmp,smoke.communities)

df = cbind(smoke.communities %>% st_drop_geometry(),tmp_smoke)

# write.csv(combined,"current_precip.csv")





colnames(df) <- c("idcomm", "prec_cur_temp_val")

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
DBI::dbWriteTable(con, "prec_cur_temp", df,overwrite=TRUE)

## create query 
# query <- "UPDATE smoke.aqhi_cur SET aqhi_cur_val = prec_cur_temp.aqhi_cur FROM public.prec_cur_temp WHERE commID = aqhi_cur.commid"
query <- "UPDATE smoke.aqhi_cur 
SET prec_cur_val = prec_cur_temp.prec_cur_temp_val
FROM public.prec_cur_temp
WHERE commid = idcomm"


#### update master table ####
DBI::dbSendQuery(con, query)

# Close the connection
dbDisconnect(con)