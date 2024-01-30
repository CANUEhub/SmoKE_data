
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

# Data is coming from here:  https://eccc-msc.github.io/open-data/msc-data/nwp_rdps/readme_rdps-datamart_en/

#### this is the file format : CMC_reg_Variable_LevelType_level_ps10km_YYYYMMDDHH_Phhh.grib2


#### import smoke data #### 

smoke.communities = "../smoke_shp/smoke.shp"
smoke.communities = st_read(smoke.communities)

HH = "00"
current.dt = as.POSIXlt(Sys.time(), tz = "UTC")
formatted.date = format(current.dt, "%Y%m%d")

################ 1. Download all pollutant files for 72 hours. ################

df <- data.frame(ID = 1:698)
  
for (temp.hour in 1:84) {
  # this is the hour we are extracting.
  # print(temp.hour)
  
  curr.hour = str_pad(temp.hour, 3, pad = "0")
  curr.month = str_pad(month(current.dt), 2, pad = "0")
  curr.day = str_pad(day(current.dt), 2, pad = "0")

  # URL 
  endpoint = paste(
    "https://dd.weather.gc.ca/model_gem_regional/10km/grib2/00/",
    curr.hour,
    "/CMC_reg_APCP_SFC_0_ps10km_",
    year(current.dt),
    curr.month,
    curr.day,
    HH,
    "_P",
    curr.hour,
    ".grib2",
    sep = ""
  )
  
  # print(endpoint)
  
  # # get the file
  poll.response <- GET(endpoint)
  

  #
  # # give the tif a name
  poll.name = paste("forecast_tmp",
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
  temp.ras = raster(poll.name)
  
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

#### eastern and atlantic time zones: ####

##### day 1 #####
ET = combined[(combined$name == 'ET'),]

et.day1 = ET[,"028"] - ET[,"005"]

##### day 2 #####

et.day2 = ET[,"052"] - ET[,"029"]

##### day 3 #####

et.day3 = ET[,"077"] - ET[,"053"]

######################

#### pacific time zone: ####

##### day 1 #####
PT = combined[(combined$name == 'PT'),]

pt.day1 = PT[,"031"] - PT[,"008"]

##### day 2 #####

pt.day2 = PT[,"055"] - PT[,"032"]

##### day 3 #####

pt.day3 = PT[,"080"] - PT[,"056"]





#### mountain standard time: ####

##### day 1 #####
MT = combined[(combined$name == 'MT'),]

mt.day1 = MT[,"030"] - MT[,"007"]

##### day 2 #####

mt.day2 = MT[,"054"] - MT[,"031"]

##### day 3 #####

mt.day3 = MT[,"079"] - MT[,"055"]





#### central standard time: ####

##### day 1 #####
CT = combined[(combined$name == 'CT'),]

ct.day1 = CT[,"029"] - CT[,"006"]

##### day 2 #####

ct.day2 = CT[,"053"] - CT[,"030"]

##### day 3 #####

ct.day3 =  CT[,"078"] - CT[,"054"]




#### atlantic standard time: ####

##### day 1 #####
AT = combined[(combined$name == 'AT'| combined$name =="NT"),]

at.day1 = AT[,"027"] - AT[,"004"]

##### day 2 #####

at.day2 = AT[,"051"] - AT[,"028"]

##### day 3 #####

at.day3 = AT[,"076"] - AT[,"052"]


# combine all the data 
day1.combine = as.data.frame(c(ct.day1, et.day1, pt.day1, mt.day1,at.day1))
day2.combine = as.data.frame(c(ct.day2, et.day2, pt.day2, mt.day2,at.day2))
day3.combine = as.data.frame(c(ct.day3, et.day3, pt.day3, mt.day3,at.day3))


day1.combine = day1.combine[order(as.numeric(rownames(day1.combine))),,drop=FALSE]
day2.combine = day2.combine[order(as.numeric(rownames(day2.combine))),,drop=FALSE]
day3.combine = day3.combine[order(as.numeric(rownames(day3.combine))),,drop=FALSE]

#  limit decimal places 
options(digits=2)

# combine in the table 
df <- data.frame(idcomm = 1:698, day1.combine,day2.combine,day3.combine)

colnames(df)[2] <- "prec_sumtoday"
colnames(df)[3] <- "prec_sumtomorrow"
colnames(df)[4] <- "prec_sumovermorrow"


#####################



## table to track timestamp 

current.time.end = as.POSIXlt(Sys.time(), tz = "UTC")

texttime = format(current.time.end, "%Y-%m-%d %H:%M:%S %Z")

val = "update"

df.update = cbind(val,texttime)

colnames(df.update) <- c("lu", "var_lu")

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
DBI::dbWriteTable(con, "prec_temp", df,overwrite=TRUE)
DBI::dbWriteTable(con, "temp_lu", df.update,overwrite=TRUE)


## create query 

query <- "UPDATE smoke.prec_for 
SET prec_sumtoday = prec_temp.prec_sumtoday,
prec_sumtomorrow = prec_temp.prec_sumtomorrow,
prec_sumovermorrow = prec_temp.prec_sumovermorrow
FROM public.prec_temp
WHERE commid = idcomm"


query.timestamp <- "UPDATE smoke.curfor_lu
SET prec_for_lu = temp_lu.var_lu
FROM public.temp_lu"

#### update master table ####
DBI::dbSendQuery(con, query)
DBI::dbSendQuery(con, query.timestamp)


# Close the connection
dbDisconnect(con)



################# UPDATE TEXT FILE ##############

# Open the file in append mode
file_path <- "update_log.txt"

con <- file(file_path, open = "a")

# Text to be added
text_to_add <- paste("The prec was last updated on.",current.time.end, "UTC")

# Append the text to the file
cat(text_to_add, file = con, "\n")

# Close the file connection
close(con)













