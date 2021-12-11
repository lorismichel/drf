# setwd("~/Documents/projects/DRF/data/air_data/")
# 
# file_list = list.files(path="downloaded_data/measurements", pattern="", full.names=TRUE, recursive=FALSE)
# measurements = data.frame()
# for(i in 1:length(file_list)){
#   print(i)
#   file = file_list[i]
#   df = read.csv(file)
#   measurements = rbind(measurements, df)
# }
# 
site_data = read.csv(file = "downloaded_data/aqs_sites.csv")
# fix altitudes
library(httr)

site_data = site_data[site_data$Latitude!=0,]
site_data = site_data[is.na(site_data$Latitude)==FALSE,]

for(i in 1:nrow(site_data)){
  if(i %% 50 == 0){
    print(i)
  }
#   
#   if(site_data$Longitude[i] == 0){
#     print('searching address')
#     request = paste("https://geocode.xyz/", as.character(site_data$Address[i]), ',', as.character(site_data$Zip.Code[i]), 
#                     ',', as.character(site_data$City.Name[i]), ',', as.character(site_data$County.Name[i]), 
#                     ',', as.character(site_data$State.Name[i]), ',USA?geoit=json', sep='')
#     request = paste(request, "&auth=573591305820653608942x4847", sep='')
#     r <- GET(request)
#     site_data$Longitude[i] = content(r)$longt
#     site_data$Latitude[i] = content(r)$latt
#     sprintf("found: \"%s\" \"%s\"\n", content(r)$longt, content(r)$latt)
# }
  
  request = paste("https://geocode.xyz/", as.character(site_data$Latitude[i]), ',', as.character(site_data$Longitude[i]), '?geoit=json', sep='')
  request = paste(request, "&auth=573591305820653608942x4847", sep='')
  r <- GET(request)
  if(is.na(site_data$Zip.Code[i]) && length(content(r)$postal) > 0){
    site_data$Zip.Code[i] = content(r)$postal
  }
  if(length(content(r)$elevation) > 0){
     site_data$Elevation[i] = content(r)$elevation
  } else {
    r <- GET(paste("https://maps.googleapis.com/maps/api/elevation/json?key=AIzaSyBGkVwUNgBo7bC0PKEIIenjkEkBUl6Vo6g&locations=", as.character(site_data$Latitude[i]), ',', as.character(site_data$Longitude[i]), sep=''))
    site_data$Elevation[i] = content(r)$results[[1]]$elevation
  }
}

# #monitors_data = read.csv(file='downloaded_data/aqs_monitors.csv') #Not currently needed
# 
#save(measurements, site_data, file="computed_data/raw_data.RData")

setwd("~/Documents/projects/heterogeneity/air_data/")
load(file="computed_data/raw_data.RData")

##################################################################
# measurements
#################################################################

#remove entries where something strange happened
measurements = measurements[measurements$Event.Type=='None',] 
# For the same site, use only one measuring device for the same pollutant
#measurements = measurements[measurements$POC==1,]
# Use only one measurement criterion for gases ('' is old criterion for PM2.5)
table(measurements$Pollutant.Standard)
measurements = measurements[!(measurements$Pollutant.Standard %in% c('', 'CO 1-hour 1971', 'SO2 3-hour 1971')),] 
measurements$Date.Local = as.Date(measurements$Date.Local)
# library(data.table)
# measurements = as.data.table(measurements)
# setorderv(measurements, c('State.Code','County.Code','Site.Num', 'Date.Local', 'Parameter.Code'))
# which(measurements[, length(Arithmetic.Mean), by = c('State.Code','County.Code','Site.Num', 'Date.Local', 'Parameter.Code')]$V1 > 1)
# measurements[, length(Arithmetic.Mean), by = c('State.Code','County.Code','Site.Num', 'Date.Local', 'Parameter.Code')][13928,]
# 
# View(measurements[measurements$State.Code==6 & measurements$County.Code==37 & measurements$Site.Num==1103 & measurements$Date.Local=='2015-01-01',])

keep_variables = c(
  'Date.Local',
  'State.Name',
  'County.Name',
  #'City.Name',
  'State.Code',
  'County.Code',
  'Site.Num',
  'Latitude',
  'Longitude',
  'Parameter.Name',
  'Arithmetic.Mean',
  'X1st.Max.Value',
  'POC'
)
df = measurements[, keep_variables]
library(data.table)
df = as.data.table(df)
# if multiple values of POC (measurement device at the same location for the same pollutant), take average
df = df[, list(mean(Arithmetic.Mean), mean(X1st.Max.Value)), by=c(keep_variables[1:9])]
names(df)[names(df) == "V1"] <- "mean"
names(df)[names(df) == "V2"] <- "max"
df$Parameter.Name <- factor(df$Parameter.Name,
                            levels = c('Carbon monoxide',
                                       'Sulfur dioxide',
                                       'Nitrogen dioxide (NO2)',
                                       'Ozone',
                                       'PM10 Total 0-10um STP',
                                       'PM2.5 - Local Conditions'),
                            labels = c('CO',
                                       'SO2',
                                       'NO2',
                                       'O3',
                                       'PM10',
                                       'PM2.5')
)

# which(df[, length(mean), by = c(keep_variables[1:8])]$V1 > 1)
# df[, length(mean), by = c(keep_variables[1:8])][4306541,]
# 
# View(df[df$State.Code==2 & df$County.Code==20 & measurements$Site.Num==18 & measurements$Date.Local=='2015-01-01',])

library(tidyr)
df = pivot_wider(data=df,
                 id_cols=keep_variables[1:8],
                 names_from='Parameter.Name', 
                 values_from=c('mean', 'max'))

names(df)[names(df) == "Site.Num"] <- "Site.Number"

site_data$State.Code = as.numeric(site_data$State.Code) #The numbering of states is messed up
df$State.Code = factor(df$State.Code)
df$State.Code = as.numeric(df$State.Code)
df$State.Code[df$State.Code == 54] = 55
df$State.Code[df$State.Code == 53] = 54
df$State.Code[df$State.Code == 52] = 53

df$County.Code = as.numeric(df$County.Code)
df$Site.Number = as.numeric(df$Site.Number)
df$Site.ID = paste(df$State.Code, df$County.Code, df$Site.Number, sep='-')
names(df)[names(df) == "Date.Local"] <- "Date"
df$Date = as.Date(df$Date)

df$State.Name = as.character(df$State.Name)
df$County.Name = as.character(df$County.Name)

sapply(df, class)

#################################################################
# sites
#################################################################

#removes trailing zeroes
site_data$State.Code = as.numeric(site_data$State.Code)
site_data$County.Code = as.numeric(site_data$County.Code)
site_data$Site.Number = as.numeric(site_data$Site.Number)
site_data$Site.ID = paste(site_data$State.Code, site_data$County.Code, site_data$Site.Number, sep='-')

keep_variables = c(
  'Site.ID',
  'State.Code',
  'County.Code',
  'Site.Number',
  
  'State.Name',
  'County.Name',
  'City.Name',
  'Zip.Code',
  'Address',
  
  'Latitude',
  'Longitude',
  'Elevation',
  'Land.Use',
  'Location.Setting',
  
  'Site.Established.Date',
  'Site.Closed.Date'
)
site_data = site_data[, keep_variables]

site_data$City.Name[site_data$City.Name == "Not in a City"] = "Not in a city"
for(i in 1:nrow(site_data)){ #convert to Zip from Zip-4 for consistency
  site_data$Zip.Code[i] = strsplit(site_data$Zip.Code[i], '-')[[1]][1]
}
site_data$Elevation = as.numeric(site_data$Elevation)
site_data$Site.Established.Date = as.Date(site_data$Site.Established.Date)
site_data$Site.Closed.Date[site_data$Site.Closed.Date == ''] = NA
site_data$Site.Closed.Date = as.Date(site_data$Site.Closed.Date)

site_data$Location.Setting[site_data$Location.Setting==''] = 'UNKNOWN'
site_data$Location.Setting = factor(site_data$Location.Setting)

site_data$Land.Use[site_data$Land.Use==''] = 'UNKNOWN'
site_data$Land.Use = factor(site_data$Land.Use)

library(vcd)
structable(~ Land.Use + Location.Setting, data = site_data)

site_data$State.Name = as.character(site_data$State.Name)
site_data$County.Name = as.character(site_data$County.Name)
site_data$City.Name = as.character(site_data$City.Name)
site_data$Address = as.character(site_data$Address)

sapply(site_data, class)

##################################################################################################
# add site info to measurements
##################################################################################################

air_data = merge(df, site_data, by=c('Site.ID', 'State.Code', 'County.Code', 'Site.Number', 'Latitude', 'Longitude', 'State.Name', 'County.Name'))#, 'City.Name'))

ordering = c(
  "Site.ID",
  "State.Name", "County.Name", "City.Name",
  "Latitude", "Longitude",  "Elevation",  "Land.Use", "Location.Setting",     
  "Date",            
  "mean_CO", "mean_SO2", "mean_NO2", "mean_O3", "mean_PM10", "mean_PM2.5",           
  "max_CO", "max_SO2", "max_NO2", "max_O3", "max_PM10", "max_PM2.5"
)
air_data = air_data[, ordering]

structable(~ Land.Use + Location.Setting, data = air_data)
sapply(air_data, class)

###################################################################################################
save(air_data, site_data, file="computed_data/air_data.RData")
