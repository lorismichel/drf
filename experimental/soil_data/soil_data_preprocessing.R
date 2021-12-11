# loading data

# libs
require(data.table)
require(geojsonio)
require(broom)

loadSoilData <- function(PATH.data = "~/Downloads/data_for_ETH.csv", 
                         PATH.coords = "~/Downloads/coords_soil.csv") {
  
  soil_data <- fread(PATH.data)
  coords <- fread(PATH.coords)
  soil_data$long <- coords$V1
  soil_data$lat <- coords$V2
  soil_data$X <- NULL
  soil_data$Y <- NULL
  
  # Geospatial data available at the geojson format for region of interest
  spdf <- geojson_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson",  what = "sp")
  # Since it is a bit to much data, I select only a subset of it:
  spdf <- spdf[ substr(spdf@data$code,1,2)  %in% c("48", "30", "34", "11", "66") , ]
  # I need to fortify the data AND keep trace of the commune code! (Takes ~2 minutes)
  spdf_fortified <- tidy(spdf, region = "code")
  
  
  # add id for commune
  loc <- data.table(spdf_fortified)
  loc[, meanLong := mean(long), by="id"]
  loc[, meanLat := mean(lat), by="id"]
  
  s <- loc[,.(meanLong = mean(long), meanLat = mean(lat)), by="id"]
  smat <- cbind(s$meanLong, 
                s$meanLat)
  ids <- c()
  for (i in 1:nrow(soil_data)) {
    ids <- c(ids,s$id[which.min(sqrt((smat[,1]-soil_data$long[i])^2 + (smat[,2]-soil_data$lat[i])^2))])
  }
  
  soil_data$id <- ids
  
  
  return(list(soil_data = soil_data, geo_data = spdf_fortified))
}





