library(ggplot2)
library(mrf)
library(fastDummies)
library(vcd)

set.seed(22)
load('~/Documents/projects/heterogeneity/air_data/computed_data/air_data.RData')
length(unique(air_data$Site.ID))

#remove outliers
for(i in 17:22){
  air_data = air_data[is.na(air_data[, i]) | air_data[, i] < quantile(air_data[, i], 0.9995, na.rm=TRUE), ] 
}

# #see how many sites we have for choice of 2 pollutants (or 1)
# for(i in 11:16){
#   for(j in 11:16){
#     df=na.omit(air_data[,c(1,i,j)])
#     print(c(i, j, length(unique(df$Site.ID))))
#   }
# }


#choose target pollutants
pollutants = c('PM2.5', 'NO2', 'O3', 'PM10', 'CO', 'SO2')
targets = c(paste('max_', pollutants, sep=''))
dataset <- air_data[-c(11:22)]
dataset <- cbind(dataset, air_data[, targets])
dataset <- dataset[!(dataset$State.Name %in% c('Alaska', 'Hawaii', 'Country Of Mexico', 'Virgin Islands', 'Puerto Rico')),]
dataset <- na.omit(dataset) 
length(unique(dataset$Site.ID))
#dataset <- dataset[sample(1:nrow(dataset), size = 50000, replace = FALSE),]
length(unique(dataset$Site.ID))
min(table(dataset$Site.ID))
site_data$count <- apply(site_data[, "Site.ID", drop=FALSE], 1, function(site_id) sum(dataset$Site.ID == site_id))
hist(site_data[site_data$count != 0, ]$count)

# target response
Y <- dataset[, targets]
Y = as.matrix(Y)

# modelling
structable(~ Land.Use + Location.Setting, data = dataset)
X <- data.frame(dataset[, c('Longitude', 'Latitude', "Elevation", "Land.Use", "Location.Setting")])
X$Land.Use <- factor(X$Land.Use)
X$Location.Setting <- factor(X$Location.Setting)
X$Weekday = as.numeric(strftime(dataset$Date, "%u"))
X$Month = as.numeric(strftime(dataset$Date, "%m"))
X$Time = as.numeric(dataset$Date - min(air_data$Date))
X <- dummy_cols(X, remove_selected_columns=TRUE)
colnames(X)
X = as.matrix(X)

#save(X, Y, file = "~/Documents/projects/heterogeneity/air_data/air_data_benchmark.Rdata")

#########################################################################
pollutants = c('PM2.5', 'O3', 'PM10')
targets = c(paste('max_', pollutants, sep=''))
dataset <- air_data[-c(11:22)]
dataset <- cbind(dataset, air_data[, targets])
dataset <- dataset[!(dataset$State.Name %in% c('Alaska', 'Hawaii', 'Country Of Mexico', 'Virgin Islands', 'Puerto Rico')),]
dataset <- dataset[!(dataset$Land.Use %in% c('UNKNOWN', 'DESERT', 'FOREST', 'MILITARY RESERVATION', 'BLIGHTED AREAS', 'MOBILE')),]
dataset <- dataset[!(dataset$Location.Setting %in% c('UNKNOWN')),]
dataset <- na.omit(dataset) 
length(unique(dataset$Site.ID))
set.seed(22)
dataset <- dataset[sample(1:nrow(dataset), size = 50000, replace = FALSE),]
length(unique(dataset$Site.ID))
min(table(dataset$Site.ID))
site_data$count <- apply(site_data[, "Site.ID", drop=FALSE], 1, function(site_id) sum(dataset$Site.ID == site_id))
hist(site_data[site_data$count != 0, ]$count)

# target response
Y <- dataset[, targets]
Y = as.matrix(Y)

# modelling
structable(~ Land.Use + Location.Setting, data = dataset)
X <- data.frame(dataset[, c('Longitude', 'Latitude', "Elevation", "Land.Use", "Location.Setting")])
X$Land.Use <- factor(X$Land.Use)
X$Location.Setting <- factor(X$Location.Setting)
X <- dummy_cols(X, remove_selected_columns=TRUE)
colnames(X)
X = as.matrix(X)

#save(X, Y, file = "~/Documents/projects/heterogeneity/air_data/air_data_benchmark2.Rdata")
