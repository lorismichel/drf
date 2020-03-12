library(ggplot2)
library(mrf)
library(viridis)
library(sf)
library(maps)
library(data.table)

us.map <- map_data("world", region = c('USA'))

# defining thresholds and variables
unhealthy_thresholds = c(9.5, 76, 101, 0.071, 155, 35.5) #threshold value for "Unhealthy for sensitive groups"
moderate_thresholds = c(4.5, 36, 54, 0.055, 55, 12.1) #threshold value for "Moderate"
names(moderate_thresholds) = c('CO', 'SO2', 'NO2', 'O3', 'PM10', 'PM2.5')
names(unhealthy_thresholds) = c('CO', 'SO2', 'NO2', 'O3', 'PM10', 'PM2.5')

# air quality category
airQuCat <- function(y) {
  ind_u <- y > unhealthy_thresholds
  ind_m <- y > moderate_thresholds
  return(if (any(ind_u)) "unhealthy" else if (any(ind_m)) "moderate" else "good")
}

# loading data and subsampling
#load('~/Downloads/air_data.RData')
load('~/Documents/projects/heterogeneity/air_data/computed_data/air_data.RData')
site_data <- unique(air_data[,c('Latitude', 'Longitude', "Elevation","Land.Use","Location.Setting")])
site_data$site_id = 1:nrow(site_data)
find_site = function(measurement){
  dist = (site_data$Latitude - as.numeric(measurement['Latitude']))^2 + (site_data$Longitude - as.numeric(measurement['Longitude']))^2 
  return(site_data$site_id[which.min(dist)])
}
air_data$site_id = apply(air_data, 1, find_site)

air_data <- air_data[, -c(7,8)]

#see how many sites we have for choice of 2 pollutants (or 1)
for(i in 13:18){
  for(j in 13:18){
    df=na.omit(air_data[,c(i,j,25)])
    print(c(i, j, length(unique(df$site_id))))
  }
}

#choose target pollutants
pollutants = c('PM2.5', 'O3') #leave only PM2.5 and O3
targets = c(paste('max_', pollutants, sep=''))
dataset <- air_data[-c(13:24)]
dataset <- cbind(dataset, air_data[, targets])
dataset <- na.omit(dataset) 
length(unique(dataset$site_id))
dataset <- dataset[sample(1:nrow(dataset), size = 100000, replace = FALSE),]
length(unique(dataset$site_id))
min(table(dataset$site_id))
site_data$count <- apply(site_data, 1, function(site) sum(dataset$site_id == as.numeric(site['site_id'])))
hist(site_data$count)

# target response
Y <- dataset[, targets]

# first modelling
X <- data.frame(dataset[,c('Longitude', 'Latitude', "Elevation","Land.Use","Location.Setting")])
X$Land.Use <- factor(X$Land.Use)
X$Location.Setting <- factor(X$Location.Setting)
# X$Weekday = factor(weekdays(dataset$Date))
# X$Month = factor(month(dataset$Date))
X <- model.matrix(~.-1, data = X)


mRF_fourier <- mrf(X = X, Y = Y, num.trees = 2000, 
           splitting.rule = "fourier", num_features = 3)

# OOB (can be expensive without subsampling)
#wOOB_fourier <- get_sample_weights(mRF_fourier)
wOOB_fourier <- predict(mRF_fourier, newdata=model.matrix(~.-1, data = site_data[,c('Longitude','Latitude',"Elevation","Land.Use","Location.Setting")]))$weights

################################################################################################################
# AQI -> joint probability

# categories
cat <- apply(Y,1,airQuCat)

# get predictions for probabilites
pR_fourier_good     <- apply(wOOB_fourier, 1, function(w) sum(w[cat=="good"]))
pR_fourier_moderate <- apply(wOOB_fourier, 1, function(w) sum(w[cat=="moderate"]))
pR_fourier_unhealthy <- apply(wOOB_fourier, 1, function(w) sum(w[cat=="unhealthy"]))

site_data$pR_fourier_good     <- pR_fourier_good
site_data$pR_fourier_moderate <- pR_fourier_moderate
site_data$pR_fourier_unhealthy <- pR_fourier_unhealthy


e <- ggplot(site_data, aes(x=Longitude, y=Latitude, color=Elevation)) + 
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) + 
  geom_point(aes(size=0.01*ifelse(count==0, NA, count))) + scale_color_viridis_c(option='magma') +
  coord_cartesian(xlim=c(-160, -70), ylim=c(18, 65)) +
  theme_bw()

p1 <- ggplot(site_data, aes(x=Longitude, y=Latitude, color=pR_fourier_good)) + 
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) + 
  geom_point(size=0.8) + scale_color_viridis_c(option='magma') +
  coord_cartesian(xlim=c(-160, -70), ylim=c(18, 65)) +
  theme_bw()

p2 <- ggplot(site_data, aes(x=Longitude, y=Latitude, color=pR_fourier_moderate)) + 
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) + 
  geom_point(size=0.8) + scale_color_viridis_c(option='magma') +
  coord_cartesian(xlim=c(-160, -70), ylim=c(18, 65)) +
  theme_bw()

p3 <- ggplot(site_data, aes(x=Longitude, y=Latitude, color=pR_fourier_unhealthy)) + 
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) + 
  geom_point(size=0.8) + scale_color_viridis_c(option='magma') +
  coord_cartesian(xlim=c(-160, -70), ylim=c(18, 65)) +
  theme_bw()

require(gridExtra)
grid.arrange(e, p1, p2, p3,
             nrow = 2, ncol = 2)

ggplot(site_data, aes(x=Elevation, y=pR_fourier_unhealthy)) + 
  geom_point(size=1) + scale_color_viridis_c(option='magma') + facet_wrap(~Land.Use) +
  theme_bw()

ggplot(site_data, aes(x=Elevation, y=pR_fourier_unhealthy)) + 
  geom_point(size=1) + scale_color_viridis_c(option='magma') + facet_wrap(~Location.Setting) +
  theme_bw()

####################################################################################################################
# prediction regions

#pooled targets
idx = sample(1:nrow(air_data), 100000)
ggplot(air_data[idx,], aes_string(x=targets[1], y=targets[2])) +
  geom_point(size=0.5) +
  geom_vline(xintercept=moderate_thresholds[pollutants[1]], linetype='dashed', color='green') +
  geom_hline(yintercept=moderate_thresholds[pollutants[2]], linetype='dashed', color='green') +
  geom_vline(xintercept=unhealthy_thresholds[pollutants[1]], linetype='dashed', color='red') +
  geom_hline(yintercept=unhealthy_thresholds[pollutants[2]], linetype='dashed', color='red')# +
 #coord_cartesian(xlim=c(0, 100), ylim = c(0,500))# +
 # theme_bw()

#all sites position
plot(air_data[idx,]$Longitude, air_data[idx,]$Latitude)
ggplot(air_data[idx,], aes(x=Longitude, y=Latitude, color=Elevation)) +
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) + 
  geom_point(size=0.4) + scale_color_viridis_c(option='magma') +
  coord_cartesian(xlim=c(-160, -70), ylim=c(18, 65)) +
  theme_bw()

point_description = function(test_point){
  out = paste(test_point$Longitude[1])
  out = paste(out, test_point$Latitude[1])
#  out = paste(out, weekdays(test_point$Date[1]))
#  out = paste(out, 'month:', month(test_point$Date[1]))
  out = paste(out, '\n', sep='')
  out = paste(out, "elevation:", floor(test_point$Elevation[1]))
  out = paste(out, "land use:", test_point$Land.Use[1])
  out = paste(out, "setting:", test_point$Location.Setting[1])

  return(out)
}
#
# predRegion <- function(y, w, nsim = 10000) {
#   if (ncol(y)!=2) {
#     stop("only valid for 2 dimenensional response.")
#   }
#   
#   require(MASS)
#   
#   Ys <- y[sample(1:nrow(y), nsim, replace = TRUE, prob = w),] 
#   
#   
#   d <- MASS::kde2d(Ys[,1], Ys[,2], n = 25, h = c(width.SJ(Ys[,1]), width.SJ(Ys[,2])))
#   
#   unlisted.z <- as.numeric(d$z) / sum(d$z) 
#   sorted.z <- sort(unlisted.z, decreasing = TRUE)
#   cum.sorted.z <- cumsum(sorted.z)
#   id <- which(cum.sorted.z >= (1-alpha))[1]
#   contour(d$x,d$y,d$z, levels = sorted.z[id] * sum(d$z), drawlabels = FALSE)
#   grid <- expand.grid(d$x, d$y)
#   points(grid[,1], grid[,2], cex = d$z)
# }
# 

#distribution of training sites
ggplot(site_data[site_data$count!=0,], aes(x=Longitude, y=Latitude)) + 
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) + 
  geom_point(aes(size=count/nrow(dataset), color=Land.Use, shape=Location.Setting)) + #scale_color_viridis_c(option='magma') +
  scale_size_area(max_size=3) +
  coord_cartesian(xlim=c(-160, -70), ylim=c(18, 65)) +
  theme_bw()

for(idx in sample((1:nrow(site_data))[site_data$count==0], 5, replace=FALSE)){
  dataset$weight = wOOB_fourier[idx,]
  site_data$weight <- apply(site_data, 1, function(site) sum(dataset$weight[as.numeric(site['site_id']) == dataset$site_id]))
  
  #plot distribution of training sites which get high weight
  gg = ggplot(site_data[site_data$weight!=0,], aes(x=Longitude, y=Latitude)) +
    geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) +
    annotate(geom='point', x=site_data$Longitude[idx], y=site_data$Latitude[idx], shape=8, color='red', size=6) + 
    geom_point(aes(size=weight, color=Land.Use, shape=Location.Setting)) +
    scale_size_area(max_size=5) +
    #scale_color_viridis_c(option='magma') +
    coord_cartesian(xlim=c(-160, -70), ylim=c(18, 65)) +
    ggtitle(point_description(site_data[idx,])) +
    #coord_cartesian(xlim=c(-130, -60), ylim=c(25, 55)) +
    theme_bw()# +
    #theme(legend.position='none')
  plot(gg)
  
  #plot distribution of pollutants
  subsample = dataset[sample(1:nrow(dataset), 3000, replace=TRUE, prob=dataset$weight),]
  gg = ggplot(data=dataset[dataset$weight!=0, ], aes_string(x=targets[1], y=targets[2])) +
    geom_point(aes(alpha=weight*100), size=0.5)+
    stat_density2d(data=subsample, aes(alpha=..level.., fill=..level..), size=2, bins=10, geom="polygon") + 
    # scale_fill_gradient(low = "yellow", high = "red") +
    # scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
    geom_density2d(data=subsample, colour="blue", bins=6) +
    geom_vline(xintercept=moderate_thresholds[pollutants[1]], linetype='dashed', color='green') +
    geom_hline(yintercept=moderate_thresholds[pollutants[2]], linetype='dashed', color='green') +
    geom_vline(xintercept=unhealthy_thresholds[pollutants[1]], linetype='dashed', color='red') +
    geom_hline(yintercept=unhealthy_thresholds[pollutants[2]], linetype='dashed', color='red') +
    coord_cartesian(xlim=c(0, 40), ylim = c(0,0.08)) +
    ggtitle(point_description(site_data[idx,])) +
    theme_bw()

  #plot(gg)


#  predRegion(matrix(cbind(dataset$max_PM2.5, dataset$max_PM10), ncol=2), as.vector(dataset$mrf_weights))
}

library(con2aqi)
# index mapping
# move to an index
map_CO2_2_index <- function(x) con2aqi(pollutant="co",con=x,type="8h")
map_S02_2_index <- function(x) con2aqi(pollutant="s02",con=1000*x,type="1h")
