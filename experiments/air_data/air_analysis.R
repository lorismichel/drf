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
air_data <- air_data[, -c(7,8)]

omit <- data.table(na.omit(air_data))
#omit <- omit[sample(1:nrow(omit), size = 5000, replace = FALSE),]
Y <- omit[,c(13:18)]

# categories
cat <- apply(Y,1,airQuCat)


# first modelling
X <- data.frame(omit[,c('Latitude', 'Longitude', "Elevation","Land.Use","Location.Setting")])
X$Land.Use <- factor(X$Land.Use)
X$Location.Setting <- factor(X$Location.Setting)
X <- model.matrix(~.-1, data = X)

mRF_fourier <- mrf(X = X, Y = Y, num.trees = 2000, 
           splitting.rule = "fourier", num_features = 3)

# OOB (can be expensive without subsampling)
#wOOB_fourier <- get_sample_weights(mRF_fourier)
site_data <- unique(air_data[,c('Latitude', 'Longitude', "Elevation","Land.Use","Location.Setting")])
wOOB_fourier <- predict(mRF_fourier, newdata=model.matrix(~.-1, data = site_data))$weights

# get predictions for probabilites
pR_fourier_good     <- apply(wOOB_fourier, 1, function(w) sum(w[cat=="good"]))
pR_fourier_moderate <- apply(wOOB_fourier, 1, function(w) sum(w[cat=="moderate"]))
pR_fourier_unhealthy <- apply(wOOB_fourier, 1, function(w) sum(w[cat=="unhealthy"]))

site_data$pR_fourier_good     <- pR_fourier_good
site_data$pR_fourier_moderate <- pR_fourier_moderate
site_data$pR_fourier_unhealthy <- pR_fourier_unhealthy



e <- ggplot(site_data, aes(x=Longitude, y=Latitude, color=Elevation)) + 
  geom_point(size=0.8) + scale_color_viridis_c(option='magma') +
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) + 
  coord_cartesian(xlim=c(-160, -70), ylim=c(18, 65)) +
  theme_bw()

p1 <- ggplot(site_data, aes(x=Longitude, y=Latitude, color=pR_fourier_good)) + 
  geom_point(size=0.8) + scale_color_viridis_c(option='magma') +
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) + 
  coord_cartesian(xlim=c(-160, -70), ylim=c(18, 65)) +
  theme_bw()

p2 <- ggplot(site_data, aes(x=Longitude, y=Latitude, color=pR_fourier_moderate)) + 
  geom_point(size=0.8) + scale_color_viridis_c(option='magma') +
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) + 
  coord_cartesian(xlim=c(-160, -70), ylim=c(18, 65)) +
  theme_bw()

p3 <- ggplot(site_data, aes(x=Longitude, y=Latitude, color=pR_fourier_unhealthy)) + 
  geom_point(size=0.8) + scale_color_viridis_c(option='magma') +
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) + 
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


# omit = data.table(na.omit(air_data))
# Y = omit[,c(13:18)]
# pairs(Y[1:5000, ], pch=16, cex=0.3)
# idx = sample(1:nrow(air_data), 100000)
# ggplot(air_data, aes(x=max_PM2.5, y=max_PM10)) +
#   geom_point(size=0.5) +
#   geom_vline(xintercept=moderate_thresholds['PM2.5'], linetype='dashed', color='yellow') +
#   geom_hline(yintercept=moderate_thresholds['PM10'], linetype='dashed', color='yellow') +
#   geom_vline(xintercept=unhealthy_thresholds['PM2.5'], linetype='dashed', color='red') +
#   geom_hline(yintercept=unhealthy_thresholds['PM10'], linetype='dashed', color='red') +
#   coord_cartesian(xlim=c(0, 100), ylim = c(0,500))# +
#  # theme_bw()
# 
# plot(air_data[idx,]$Longitude, air_data[idx,]$Latitude)
# ggplot(air_data[idx,], aes(x=Longitude, y=Latitude, color=Elevation)) + 
#   geom_point(size=0.4) + scale_color_viridis_c(option='magma') +
#   theme_bw()
# 
# View(air_data[air_data$Longitude==0,])
# us.map <- map_data("world", region = c('USA'))
# ggplot(us.map, aes(x = long, y = lat)) +
#   geom_polygon(aes(group = group), alpha=0.5) +
#   geom_point(data=air_data, size=0.4, aes(x=Longitude, y=Latitude, color=Elevation)) + 
#   scale_color_viridis_c(option='magma') +
#   coord_cartesian(xlim=c(-170, -60)) +
#   theme_bw()
# 
# ggplot(us.map, aes(x = long, y = lat)) +
#   geom_polygon(aes(group = group), alpha=0.3) +
#   geom_point(data=omit, size=0.7, aes(x=Longitude, y=Latitude, color=Elevation)) + 
#   scale_color_viridis_c(option='magma') +
#   coord_cartesian(xlim=c(-170, -60)) +
#   theme_bw()
# 
# X = omit[, 8:12]
# X$Weekday = factor(weekdays(omit$Date))
# X$Month = factor(month(omit$Date))
# X$Land.Use = droplevels(X$Land.Use)
# X$Location.Setting = droplevels(X$Location.Setting)
# X = model.matrix(~., X)[,-1]
# #X = as.numeric(X)
# Y = as.matrix(omit[, 19:24], ncol=6)
# #Y = as.numeric(Y)
# 
# mrf_fit = mrf(X=X, Y=Y, splitting.rule = "fourier", num_features=3, node_scaling = FALSE, min.node.size = 10)
# 
# point_description = function(test_point){
#   out = paste(test_point$State.Name[1])
#   out = paste(out, weekdays(test_point$Date[1]))
#   out = paste(out, 'month:', month(test_point$Date[1]))
#   out = paste(out, '\n', sep='')
#   out = paste(out, "elevation:", floor(test_point$Elevation[1]))
#   out = paste(out, "land use:", test_point$Land.Use[1])
#   out = paste(out, "setting:", test_point$Location.Setting[1])
#   
#   return(out)
# }
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
# for(idx in sample(1:nrow(X), 10, replace=FALSE)){
#   test_point = matrix(X[idx,], nrow=1)
#   omit$mrf_weights = predict(mrf_fit, newdata=test_point)$weights[1, ]
#   tmp = omit[, mean(mrf_weights), by=c('Longitude', 'Latitude', 'Elevation')]
#   names(tmp)[names(tmp) == "V1"] <- "mrf_weights"
#   
#   # gg = ggplot(us.map, aes(x = long, y = lat)) +
#   #   geom_polygon(aes(group = group), alpha=0.3) +
#   #   geom_point(data=tmp, aes(x=Longitude, y=Latitude, color=Elevation, size=ifelse(mrf_weights==0, NA, mrf_weights^0.5))) +
#   #   scale_size_area(max_size=3) + 
#   #   scale_color_viridis_c(option='magma') +
#   #   coord_cartesian(xlim=c(-130, -60), ylim=c(25, 55)) +
#   #   theme_bw()
#   
#   gg = ggplot(omit, aes(x=max_PM2.5, y=max_PM10)) +
#     geom_point(aes(size=ifelse(mrf_weights==0, NA, mrf_weights^0.5)))+
#     scale_size_area(max_size=2) +
#     geom_vline(xintercept=moderate_thresholds['PM2.5'], linetype='dashed', color='green') +
#     geom_hline(yintercept=moderate_thresholds['PM10'], linetype='dashed', color='green') +
#     geom_vline(xintercept=unhealthy_thresholds['PM2.5'], linetype='dashed', color='red') +
#     geom_hline(yintercept=unhealthy_thresholds['PM10'], linetype='dashed', color='red') +
#     coord_cartesian(xlim=c(0, 60), ylim = c(0,200)) +
#     ggtitle(point_description(omit[idx,]))
# #    theme_bw()
#   
#   plot(gg)
#   
#   
#   predRegion(matrix(cbind(omit$max_PM2.5, omit$max_PM10), ncol=2), as.vector(omit$mrf_weights))
# }
# 
# library(con2aqi)
# # index mapping
# # move to an index 
# map_CO2_2_index <- function(x) con2aqi(pollutant="co",con=x,type="8h")
# map_S02_2_index <- function(x) con2aqi(pollutant="s02",con=1000*x,type="1h")
