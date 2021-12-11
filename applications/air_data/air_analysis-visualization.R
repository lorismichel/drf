library(ggplot2)
library(mrf)
library(viridis)
library(sf)
library(maps)
library(data.table)
library(fastDummies)
library(ggtern)
library(vcd)
library(GGally)

us.map <- map_data("world", region = c('USA'))

# defining thresholds and variables
unhealthy_thresholds = c(9.5, 76, 101, 0.071, 155, 35.5) #threshold value for "Unhealthy for sensitive groups"
moderate_thresholds = c(4.5, 36, 54, 0.055, 55, 12.1) #threshold value for "Moderate"
names(moderate_thresholds) = c('CO', 'SO2', 'NO2', 'O3', 'PM10', 'PM2.5')
names(unhealthy_thresholds) = c('CO', 'SO2', 'NO2', 'O3', 'PM10', 'PM2.5')

# loading data and subsampling
#load('~/Downloads/air_data.RData')
load('~/Documents/projects/heterogeneity/air_data/computed_data/air_data.RData')
length(unique(air_data$Site.ID))

#remove outliers
for(i in 17:22){
  air_data = air_data[is.na(air_data[, i]) | air_data[, i] < quantile(air_data[, i], 0.9995, na.rm=TRUE), ] 
}

#visualize pollutants
#ggpairs(data.frame(as.matrix(air_data[sample(1:nrow(air_data), 20000), 11:16])))
ggpairs(air_data[sample(1:nrow(air_data), 20000), 17:22])

#see how many sites we have for choice of 2 pollutants (or 1)
for(i in 11:16){
  for(j in 11:16){
    df=na.omit(air_data[,c(1,i,j)])
    print(c(i, j, length(unique(df$Site.ID))))
  }
}

for(i in 1:6){
  print(sprintf("%s, prob_moderate: %.2f%%,  prob_unhealthy: %.2f%%", names(moderate_thresholds)[i], 
                100*mean(air_data[, 16+i] > moderate_thresholds[i], na.rm=TRUE), 
                100*mean(air_data[, 16+i] > unhealthy_thresholds[i], na.rm=TRUE)))
}

#choose target pollutants
pollutants = c('PM2.5', 'NO2', 'O3', 'PM10', 'CO', 'SO2')
targets = c(paste('max_', pollutants, sep=''))
dataset <- air_data[-c(11:22)]
dataset <- cbind(dataset, air_data[, targets])
dataset <- dataset[!(dataset$State.Name %in% c('Alaska', 'Hawaii', 'Country Of Mexico', 'Virgin Islands', 'Puerto Rico')),]
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

# modelling
structable(~ Land.Use + Location.Setting, data = dataset)
X <- data.frame(dataset[, c('Longitude', 'Latitude', "Elevation", "Land.Use", "Location.Setting")])
#X$Land.Use <- factor(X$Land.Use)
#X$Location.Setting <- factor(X$Location.Setting)
# X$Weekday = factor(weekdays(dataset$Date))
# X$Month = factor(month(dataset$Date))
X <- dummy_cols(X, remove_selected_columns=TRUE)
colnames(X)

# fit DRF
set.seed(22)
mRF_fourier <- mrf(X = X, Y = Y, num.trees = 2000, splitting.rule = "fourier", num_features = 10, min.node.size = 20)
test = dummy_cols(site_data[,c('Longitude','Latitude',"Elevation","Land.Use","Location.Setting")], remove_selected_columns=TRUE)
weights_fourier <- predict(mRF_fourier, newdata=test)$weights

#save(mRF_fourier, weights_fourier, file="~/Documents/projects/heterogeneity/air_data/computed_data/visualization")
load("~/Documents/projects/heterogeneity/air_data/computed_data/visualization")

##################################################################
# Visualization of weight distribution
##################################################################

#pooled targets
ggplot(dataset, aes_string(x=targets[1], y=targets[2])) +
  geom_point(size=0.5) +
  geom_vline(xintercept=moderate_thresholds[pollutants[1]], linetype='dashed', color='green') +
  geom_hline(yintercept=moderate_thresholds[pollutants[2]], linetype='dashed', color='green') +
  geom_vline(xintercept=unhealthy_thresholds[pollutants[1]], linetype='dashed', color='red') +
  geom_hline(yintercept=unhealthy_thresholds[pollutants[2]], linetype='dashed', color='red') +
  coord_cartesian(xlim=c(0, 1.05*unhealthy_thresholds[pollutants[1]]), ylim = c(0,1.05*unhealthy_thresholds[pollutants[2]]))# +
#coord_cartesian(xlim=c(0, 100), ylim = c(0,500))# +
# theme_bw()

#all sites visualization
ggplot(site_data, aes(x=Longitude, y=Latitude, color=Elevation)) +
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) + 
  geom_point(size=0.4) + scale_color_viridis_c(option='magma') +
  coord_cartesian(xlim=c(-160, -70), ylim=c(18, 65)) +
  theme_bw()

#distribution of training sites
ggplot(site_data[site_data$count!=0,], aes(x=Longitude, y=Latitude)) + 
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) + 
  geom_point(aes(size=count/nrow(dataset), color=Land.Use, shape=Location.Setting)) + #scale_color_viridis_c(option='magma') +
  scale_size_area(max_size=3) +
  coord_cartesian(xlim=c(-160, -70), ylim=c(18, 65)) +
  theme_bw()

point_description = function(test_point){
  out = ''
  #out = paste(out, test_point$Longitude[1])
  #out = paste(out, test_point$Latitude[1])
  #  out = paste(out, weekdays(test_point$Date[1]))
  #  out = paste(out, 'month:', month(test_point$Date[1]))
  #out = paste(out, '\n', sep='')
  out = paste(out, "Elevation:", floor(test_point$Elevation[1]))
  out = paste(out, "m,", sep='')
  out = paste(out, "Land use:", test_point$Land.Use[1])
  out = paste(out, ",", sep='')
  out = paste(out, "Setting:", test_point$Location.Setting[1])
  
  return(out)
}

#17238
#13234
set.seed(23)
train_sites = site_data[site_data$count!=0, ]
for(idx in sample((1:nrow(site_data))[site_data$count==0], 100, replace=FALSE)){
  print(idx)
  dataset$weight = weights_fourier[idx,]
  train_sites$weight = apply(train_sites, 1, function(site){sum(dataset$weight[site[1] == dataset$Site.ID])})
  
  #plot distribution of training sites which get high weight
  gg = ggplot(train_sites[train_sites$weight!=0,], aes(x=Longitude, y=Latitude)) +
    geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) +
    annotate(geom='point', x=site_data$Longitude[idx], y=site_data$Latitude[idx], shape=8, color='red', size=6) + 
    geom_point(aes(size=weight, color=Land.Use, shape=Location.Setting)) +
    scale_size_area(max_size=5) +
    #scale_color_viridis_c(option='magma') +
    coord_cartesian(xlim=c(-125, -70), ylim=c(24, 50)) +
    ggtitle(point_description(site_data[idx,])) +
    #coord_cartesian(xlim=c(-130, -60), ylim=c(25, 55)) +
    theme_bw()# +
  #theme(legend.position='none')
  plot(gg)
  
  #plot distribution of pollutants
  subsample = dataset[sample(1:nrow(dataset), 2000, replace=TRUE, prob=dataset$weight),]
  gg = ggplot(data=dataset[dataset$weight!=0, ], aes_string(x=targets[1], y=targets[2])) +
    geom_point(aes(alpha=weight*100), size=0.5)+
    #stat_density2d(data=subsample, aes(alpha=..level.., fill=..level..), size=2, bins=10, geom="polygon") + 
    # scale_fill_gradient(low = "yellow", high = "red") +
    # scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
    geom_density2d(data=subsample, colour="blue", bins=6) +
    geom_vline(xintercept=moderate_thresholds[pollutants[1]], linetype='dashed', color='green') +
    geom_hline(yintercept=moderate_thresholds[pollutants[2]], linetype='dashed', color='green') +
    geom_vline(xintercept=unhealthy_thresholds[pollutants[1]], linetype='dashed', color='red') +
    geom_hline(yintercept=unhealthy_thresholds[pollutants[2]], linetype='dashed', color='red') +
    coord_cartesian(xlim=c(0, 1.05*unhealthy_thresholds[pollutants[1]]), ylim = c(0,1.05*unhealthy_thresholds[pollutants[2]])) +
    #coord_cartesian(xlim=c(0, 1.05*unhealthy_thresholds[pollutants[1]]), ylim = c(0, 50)) +
    ggtitle(sprintf('%g', idx)) +
    theme_bw()
  
  plot(gg)
}

#library(con2aqi)
# index mapping
# move to an index
#map_CO2_2_index <- function(x) con2aqi(pollutant="co",con=x,type="8h")
#map_S02_2_index <- function(x) con2aqi(pollutant="s02",con=1000*x,type="1h")


##############################################################
# generate plots for paper
##################################################
point_description = function(test_point){
  out = ''
  #out = paste(out, test_point$Longitude[1])
  #out = paste(out, test_point$Latitude[1])
  #  out = paste(out, weekdays(test_point$Date[1]))
  #  out = paste(out, 'month:', month(test_point$Date[1]))
  #out = paste(out, '\n', sep='')
  out = paste(out, "Elevation:", floor(test_point$Elevation[1]))
  out = paste(out, "m,", sep='')
  out = paste(out, "Land use:", test_point$Land.Use[1])
  out = paste(out, ",", sep='')
  out = paste(out, "Setting:", test_point$Location.Setting[1])
  
  return(out)
}
levels(site_data$Land.Use)[levels(site_data$Land.Use)=="MILITARY RESERVATION"] <- "MILITARY"
levels(site_data$Location.Setting)[levels(site_data$Location.Setting)=="URBAN AND CENTER CITY"] <- "URBAN"

idx= 17238
plot_df1 = site_data[site_data$count!=0, ]
dataset$weight = weights_fourier[idx,]
plot_df1$weight = apply(plot_df1, 1, function(site){sum(dataset$weight[site[1] == dataset$Site.ID])})

plot_df1 = plot_df1[plot_df1$weight!=0,]
plot_df1$which = point_description(site_data[idx,])
annotate_df = data.frame(which=point_description(site_data[idx,]), Longitude=site_data[idx,]$Longitude, Latitude=site_data[idx,]$Latitude)

idx= 13234
plot_df2 = site_data[site_data$count!=0, ]
dataset$weight = weights_fourier[idx,]
plot_df2$weight = apply(plot_df2, 1, function(site){sum(dataset$weight[site[1] == dataset$Site.ID])})

plot_df2 = plot_df2[plot_df2$weight!=0,]
plot_df2$which = point_description(site_data[idx,])
annotate_df = rbind(annotate_df, data.frame(which=point_description(site_data[idx,]), Longitude=site_data[idx,]$Longitude, Latitude=site_data[idx,]$Latitude))

plot_df = rbind(plot_df1, plot_df2)
plot_df = plot_df[plot_df$Land.Use!='UNKNOWN' & plot_df$Location.Setting!='UNKNOWN',]

#plot distribution of training sites which get high weight
gg1 = ggplot(plot_df, aes(x=Longitude, y=Latitude)) +
  facet_wrap(~which) +
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) +
  geom_point(color='black', aes(size=1.2*weight^0.5, shape=Location.Setting)) +
  geom_point(aes(size=weight^0.5, color=Land.Use, shape=Location.Setting)) + 
  labs(shape="Setting", color="Land use") +
  scale_size_area(max_size=6.7) + guides(size=FALSE) + 
  geom_point(data=annotate_df, aes(x=Longitude, y=Latitude), shape=8, color='black', size=3.5) + 
  theme_light()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        strip.text.x = element_text(size = 13),
        legend.text=element_text(size=13),
        legend.title=element_text(size=16)) +
  coord_cartesian(xlim=c(-106, -71), ylim=c(29, 46.5))# +
plot(gg1)
ggsave('~/Documents/projects/heterogeneity/paper/air_data/site_weights3.png', width=33.3, height=7.5, units='cm')


idx = 13234 #17238 
dataset$weight = weights_fourier[idx,]

subsample = dataset[sample(1:nrow(dataset), 1000, replace=TRUE, prob=dataset$weight),]
poly1=data.frame(
  x=c(moderate_thresholds[pollutants[1]], moderate_thresholds[pollutants[1]], 100, 100), 
  y=c(-moderate_thresholds[pollutants[2]], moderate_thresholds[pollutants[2]], moderate_thresholds[pollutants[2]], -moderate_thresholds[pollutants[2]])
)
poly2=data.frame(
  x=c(-moderate_thresholds[pollutants[1]], moderate_thresholds[pollutants[1]], moderate_thresholds[pollutants[1]], -moderate_thresholds[pollutants[1]]),
  y=c(moderate_thresholds[pollutants[2]], moderate_thresholds[pollutants[2]], 100, 100)
)
poly3=data.frame(
  x=c(moderate_thresholds[pollutants[1]], 100, 100, moderate_thresholds[pollutants[1]]),
  y=c(moderate_thresholds[pollutants[2]], moderate_thresholds[pollutants[2]], 100, 100)
)
poly4=data.frame(
  x=c(-100, moderate_thresholds[pollutants[1]], moderate_thresholds[pollutants[1]], -100),
  y=c(-100, -100, moderate_thresholds[pollutants[2]], moderate_thresholds[pollutants[2]])
)
gg2 = ggplot(data=dataset[dataset$weight!=0 & dataset$max_NO2 > 0 & dataset$max_PM2.5 > 0, ], aes_string(x=targets[1], y=targets[2])) +
  geom_jitter(aes(alpha=weight), size=1.5, width=1) + guides(alpha=FALSE) +
  #stat_density2d(data=subsample, aes(alpha=..level.., fill=..level..), size=2, bins=10, geom="polygon") + 
  #scale_fill_gradient(low = "yellow", high = "red") +
  #scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
  geom_density2d(data=subsample, colour="blue", bins=4, alpha=0.7) + 
  #geom_vline(xintercept=moderate_thresholds[pollutants[1]], linetype='dashed', color='green') +
  #geom_hline(yintercept=moderate_thresholds[pollutants[2]], linetype='dashed', color='green') +
  #geom_vline(xintercept=unhealthy_thresholds[pollutants[1]], linetype='dashed', color='red') +
  #geom_hline(yintercept=unhealthy_thresholds[pollutants[2]], linetype='dashed', color='red') +
  geom_polygon(poly1, mapping=aes(x=x, y=y), fill='red', alpha=0.1)+
  geom_polygon(poly2, mapping=aes(x=x, y=y), fill='red', alpha=0.1)+
  geom_polygon(poly3, mapping=aes(x=x, y=y), fill='red', alpha=0.1)+
  geom_polygon(poly4, mapping=aes(x=x, y=y), fill='green', alpha=0.12)+
  coord_cartesian(xlim=c(0, 1.3*moderate_thresholds[pollutants[1]]), ylim = c(0, 1.15*moderate_thresholds[pollutants[2]])) +
  #coord_cartesian(xlim=c(0, 1.05*unhealthy_thresholds[pollutants[1]]), ylim = c(0, 50)) +
  #ggtitle(point_description(site_data[idx,])) +
  theme_light() +
  theme(axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=13)) +
  labs(x="Fine particulates (µg/m³)", y='Nitrogen dioxyde (ppb)')
plot(gg2)
ggsave('~/Documents/projects/heterogeneity/paper/air_data/pollutant2.png', width=14.5, height=6, units='cm')
