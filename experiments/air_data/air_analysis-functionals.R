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

# air quality category
airQuCat <- function(y) {
  ret = rep("good", nrow(y))
  for(j in 1:nrow(y)){
    for(i in 1:ncol(y)){
      pollutant = strsplit(colnames(y)[i], '_')[[1]][2]
      idx = which.max(names(moderate_thresholds) == pollutant)
      if(y[j,i] > unhealthy_thresholds[idx]){
        ret[j] = 'unhealthy'
      }
      if(ret[j] == 'good' && (y[j,i] > moderate_thresholds[idx])){
        ret[j] = 'moderate'
      }
    }
  }
  return(ret)  
}

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
ggpairs(air_data[sample(1:nrow(air_data), 50000), 17:22])

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
pollutants = c('O3', 'PM2.5', 'SO2')
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

set.seed(22)
mRF_fourier <- mrf(X = X, Y = Y, num.trees = 2000, splitting.rule = "fourier", num_features = 10, min.node.size = 20)

#compute weights
test = dummy_cols(site_data[,c('Longitude','Latitude',"Elevation","Land.Use","Location.Setting")], remove_selected_columns=TRUE)
weights_fourier <- predict(mRF_fourier, newdata=test)$weights

#save(mRF_fourier, weights_fourier, file="~/Documents/projects/heterogeneity/air_data/computed_data/functionals")
load("~/Documents/projects/heterogeneity/air_data/computed_data/functionals")

################################################################
# Compute CDF estimate
################################################################
# categories
cat <- airQuCat(Y)

# get predictions for probabilites
library(raster)
site_data$prob_good_fourier = rowSums(weights_fourier[, cat=="good"])

site_data$title = "P(O3 < 0.055ppm, SO2 < 36ppb, PM2.5 < 12.1µg/m³ | test_site)"
p1 <- ggplot(site_data, aes(x=Longitude, y=Latitude, color=prob_good_fourier)) +
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) +
  geom_point(size=0.2, alpha=0.9) + #scale_colour_gradient2(low='red', mid='yellow', high='green', midpoint=0.7, limits=c(0.5, 0.7, 1)) +
  scale_color_viridis_c(option='A', values=c(0, 0.5, 0.7, 1), direction=-1) + labs(color='P(Good)')+
  coord_cartesian(xlim=c(-123.5, -69), ylim=c(25.5, 48.5)) +
  theme_light() + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        strip.text.x = element_text(size = 9.5),
        legend.text=element_text(size=11.5),
        legend.title=element_text(size=14))+
  facet_wrap(~title)
plot(p1)
ggsave('~/Documents/projects/heterogeneity/paper/air_data/cdf.png', width=13.3, height=6, units='cm')

qplot(Land.Use, prob_good_fourier, geom='boxplot', data=site_data)
qplot(Location.Setting, prob_good_fourier, geom='boxplot', data=site_data)
qplot(Elevation, prob_good_fourier, color=Location.Setting, data=site_data)

####
site_data$prob_good_indep = rep(1, nrow(site_data))
for(i in 1:ncol(Y)){
  print(i)
  fit <- mrf(X = X, Y = Y[, i, drop=FALSE], num.trees = 2000, splitting.rule = "fourier", num_features = 3, min.node.size = 20)
  weights = predict(fit, newdata=test)$weights
  cat <- airQuCat(Y[, i, drop=FALSE])
  site_data[, paste("prob_good", pollutants[i], sep="_")] = rowSums(weights[, cat=="good"])
  site_data$prob_good_indep <- site_data$prob_good_indep * site_data[, paste("prob_good", pollutants[i], sep="_")]
}

p2 <- ggplot(site_data, aes(x=Longitude, y=Latitude, color=prob_good_indep)) +
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) +
  geom_point(size=0.6) +
  scale_color_viridis_c(option='B', values=c(0, 0.5, 0.7, 1), direction=-1) + labs(color='P(good)')+
  coord_cartesian(xlim=c(-123.5, -69), ylim=c(25, 49)) +
  theme_bw()
plot(p2)

ggplot(site_data, aes(x=Longitude, y=Latitude, color=prob_good_O3)) +
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) +
  geom_point(size=0.8) +
  scale_color_viridis_c(option='B', values=c(0, 0.6, 0.8, 1), direction=-1) + labs(color='P(good)')+
  coord_cartesian(xlim=c(-123.5, -69), ylim=c(25, 49)) +
  theme_bw()

qplot(prob_good_indep, prob_good_fourier, data=site_data) + geom_abline(color='red')

####
cat = airQuCat(Y)
fit <- mrf(X = X, Y = (cat=="good"), num.trees = 2000, 
           splitting.rule = "gini", min.node.size = 20)
weights = predict(fit, newdata=test)$weights
site_data$prob_good_indicator = rowSums(as.matrix(weights[, cat=="good"]))

p3 <- ggplot(site_data, aes(x=Longitude, y=Latitude, color=prob_good_indicator)) +
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) +
  geom_point(size=0.6) + #scale_colour_gradient2(low='red', mid='yellow', high='green', midpoint=0.7, limits=c(0.5, 0.7, 1)) +
  scale_color_viridis_c(option='B', values=c(0, 0.5, 0.7, 1), direction=-1) + labs(color='P(good)')+
  coord_cartesian(xlim=c(-123.5, -69), ylim=c(25, 49)) +
  theme_bw()
plot(p3)

qplot(prob_good_indicator, prob_good_fourier, data=site_data, alpha=I(0.4), size=I(0.1)) + 
  geom_abline(color='red', linetype='dashed') +
  xlab('DRF estimates') +
  ylab('classification forest estimates') +
  xlim(c(0.5, 1)) + ylim(c(0.5, 1)) + 
  theme_light()+
  theme(axis.title.x=element_text(size=11),
        axis.title.y=element_text(size=9))
ggsave('~/Documents/projects/heterogeneity/paper/air_data/comparison.png', width=13, height=5.5, units='cm')


##
ggpairs(site_data[, c("prob_good_fourier", "prob_good_indep", "prob_good_indicator")])
require(gridExtra)
grid.arrange(p1, p2, p3, nrow = 1)

########################################
# compute CDF loss
########################################
cdf_loss <- function(prob_good, data){
  ret = 0
  data = data[, c("Site.ID", targets)]
  data = na.omit(data)
  
  for(i in 1:nrow(site_data)){
    print(i)
    phat = prob_good[i]
    
    measurements = data[data$Site.ID == site_data$Site.ID[i], targets]
    if(nrow(measurements) == 0){
      next
    }
    cat = airQuCat(measurements)
    
    loss = (-log(1e-10 + phat)*(cat=="good") - log(1e-10 + 1-phat)*(cat!="good")) #/ (-log(1e-10 + phat*(1-phat)))
    ret = ret + sum(loss) / nrow(data)
  }
  return(ret)
}

loss_fourier = cdf_loss(site_data$prob_good_fourier, air_data)
loss_indicator = cdf_loss(site_data$prob_good_indicator, air_data)

############################################
# monotonicity of the probability estimates
############################################
test_sites_idx = sample((1:nrow(site_data))[site_data$count==0], 100, replace=FALSE)

grid = seq(0, 0.071, length.out=20)
target = Y[,1]

results_DRF = matrix(0, nrow=nrow(test_sites), ncol=length(grid))
results_indicator = matrix(0, nrow=nrow(test_sites), ncol=length(grid))

for(i in 1:length(grid)){
  print(i)
  
  threshold = grid[i]
  results_DRF[, i] = apply(weights_fourier[test_sites_idx, ], 1, function(w){sum(w * (target < threshold))})
  
  fit <- mrf(X = X, Y = matrix(target < threshold, ncol=1), num.trees = 2000, 
               splitting.rule = "gini", min.node.size = 20)
  weights = predict(fit, newdata=test[test_sites_idx, ])$weights
  results_indicator[, i] = apply(weights, 1, function(w){sum(w * (target < threshold))})
}
load(file = "~/Documents/projects/heterogeneity/air_data/saved_data")

plotdf = data.frame(x=grid, prob1=results_DRF[43, ], prob2=results_indicator[43, ], label='P(O3 < threshold | test site)')
ggplot(plotdf, aes(x=x, y=prob1)) +
  geom_line(color='red', size=0.85) + 
  geom_line(aes(x=x, y=prob2), color='blue', size=0.85) +
  ylim(c(0,1)) + #ggtitle(sprintf("%d %s", i, site_data[test_sites_idx[i],]$Site.ID))+
  facet_grid(~label) + #theme(strip.text.y = element_blank()) + 
  xlab('threshold (ppm)') + 
  ylab('probability') +
  theme_light() + 
  theme(axis.title.x=element_text(size=11),
        axis.title.y=element_text(size=11),
        strip.text.x = element_text(size = 10.5))+
  facet_wrap(~label)
ggsave('~/Documents/projects/heterogeneity/paper/air_data/non-monotonicity.png', width=13, height=5.5, units='cm')

ggplot(data.frame(x=grid, prob=results_DRF[31, ]), aes(x=x, y=prob))+
  geom_line(color='red', linetype='dashed') + 
  geom_line(data=data.frame(x=grid, prob=results_indicator[31, ]), aes(x=x, y=prob), color='red') +
  geom_line(data=data.frame(x=grid, prob=results_DRF[43, ]), aes(x=x, y=prob), color='blue', linetype='dashed') +
  geom_line(data=data.frame(x=grid, prob=results_indicator[43, ]), aes(x=x, y=prob), color='blue') +
  ylim(c(0,1)) + ggtitle(sprintf("%d %s", i, site_data[test_sites_idx[i],]$Site.ID))
############################################
# conditional correlation
############################################
site_data$cor = 0
k = 2
j = 3
test_sites_idx = 1:nrow(site_data)#sample((1:nrow(site_data))[site_data$count==0], 1000, replace=FALSE)

for(i in 1:length(test_sites_idx)){
  print(i)
  weights = weights_fourier[test_sites_idx[i], ]
  which = weights != 0
  weights = weights[which]
  site_data$cor[test_sites_idx[i]] = sum(weights*Y[which, k]*Y[which, j]) / (sum(weights * Y[which, k]^2)^0.5 * sum(weights * Y[which, j]^2)^0.5)
}
#save(site_data, file = "~/Documents/projects/heterogeneity/air_data/computed_data/corr")
load(file = "~/Documents/projects/heterogeneity/air_data/computed_data/corr")

#out = predict(mRF_fourier, type='cor', newdata=test[i, , drop=FALSE])
#site_data$cor = out$cor[1, 2, ]
site_data$title = "Corr(SO2, PM2.5 | test_site)"
ggplot(site_data[test_sites_idx,], aes(x=Longitude, y=Latitude, color=cor)) +
  geom_polygon(data=us.map, aes(x=long, y=lat, group = group), color='grey', alpha=0.3) +
  geom_point(size=0.25, alpha=0.8) + #scale_colour_gradient2(low='red', mid='yellow', high='green', midpoint=0.7, limits=c(0.5, 0.7, 1)) +
  scale_color_viridis_c(option='A', values=c(-0.33, 0.6, 0.75, 1)) + labs(color='corr')+
  coord_cartesian(xlim=c(-123.5, -69), ylim=c(25.5, 48.5)) +
  theme_light() + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        strip.text.x = element_text(size = 10.5),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14))+
  facet_wrap(~title)
ggsave('~/Documents/projects/heterogeneity/paper/air_data/corr.png', width=13, height=6, units='cm')
