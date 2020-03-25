# Soil data analysis

# libs
require(data.table)
require(mrf)
require(ggplot2)

# source
source("./experiments/soil_data/soil_data_preprocessing.R")

# repro
set.seed(1)

# loading the data
# data
d <- loadSoilData()

# defining responses and covariates 
X <- as.matrix(d$soil_data[,-c("id","id_prof","ph0_30","sand0_30","silt0_30")])
Y <- matrix(cbind(d$soil_data$ph0_30, d$soil_data$sand0_30, d$soil_data$silt0_30), ncol=3)

# fitting mrf
mRF_fourier <- mrf(X = X, Y = Y, num.trees = 2000, splitting.rule = "fourier", 
                   num_features = 3)
mRF_gini    <- mrf(X = X, Y = Y, num.trees = 2000, splitting.rule = "gini")

# predictions
cond.cor   <- predict(object = mRF_fourier, 
                      type = "cor")
cond.mean  <- predict(object = mRF_fourier, 
                      type = "mean")
cond.sd    <- predict(object = mRF_fourier, 
                      type = "sd")

# predictions
d$soil_data$cond.mean_1 <- cond.mean$mean[,1]
d$soil_data$cond.mean_2 <- cond.mean$mean[,2]
d$soil_data$cond.mean_3 <- cond.mean$mean[,3]

d$soil_data$cond.corr_12 <- cond.cor$cor[,1,2]
d$soil_data$cond.corr_13 <- cond.cor$cor[,1,3]
d$soil_data$cond.corr_23 <- cond.cor$cor[,2,3]

d$soil_data$cond.sd_1 <- cond.sd$sd[,1]
d$soil_data$cond.sd_2 <- cond.sd$sd[,2]
d$soil_data$cond.sd_3 <- cond.sd$sd[,3]

# plots of responses
m <- match(d$geo_data$id, d$soil_data$id)

d$soil_data[,cond.mean_1_com:= mean(cond.mean_1),by="id"]
d$soil_data[,cond.mean_2_com:= mean(cond.mean_2),by="id"]
d$soil_data[,cond.mean_3_com:= mean(cond.mean_3),by="id"]

d$soil_data[,cond.sd_1_com:= mean(cond.sd_1),by="id"]
d$soil_data[,cond.sd_2_com:= mean(cond.sd_2),by="id"]
d$soil_data[,cond.sd_3_com:= mean(cond.sd_3),by="id"]

d$soil_data[,cond.corr_12_com:= mean(cond.corr_12),by="id"]
d$soil_data[,cond.corr_13_com:= mean(cond.corr_13),by="id"]
d$soil_data[,cond.corr_23_com:= mean(cond.corr_23),by="id"]

d$geo_data$cond.mean_1_com <- d$soil_data$cond.mean_1_com[m]
d$geo_data$cond.mean_2_com <- d$soil_data$cond.mean_2_com[m]
d$geo_data$cond.mean_3_com <- d$soil_data$cond.mean_3_com[m]

d$geo_data$cond.sd_1_com <- d$soil_data$cond.sd_1_com[m]
d$geo_data$cond.sd_2_com <- d$soil_data$cond.sd_2_com[m]
d$geo_data$cond.sd_3_com <- d$soil_data$cond.sd_3_com[m]

d$geo_data$cond.corr_12_com <- d$soil_data$cond.corr_12_com[m]
d$geo_data$cond.corr_13_com <- d$soil_data$cond.corr_13_com[m]
d$geo_data$cond.corr_23_com <- d$soil_data$cond.corr_23_com[m]

m1 <- ggplot() +
  geom_polygon(data = d$geo_data, aes(fill = cond.mean_1_com, x = long, y = lat, group = group)) +
  theme_void() +
  labs(fill='mean') +
  coord_map()

m2 <- ggplot() +
  geom_polygon(data = d$geo_data, aes(fill = cond.mean_2_com, x = long, y = lat, group = group)) +
  theme_void() +
  labs(fill='mean') +
  coord_map()

m3 <- ggplot() +
  geom_polygon(data = d$geo_data, aes(fill = cond.mean_3_com, x = long, y = lat, group = group)) +
  theme_void() +
  labs(fill='mean') +
  coord_map()

s1 <- ggplot() +
  geom_polygon(data = d$geo_data, aes(fill = cond.sd_1_com, x = long, y = lat, group = group)) +
  theme_void() +
  labs(fill='sd') +
  coord_map()

s2 <- ggplot() +
  geom_polygon(data = d$geo_data, aes(fill = cond.sd_2_com, x = long, y = lat, group = group)) +
  theme_void() +
  labs(fill='sd') +
  coord_map()

s3 <- ggplot() +
  geom_polygon(data = d$geo_data, aes(fill = cond.sd_3_com, x = long, y = lat, group = group)) +
  theme_void() +
  labs(fill='sd') +
  coord_map()

c1 <- ggplot() +
  geom_polygon(data = d$geo_data, aes(fill = cond.corr_12_com, x = long, y = lat, group = group)) +
  theme_void() +
  labs(fill='correlation') +
  coord_map()

c2 <- ggplot() +
  geom_polygon(data = d$geo_data, aes(fill = cond.corr_13_com, x = long, y = lat, group = group)) +
  theme_void() +
  labs(fill='correlation') +
  coord_map()

c3 <- ggplot() +
  geom_polygon(data = d$geo_data, aes(fill = cond.corr_23_com, x = long, y = lat, group = group)) +
  theme_void() +
  labs(fill='correlation') +
  coord_map()




png("./experiments/soil_data/Plots/PLOT_SOIL_COND_QUANTITIES.png", width = 700, height = 700)
require(gridExtra)
grid.arrange(m1, m2, m3,
             s1, s2, s3,
             c1, c2, c3,  
            # layout_matrix = rbind(c(1, 3, 4),
            #                       c(4, 5, 6)),
             nrow = 3, ncol=3)
dev.off()



# comparing with independent fits

# gini
RF_gini_1 <- mrf(X = X, Y = Y[,1,drop=FALSE], num.trees = 2000, splitting.rule = "gini")
RF_gini_2 <- mrf(X = X, Y = Y[,2,drop=FALSE], num.trees = 2000, splitting.rule = "gini")
RF_gini_3 <- mrf(X = X, Y = Y[,3,drop=FALSE], num.trees = 2000, splitting.rule = "gini")

# fourier
RF_fourier_1 <- mrf(X = X, Y = Y[,1,drop=FALSE], num.trees = 2000, splitting.rule = "fourier", num_features = 3)
RF_fourier_2 <- mrf(X = X, Y = Y[,2,drop=FALSE], num.trees = 2000, splitting.rule = "fourier", num_features = 3)
RF_fourier_3 <- mrf(X = X, Y = Y[,3,drop=FALSE], num.trees = 2000, splitting.rule = "fourier", num_features = 3)

# which split
th <- c(quantile(d$soil_data$ph0_30, 0.5),
        quantile(d$soil_data$sand0_30, 0.6),
        quantile(d$soil_data$silt0_30, 0.5))
# which direction
directions <- c(">", ">", ">")

# marginal prob
pR_fourier_1 <- as.numeric(predict(RF_fourier_1, type = "functional", f = function(y) y[1]>th[1])$functional)
pR_fourier_2 <- as.numeric(predict(RF_fourier_2, type = "functional", f = function(y) y[1]>th[2])$functional)
pR_fourier_3 <- as.numeric(predict(RF_fourier_3, type = "functional", f = function(y) y[1]>th[3])$functional)

pR_gini_1 <- as.numeric(predict(RF_gini_1, type = "functional", f = function(y) y[1]>th[1])$functional)
pR_gini_2 <- as.numeric(predict(RF_gini_2, type = "functional", f = function(y) y[1]>th[2])$functional)
pR_gini_3 <- as.numeric(predict(RF_gini_3, type = "functional", f = function(y) y[1]>th[3])$functional)

# joint prob
pR_gini <- pR_gini_1 * pR_gini_2* pR_gini_3
pR_fourier <- pR_fourier_1 * pR_fourier_2 * pR_fourier_3





# plotting resulting probabilitie
d$soil_data$prob_mrf_fourier <- as.numeric(predict(mRF_fourier, type = "functional", f = function(y) y[1]>th[1] & y[2]>th[2] & y[3]>th[3])$functional)
d$soil_data$prob_mrf_gini <- as.numeric(predict(mRF_gini, type = "functional", f = function(y) y[1]>th[1] & y[2]>th[2] & y[3]>th[3])$functional)
d$soil_data$prob_indep_gini <- pR_gini
d$soil_data$prob_indep_fourier <- pR_fourier

d$soil_data[,prob_mrf_fourier_com:= mean(prob_mrf_fourier),by="id"]
d$soil_data[,prob_mrf_gini_com:= mean(prob_mrf_gini),by="id"]
d$soil_data[,prob_indep_fourier_com:= mean(prob_indep_fourier),by="id"]
d$soil_data[,prob_indep_gini_com:= mean(prob_indep_gini),by="id"]

d$geo_data$prob_mrf_fourier_com <- d$soil_data$prob_mrf_fourier_com[m]
d$geo_data$prob_mrf_gini_com <- d$soil_data$prob_mrf_gini_com[m]
d$geo_data$prob_indep_fourier_com <- d$soil_data$prob_indep_fourier_com[m]
d$geo_data$prob_indep_gini_com <- d$soil_data$prob_indep_gini_com[m]



pt_mrf_fourier <- ggplot() +
  geom_polygon(data = d$geo_data, aes(fill = prob_mrf_fourier_com, x = long, y = lat, group = group)) +
  labs(fill='Probabilities') +
  theme_void() +
  coord_map()

pt_mrf_gini <- ggplot() +
  geom_polygon(data = d$geo_data, aes(fill = prob_mrf_gini_com, x = long, y = lat, group = group)) +
  theme_void() + 
  coord_map()

pt_indep_gini <- ggplot() + 
  geom_polygon(data = d$geo_data, aes(fill = prob_indep_gini_com, x = long, y = lat, group = group)) +
  theme_void() +
  coord_map()

pt_indep_fourier <- ggplot() +
  geom_polygon(data = d$geo_data, aes(fill = prob_indep_fourier_com, x = long, y = lat, group = group)) +
  labs(fill='Probabilities') +
  theme_void() +
  coord_map()

png("./experiments/soil_data/Plots/PLOT_SOIL_COND_PROB.png", width = 700, height = 700)
require(gridExtra)
grid.arrange(pt_mrf_fourier,
             pt_indep_gini,
             nrow=1, ncol=2)
             # layout_matrix = rbind(c(1, 3, 4),
dev.off()

png("./experiments/soil_data/Plots/PLOT_SOIL_COND_PROB_SCATTER.png", width = 700, height = 700)
plot(d$soil_data$prob_mrf_fourier,d$soil_data$prob_indep_fourier,pch=19,xlab="MRF", ylab="indep. RF",xlim=c(0,0.25),ylim=c(0,0.25))
abline(0,1,col="blue")
dev.off()






