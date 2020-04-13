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
d <- loadSoilData("~/Downloads/data_for_ETH.csv")

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
    