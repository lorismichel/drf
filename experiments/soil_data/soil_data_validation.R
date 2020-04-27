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
d <- loadSoilData(PATH.data = "~/Downloads/data_for_ETH_2.csv",
                  PATH.coords = "~/Downloads/coords_soil_2.csv")

# defining responses and covariates 
X <- as.matrix(d$soil_data[,-c("id","id_prof","clay5_15","oc5_15","ph5_15")])
Y <- matrix(cbind(d$soil_data$clay5_15, d$soil_data$oc5_15, d$soil_data$ph5_15), ncol=3)


# loop analysis
nrep <- 10
p.seq <- c(0.3, 0.5, 0.9)
coverage <- matrix(nrow=nrep,ncol=length(p.seq))
for (i in 1:nrep) {

  id.out <- sample(1:nrow(X), size = 0.25*nrow(X), replace = FALSE)
  mRF_fourier <- mrf(X = X[-id.out,], Y = Y[-id.out,], num.trees = 2000, splitting.rule = "fourier", 
                     num_features = 3)
  preds <- predict(mRF_fourier, newdata = X[id.out,], type = "normalPredictionScore")
  
  co <- c()
  for (p in p.seq) {
    predScores <- sapply(1:nrow(Y[id.out,]), function(i) preds$normalPredictionScore[[i]](Y[id.out,][i,]))
    q <- qf(df1 = ncol(Y), df2 = nrow(Y)-ncol(Y), p = p)

    co <- c(co, mean(predScores<=q))
  }
  coverage[i,] <- co
}


