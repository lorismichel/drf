# andro dataset

# ANDRO
# The Andromeda dataset (Hatzikos et al. 2008) concerns the prediction of future values for
# six water quality variables (temperature, pH, conductivity, salinity, oxygen, turbidity) in
# Thermaikos Gulf of Thessaloniki, Greece. Measurements of the target variables are taken
# from under-water sensors with a sampling interval of 9 seconds and then averaged to get a
# single measurement for each variable over each day. The specific dataset that we use here
# corresponds to using a window of 5 days (i.e. features attributes correspond to the values of
# the six water quality variables up to 5 days in the past) and a lead of 5 days (i.e. we predict

# repro
set.seed(1)

# libs
require(mrf)

# source
source("./experiments/mtr/helpers.R")

# 
d <- loadMTRdata(dataset.name = "andro")

# exploratory analysis of responses
pairs(d$Y, pch=19)

# fit an mrf
mRF <- mrf(X = d$X, Y = d$Y, splitting.rule = "fourier", num_features = 100)

# variable importance
plot(factor(colnames(d$X)),as.numeric(variable_importance(mRF)))


# kfold validation
folds <- kFoldCV(n = nrow(d$X), k = 10)

RMSE_t_mat <- matrix(0,nrow=2, ncol=ncol(d$Y))
colnames(RMSE_t_mat) <- colnames(d$Y)

# fourier
for (k in 1:10) {
  mRF <- mrf(X = d$X[-folds[[k]],], Y = d$Y[-folds[[k]],], splitting.rule = "fourier", num_features = 100)
  p_fourier <- predict(mRF, newdata = d$X[folds[[k]],])
  Yhat <- sapply(1:ncol(p_fourier$y), function(d) apply(p_fourier$weights, 1, function(w) sum(w*p_fourier$y[,d])))
  RMSE_t_mat[1,] <- RMSE_t_mat[1,] + RMSE_t(d$Y[folds[[k]],],Yhat)/10
  print(k)
}

# gini
for (k in 1:10) {
  mRF <- mrf(X = d$X[-folds[[k]],], Y = d$Y[-folds[[k]],], splitting.rule = "gini")
  p_gini <- predict(mRF, newdata = d$X[folds[[k]],])
  Yhat <- sapply(1:ncol(p_gini$y), function(d) apply(p_gini$weights, 1, function(w) sum(w*p_gini$y[,d])))
  RMSE_t_mat[2,] <- RMSE_t_mat[2,] + RMSE_t(d$Y[folds[[k]],],Yhat)/10
  print(k)
}

# see the results 
print(RMSE_t_mat)
