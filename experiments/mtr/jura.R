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
d <- loadMTRdata(dataset.name = "jura")

# exploratory analysis of responses
par(mfrow=c(1,1))

# plot of coordinates
plot(d$X[,1], d$X[,2],xlab="X", ylab="Y",pch=15)
pairs(d$Y,pch=19)
summary(d$Y)

# split train et test
train <- sample(1:nrow(d$X),nrow(d$X)/2,replace=FALSE)
test <- setdiff(1:nrow(d$X), train)

# fit an mrf
mRF <- mrf(X = d$X[train,-c(1:2)], Y = d$Y[train,], splitting.rule = "fourier", num_features = 3)

# variable importance
plot(factor(colnames(d$X)),as.numeric(variable_importance(mRF)))


p_preds <- predict(mRF, X[test,-c(1:2)])

par(mfrow=c(3,3))
for (i in 1:length(test)) {
  plot(p_preds$y[,1],p_preds$y[,2], cex = p_preds$weights[i,]*20, pch=19,col="darkblue",xlim=range(d$Y[,1]),ylim=range(d$Y[,2]))
  points(d$Y[test,][i,1],d$Y[test,][i,2],col="red",pch=19)
  
}

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
