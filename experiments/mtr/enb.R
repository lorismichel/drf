# enb dataset

# ENB
# The Energy Building dataset (Tsanas and Xifara 2012) concerns the prediction of the heating
# load and cooling load requirements of buildings (i.e. energy efficiency) as a function of eight
# building parameters such as glazing area, roof area, and overall height, amongst others.

# repro
set.seed(1)

# libs
require(mrf)

# source
source("./experiments/mtr/helpers.R")

# 
d <- loadMTRdata(dataset.name = "enb")

# exploratory analysis of responses
plot(d$Y, pch=19)

# fit an mrf
mRF <- mrf(X = d$X, Y = d$Y, splitting.rule = "fourier", num_features = 100)

# variable importance
plot(factor(colnames(d$X)),as.numeric(variable_importance(mRF)))

# look at heterogeneity
plot(d$Y, col= cut(d$X[,"Relative Compactness"], breaks = 3, labels=FALSE), pch=19)
plot(d$Y, col= cut(d$X[,"Surface Area"], breaks = 3, labels=FALSE), pch=19)


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
