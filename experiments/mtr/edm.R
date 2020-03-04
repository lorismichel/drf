# edm dataset

# EDM
# The Electrical Discharge Machining dataset (Karalic and Bratko 1997) represents a two-target
# regression problem. The task is to shorten the machining time by reproducing the behaviour
# of a human operator that controls the values of two variables. Each of the target variables
# takes 3 distinct numeric values ({−1, 0, 1}) and there are 16 continuous input variables.


# repro
set.seed(1)

# libs
require(mrf)

# source
source("./experiments/mtr/helpers.R")

# 
d <- loadMTRdata(dataset.name = "edm")

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
