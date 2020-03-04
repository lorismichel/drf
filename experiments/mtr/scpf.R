# scpf dataset

# SCPF
# This is a pre-processed version of the dataset used in Kaggle’s “See Click Predict Fix”
# competition (Kaggle 2013). It concerns the prediction of three target variables that represent
# the number of views, clicks and comments that a specific 311 issue will receive. The issues
# have been collected from 4 cities (Oakland, Richmond, New Haven, Chicago) in the US and
# span a period of 12 months (01/2012–12/2012). The version of the dataset that we use here is
# a random 1% sample of the data. In terms of features we use the number of days that an issues
# stayed online, the source from where the issue was created (e.g. android, iphone, remote api,
#                                                             etc.), the type of the issue (e.g. graffiti, pothole, trash, etc.), the geographical co-ordinates of
# the issue, the city it was published from and the distance from the city center. All multi-valued
# nominal variables were first transformed to binary and then rare binary variables (being true
#                                                                                    for less than 1% of the cases) were removed.

# repro
set.seed(1)

# libs
require(mrf)

# source
source("./experiments/mtr/helpers.R")

# 
d <- loadMTRdata(dataset.name = "scpf")

# exploratory analysis of responses
pairs(d$Y, pch=19)

# fit an mrf
mRF <- mrf(X = d$X, Y = d$Y, splitting.rule = "fourier", num_features = 100)

# variable importance
plot(factor(colnames(d$X)),as.numeric(variable_importance(mRF)))

# look at heterogeneity
pairs(d$Y, col=cut(d$X[,"daysUntilLastIssue"], breaks = 3, labels=FALSE), pch=19)
pairs(d$Y, col=cut(d$X[,"longitude"], breaks = 3, labels=FALSE), pch=19)


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

