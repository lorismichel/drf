library(grf)
library(mrf)

n = 1600
p = 20

#confounding
X <- matrix(runif(n*p), nrow=n, ncol=p)
W <- rbinom(n, size=1, prob=(1+dbeta(X[, 3], 2, 4))/4)
beta = rep(0, n)
Y <- (2*X[,3]  - 1) + (W - 0.5)*beta + rnorm(n)

plot(W, Y, col=(1 + (X[, 3]>0.2)))
lm(Y~W)

t = seq(0, 1, by=0.01)
plot(t, (1+dbeta(t, 2, 4))/4, type='l')


mrf_fit <- mrf(Y=cbind(Y,W), X=X, splitting.rule = "fourier", num_features=3)
mrf_fit2 <- mrf(Y=cbind(Y,W), X=X, splitting.rule = "fourier", num_features=100)
mrf_fit3 <- mrf(Y=cbind(Y,W), X=X, splitting.rule = "fourier", num_features=200)
mrf_fit4 <- mrf(Y=cbind(Y,W), X=X, splitting.rule = "gini", num_features=200)
grf_fit <- causal_forest(X=X, Y=Y, W=W)

#____________________________________________________
point = matrix(c(0.35, 0, 0.75, rep(0, p-3)), 1, p) 
#point = matrix(X[1,], 1, p)
weights = predict(mrf_fit, newdata=point)$weights
plot(X[,1], X[,3], cex=200*weights[1,], pch=19, asp=1)
points(point[,1], point[,3], col='red', pch=18, cex=3)
##
weights = predict(mrf_fit2, newdata=point)$weights
plot(X[,1], X[,3], cex=200*weights[1,], pch=19, asp=1)
points(point[,1], point[,3], col='red', pch=18, cex=3)
##
weights = predict(mrf_fit3, newdata=point)$weights
plot(X[,1], X[,3], cex=200*weights[1,], pch=19, asp=1)
points(point[,1], point[,3], col='red', pch=18, cex=3)
##
weights = predict(mrf_fit4, newdata=point)$weights
plot(X[,1], X[,3], cex=200*weights[1,], pch=19, asp=1)
points(point[,1], point[,3], col='red', pch=18, cex=3)
#________________________________________________________

n_test = 1000
X_test <- matrix(runif(n_test*p), nrow=n_test, ncol=p)
beta_test <- rep(0, n_test)

grf_predictions = predict(grf_fit, newdata=X_test)$predictions
plot(X_test[, 3], grf_predictions)
abline(0,0, col='red')

mean((beta_test - grf_predictions)^2)

##
weights = predict(mrf_fit, newdata = X_test)$weights
mrf_predictions = rep(0, n_test)
for(i in 1:n_test){
  mrf_predictions[i] = lm(Y~W, weights=weights[i,])$coefficients[2]
}

plot(X_test[, 3], mrf_predictions)
abline(0,0, col='red')

mean((beta_test - mrf_predictions)^2)

##
weights = predict(mrf_fit2, newdata = X_test)$weights
mrf_predictions = rep(0, n_test)
for(i in 1:n_test){
  mrf_predictions[i] = lm(Y~W, weights=weights[i,])$coefficients[2]
}

plot(X_test[, 3], mrf_predictions)
abline(0,0, col='red')

mean((beta_test - mrf_predictions)^2)

##
weights = predict(mrf_fit3, newdata = X_test)$weights
mrf_predictions = rep(0, n_test)
for(i in 1:n_test){
  mrf_predictions[i] = lm(Y~W, weights=weights[i,])$coefficients[2]
}

plot(X_test[, 3], mrf_predictions)
abline(0,0, col='red')

mean((beta_test - mrf_predictions)^2)

##
weights = predict(mrf_fit3, newdata = X_test)$weights
mrf_predictions = rep(0, n_test)
for(i in 1:n_test){
  mrf_predictions[i] = lm(Y~W, weights=weights[i,])$coefficients[2]
}

plot(X_test[, 3], mrf_predictions)
abline(0,0, col='red')

mean((beta_test - mrf_predictions)^2)

