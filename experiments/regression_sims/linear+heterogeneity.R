library(grf)
library(mrf)

n = 1600
p = 20

#heterogeneity
X <- matrix(runif(n*p), nrow=n, ncol=p)
W <- rnorm(n, 0.5, 1)#rbinom(n, size=1, prob=0.5)
beta = (1+1/(1+exp(-20*(X[,1] - 1/3)))) * (1+1/(1+exp(-20*(X[,2] - 1/3)))) #one function has range [1, 2]
Y <- (W - 0.5)*beta + rnorm(n)

plot(W, Y, col=(1 + (X[, 3]>0.2)))
lm(Y~W)

t = seq(0, 1, by=0.01)
plot(t, (1+1/(1+exp(-20*(t - 1/3)))), type='l')


mrf_fit <- mrf(Y=cbind(Y,W), X=X, splitting.rule = "fourier", num_features=3, node_scaling = TRUE, min.node.size = 30)
mrf_fit2 <- mrf(Y=cbind(Y,W), X=X, splitting.rule = "fourier", num_features=50, node_scaling = TRUE, min.node.size = 30)
mrf_fit3 <- mrf(Y=cbind(Y,W), X=X, splitting.rule = "fourier", num_features=200, node_scaling = TRUE, min.node.size = 30)
grf_fit <- causal_forest(X=X, Y=Y, W=W)

#____________________________________________________
grf_predictions = grf_fit$predictions
plot(beta, grf_predictions)
abline(0,1, col='red')

plot(X[,1], grf_predictions - beta)

weights = predict(mrf_fit, newdata = X)$weights
mrf_predictions = rep(0, n)
for(i in 1:n){
  mrf_predictions[i] = lm(Y~W, weights=weights[i,])$coefficients[2]
}

plot(beta, mrf_predictions)
abline(0,1, col='red')

plot(X[,1], mrf_predictions - beta)

#____________________________________________________
point = matrix(c(0.9, 0.9, rep(0, p-2)), 1, p) 
#point = matrix(X[1,], 1, p)
weights = predict(mrf_fit, newdata=point)$weights
plot(X[,1], X[,2], cex=200*weights[1,], pch=19, asp=1)
points(point[,1], point[,2], col='red', pch=18, cex=3)
##
weights = predict(mrf_fit2, newdata=point)$weights
plot(X[,1], X[,2], cex=200*weights[1,], pch=19, asp=1)
points(point[,1], point[,2], col='red', pch=18, cex=3)
##
weights = predict(mrf_fit3, newdata=point)$weights
plot(X[,1], X[,2], cex=200*weights[1,], pch=19, asp=1)
points(point[,1], point[,2], col='red', pch=18, cex=3)
#________________________________________________________

n_test = 1000
X_test <- matrix(runif(n_test*p), nrow=n_test, ncol=p)
beta_test <- (1+1/(1+exp(-20*(X_test[,1] - 1/3)))) * (1+1/(1+exp(-20*(X_test[,2] - 1/3))))

grf_predictions = predict(grf_fit, newdata=X_test)$predictions
plot(beta_test, grf_predictions)
abline(0,1, col='red')

mean((beta_test - grf_predictions)^2)

##
weights = predict(mrf_fit, newdata = X_test)$weights
mrf_predictions = rep(0, n_test)
for(i in 1:n_test){
  mrf_predictions[i] = lm(Y~W, weights=weights[i,])$coefficients[2]
}

plot(beta_test, mrf_predictions, col=(2+X_test[,1]>0.5))
abline(0,1, col='red')

mean((beta_test - mrf_predictions)^2)

##
weights = predict(mrf_fit2, newdata = X_test)$weights
mrf_predictions = rep(0, n_test)
for(i in 1:n_test){
  mrf_predictions[i] = lm(Y~W, weights=weights[i,])$coefficients[2]
}

plot(beta_test, mrf_predictions)
abline(0,1, col='red')

mean((beta_test - mrf_predictions)^2)

##
weights = predict(mrf_fit3, newdata = X_test)$weights
mrf_predictions = rep(0, n_test)
for(i in 1:n_test){
  mrf_predictions[i] = lm(Y~W, weights=weights[i,])$coefficients[2]
}

plot(beta_test, mrf_predictions)
abline(0,1, col='red')

mean((beta_test - mrf_predictions)^2)

