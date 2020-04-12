library(grf)
library(mrf)

n = 2500
p = 10

#confounding + heterogeneity
X <- matrix(runif(n*p), nrow=n, ncol=p)
#W <- rbinom(n, size=1, prob=(1+dbeta(X[, 3], 2, 4))/4)
W <- 1+dbeta(X[, 3], 2, 4)/10 + 0.1*rnorm(n)
beta = 0.5 + 3 * X[, 1] * X[, 2]
Y <- (2*X[,3]  - 1)/4 + W^beta*exp(rnorm(n))

plot(W, Y, col=(1 + (X[, 3]>0.5)))
plot(W, Y, col=3*(2 + (X[, 1]>0.5)))


mrf_fit <- mrf(Y=cbind(Y,W), X=X, splitting.rule = "fourier", num_features=1, node_scaling = FALSE, min.node.size = 20)
mrf_fit2 <- mrf(Y=cbind(Y,W), X=X, splitting.rule = "fourier", num_features=3, node_scaling = FALSE, min.node.size = 20)
mrf_fit3 <- mrf(Y=cbind(Y,W), X=X, splitting.rule = "fourier", num_features=20, node_scaling = FALSE, min.node.size = 20)
mrf_fit4 <- mrf(Y=cbind(Y,W), X=X, splitting.rule = "fourier", num_features=100, node_scaling = FALSE, min.node.size = 20)
#grf_fit <- causal_forest(X=X, Y=Y, W=W)

#____________________________________________________
point = matrix(c(0.1, 0.1, 0.75, rep(0, p-3)), 1, p) 
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
#____________________________________________________
point = matrix(c(0.1, 0.1, 0.75, rep(0, p-3)), 1, p) 
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
##
weights = predict(mrf_fit4, newdata=point)$weights
plot(X[,1], X[,2], cex=200*weights[1,], pch=19, asp=1)
points(point[,1], point[,2], col='red', pch=18, cex=3)
#________________________________________________________

n_test = 1000
X_test <- matrix(runif(n_test*p), nrow=n_test, ncol=p)
beta_test <- (1+1/(1+exp(-20*(X_test[,1] - 1/3)))) * (1+1/(1+exp(-20*(X_test[,2] - 1/3))))

#grf_predictions = predict(grf_fit, newdata=X_test)$predictions
#plot(beta_test, grf_predictions)
#plot(X_test[, 3], grf_predictions)
#abline(0,1, col='red')

#mean((beta_test - grf_predictions)^2)

##
weights = predict(mrf_fit, newdata = X_test)$weights
mrf_predictions = rep(0, n_test)
for(i in 1:n_test){
  mrf_predictions[i] = lm(log(Y)~W, weights=weights[i,])$coefficients[2]
}

plot(beta_test, mrf_predictions)
#plot(X_test[, 3], mrf_predictions)
abline(0,1, col='red')

mean((beta_test - mrf_predictions)^2)

##
weights = predict(mrf_fit2, newdata = X_test)$weights
mrf_predictions = rep(0, n_test)
for(i in 1:n_test){
  mrf_predictions[i] = lm(log(Y)~W, weights=weights[i,])$coefficients[2]
}

plot(beta_test, mrf_predictions)
#plot(X_test[, 3], mrf_predictions)
abline(0,1, col='red')

mean((beta_test - mrf_predictions)^2)

##
weights = predict(mrf_fit3, newdata = X_test)$weights
mrf_predictions = rep(0, n_test)
for(i in 1:n_test){
  mrf_predictions[i] = lm(log(Y)~W, weights=weights[i,])$coefficients[2]
}

plot(beta_test, mrf_predictions)
#plot(X_test[, 3], mrf_predictions)
abline(0,1, col='red')

mean((beta_test - mrf_predictions)^2)

##
weights = predict(mrf_fit4, newdata = X_test)$weights
mrf_predictions = rep(0, n_test)
for(i in 1:n_test){
  mrf_predictions[i] = lm(log(Y)~W, weights=weights[i,])$coefficients[2]
}

plot(beta_test, mrf_predictions)
#plot(X_test[, 3], mrf_predictions)
abline(0,1, col='red')

mean((beta_test - mrf_predictions)^2)

