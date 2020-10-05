library(drf)
library(ranger)
library(ggplot2)

n = 6000
p = 20

#confounding + heterogeneity
X <- matrix(runif(n*p), nrow=n, ncol=p)
#W <- rbinom(n, size=1, prob=(1+dbeta(X[, 3], 2, 4))/4)
W <- abs(0.5 + 2*X[, 3] + 0.5*rnorm(n)) 
truth <- function(W, X){
  beta = 3*X[,1]
  expectation = 3*(X[, 3]-0.5) + sin(3*W)*beta
  return(expectation)
}
Y <- truth(W, X) + X[, 2]*rnorm(n) #+ abs(X[,2]-0.5)

par(mfrow=c(3,1))
qplot(W, Y, col=X[, 3])
qplot(W, Y, col=X[, 1])
qplot(W, Y, col=X[, 2])

drf_fit <- drf(Y=cbind(Y,W), X=X)
Y_fit <- ranger(y~., data=data.frame(y=Y, w=W, x=X), num.trees=2000, min.node.size = 20)
Y_fit2 <- drf(Y=Y, X=cbind(W, X))

par(mfrow=c(2,2))
par(mar = c(2, 2, 1, 1))
# Inspect where the weight is assigned
##############################
point = matrix(c(0.2, 0.2, 0.2, runif(p-3)), 1, p) 
#point = matrix(X[1,], 1, p)
weights = predict(drf_fit, newdata=point)$weights
plot(X[,1], X[,3], cex=200*weights[1,], pch=19, xlim=c(0,1), ylim=c(0,1))
points(point[,1], point[,3], col='red', pch=18, cex=3)
################################
point = matrix(c(0.2, 0.2, 0.2, runif(p-3)), 1, p) 
#point = matrix(X[1,], 1, p)
weights = predict(drf_fit, newdata=point)$weights
plot(X[,1], X[,2], cex=200*weights[1,], pch=19, xlim=c(0,1), ylim=c(0,1), xlab='a', ylab='b')
points(point[,1], point[,2], col='red', pch=18, cex=3)
###############################
point = matrix(c(0.2, 0.2, 0.2, runif(p-3)), 1, p) 
#point = matrix(X[1,], 1, p)
weights = predict(drf_fit, newdata=point)$weights
plot(X[,2], X[,3], cex=200*weights[1,], pch=19, xlim=c(0,1), ylim=c(0,1))
points(point[,1], point[,3], col='red', pch=18, cex=3)
###############################
point = matrix(c(0.2, 0.2, 0.2, 0.2, 0.2, runif(p-5)), 1, p) 
#point = matrix(X[1,], 1, p)
weights = predict(drf_fit, newdata=point)$weights
plot(X[,4], X[,2], cex=200*weights[1,], pch=19, xlim=c(0,1), ylim=c(0,1))
points(point[,4], point[,2], col='red', pch=18, cex=3)

# k=20
# w_do = seq(0, 2, length.out = k)
# causal_effect = rep(0, k)
# N=500
# for(i in sample(1:n, N, replace=TRUE)){
#   fit <- loess(y~w, data=data.frame(y=Y,w=W), weights=as.vector(predict(mrf_fit, newdata=matrix(X[i,], nrow=1, ncol=p))$weights))
#   #which = sample(1:n, 1000, replace=TRUE, prob=as.vector(predict(mrf_fit, newdata=matrix(X[i,], nrow=1, ncol=p))$weights))
#   #fit <- mrf(Y=Y[which], X=cbind(W[which], X[which,5]), splitting.rule="fourier", num_features=3)
#   causal_effect = causal_effect + predict(fit, newdata=w_do)/N
# }
# par(mfrow=c(1,1))
# plot(w_do, causal_effect, type='l', col='red')
# truth = (5*mean(X[,3])  - 1) + 2*sin(4*w_do)*mean(beta)
# lines(w_do, truth, col='black', lty=2)
# fit <- loess(Y~W)
# lines(w_do, predict(fit, newdata = w_do), col='blue', lty=3)

#plot(W, Y,xlim=c(0, 3), col=(1 + (X[, 3]>0.5)))
#lines(W_test, predict(loess(y~w, data=data.frame(w=W, y=Y)), newdata=W_test), col='blue')

#plot(W, Y,xlim=c(0, 3), col=(1 + (X[, 1]>0.5)))
#lines(W_test, predict(loess(y~w, data=data.frame(w=W, y=Y)), newdata=W_test), col='blue')


# Conditional regression plots
par(mfrow=c(3,3))
par(mar = c(4, 4, 1, 1))
x1 = c(0.1, 0.1, 0.1, 0.5, 0.5, 0.5, 0.9, 0.9, 0.9)
x2 = rep(0, 9)
x3 = c(0.1, 0.5, 0.9, 0.1, 0.5, 0.9, 0.1, 0.5, 0.9)
W_test = matrix(seq(min(W), max(W), length.out=100), ncol=1)

regress <- function(W, Y, weights, W_test){
  idx = sample(1:length(W), 1000000, prob=as.vector(weights), replace=TRUE)
  fit = smooth.spline(W[idx], Y[idx], cv=TRUE)$fit
  return(predict(fit, x=W_test)$y)
}

for(i in 1:9){
  point = matrix(c(x1[i], x2[i], x3[i], rep(0.5, p-3)), 1, p)
  test = Reduce(rbind, lapply(W_test, function(tmp) return(point)))
  weights = predict(drf_fit, newdata=point)$weights
  plot(W, Y, cex=100*weights[1,], pch=19, main=paste0("X1=", x1[i], ", X2=", x2[i]))
  lines(W_test, regress(W, Y, weights, W_test), col='blue', lwd=2)
  #lines(W_test, regress(W, Y, weights=rep(1, n), W_test), col='blue')
  lines(W_test, truth(W_test, test), col='red', lty=2, lwd=2)
  lines(W_test, predict(Y_fit, data=data.frame(w=W_test, x=test))$predictions, col='green', lwd=2)
  #lines(W_test, predict(Y_fit2, newdata = cbind(W_test, test))$weights %*% Y, col='purple')
}


causal_effect = function(obj, w_do){
  ret = rep(0, length(w_do))
  N = 1000
  cnt=0
  for(i in sample(1:n, N, replace=TRUE)){
    print(cnt)
    cnt = cnt + 1
    idx = sample(1:length(W), 200000, prob=as.vector(predict(obj, newdata=matrix(X[i,], nrow=1, ncol=p))$weights), replace=TRUE)
    fit = smooth.spline(W[idx], Y[idx], cv=TRUE)$fit
    for(j in 1:length(w_do)){
      ret[j] = ret[j] + predict(fit, x=w_do[j])$y/N
    }
  }
  return(ret)
}

causal_effect2 = function(obj, w_do){
  ret = rep(0, length(w_do))
  N = 1000
  cnt=0
  for(i in sample(1:n, N, replace=TRUE)){
    print(cnt)
    cnt = cnt + 1
    fn = predict(obj, newdata = cbind(w_do, Reduce(rbind, lapply(w_do, function(tmp) return(point)))))$weights %*% Y
    for(j in 1:length(w_do)){
      ret[j] = ret[j] + fn[j]/N
    }
  }
  return(ret)
}

k=500
w_do = seq(0, 3.8, length.out=k)
effect = rep(0, k)
ans = rep(0, k)
effect = 
for(i in 1:k){
  ans[i] = mean(truth(w_do[i], X))
}
par(mfrow=c(1,1))
effect = causal_effect(mrf_fit, w_do)
plot(w_do, effect, type='l', col='red', ylim=c(-3,3))
lines(w_do, ans, col='green')
lines(w_do, regress(W, Y, weights=rep(1, n), w_do), col='blue')
effect2 = causal_effect2(Y_fit2, w_do)
lines(w_do, effect2, col='purple')
effect3 = causal_effect2(Y_fit, w_do)
lines(w_do, effect2, col='purple')

#interventional_dist = function(obj, w_do){
#  ret_weights = rep(0, n)
#  all_weights -> predict(obj, newdata=X)$weights  
#  for(i in 1:n){
#    which = sample(1:n, 10000, replace=TRUE, prob=as.vector(all_weights[i,]))
#    fit <- mrf(y~w, Y=Y[which], X=W[which], splitting.rule = "fourier", num_features=3)
#    ret_weights = ret_weights + predict(fit, newdata=as.matrix(w_do, nrow=1, ncol=1))$weights/n 
#  }
#  return(ret_weights)
#}
#
#density(Y, weights=interventional_dist(mrf_predict, w_do=0))

