library(grf)
library(mrf)

n = 5000
p = 20

#confounding + heterogeneity
X <- matrix(runif(n*p), nrow=n, ncol=p)
#W <- rbinom(n, size=1, prob=(1+dbeta(X[, 3], 2, 4))/4)
W <- abs(4*X[, 3] - 2 + rnorm(n)/3)# + 3*abs(X[,2]-0.5) 
beta = (1+1/(1+exp(-20*(X[,1] - 1/3))))^2
Y <- (5*X[,3]  - 1) + 2*sin(4*W)*beta + 0.5*rnorm(n) #+ abs(X[,2]-0.5) 

par(mfrow=c(1,1))
t = seq(0, 1, by=0.01)
plot(t, (1+1/(1+exp(-20*(t - 1/3))))^2, type='l')

par(mfrow=c(3,1))
plot(W, Y, col=(1 + (X[, 3]>0.5)))
plot(W, Y, col=3*(2 + (X[, 1]>0.5)))
plot(W, Y, col=4*(2 + (X[, 5]>0.5)))

mrf_fit <- mrf(Y=cbind(Y,W), X=X, splitting.rule = "fourier", num_features=3, node_scaling = FALSE, min.node.size = 20)

k=20
w_do = seq(0, 2, length.out = k)
causal_effect = rep(0, k)
N=500
for(i in sample(1:n, N, replace=TRUE)){
  fit <- loess(y~w, data=data.frame(y=Y,w=W), weights=as.vector(predict(mrf_fit, newdata=matrix(X[i,], nrow=1, ncol=p))$weights))
  #which = sample(1:n, 1000, replace=TRUE, prob=as.vector(predict(mrf_fit, newdata=matrix(X[i,], nrow=1, ncol=p))$weights))
  #fit <- mrf(Y=Y[which], X=cbind(W[which], X[which,5]), splitting.rule="fourier", num_features=3)
  causal_effect = causal_effect + predict(fit, newdata=w_do)/N
}
par(mfrow=c(1,1))
plot(w_do, causal_effect, type='l', col='red')
truth = (5*mean(X[,3])  - 1) + 2*sin(4*w_do)*mean(beta)
lines(w_do, truth, col='black', lty=2)
fit <- loess(Y~W)
lines(w_do, predict(fit, newdata = w_do), col='blue', lty=3)

par(mfrow=c(3,2))
plot(W, Y,xlim=c(0, 3), col=(1 + (X[, 3]>0.5)))
lines(sort(W), predict(loess(y~w, data=data.frame(w=W, y=Y)), newdata=sort(W)), col='blue')

plot(W, Y,xlim=c(0, 3), col=(1 + (X[, 1]>0.5)))
lines(sort(W), predict(loess(y~w, data=data.frame(w=W, y=Y)), newdata=sort(W)), col='blue')

point = matrix(c(0.1, 0.1, 0.6, rep(0, p-3)), 1, p) 
weights = predict(mrf_fit, newdata=point)$weights
plot(W, Y, cex=200*weights[1,], pch=19, xlim=c(0, 3))
lines(sort(W), predict(loess(y~w, data=data.frame(w=W, y=Y), weights=as.vector(weights)), newdata=sort(W)), col='red')
lines(sort(W), predict(loess(y~w, data=data.frame(w=W, y=Y)), newdata=sort(W)), col='blue')
lines(sort(W),  (5*point[1,3]  - 1) + 2*sin(4*sort(W))*(1+1/(1+exp(-20*(point[1,1] - 1/3))))^2, col='black')

point = matrix(c(0.9, 0.1, 0.9, rep(0, p-3)), 1, p)
weights = predict(mrf_fit, newdata=point)$weights
plot(W, Y, cex=200*weights[1,], pch=19, xlim=c(0, 3))
lines(sort(W), predict(loess(y~w, data=data.frame(w=W, y=Y), weights=as.vector(weights)), newdata=sort(W)), col='red')
lines(sort(W), predict(loess(y~w, data=data.frame(w=W, y=Y)), newdata=sort(W)), col='blue')
lines(sort(W),  (5*point[1,3]  - 1) + 2*sin(4*sort(W))*(1+1/(1+exp(-20*(point[1,1] - 1/3))))^2, col='black')

point = matrix(c(0.9, 0.1, 0.6, rep(0, p-3)), 1, p)
weights = predict(mrf_fit, newdata=point)$weights
plot(W, Y, cex=200*weights[1,], pch=19, xlim=c(0, 3))
lines(sort(W), predict(loess(y~w, data=data.frame(w=W, y=Y), weights=as.vector(weights)), newdata=sort(W)), col='red')
lines(sort(W), predict(loess(y~w, data=data.frame(w=W, y=Y)), newdata=sort(W)), col='blue')
lines(sort(W),  (5*point[1,3]  - 1) + 2*sin(4*sort(W))*(1+1/(1+exp(-20*(point[1,1] - 1/3))))^2, col='black')

point = matrix(c(0.1, 0.1, 0.9, rep(0, p-3)), 1, p)
weights = predict(mrf_fit, newdata=point)$weights
plot(W, Y, cex=200*weights[1,], pch=19, xlim=c(0, 3))
lines(sort(W), predict(loess(y~w, data=data.frame(w=W, y=Y), weights=as.vector(weights)), newdata=sort(W)), col='red')
lines(sort(W), predict(loess(y~w, data=data.frame(w=W, y=Y)), newdata=sort(W)), col='blue')
lines(sort(W),  (5*point[1,3]  - 1) + 2*sin(4*sort(W))*(1+1/(1+exp(-20*(point[1,1] - 1/3))))^2, col='black')

# causal_effect = function(obj, w_do){
#   ret = 0
#   N=100
#   for(i in sample(1:n, N, replace=TRUE)){
#     fit <- loess(y~w, data=data.frame(y=Y,w=W), weights=as.vector(predict(obj, newdata=matrix(X[i,], nrow=1, ncol=p))$weights))
#     ret = ret + predict(fit, w_do)/N
#   }
#   return(ret)
# }
# effect = rep(0, k)
# for(i in 1:k){
#   print(i)
#   effect[i] = causal_effect(mrf_fit, w_do[i])
# }
# plot(w_do, effect, type='l', col='red', ylim=c(-3,3))
# truth = (2*mean(X[,3])  - 1) + sin(w_do)*mean(beta)
# lines(w_do, truth, col='black', lty=2)
# fit <- loess(Y~W)
# lines(w_do, predict(fit, newdata = w_do), col='blue', lty=3)

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

