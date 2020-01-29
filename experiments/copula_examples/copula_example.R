# generate Gaussian copula example

d.noise <- 29
n <- 5000

set.seed(1)
x.active <- runif(0,min = -1, max = 1)
x.noise <- matrix(runif(n*d.noise, min = -1, max = 1),ncol=d.noise)

Sigma.fun <- function(x) diag(1 - x, 2) + matrix(x, ncol=2, nrow=2)


X <- cbind(x.active, x.noise)
Y <- t(apply(X, 1, function(xx) MASS::mvrnorm(n = 1, mu = c(0,0), Sigma = Sigma.fun(xx[1]))))

require(mrf)
mRF_fourier_node_scaling <- mrf(X = X, Y = Y, num.trees = 500, splitting.rule = "fourier", num_features = 50,  bandwidth = 1, node_scaling = TRUE, min.node.size = 20)
mRF_fourier <- mrf(X = X, Y = Y, num.trees = 500, splitting.rule = "fourier", num_features = 50,  bandwidth = 1, node_scaling = FALSE, min.node.size = 20)
mRF_gini <- mrf(X = X, Y = Y, num.trees = 500, splitting.rule = "gini", num_features = 50, bandwidth = 1, min.node.size = 20)
p_fourier_node_scaling <- predict(mRF_fourier_node_scaling, newdata = cbind(seq(-1,1,length.out = 15), matrix(0, ncol=d.noise,nrow=15)))
p_fourier <- predict(mRF_fourier, newdata = cbind(seq(-1,1,length.out = 15), matrix(0, ncol=d.noise,nrow=15)))
p_gini <- predict(mRF_gini, newdata = cbind(seq(-1,1,length.out = 15), matrix(0, ncol=d.noise,nrow=15)))


# produce plots 
par(mfrow=c(4,4))
#par(mar=rep(2,4))
plot(col="black",p_fourier_node_scaling$y,pch=19,main="original data",cex=0.2)
for (i in 1:nrow(p_fourier_node_scaling$weights)) {
  plot(col="darkblue", p_fourier_node_scaling$y, cex=p_fourier_node_scaling$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
  #plotBivariate(correl = FALSE, col="darkblue", x = p_fourier$y[,1], y = p_fourier$y[,2], cex.points = p_fourier$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
}

# produce plots 
par(mfrow=c(4,4))
#par(mar=rep(2,4))
plot(col="black",p_fourier$y,pch=19,main="original data",cex=0.2)
for (i in 1:nrow(p_fourier$weights)) {
  plot(col="darkblue", p_fourier$y, cex=p_fourier$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
  #plotBivariate(correl = FALSE, col="darkblue", x = p_fourier$y[,1], y = p_fourier$y[,2], cex.points = p_fourier$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
  }

par(mfrow=c(4,4))
#par(mar=rep(2,4))
plot(col="black",p_gini$y,pch=19,main="original data",cex=0.2)
for (i in 1:nrow(p_gini$weights)) {
  plot(col="darkblue", p_gini$y, cex=p_gini$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
}

get_corr <- function(fit_obj, x_seq){
  require(wCorr)
  l = length(x_seq)
  ret_corr = rep(0, length(l))
  for(i in 1:l){
    point = matrix(c(x[i], rep(0, d.noise)), nrow=1, ncol=(d.noise+1))
    weights = predict(fit_obj, point)$weights
    ret_corr[i] = weightedCorr(Y[,1], Y[,2], weights=weights)
  }
  return(ret_corr)
}
x = seq(-1, 1, by=0.03)
par(mfrow=c(1,1))
plot(x, x, type='l')
lines(x, get_corr(mRF_fourier_node_scaling, x), col='blue', lty=2)
lines(x, get_corr(mRF_fourier, x), col='red', lty=2)
lines(x, get_corr(mRF_gini, x), col='green', lty=2)
