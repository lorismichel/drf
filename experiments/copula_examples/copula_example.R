# generate Gaussian copula example
d.noise <- 19
n <- 5000

set.seed(1)
x.active <- runif(0,min = -1, max = 1)
x.noise <- matrix(runif(n*d.noise, min = -1, max = 1),ncol=d.noise)

Sigma.fun <- function(x) diag(1 - x, 2) + matrix(x, ncol=2, nrow=2)


X <- cbind(x.active, x.noise)
Y <- t(apply(X, 1, function(xx) MASS::mvrnorm(n = 1, mu = c(0,0), Sigma = Sigma.fun(xx[1]))))

require(mrf)
mRF_fourier <- mrf(X = X, Y = Y, num.trees = 500, splitting.rule = "fourier", num_features = 50)
mRF_gini <- mrf(X = X, Y = Y, num.trees = 500, splitting.rule = "gini", num_features = 50)
p_fourier <- predict(mRF_fourier, newdata = cbind(seq(-1,1,length.out = 15), matrix(0, ncol=19,nrow=15)))
p_gini <- predict(mRF_gini, newdata = cbind(seq(-1,1,length.out = 15), matrix(0, ncol=19,nrow=15)))



# produce plots 
par(mfrow=c(4,4))
par(mar=rep(2,4))
plot(col="black",p_fourier$y,pch=19,main="original data",cex=0.2)
for (i in 1:nrow(p_fourier$weights)) {
  plot(col="darkblue", p$y, cex=p_fourier$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
}

par(mfrow=c(4,4))
par(mar=rep(2,4))
plot(col="black",p_gini$y,pch=19,main="original data",cex=0.2)
for (i in 1:nrow(p_gini$weights)) {
  plot(col="darkblue", p$y, cex=p_gini$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
}

