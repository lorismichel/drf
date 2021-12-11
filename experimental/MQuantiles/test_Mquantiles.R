# application of multvariate quantiles: confidence intervals

# very simple example, corresponds to a two-dimensional Gaussian example
X <- matrix(rnorm(1000),ncol=2)
Y <- cbind(rnorm(500),rnorm(500))
Y[,1] <- ifelse(X[,1]>0, Y[,1]-Y[,2], Y[,1]+Y[,2])
plot(Y,col=(X[,1]>0)+1,pch=19)

# DRF load
require(drf)

# fit a DRF object
d <- drf(X = X, Y = Y)

# predict the multivariate quantiles
u <- as.matrix(expand.grid(seq(0.1,0.9,0.1),seq(0.1,0.9,0.1)))
#u <- matrix(runif(100),ncol=2)
getUhat <- function(d, u) {
  pred <- predict(d,
                  functional = "multivariateQuantiles",
                  u = u)
  uhat <- t(sapply(1:nrow(d$Y.orig), function(i) {
    id <- which.min(apply(pred$multvariateQuantiles$yhat[[i]],1,function(x) sum((x-d$Y.orig[i,])^2)))
    u[id,,drop=F]
  }))
  
} 
# we want to solve a mapping now
uhat <- getUhat(d = d, u = u)

# check the alignements between the percentages of points
plot(sapply(seq(0.1,0.9,0.1), function(i) mean(u[,1]>=i & u[,2]>=i)),
sapply(seq(0.1,0.9,0.1), function(i) mean(uhat[,1]>=i & uhat[,2]>=i)),type="b",pch=19)
abline(0,1,col="red")

plot(sapply(seq(0.1,0.9,0.1), function(i) mean(u[,1]>=i & u[,2]>=0.3)),
     sapply(seq(0.1,0.9,0.1), function(i) mean(uhat[,1]>=i & uhat[,2]>=0.3)),type="b",pch=19)
abline(0,1,col="red")

plot(sapply(seq(0.1,0.9,0.1), function(i) mean(u[,1]>=i & u[,2]>=0.7)),
     sapply(seq(0.1,0.9,0.1), function(i) mean(uhat[,1]>=i & uhat[,2]>=0.7)),type="b",pch=19)
abline(0,1,col="red")

plot(sapply(seq(0.1,0.9,0.1), function(i) mean(u[,1]>=i & u[,2]<=0.4)),
     sapply(seq(0.1,0.9,0.1), function(i) mean(uhat[,1]>=i & uhat[,2]<=0.4)),type="b",pch=19)
abline(0,1,col="red")


# actually we are able to define a region out of it!

plot(Y,pch=19,col=(X[,1]>0)+1)
points(Y[uhat[,1]>=0.9 & uhat[,2]>=0.9,],cex=1.5,col="blue")



source("../../experiments/mtr/helpers.R")
set.seed(1)

# checking for jura
dat <- loadMTRdata(dataset.name = "jura", path = "~/Downloads/mtr-datasets/")
#dat$Y <- dat$Y[,1:2]
# fit a DRF object
d <- drf(X = dat$X, Y = dat$Y)

# predict the multivariate quantiles
u <- as.matrix(expand.grid(seq(0.1,0.9,0.1),seq(0.1,0.9,0.1),seq(0.1,0.9,0.1)))

uhat <- getUhat(d = d, u = u)



# check the alignements between the percentages of points
plot(sapply(seq(0.1,0.9,0.1), function(i) mean(u[,1]>=i & u[,2]>=i & u[,3]>=i)),
     sapply(seq(0.1,0.9,0.1), function(i) mean(uhat[,1]>=i & uhat[,2]>=i & uhat[,3]>=i)),type="b",pch=19)
abline(0,1,col="red")



# actually we are able to define a region out of it!
par(mfrow=c(1,1))
plot(dat$Y,pch=19)
points(dat$Y[uhat[,1]>=0.8 & uhat[,2]>=0.8,],cex=2,col="blue")

