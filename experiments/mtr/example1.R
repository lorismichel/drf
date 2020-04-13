# example 1 dataset

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

# load the data
d <- loadMTRdata(dataset.name = "example1")

# run analysis
res <- runRandomPinballAnalysis(X=d$X, Y=d$Y,k=2, num_features = 100)


for (i in 1:nrow(res$mrf_loss)) {
  if (i == 1) {
    plot(res$mrf_loss[i,],type="l",ylim=c(0,5))
    lines(res$gini_loss[i,],type = "l",col="red")
  } else {
    lines(res$mrf_loss[i,],type = "l")
    lines(res$gini_loss[i,],type = "l",col="red")
  }
}


# non-linear functionals (idea of why the correlation is working good)
mRF <- mrf(X = d$X, Y = d$Y, splitting.rule = "fourier", num_features = 100)
giniRF <- mrf(X = d$X, Y = d$Y, splitting.rule = "gini")

l2_mrf <- predict(mRF, d$X, type="functional", f = function(y) sum(y[1]*y[2]))
l2_gini <- predict(giniRF, d$X, type="functional", f = function(y) sum(y[1]*y[2]))

plot(l2_gini$functional,l2_mrf$functional,pch=19,col=(d$X[,1]>0)+1)
# multivariate confidence regions
require(jocre)

cors <- predict(mRF, d$X, type="cor")
sds <- predict(mRF, d$X, type="sd")
means <- predict(mRF, d$X, type="mean")


i <- 2
cond_cov <- cors$cor[i,,] %*% diag(sds$sd[i,],2)

x.grid <- expand.grid(seq(-4,4,length.out = 20),seq(-4,4,length.out = 20))
p <- c()
inv <- solve(cond_cov)
for (j in 1:nrow(x.grid)) {
  x <- as.numeric(x.grid[j,])
  p <- c(p, (x-means$mean[i,])%*%inv%*%(x-means$mean[i,]))
}
plot(x.grid[p<=qchisq(df = 2, p = 1-0.05),],pch=19,xlim=c(-3,3),ylim=c(-3,3))


# OOB prediction scores
predsOOB_mrf <- predict(mRF, type = "normalPredictionScore")
predsOOB_gini <- predict(giniRF, type = "normalPredictionScore")

scoresOOB_mrf <- sapply(1:nrow(d$Y), function(i) predsOOB_mrf$normalPredictionScore[[i]](d$Y[i,]))
scoresOOB_gini <- sapply(1:nrow(d$Y), function(i) predsOOB_gini$normalPredictionScore[[i]](d$Y[i,]))

par(mfrow=c(2,1))
alpha <- 0.05
hist(scoresOOB_mrf)
abline(v=qchisq(p = 1-alpha, df = ncol(d$Y)),col="red")
mean(scoresOOB_mrf>qchisq(p = 1-alpha, df = ncol(d$Y)))
hist(scoresOOB_gini)
abline(v=qchisq(p = 1-alpha, df = ncol(d$Y)),col="red")
mean(scoresOOB_gini>qchisq(p = 1-alpha, df = ncol(d$Y)))

par(mfrow=c(4,3))
## vizu of predictions scores
y.grid <- expand.grid(seq(-4,4,length.out = 100),seq(-4,4,length.out = 100))
x.grid <- cbind(seq(-1,1, length.out = 20), matrix(0,nrow=20,ncol=ncol(d$Y)-1))
preds <- predict(mRF, newdata = x.grid,type = "normalPredictionScore")
p1 <- apply(y.grid, 1,function(y) preds$normalPredictionScore[[1]](y))
p2 <- apply(y.grid, 1,function(y) preds$normalPredictionScore[[5]](y))
p3 <- apply(y.grid, 1,function(y) preds$normalPredictionScore[[9]](y))
p4 <- apply(y.grid, 1,function(y) preds$normalPredictionScore[[11]](y))
p5 <- apply(y.grid, 1,function(y) preds$normalPredictionScore[[15]](y))
p6 <- apply(y.grid, 1,function(y) preds$normalPredictionScore[[20]](y))
plot(y.grid[p1<=qchisq(df = 2, p = 1-0.05),],pch=19,xlim=c(-3,3),ylim=c(-3,3))
plot(y.grid[p2<=qchisq(df = 2, p = 1-0.05),],pch=19,xlim=c(-3,3),ylim=c(-3,3))
plot(y.grid[p3<=qchisq(df = 2, p = 1-0.05),],pch=19,xlim=c(-3,3),ylim=c(-3,3))
plot(y.grid[p4<=qchisq(df = 2, p = 1-0.05),],pch=19,xlim=c(-3,3),ylim=c(-3,3))
plot(y.grid[p5<=qchisq(df = 2, p = 1-0.05),],pch=19,xlim=c(-3,3),ylim=c(-3,3))
plot(y.grid[p6<=qchisq(df = 2, p = 1-0.05),],pch=19,xlim=c(-3,3),ylim=c(-3,3))

# assess volum
v1 <- apply(matrix(runif(2*10000, min = -4, max = 4),ncol=2), 1,function(y) preds$normalPredictionScore[[1]](y))
v2 <- apply(matrix(runif(2*10000, min = -4, max = 4),ncol=2), 1,function(y) preds$normalPredictionScore[[5]](y))
v3 <- apply(matrix(runif(2*10000, min = -4, max = 4),ncol=2), 1,function(y) preds$normalPredictionScore[[9]](y))
v4 <- apply(matrix(runif(2*10000, min = -4, max = 4),ncol=2), 1,function(y) preds$normalPredictionScore[[11]](y))
v5 <- apply(matrix(runif(2*10000, min = -4, max = 4),ncol=2), 1,function(y) preds$normalPredictionScore[[15]](y))
v6 <- apply(matrix(runif(2*10000, min = -4, max = 4),ncol=2), 1,function(y) preds$normalPredictionScore[[20]](y))
mean(v1>qchisq(p = 1-alpha, df = ncol(d$Y)))
mean(v2>qchisq(p = 1-alpha, df = ncol(d$Y)))
mean(v3>qchisq(p = 1-alpha, df = ncol(d$Y)))
mean(v4>qchisq(p = 1-alpha, df = ncol(d$Y)))
mean(v5>qchisq(p = 1-alpha, df = ncol(d$Y)))
mean(v6>qchisq(p = 1-alpha, df = ncol(d$Y)))

# save results


## vizu of predictions scores
y.grid <- expand.grid(seq(-4,4,length.out = 100),seq(-4,4,length.out = 100))
x.grid <- cbind(seq(-1,1, length.out = 20), matrix(0,nrow=20,ncol=ncol(d$Y)-1))
preds <- predict(giniRF, newdata = x.grid,type = "normalPredictionScore")
p1 <- apply(y.grid, 1,function(y) preds$normalPredictionScore[[1]](y))
p2 <- apply(y.grid, 1,function(y) preds$normalPredictionScore[[5]](y))
p3 <- apply(y.grid, 1,function(y) preds$normalPredictionScore[[9]](y))
p4 <- apply(y.grid, 1,function(y) preds$normalPredictionScore[[11]](y))
p5 <- apply(y.grid, 1,function(y) preds$normalPredictionScore[[15]](y))
p6 <- apply(y.grid, 1,function(y) preds$normalPredictionScore[[20]](y))
plot(y.grid[p1<=qchisq(df = 2, p = 1-0.05),],pch=19,xlim=c(-3,3),ylim=c(-3,3))
plot(y.grid[p2<=qchisq(df = 2, p = 1-0.05),],pch=19,xlim=c(-3,3),ylim=c(-3,3))
plot(y.grid[p3<=qchisq(df = 2, p = 1-0.05),],pch=19,xlim=c(-3,3),ylim=c(-3,3))
plot(y.grid[p4<=qchisq(df = 2, p = 1-0.05),],pch=19,xlim=c(-3,3),ylim=c(-3,3))
plot(y.grid[p5<=qchisq(df = 2, p = 1-0.05),],pch=19,xlim=c(-3,3),ylim=c(-3,3))
plot(y.grid[p6<=qchisq(df = 2, p = 1-0.05),],pch=19,xlim=c(-3,3),ylim=c(-3,3))

# assess volum
v1 <- apply(matrix(runif(2*10000, min = -4, max = 4),ncol=2), 1,function(y) preds$normalPredictionScore[[1]](y))
v2 <- apply(matrix(runif(2*10000, min = -4, max = 4),ncol=2), 1,function(y) preds$normalPredictionScore[[5]](y))
v3 <- apply(matrix(runif(2*10000, min = -4, max = 4),ncol=2), 1,function(y) preds$normalPredictionScore[[9]](y))
v4 <- apply(matrix(runif(2*10000, min = -4, max = 4),ncol=2), 1,function(y) preds$normalPredictionScore[[11]](y))
v5 <- apply(matrix(runif(2*10000, min = -4, max = 4),ncol=2), 1,function(y) preds$normalPredictionScore[[15]](y))
v6 <- apply(matrix(runif(2*10000, min = -4, max = 4),ncol=2), 1,function(y) preds$normalPredictionScore[[20]](y))
mean(v1>qchisq(p = 1-alpha, df = ncol(d$Y)))
mean(v2>qchisq(p = 1-alpha, df = ncol(d$Y)))
mean(v3>qchisq(p = 1-alpha, df = ncol(d$Y)))
mean(v4>qchisq(p = 1-alpha, df = ncol(d$Y)))
mean(v5>qchisq(p = 1-alpha, df = ncol(d$Y)))
mean(v6>qchisq(p = 1-alpha, df = ncol(d$Y)))
# save results




save(res, file = "./experiments/mtr/data/example1.Rdata")
