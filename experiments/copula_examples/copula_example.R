# generate copula examples

# libs
library(copula)
library(drf)
library(MASS)
library(ggplot2)

# source
source("./helpers.R")

# repro
set.seed(100)


# SC0: normal copula example with N(0,1) marginals, change of correlation of Y1 and Y2 depends on X1
# SC1: t-copula, X1 is tail parameter, X2 is covariance and X3 is marginal
# SC2: normal copula example with N(0,1) marginals, toeplitz covariance rho depends on X1
# SC3: normal copula example with N(0,1) marginals, equicorrelation covariance rho depends on X1

# PARAMS
# choice of SC
SC <- 3

# dimensions
p <- 30
n <- 5000
d_ <- 5

# GENERATE DATA

# predictors
if(SC == 3 || SC == 4){
  X  <- matrix(runif(n*p, min = 0, max = 1), ncol = p)
} else if(SC == 0 || SC == 1){
  X  <- matrix(runif(n*p, min = -1, max = 1), ncol = p)
}
# responses
gen = function(xx) {
  if (SC == 0) {
    # copula
    normCop <- normalCopula(param=c(xx[1]), dim = 2)
    # margins
    paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
    mdvNorm <- mvdc(copula=normCop, margins=c("norm", "norm"), paramMargins=paramMargins)
    # gen
    c(rMvdc(n = 1, mvdc = mdvNorm), rnorm(d_-2))
  }  
  else if (SC == 1) {
    # copula
    tCop <- tCopula(xx[2],  df=ifelse(xx[1] <= (-1 + 2/3), 1, ifelse(xx[1]<= (-1 + 4/3), 3, 10)))
    # margins
    if (xx[3]<=0) {
      margins <- c("norm", "norm")
      paramMargins <- list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
    } else {
      margins <- c("norm", "exp")
      paramMargins <- list(list(mean = 0, sd = 1), list(rate = 1))
    }
    mdvT <- mvdc(copula=tCop, margins=margins, paramMargins=paramMargins)
    # gen
    c(rMvdc(n = 1, mvdc = mdvT), rnorm(d_-2))
    
  } 
  else if (SC == 2) {
    mu = rep(0, d_)
    rho = xx[1]
    Sigma = toeplitz(rho^{pmin(0:(d_-1), d_ - 0:(d_-1))})
    mvrnorm(mu=mu, Sigma=Sigma)
  } 
  else if (SC == 3) {
    mu = rep(0, d_)
    rho = xx[1]
    Sigma = toeplitz(c(1, rep(rho, d_-1)))
    mvrnorm(mu=mu, Sigma=Sigma)
  }
}
Y <- t(apply(X, 1, gen))
names = c()
for(i in 1:d_){
  names = c(names, paste0('Y', i))
}
colnames(Y) <- names

## FIT MODELS
drf_MMD <- drf(X = X, Y = Y, splitting.rule = "FourierMMD", num.features=50)
drf_CART <- drf(X = X, Y = Y, splitting.rule = "CART")

# plot pooled data
png(filename = paste0("./plots/SC/", SC, "/POOLED.png"), 
    width = 2400, height = 1600, res=300)
par(mfrow=c(1,1))
par(mar=rep(4.7,4))
plot(Y[,1],Y[,2],pch=19,col="darkblue",xlim=c(-4,4),ylim=c(-4,4),xlab=expression(Y[1]),ylab=expression(Y[2]),font.main=1,font.lab=1,font.axis=1,cex.lab=2,cex.axis=1.5,cex=0.1)
dev.off()

# get predictions on a grid
if (SC == 0) {
  grid <- cbind(seq(-1,1, length.out = 9), matrix(0,nrow=9,ncol=p-1))
} else if (SC == 1) {
  grid <- cbind(expand.grid(seq(-1, 1,length.out = 3),
                            seq(-0.7,0.7,length.out = 3),
                            seq(-1,1,length.out = 2)),
                matrix(0,nrow=18,ncol=p-3))
} else if (SC == 2 || SC == 3){
  grid <- cbind(seq(0,1, length.out = 9), matrix(0,nrow=9,ncol=p-1))
}

p_fourier <- predict(drf_MMD, newdata = as.matrix(grid))


## PLOT KERNELS AND TRUE CONTOURS
png(filename = paste0("./plots/SC", SC, "/COPULA_KERNEL.png"), 
    width = 2000*nrow(grid)/3, height = 2000, res=300)
  
par(mfrow=c(3,nrow(grid)/3))
for (i in 1:nrow(grid)) {
  print(i)
  plot(p_fourier$y[,1:2], col="darkblue", cex=10*p_fourier$weights[i,]^{0.5}, pch=19, asp=1, 
       main=paste0("X1=",grid[i,1], ", X2=",grid[i,2], "X3=",grid[i,3]),
       xlim = c(-4,4),
       ylim = c(-4,4)
       )
  
  # truth
  sim.data <- t(replicate(10000, gen(grid[i,])))
  f1 <- kde2d(sim.data[,1], sim.data[,2], h = rep(1.5, 2), n = 100, lims = c(-4, 4, -4, 4))
  
  contour(f1, nlevels = 5, col="purple",add =TRUE, lwd=3)
}
dev.off()


par(mfrow=c(1,1))
# look at simulated data
for (i in 1:9) {
  ids <- sample(1:nrow(p_fourier$y), size = 10000, replace = TRUE, prob = p_fourier$weights[i,])
  y.sample <- p_fourier$y[ids,]
  y.sample = y.sample + matrix(rnorm(length(ids)*d_, sd=0.01), ncol=d_)
  # truth
  sim.data <- t(replicate(10000, gen(grid[i,])))
  f1 <- kde2d(sim.data[,1], sim.data[,2], h = rep(1.5, 2), n = 50, lims = c(-4, 4, -4, 4))
  
  png(filename = paste0("./plots/SC", SC, "/COPULA_SAMPLE_TESTPOINT_", i, ".png"), 
      width = 1600, height = 1600, res=300)
  par(mar=c(5.1, 5.1, 4.1, 2.1))
  plotBivariate(correl = FALSE,
                col="darkblue", 
                density.xy = f1,
                main=parse(text=paste0("~X[1] ==", grid[i,1])),
                x = y.sample[,1], 
                y = y.sample[,2], 
                cex.points = 0.1, 
                xlim = c(-4.3,4.3), 
                ylim = c(-4.3,4.3),
                asp=1,pch=19,cex.lab = 2,cex.axis=1.5,cex.main=2,font.main=1,font.axis=1,font.lab=1)
  dev.off()
}


#CONDITIONAL CORRELATION PLOTS
png(filename = paste0("./plots/SC", SC, "/PLOT_COPULA_CORRELATION.png"),
    width = 2400, height = 1600, res=300)
par(mar=c(5.1, 5.3, 4.1, 2.1))

x = matrix(seq(min(X[,1]), max(X[,1]), length.out=100), ncol=1)
x_test = cbind(x, matrix(median(X), nrow=nrow(x), ncol=p-1))

par(mfrow=c(1,1))
plot(x, x, type='l',xlab=expression(X[1]),ylab=expression(Cor(Y[1], Y[2])),font.main=1,font.lab=1,font.axis=1,cex.lab=2,cex.axis=1.5,lwd=3,lty=2)
lines(x, predict(drf_MMD, newdata=x_test, functional='cor')$cor[,1,2], col='blue', lty=1, lwd=3)
lines(x, predict(drf_CART, newdata=x_test, functional='cor')$cor[,1,2], col='red', lty=1, lwd=3)
dev.off()



#CONDITIONAL INDEPENDENCE PLOTS
get_hsic <- function(fit_obj, x_test){
  require(dHSIC)
  l = dim(x_test)[1]
  ret = rep(l, 0)
  for(i in 1:l){
    print(i)
    weights = predict(fit_obj, x_test[i,])$weights
    which = sample(1:n, 15000, replace=TRUE, prob=as.vector(weights))
    ret[i] = dhsic(Y[which,1], Y[which,2])
  }
  return(ret)
}

png(filename = paste0("./plots/PLOT_COPULA_HSIC_SC_", SC, ".png"),
    width = 2400, height = 1600, res=300)

x = matrix(seq(min(X[,1]), max(X[,1]), length.out=10), ncol=1)
x_test = cbind(x, matrix(median(X), nrow=nrow(x), ncol=p-1))

par(mar=c(5.1, 5.3, 4.1, 2.1))
par(mfrow=c(1,1))
plot(x, get_hsic(drf_MMD, x_test), col='blue', lwd=4, xlab=expression(X[1]),ylab="HSIC",type='l', ylim=c(0, 0.08),font.main=1,font.lab=1,font.axis=1,cex.lab=2,cex.axis=1.5)
lines(x, get_hsic(drf_CART, x_test), col='red', lwd = 4)
truth=c()
for(xval in x){
  R = 20000
  Ytrue = matrix(0, nrow=R, ncol=d_)
  for(j in 1:R){
    Ytrue[j,] =gen(c(xval, rep(0, p-1)))
  }
  truth = c(truth, dhsic(Ytrue[, 1], Ytrue[, 2])$dHSIC)
}
lines(x, truth, col='black', lwd = 4, lty=2)

dev.off()
