# generate copula examples

# libs
library(copula)
library(mrf)
library(MASS)

# source
source("./experiments/copula_examples/helpers.R")

# repro
set.seed(100)


# SC0: normal copula example with N(0,1),N(0,1) marginals, continuous change of cor
# SC1: normal copula example with N(0,1),Exp(1) marginals, continuous change of cor
# SC2: tCopula example with N(0,1),N(0,1) marginals, continuous change of cor
# SC3: tCopula example with N(0,1),N(0,1) marginals, hard change of tails
# SC4: tCopula complete change case, X1 is tail, X2 is covariance and X3 is marginal

# at the moment only scenarios 1 and 2 are available
# choice of SC
SC <- 0

# Examples 
d <- 10
n <- 10000



# constructing the data

# predictors
X  <- matrix(runif(n*d, min = -1, max = 1),ncol = d)
# responses
Y <- t(apply(X, 1, function(xx) {
  
          if (SC == 0) {
            
            # copula
            normCop <- normalCopula(param=c(xx[1]), dim = 2)
            
            # margins
            mdvNorm <- mvdc(copula=normCop, margins=c("norm", "norm"),
                               paramMargins=list(list(mean = 0, sd = 1),
                                                 list(mean = 0, sd = 1)))
            # gen
            rMvdc(n = 1, mvdc = mdvNorm)
          } else if (SC == 1) {
            
            # copula
            normCop <- normalCopula(param=c(xx[1]), dim = 2)
            # margins
            mdvNorm <- mvdc(copula=normCop, margins=c("norm", "exp"),
                            paramMargins=list(list(mean = 0, sd = 1),
                                              list(rate = 1)))
            # gen
            rMvdc(n = 1, mvdc = mdvNorm)
          } else if (SC == 2) {
            
            # copula
            tCop <- tCopula(dim=2, xx[1], df=2)
            # margins
            mdvT <- mvdc(copula=tCop, margins=c("norm", "norm"),
                              paramMargins=list(list(mean = 0, sd = 1),
                                                list(mean = 0, sd = 1)))
            # gen
            rMvdc(n = 1, mvdc = mdvT)
            
          } else if (SC == 3) {
            
            # copula
            tCop <- tCopula(0.5,  df=ifelse(xx[1]<=0, 1, 8))
            # margins
            mdvT <- mvdc(copula=tCop, margins=c("norm", "norm"),
                         paramMargins=list(list(mean = 0, sd = 1),
                                           list(mean = 0, sd = 1)))
            # gen
            rMvdc(n = 1, mvdc = mdvT)
            
          } else if (SC == 4) {
            
            # copula
            tCop <- tCopula(xx[2],  df=ifelse(xx[1] <= (-1 + 2/3), 1, ifelse(xx[1]<= (-1 + 4/3), 3, 10)))
            # margins
            if (xx[3]<=0) {
              margins <- c("norm", "norm")
              paramMargins <- list(list(mean = 0, sd = 1),
                               list(mean = 0, sd = 1))
            } else {
              margins <- c("norm", "exp")
              paramMargins <- list(list(mean = 0, sd = 1),
                               list(rate = 1))
            }
            mdvT <- mvdc(copula=tCop, margins=margins,
                         paramMargins=paramMargins)
            # gen
            rMvdc(n = 1, mvdc = mdvT)
            
          }
          }))
colnames(Y) <- c("Y1", "Y2")


## fitting the models and getting predictions

# fits
mRF_fourier <- mrf(X = X, Y = Y, num.trees = 500, splitting.rule = "fourier",
                   num_features = 100,  bandwidth = 1, node_scaling = FALSE,
                   min.node.size = 20)


# predictions
if (SC == 0) {
  grid <- cbind(seq(-1,1, length.out = 9), matrix(0,nrow=9,ncol=d-1))
} else if (SC == 4) {
  grid <- cbind(expand.grid(seq(-1, 1,length.out = 3),seq(-0.7,0.7,length.out = 3),seq(-1,1,length.out = 2)),matrix(0,nrow=18,ncol=d-3))
}


# get predictions
p_fourier <- predict(mRF_fourier, newdata = grid)




## produce plots for inspection

if (SC == 0) {
  
  
  # look at kernels
  png(filename = paste0("./experiments/copula_examples/plots/PLOT_COPULA_KERNEL_SC_",
                        SC, 
                        ".png"), 
      width = 2000, height = 2000)
  
  par(mfrow=c(3,3))
  for (i in 1:9) {
    # plot(p_fourier$y, pch=19,main = paste0("X1=", 
    #                                        grid[i,1],
    #                                        ", X2=", 
    #                                        grid[i,2]), 
    #      cex=0.2,col="grey")
    
    plot(col="darkblue", p_fourier$y, 
           cex=p_fourier$weights[i,]^{0.5},
           pch=19, asp=1, main=paste0("X1=",grid[i,1],3, 
                                      ", X2=",grid[i,2],3))
    
    # truth
    xx <- grid[i,]
    # copula
    normCop <- normalCopula(param=c(xx[1]), dim = 2)
    
    # margins
    mdvNorm <- mvdc(copula=normCop, margins=c("norm", "norm"),
                    paramMargins=list(list(mean = 0, sd = 1),
                                      list(mean = 0, sd = 1)))

    sim.data <- sim.data <- rMvdc(n = 10000, mvdc = mdvNorm)
    f1 <- kde2d(sim.data[,1], sim.data[,2], h = rep(1.5, 2), n = 50, lims = c(-4, 4, -4, 4))
    
    contour(f1, nlevels = 5, col="purple",add =TRUE, lty = 2,lwd=2)
  }
  dev.off()
  
  
  
  par(mfrow=c(1,1))
  # look at simulated data
  for (i in 1:9) {
    ids <- sample(1:nrow(p_fourier$y), size = 10000, replace = TRUE, prob = p_fourier$weights[i,])
    y.sample <- p_fourier$y[ids,]
    
    # truth
    xx <- grid[i,]
    # copula
    normCop <- normalCopula(param=c(xx[1]), dim = 2)
    
    # margins
    mdvNorm <- mvdc(copula=normCop, margins=c("norm", "norm"),
                    paramMargins=list(list(mean = 0, sd = 1),
                                      list(mean = 0, sd = 1)))
    
    sim.data <- sim.data <- rMvdc(n = 10000, mvdc = mdvNorm)
    f1 <- kde2d(sim.data[,1], sim.data[,2], h = rep(1.5, 2), n = 50, lims = c(-4, 4, -4, 4))
    
    png(filename = paste0("./experiments/copula_examples/plots/PLOT_COPULA_SAMPLE_SC_",
                          SC, 
                          "_TESTPOINT_",
                          i, 
                          ".png"),
        width = 1000)
    
    plotBivariate(correl = FALSE,
                  col="darkblue", 
                  density.xy = f1,
                  x = y.sample[,1], 
                  y = y.sample[,2], 
                  cex.points = 0.5, 
                  asp=1)
    dev.off()
  }
}
  


if (SC == 4) {
  
  
  # look at kernels
  png(filename = paste0("./experiments/copula_examples/plots/PLOT_COPULA_KERNEL_SC_",
                        SC, 
                        "_1.png"), 
      width = 2000, height = 2000)
  
  par(mfrow=c(3,3))
  for (i in 1:9) {
    #plot(p_fourier$y,pch=19,main=paste0("X1=",grid[i,1], ", X2=",grid[i,2]),cex=0.2,col="grey")
    
    plot(col="darkblue", p_fourier$y, cex=p_fourier$weights[i,]^{0.5},pch=19, asp=1, main=paste0("X1=",grid[i,1],3, ", X2=",grid[i,2],3))
    
    # truth
    xx <- grid[i,]
    tCop <- tCopula(xx[2],
                    df=ifelse(xx[1] <= (-1 + 2/3), 1, 
                              ifelse(xx[1]<= (-1 + 4/3), 3, 10)))
    margins <- c("norm", "norm")
    paramMargins <- list(list(mean = 0, sd = 1),
                         list(mean = 0, sd = 1))
    mdvT <- mvdc(copula=tCop, margins=margins,
                 paramMargins=paramMargins)
    sim.data <- sim.data <- rMvdc(n = 100000, mvdc = mdvT)
    f1 <- kde2d(sim.data[,1], sim.data[,2], h = rep(1.5, 2), n = 50, lims = c(-4, 4, -4, 4))
    
    contour(f1, nlevels = 5, col="purple", add =TRUE, lty = 2,lwd=2)
    
    #plotBivariate(correl = FALSE, col="darkblue", x = p_fourier$y[,1], y = p_fourier$y[,2], cex.points = p_fourier$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
  }
  
  dev.off()

  par(mfrow=c(1,1))
  
  # look at simulated data
  for (i in 1:9) {
    ids <- sample(1:nrow(p_fourier$y), size = 10000, replace = TRUE, prob = p_fourier$weights[i,])
    y.sample <- p_fourier$y[ids,]
    
    # truth
    xx <- grid[i,]
    tCop <- tCopula(xx[2],
                    df=ifelse(xx[1] <= (-1 + 2/3), 1, 
                              ifelse(xx[1]<= (-1 + 4/3), 3, 10)))
    margins <- c("norm", "norm")
    paramMargins <- list(list(mean = 0, sd = 1),
                         list(mean = 0, sd = 1))
    mdvT <- mvdc(copula=tCop, margins=margins,
                 paramMargins=paramMargins)
    
    sim.data <- sim.data <- rMvdc(n = 10000, mvdc = mdvT)
    f1 <- kde2d(sim.data[,1], sim.data[,2], h = rep(1.5, 2), n = 50, lims = c(-4, 4, -4, 4))
    
    png(filename = paste0("./experiments/copula_examples/plots/PLOT_COPULA_SAMPLE_SC_",
                          SC, 
                          "_TESTPOINT_",
                          i, 
                          ".png"),
        width = 1000)
    
    plotBivariate(correl = FALSE,
                  col="darkblue", 
                  density.xy = f1,
                  x = y.sample[,1], 
                  y = y.sample[,2], 
                  cex.points = 0.5, 
                  asp=1)
    dev.off()
  }
  
  
  # look at the kernels
  
  # look at kernels
  png(filename = paste0("./experiments/copula_examples/plots/PLOT_COPULA_KERNEL_SC_",
                        SC, 
                        "_2.png"), 
      width = 2000, height = 2000)
  
  par(mfrow=c(3,3))
  #par(mar=rep(2,4))
  #plot(col="black",p_fourier$y,pch=19,main="original data",cex=0.2)
  for (i in 10:18) {
    #plot(p_fourier$y,pch=19,main=paste0("X1=",grid[i,1], ", X2=",grid[i,2]),cex=0.2,col="grey")
    
    plot(p_fourier$y, cex=p_fourier$weights[i,]^{0.5},pch=19, col="darkblue", asp=1, main=paste0("X1=",grid[i,1],3, ", X2=",grid[i,2],3))
    
    # truth
    xx <- grid[i,]
    tCop <- tCopula(xx[2],  
                    df=ifelse(xx[1] <= (-1 + 2/3), 1, 
                              ifelse(xx[1]<= (-1 + 4/3), 3, 10)))
    margins <- c("norm", "exp")
    paramMargins <- list(list(mean = 0, sd = 1),
                         list(rate = 1))
    mdvT <- mvdc(copula=tCop, margins=margins,
                 paramMargins=paramMargins)
    
    sim.data <- sim.data <- rMvdc(n = 100000, mvdc = mdvT)
    f1 <- kde2d(sim.data[,1], sim.data[,2], h = rep(1.5, 2), n = 50, lims = c(-4, 4, 0, 10))
    
    contour(f1, nlevels = 5, col="purple",add = TRUE, lty = 2,lwd=2)
    #plotBivariate(correl = FALSE, col="darkblue", x = p_fourier$y[,1], y = p_fourier$y[,2], cex.points = p_fourier$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
  }
  
  dev.off()
  
  par(mfrow=c(1,1))
  
  # look at simulated data
  for (i in 10:18) {
    ids <- sample(1:nrow(p_fourier$y), size = 10000, replace = TRUE, prob = p_fourier$weights[i,])
    y.sample <- p_fourier$y[ids,]
    
    # truth
    xx <- grid[i,]
    tCop <- tCopula(xx[2],
                    df=ifelse(xx[1] <= (-1 + 2/3), 1, 
                              ifelse(xx[1]<= (-1 + 4/3), 3, 10)))
    margins <- c("norm", "norm")
    paramMargins <- list(list(mean = 0, sd = 1),
                         list(mean = 0, sd = 1))
    mdvT <- mvdc(copula=tCop, margins=margins,
                 paramMargins=paramMargins)
    
    sim.data <- sim.data <- rMvdc(n = 10000, mvdc = mdvT)
    f1 <- kde2d(sim.data[,1], sim.data[,2], h = rep(1.5, 2), n = 50, lims = c(-4, 4, 0, 10))
    
    png(filename = paste0("./experiments/copula_examples/plots/PLOT_COPULA_SAMPLE_SC_",
                          SC, 
                          "_TESTPOINT_",
                          i, 
                          ".png"), 
        width = 1000)
    
    plotBivariate(correl = FALSE,
                  col="darkblue", 
                  density.xy = f1,
                  x = y.sample[,1], 
                  y = y.sample[,2], 
                  cex.points = 0.5, 
                  asp=1)
    dev.off()
  }
}






# # produce plots 
# par(mfrow=c(4,4))
# #par(mar=rep(2,4))
# plot(col="black",p_fourier$y,pch=19,main="original data",cex=0.2)
# for (i in 1:nrow(p_fourier$weights)) {
#   plot(col="darkblue", p_fourier$y, cex=p_fourier$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
#   #plotBivariate(correl = FALSE, col="darkblue", x = p_fourier$y[,1], y = p_fourier$y[,2], cex.points = p_fourier$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
# }
# 
# # produce plots 
# par(mfrow=c(4,4))
# #par(mar=rep(2,4))
# plot(col="black",p_fourier2$y,pch=19,main="original data",cex=0.2)
# for (i in 1:nrow(p_fourier2$weights)) {
#   plot(col="darkblue", p_fourier2$y, cex=p_fourier2$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
#   #plotBivariate(correl = FALSE, col="darkblue", x = p_fourier$y[,1], y = p_fourier$y[,2], cex.points = p_fourier$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
# }
# 
# par(mfrow=c(4,4))
# #par(mar=rep(2,4))
# plot(col="black",p_gini$y,pch=19,main="original data",cex=0.2)
# for (i in 1:nrow(p_gini$weights)) {
#   plot(col="darkblue", p_gini$y, cex=p_gini$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
# }
# 
# get_corr <- function(fit_obj, x_seq){
#   require(wCorr)
#   l = length(x_seq)
#   ret_corr = rep(0, length(l))
#   for(i in 1:l){
#     point = matrix(c(x_seq[i], rep(0, d.noise)), nrow=1, ncol=(d.noise+1))
#     weights = predict(fit_obj, point)$weights
#     ret_corr[i] = weightedCorr(Y[,1], Y[,2], weights=weights)
#   }
#   return(ret_corr)
# }
# x = seq(-1, 1, by=0.03)
# par(mfrow=c(1,1))
# plot(x, x, type='l')
# lines(x, get_corr(mRF_fourier, x), col='blue', lty=2)
# lines(x, get_corr(mRF_fourier2, x), col='red', lty=2)
# lines(x, get_corr(mRF_gini, x), col='green', lty=2)
# 
# 
# get_hsic <- function(fit_obj, x_seq){
#   require(dHSIC)
#   l = length(x_seq)
#   ret_corr = rep(0, length(l))
#   for(i in 1:l){
#     point = matrix(c(x_seq[i], rep(0, d.noise)), nrow=1, ncol=(d.noise+1))
#     weights = predict(fit_obj, point)$weights
#     which = sample(1:n, 10000, replace=TRUE, prob=as.vector(weights))
#     ret_corr[i] = dhsic(Y[which,1], Y[which,2])
#   }
#   return(ret_corr)
# }
# x = seq(-1, 1, by=0.05)
# par(mfrow=c(1,1))
# plot(x, get_hsic(mRF_fourier, x), col='blue', lty=2, type='l', ylim=c(0, 0.06))
# lines(x, get_hsic(mRF_fourier2, x), col='red', lty=2)
# lines(x, get_hsic(mRF_gini, x), col='green', lty=2)
