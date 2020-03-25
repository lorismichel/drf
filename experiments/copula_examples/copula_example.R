# generate copula examples

# libs
library(copula)
library(mrf)
library(MASS)
library(ggplot2)

# source
setwd('~/Documents/projects/heterogeneity/mrf')
source("./experiments/copula_examples/helpers.R")

# repro
set.seed(100)


# SC0: normal copula example with N(0,1),N(0,1) marginals, continuous change of correlation (rho) between -1 and 1.
# SC1: tCopula complete change case, X1 is tail parameter, X2 is covariance (correlation) and X3 is marginal

# PARAMS
# choice of SC
SC <- 0

# dimensions
d <- 5
n <- 10000


# CONSTRUCTION

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
          }  else if (SC == 1) {
            
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
mRF_fourier <- mrf(X = X, Y = Y, num.trees = 2000, 
                   splitting.rule = "fourier",
                   num_features = 3,  
                   bandwidth = 1, 
                   node_scaling = FALSE,
                   min.node.size = 20)


# plot pooled data in case of SC = 0
if (SC == 0) {
  png(filename = paste0("./experiments/copula_examples/plots/PLOT_POOLED_",
                        SC, 
                        ".png"), 
      width = 400, height = 400)
  par(mfrow=c(1,1))
  par(mar=rep(4.7,4))
  plot(Y[,1],Y[,2],pch=19,col="darkblue",xlim=c(-4,4),ylim=c(-4,4),xlab=expression(Y[1]),ylab=expression(Y[2]),font.main=1,font.lab=1,font.axis=1,cex.lab=2,cex.axis=1,cex=0.1)
  dev.off()
}

# get predictions on a grid
if (SC == 0) {
  grid <- cbind(seq(-1,1, length.out = 9), 
                matrix(0,nrow=9,ncol=d-1))
} else if (SC == 1) {
  grid <- cbind(expand.grid(seq(-1, 1,length.out = 3), 
                            seq(-0.7,0.7,length.out = 3), 
                            seq(-1,1,length.out = 2)), 
                matrix(0,nrow=18,ncol=d-3))
}

p_fourier <- predict(mRF_fourier, newdata = grid)


## produce plots for inspection
if (SC == 0) {
  
  # repro
  set.seed(100)
  
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
           cex=5*p_fourier$weights[i,]^{0.5},
           pch=19, asp=1, main=paste0("X1=",grid[i,1], 
                                      ", X2=",grid[i,2]))
    
    # truth
    xx <- grid[i,]
    # copula
    normCop <- normalCopula(param=c(xx[1]), dim = 2)
    
    # margins
    mdvNorm <- mvdc(copula=normCop, margins=c("norm", "norm"),
                    paramMargins=list(list(mean = 0, sd = 1),
                                      list(mean = 0, sd = 1)))

    sim.data <- rMvdc(n = 10000, mvdc = mdvNorm)
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
        width = 400, height = 400)
    par(mar=c(5.1, 5.1, 4.1, 2.1))
    plotBivariate(correl = FALSE,
                  col="darkblue", 
                  density.xy = f1,
                  main=parse(text=paste0("~X[1] ==", xx[1])),
                  x = y.sample[,1], 
                  y = y.sample[,2], 
                  cex.points = 0.1, 
                  xlim = c(-4.3,4.3), 
                  ylim = c(-4.3,4.3),
                  asp=1,pch=19,cex.lab = 2,cex.axis=1,cex.main=2,font.main=1,font.axis=1,font.lab=1)
    dev.off()
  }
}
  
if (SC == 1) {
  
  # repro
  set.seed(10)
  
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
        width = 500, height = 500)
    par(mar=c(5.1, 5.1, 4.1, 2.1))
    plotBivariate(correl = FALSE,
                  col="darkblue", 
                  main = parse(text=paste0("~X[1] ==", xx[1], "~X[2] ==", xx[2], "~X[3] ==", xx[3])),
                  density.xy = f1,
                  x = y.sample[,1], 
                  y = y.sample[,2], 
                  xlim = c(-4.2,4.2),
                  ylim = c(-4.2,4.2),
                  cex.points = 0.1, 
                  asp=1,pch=19,cex.lab = 2,cex.axis=1,
                  font.main=1,cex.main=2,font.axis=1,font.lab=1)
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
                  main = parse(text=paste0("~X[1] ==", xx[1], "~X[2] ==", xx[2], "~X[3] ==", xx[3])),
                  cex.points = 0.1, 
                  asp=1,pch=19,cex.lab = 2,cex.axis=1,cex.main=2,font.main=1,font.axis=1,font.lab=1)
    dev.off()
  }
}

# comparison with gini for corr and hsic

mRF_gini <- mrf(X = X, Y = Y, num.trees = 500, 
                   splitting.rule = "gini",
                   bandwidth = 1, 
                   node_scaling = FALSE,
                   min.node.size = 20)

### helpers functions (! need to be defined here since using variables defined in this file)
get_corr <- function(fit_obj, x_seq){
  require(wCorr)
  l = length(x_seq)
  ret_corr = rep(0, length(l))
  for(i in 1:l){
    point = matrix(c(x_seq[i], rep(0, d-1)), nrow=1, ncol=(d))
    weights = predict(fit_obj, point)$weights
    ret_corr[i] = weightedCorr(Y[,1], Y[,2], weights=weights)
  }
  return(ret_corr)
}
get_corr_grid <- function(fit_obj, grid){
  require(wCorr)
  l = nrow(grid)
  ret_corr = rep(0, length(l))
  for(i in 1:l){
    weights = predict(fit_obj, grid[i,])$weights
    ret_corr[i] = weightedCorr(Y[,1], Y[,2], weights = weights)
  }
  return(ret_corr)
}


if (SC == 0) {
   png(filename = paste0("./experiments/copula_examples/plots/PLOT_COPULA_CORRELATION_SC_", SC, ".png"),
       width = 500, height = 500)
   par(mar=c(5.1, 5.3, 4.1, 2.1))
   x = seq(-1, 1, by=0.03)
   par(mfrow=c(1,1))
   plot(x, x, type='l',xlab=expression(X[1]),ylab=expression(hat(rho)(X[1])),font.main=1,font.lab=1,font.axis=1,cex.lab=2,cex.axis=1,lwd=2,lty=2)
   lines(x, get_corr(mRF_fourier, x), col='blue', lty=2, lwd=3)
   lines(x, get_corr(mRF_gini, x), col='red', lty=2, lwd=3)
   dev.off()
} else if (SC == 1) {

  require(ggplot2)
  png(filename = paste0("./experiments/copula_examples/plots/PLOT_COPULA_CORRELATION_SC_", SC, ".png"),
      width = 1000, height = 1000)
  grid2D <- cbind(expand.grid(seq(-1, 1,length.out = 15), 
                              seq(-1, 1,length.out = 15), 
                              -1), 
                  matrix(0,nrow=225,ncol=d-3))
  names(grid2D)[1:3] <- c('X1', 'X2', 'X3')
  grid2D$corr <- get_corr_grid(mRF_fourier, grid2D)
  
  
  ggplot(grid2D, aes(x=X1, y=X2))+
    geom_raster(aes(fill=abs(corr)))+
    scale_fill_viridis_c(option='B') 
  
  #plot(x = grid[grid[,1]==-1 & grid[,3]==-1,1], y = cor_grid[grid[,1]==-1 & grid[,3]==-1],type="l",xlim=c(0,1))
  #lines(x = grid[grid[,1]==0 & grid[,3]==-1,1], y = cor_grid[grid[,1]==0 & grid[,3]==-1],type="l",col="red")
  #lines(x = grid[grid[,1]==1 & grid[,3]==-1,1], y = cor_grid[grid[,1]==1 & grid[,3]==-1],type="l",col="purple")
  dev.off()
}

get_hsic <- function(fit_obj, x_seq){
   require(dHSIC)
   l = length(x_seq)
   ret_corr = rep(0, length(l))
   for(i in 1:l){
     point = matrix(c(x_seq[i], rep(0, d-1)), nrow=1, ncol=(d))
     weights = predict(fit_obj, point)$weights
     which = sample(1:n, 10000, replace=TRUE, prob=as.vector(weights))
     ret_corr[i] = dhsic(Y[which,1], Y[which,2])
   }
   return(ret_corr)
}
get_hsic_grid <- function(fit_obj, grid){
  require(dHSIC)
  l = nrow(grid)
  ret_hsic = rep(0, l)
  for(i in 1:l){
    print(i)
    point = matrix(grid[i, ], nrow=1)
    weights = predict(fit_obj, grid[i,])$weights
    which = sample(1:n, 10000, replace=TRUE, prob=as.vector(weights))
    ret_hsic[i] = dhsic(Y[which, 1], Y[which, 2])$dHSIC
  }
  return(ret_hsic)
}

if (SC == 0) {
  png(filename = paste0("./experiments/copula_examples/plots/PLOT_COPULA_HSIC_SC_", SC, ".png"),
    width = 500, height = 500)

  x = seq(-1, 1, by=0.05)
  par(mar=c(5.1, 5.3, 4.1, 2.1))
  par(mfrow=c(1,1))
  plot(x, get_hsic(mRF_fourier, x), col='blue', lwd=3, xlab=expression(X[1]),ylab="HSIC",type='l', ylim=c(0, 0.06),font.main=1,font.lab=1,font.axis=1,cex.lab=2,cex.axis=1)
  lines(x, get_hsic(mRF_gini, x), col='red', lwd = 3)
dev.off()
} else if (SC == 1) {
  
  
  Grid2D <- cbind(expand.grid(seq(-1, 1,length.out = 15), 
                              seq(-1, 1,length.out = 15), 
                              -1), 
                  matrix(0,nrow=225,ncol=d-3))
  names(Grid2D)[1:3] <- c('X1', 'X2', 'X3')
  Grid2D$HSIC <- get_hsic_grid(mRF_fourier, Grid2D)
  
  png(filename = paste0("./experiments/copula_examples/plots/PLOT_COPULA_HSIC_SC_", SC, ".png"),
      width = 500, height = 500)
  
  ggplot(Grid2D, aes(x=X1, y=X2))+
    labs(x=expression(X[1]),y=expression(X[2])) +
    theme(
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 10),
      axis.title = element_text(size = 14)
    ) +
    geom_raster(aes(fill=HSIC))+
    scale_fill_viridis_c(option='C') 
  
  dev.off()
  
}
