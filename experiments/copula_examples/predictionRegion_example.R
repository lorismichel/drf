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
# SC1: tCopula complete change case, X1 is tail, X2 is covariance and X3 is marginal

# at the moment only scenarios 1 and 2 are available
# choice of SC
SC <- 1

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


mRF_fourier <- mrf(X = X, Y = Y, num.trees = 500, splitting.rule = "fourier", num_features = 100)


# predictions
if (SC == 0) {
  grid <- cbind(seq(-1,1, length.out = 9), matrix(0,nrow=9,ncol=d-1))
} else if (SC == 4) {
  grid <- cbind(expand.grid(seq(-1, 1,length.out = 3),seq(-0.7,0.7,length.out = 3),seq(-1,1,length.out = 2)),matrix(0,nrow=18,ncol=d-3))
}


# get predictions
p_fourier <- predict(mRF_fourier, newdata = grid)






# density estimate only valid in two dimensions

predRegion <- function(y, w, nsim = 10000) {
  if (ncol(y)!=2) {
    stop("only valid for 2 dimenensional response.")
  }
  
  require(MASS)
  
  Ys <- y[sample(1:nrow(y), nsim, replace = TRUE, prob = w),] 
  
  
  d <- MASS::kde2d(Ys[,1], Ys[,2], n = 25, h = c(width.SJ(Ys[,1]), width.SJ(Ys[,2])))
  
  unlisted.z <- as.numeric(d$z) / sum(d$z) 
  sorted.z <- sort(unlisted.z, decreasing = TRUE)
  cum.sorted.z <- cumsum(sorted.z)
  id <- which(cum.sorted.z >= (1-alpha))[1]
  contour(d$x,d$y,d$z, levels = sorted.z[id] * sum(d$z), drawlabels = FALSE)
  grid <- expand.grid(d$x, d$y)
  points(grid[,1], grid[,2], cex = d$z)
}


if (SC == 0) {
  png(filename = paste0("./experiments/copula_examples/plots/PLOT_COPULA_PREDICTION_REGION_SC_", SC, ".png"), 
      width = 2000)
  par(mfrow=c(3,3))
  for (i in 1:9) {
    predRegion(y = p_fourier$y, w = p_fourier$weights[i,])
  }
  dev.off()
} else if (SC == 1) {
  png(filename = paste0("./experiments/copula_examples/plots/PLOT_COPULA_PREDICTION_REGION_SC_", SC), "_1.png", 
      width = 2000)
  par(mfrow=c(3,3))
  for (i in 1:9) {
    predRegion(y = p_fourier$y, w = p_fourier$weights[i,])
  }
  dev.off()
  png(filename = paste0("./experiments/copula_examples/plots/PLOT_COPULA_PREDICTION_REGION_SC_", SC), "_2.png", 
      width = 2000)
  par(mfrow=c(3,3))
  for (i in 10:18) {
    predRegion(y = p_fourier$y, w = p_fourier$weights[i,])
  }
  dev.off()
}





# # upper right limiting point
# findCopulaRegion <- function(w, y, alpha = 0.05, type = "extremes++", delta = 0.01) {
#   
#   u <- apply(y, 2, rank)/(nrow(y) + 1)
#   
#   if (type == "rectangle") {
#     bands.x <- c(0,1)
#     bands.y <- c(0,1)
#     conf <- 1
#     while (conf > (1-alpha)) {
#       bands.x <- bands.x + c(delta, -delta)
#       bands.y <- bands.y + c(delta, -delta)
#       ids <- which(apply(u, 1, function(x) x[1] >= bands.x[1] & x[1] <= bands.x[2] & x[2] >= bands.y[1] & x[2] <= bands.y[2]))
#       conf <- sum(w[ids])
#     }
#     bands.x <- bands.x + c(delta, -delta)
#     bands.y <- bands.y + c(delta, -delta)
#     return(list(bands.x=bands.x, bands.y=bands.y))
#   } else if (type == "extremes++") {
#     bands.x <- 1
#     bands.y <- 1
#     conf <- 1
#     while (conf > (1-alpha)) {
#       bands.x <- bands.x - delta
#       bands.y <- bands.y - delta
#       ids <- which(apply(u, 1, function(x) !(x[1] > bands.x & x[2] > bands.y)))
#       conf <- sum(w[ids])
#     }
#     bands.x <- bands.x + delta
#     bands.y <- bands.y + delta
#     return(list(bands.x=bands.x, bands.y=bands.y))
#   }
# }
# 
# 
# par(mfrow=c(3,3)) 
# for (i in which(grid$Var1 == 1)) {
#   Ytest <- Y[sample(c(1:nrow(Y)), size = 10000, replace=TRUE, prob = p_fourier$weights[i,]),]
#   Utest <- apply(Ytest,2,function(x) rank(x)/(nrow(Ytest)+1))
#   #plot(Utest,pch=19,xlim=c(0,1),ylim=c(0,1))
#   rg <- findCopulaRegion(w = p_fourier$weights[i,], y = p_fourier$y)
#   #segments(x0 = rg$bands.x, y0 = rg$bands.y, x1 = rg$bands.x, y1 = 2,col="red")
#   #segments(x0 = rg$bands.x, y0 = rg$bands.y, x1 = 2, y1 = rg$bands.y,col="red")
#   u <- apply(p_fourier$y, 2, function(x) rank(x) / (length(x)+1))
#   yl <- c(quantile(x = p_fourier$y[,1], rg$bands.x) , quantile(p_fourier$y[,1], rg$bands.y))
#   
#   plot(Ytest,pch=19)
#   points(yl[1], yl[2],col="red", cex=2,pch=19)
#   segments(x0 = yl[1], y0 = yl[2], x1 = yl[1], y1 = 10,col="red")
#   segments(x0 = yl[1], y0 = yl[2], x1 = 10, y1 = yl[2],col="red")
# }
# 
# 
