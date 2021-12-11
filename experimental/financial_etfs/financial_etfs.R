# ETF financial dataset

# options
#PATH.DATA <- "~/Downloads/ETF.csv"
PATH.DATA <- "~/Documents/projects/heterogeneity/finance/ETF.csv"
PREPRO <- "futureLogReturns"

# libs
require(data.table)
require(mrf)

# libs
source("./experiments/copula_examples/helpers.R")

# data
etfs <- fread(PATH.DATA)
etfs$V1 <- as.Date(etfs$V1)

date <- etfs$V1

etfs <- as.matrix(etfs[,-c(1)])
# missing values
print(paste0("number of missing values: ", sum(is.na(etfs))))

# dates
summary(date)

num_etfs = ncol(etfs)
for (i in 1:num_etfs) {
  if (i == 1) {
    plot(date, etfs[,i],type="l",col="black",ylim=c(0,500),xlab="date", ylab="value")
  } else if (i > 1 & i <= 11) {
    lines(date, etfs[,i],type="l",col="black")
  } else {
    lines(date, etfs[,i],type="l",col="red")
  }
}

if (PREPRO == "futureLogReturns") {
  logret_day <- apply(etfs, 2, function(x) log(shift(x, n = 1, type = "lag")/x))
  colnames(logret_day) <- paste(colnames(etfs), 'daily return')
  
  logret_month <- apply(etfs, 2, function(x) log(shift(x, n = 30, type = "lag")/x))
  colnames(logret_month) <- paste(colnames(etfs), 'monthly return')
  
  etfs <- cbind(etfs, logret_day, logret_month)
  etfs <- etfs[-(1:30),]
  date <- date[-(1:30)]
}

for (i in 1:num_etfs) {
  if (i == 1) {
    plot(date, etfs[,i+num_etfs],type="l",col="black",ylim=c(-0.2,0.2),xlab="date", ylab="value")
  } else if (i > 1 & i <= 11) {
    lines(date, etfs[,i+num_etfs],type="l",col="black")
  } else {
    lines(date, etfs[,i+num_etfs],type="l",col="red")
  }
}

# define the response and the predictors
Y <- as.matrix(etfs[,1*num_etfs + c(4,8)])
X <- as.matrix(etfs[,0*num_etfs + c(12:num_etfs)])

mrf_fit <- mrf(X = X, Y = Y, splitting.rule = "fourier", num_features = 3)

for(i in c(4,5)){
  par(mfrow=c(2,2))
  for(q in c(0.1, 0.3, 0.7, 0.9)){
    test_point = matrix(apply(X, 2, median), 1, ncol(X))
    test_point[1, i] = quantile(X[,i], q)
    weights <- predict(mrf_fit, newdata=test_point)$weights
    plot(Y[,1], Y[,2], cex=5*weights[1,]^0.5, pch=19, main=colnames(X)[i], xlim=c(-0.05, 0.05), ylim=c(-0.05, 0.05))
  }
}

# # reduce for vizu
# pcaY_2d <- prcomp(x = Y)$x[,1:2]
# 
# # fit an mrf
# mRF_pca_factors_100 <- mrf(X = X, Y = pcaY_2d, num.trees = 500, num_features = 100)
# mRF_pca_time_100 <- mrf(X = matrix(1:nrow(X),ncol=1), Y =pcaY_2d, num.trees = 500, num_features = 100)
# 
# # exporatory plots
# plot(pcaY_2d,pch=19)
# par(mfrow=c(2,1))
# plot(date,pcaY_2d[,1],type="l")
# plot(date,pcaY_2d[,2],type="l")
# 
# p_pca_time_100 <- predict(mRF_pca_time_100, newdata = matrix(quantile(1:nrow(X), probs = seq(0.1,0.9,0.1)),ncol=1))
# p_pca_factors_100 <- predict(mRF_pca_factors_100, newdata = matrix(quantile(1:nrow(X), probs = seq(0.1,0.9,0.1)),ncol=1))
# 
# # plotting the dependence
# plot(pcaY_2d,pch=19)
# for (i in 1:9) {
#   plotBivariateCopula(x = pcaY_2d[,1],y = pcaY_2d[,2], cex.points = p_pca_time_100$weights[i,]*20,pch=19, smooth = FALSE)
# }
# 
# # par(mfrow=c(1,2))
# # for (i in 1:nrow(p_pca_time_100$weights)) {
# #   #plot(col="darkblue", p_fourier$y, cex=p_fourier$weights[i,]*10,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
# #   #plotBivariate(correl = FALSE, col="darkblue", x = p_fourier$y[,1], y = p_fourier$y[,2], cex.points = p_fourier$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
# #   if (i == 1) {
# #     plot(date, pcaY_2d[,1],col="grey",cex = 0.5)
# #     points(date, pcaY_2d[,1],pch=19,cex=sqrt(p_pca_time_100$weights[i,]),col=i+1)
# #     abline(v = date[quantile(1:nrow(X),  probs = seq(0.1,0.9,0.1))][i],col=i+1,lty=2)
# #     #  plot(as.Date(series$V1), series$DBC,col="grey", cex = 0.5)
# #     #  points(as.Date(series$V1), series$DBC,pch=19,cex=sqrt(p_fourier$weights[i,]))
# #     #  abline(v = as.Date(series$V1)[quantile(1:nrow(series), seq(0,1,length.out = 5))],col="blue",lty=2)
# #   } else {
# #     # points(as.Date(series$V1), series$AGG,col="grey",cex = 0.5)
# #     points(date, pcaY_2d[,1],pch=19,cex=sqrt(p_pca_time_100$weights[i,]),col=i+1)
# #     abline(v = date[quantile(1:nrow(X), probs = seq(0.1,0.9,0.1))][i],col=i+1,lty=2)
# #     # abline(v = as.Date(series$V1)[quantile(1:nrow(series), seq(0,1,length.out = 5))],col="blue",lty=2)
# #     # plot(as.Date(series$V1), series$DBC,col="grey", cex = 0.5)
# #     # points(as.Date(series$V1), series$DBC,pch=19,cex=sqrt(p_fourier$weights[i,]),col=i+1)
# #     # abline(v = as.Date(series$V1)[quantile(1:nrow(series), seq(0,1,length.out = 5))],col="blue",lty=2)
# #   }
# # }
# # 
# # for (i in 1:nrow(p_pca_time_100$weights)) {
# #   #plot(col="darkblue", p_fourier$y, cex=p_fourier$weights[i,]*10,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
# #   #plotBivariate(correl = FALSE, col="darkblue", x = p_fourier$y[,1], y = p_fourier$y[,2], cex.points = p_fourier$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
# #   if (i == 1) {
# #     plot(date, pcaY_2d[,1],col="grey",cex = 0.5)
# #     points(date, pcaY_2d[,2],pch=19,cex=sqrt(p_pca_time_100$weights[i,]),col=i+1)
# #     abline(v = date[quantile(1:nrow(X),  probs = seq(0.1,0.9,0.1))][i],col=i+1,lty=2)
# #     #  plot(as.Date(series$V1), series$DBC,col="grey", cex = 0.5)
# #     #  points(as.Date(series$V1), series$DBC,pch=19,cex=sqrt(p_fourier$weights[i,]))
# #     #  abline(v = as.Date(series$V1)[quantile(1:nrow(series), seq(0,1,length.out = 5))],col="blue",lty=2)
# #   } else {
# #     # points(as.Date(series$V1), series$AGG,col="grey",cex = 0.5)
# #     points(date, pcaY_2d[,2],pch=19,cex=sqrt(p_pca_time_100$weights[i,]),col=i+1)
# #     abline(v = date[quantile(1:nrow(X), probs = seq(0.1,0.9,0.1))][i],col=i+1,lty=2)
# #     # abline(v = as.Date(series$V1)[quantile(1:nrow(series), seq(0,1,length.out = 5))],col="blue",lty=2)
# #     # plot(as.Date(series$V1), series$DBC,col="grey", cex = 0.5)
# #     # points(as.Date(series$V1), series$DBC,pch=19,cex=sqrt(p_fourier$weights[i,]),col=i+1)
# #     # abline(v = as.Date(series$V1)[quantile(1:nrow(series), seq(0,1,length.out = 5))],col="blue",lty=2)
# #   }
# # }
# 
# 
# 
# ## prediction analysis on pcaY
# set.seed(1)
# X.train <- X[1:(nrow(X)/2),] 
# pcaY_2d.train <- pcaY_2d[1:(nrow(X)/2),] 
# X.test <- X[-c(1:(nrow(X)/2)),] 
# pcaY_2d.test <- pcaY_2d[-c(1:(nrow(X)/2)),] 
# mRF_100 <- mrf(X = X.train, Y = pcaY_2d.train, num.trees = 500, num_features = 100)
# 
# # see how the predictions perform
# p_fourier <- predict(mRF_100, newdata = X.test)
# 
# require(profvis)
# 
# par(mfrow=c(1,1))
# for (i in 1:nrow(p_fourier$weights)) {
#   plot(x = pcaY_2d.test[,1], y = pcaY_2d.test[,2], pch=19,cex = p_fourier$weights[i,]*50, xlim=c(-0.39,0.33),ylim=c(-0.11,0.15))
#   points(x = pcaY_2d.test[i,1],y = pcaY_2d.test[i,2],col="red",pch=19)
#   pause(1)
#   print(i)
# }
# 
