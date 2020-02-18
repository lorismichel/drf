# time series example: returns of etf
require(mrf)
require(data.table)

series <- fread("./experiments/timeSeries/data/returns.txt")

par(mfrow=c(1,1))
# pair
plot(series$AGG, series$DBC,pch=19,xlab="AGG",ylab="DBC",cex=0.5)
# along time
par(mfrow=c(2,1))
plot(as.Date(series$V1), series$AGG,pch=19,cex = 0.5)
abline(v = as.Date(series$V1)[quantile(1:nrow(series), seq(0,1,length.out = 5))],col="blue",lty=2)
plot(as.Date(series$V1), series$DBC,pch=19,cex = 0.5)
abline(v = as.Date(series$V1)[quantile(1:nrow(series), seq(0,1,length.out = 5))],col="blue",lty=2)

# plot the series
# first two analysed
mRF <- mrf(X = matrix(1:nrow(series),ncol=1), Y = as.matrix(series[,2:3, with=F]), num.trees = 500, num_features = 100, splitting.rule = "fourier")
p_fourier <- predict(mRF, newdata = matrix(quantile(1:nrow(series), seq(0,1,length.out = 5)),ncol=1))
# fourier 
par(mfrow=c(2,1))
#par(mar=rep(2,4))
#plot(col="black",p_fourier$y,pch=19,main="original data",cex=0.2)
for (i in 1:nrow(p_fourier$weights)) {
  #plot(col="darkblue", p_fourier$y, cex=p_fourier$weights[i,]*10,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
  #plotBivariate(correl = FALSE, col="darkblue", x = p_fourier$y[,1], y = p_fourier$y[,2], cex.points = p_fourier$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
  if (i == 1) {
    plot(as.Date(series$V1), series$AGG,col="grey",cex = 0.5)
    points(as.Date(series$V1), series$AGG,pch=19,cex=sqrt(p_fourier$weights[i,]),col=i+1)
    abline(v = as.Date(series$V1)[quantile(1:nrow(series), seq(0,1,length.out = 5))][i],col=i+1,lty=2)
  #  plot(as.Date(series$V1), series$DBC,col="grey", cex = 0.5)
  #  points(as.Date(series$V1), series$DBC,pch=19,cex=sqrt(p_fourier$weights[i,]))
  #  abline(v = as.Date(series$V1)[quantile(1:nrow(series), seq(0,1,length.out = 5))],col="blue",lty=2)
  } else {
   # points(as.Date(series$V1), series$AGG,col="grey",cex = 0.5)
    points(as.Date(series$V1), series$AGG,pch=19,cex=sqrt(p_fourier$weights[i,]),col=i+1)
    abline(v = as.Date(series$V1)[quantile(1:nrow(series), seq(0,1,length.out = 5))][i],col=i+1,lty=2)
   # plot(as.Date(series$V1), series$DBC,col="grey", cex = 0.5)
   # points(as.Date(series$V1), series$DBC,pch=19,cex=sqrt(p_fourier$weights[i,]),col=i+1)
   # abline(v = as.Date(series$V1)[quantile(1:nrow(series), seq(0,1,length.out = 5))],col="blue",lty=2)
  }
  
  }

for (i in 1:nrow(p_fourier$weights)) {
  #plot(col="darkblue", p_fourier$y, cex=p_fourier$weights[i,]*10,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
  #plotBivariate(correl = FALSE, col="darkblue", x = p_fourier$y[,1], y = p_fourier$y[,2], cex.points = p_fourier$weights[i,]*200,pch=19, asp=1, main=paste0("X1=",round(seq(-1,1,length.out = 16)[i],3)))
  if (i == 1) {
    plot(as.Date(series$V1), series$DBC,col="grey",cex = 0.5)
    points(as.Date(series$V1), series$DBC,pch=19,cex=sqrt(p_fourier$weights[i,]),col=i+1)
    abline(v = as.Date(series$V1)[quantile(1:nrow(series), seq(0,1,length.out = 5))][i],col=i+1,lty=2)
    #  plot(as.Date(series$V1), series$DBC,col="grey", cex = 0.5)
    #  points(as.Date(series$V1), series$DBC,pch=19,cex=sqrt(p_fourier$weights[i,]))
    #  abline(v = as.Date(series$V1)[quantile(1:nrow(series), seq(0,1,length.out = 5))],col="blue",lty=2)
  } else {
    # points(as.Date(series$V1), series$AGG,col="grey",cex = 0.5)
    points(as.Date(series$V1), series$DBC,pch=19,cex=sqrt(p_fourier$weights[i,]),col=i+1)
    abline(v = as.Date(series$V1)[quantile(1:nrow(series), seq(0,1,length.out = 5))][i],col=i+1,lty=2)
    # abline(v = as.Date(series$V1)[quantile(1:nrow(series), seq(0,1,length.out = 5))],col="blue",lty=2)
    # plot(as.Date(series$V1), series$DBC,col="grey", cex = 0.5)
    # points(as.Date(series$V1), series$DBC,pch=19,cex=sqrt(p_fourier$weights[i,]),col=i+1)
    # abline(v = as.Date(series$V1)[quantile(1:nrow(series), seq(0,1,length.out = 5))],col="blue",lty=2)
  }
  
}



