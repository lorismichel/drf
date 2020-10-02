# creating the summaries for the paper about univariate analysis



# pinball losses
load("./../../experiments/univariate_comparison/results_univariate_paper_w3.Rdata")

# pinball for first example
print("synthetic1")
mat.res.pinball <- Reduce(x = results$synthetic1[[1]]$q.losses[1:5], f = rbind)
for (i in 2:length(results$synthetic1)) {
  mat.res.pinball <- mat.res.pinball + Reduce(x = results$synthetic1[[i]]$q.losses[1:5], f = rbind)
}
mat.res.pinball <- mat.res.pinball / 10
print(mat.res.pinball)

# pinball for second example
print("synthetic2")
mat.res.pinball <- Reduce(x = results$synthetic2[[1]]$q.losses[1:5], f = rbind)
for (i in 2:length(results$synthetic2)) {
  mat.res.pinball <- mat.res.pinball + Reduce(x = results$synthetic2[[i]]$q.losses[1:5], f = rbind)
}
mat.res.pinball <- mat.res.pinball / 10
print(mat.res.pinball)

# pinball for third example
print("synthetic3")
mat.res.pinball <- Reduce(x = results$synthetic3[[1]]$q.losses[1:5], f = rbind)
for (i in 2:length(results$synthetic3)) {
  mat.res.pinball <- mat.res.pinball + Reduce(x = results$synthetic3[[i]]$q.losses[1:5], f = rbind)
}
mat.res.pinball <- mat.res.pinball / 10
print(mat.res.pinball)


# wasserstein distance on the synthetic datasets
load("./../../experiments/univariate_comparison/results_univariate_paper_final.Rdata")
x <- c(- 50:1 / 51, 1:50 / 51)
qs <- setdiff(seq(0,1,length.out = 20),c(0,1))
sapply(x, function(xx) qnorm(qs, mean = 0.8*(xx >  0)))
for (i in 1) {
  QRF_W2 <- apply((results$synthetic1[[i]]$qQRF$predictions-t(sapply(x, function(xx) qnorm(qs, mean = 0.8*(xx >  0)))))^2,1,mean)
  DRF_W2 <- apply((results$synthetic1[[i]]$qDRF[,1,]-t(sapply(x, function(xx) qnorm(qs, mean = 0.8*(xx >  0)))))^2,1,mean)
  GRF_W2 <- apply((results$synthetic1[[i]]$qGRF-t(sapply(x, function(xx) qnorm(qs, mean = 0.8*(xx >  0)))))^2,1,mean)
  trf.quant <- sapply(results$synthetic1[[i]]$qTRF,unlist)
  TRF_W2 <- apply((trf.quant-t(sapply(x, function(xx) qnorm(qs, mean = 0.8*(xx >  0)))))^2,1,mean)
  if (i==1) {
    plot(x,QRF_W2,ylim=c(0,1.1),col="darkgreen",type="l",xlab=expression(X[1]),ylab="2-Wassertein distance",lwd=1.2^2)
  } else {
    lines(x,QRF_W2,col="darkgreen",type="l",lwd=1.2^2)
  }
  
  lines(x,DRF_W2,col="blue",type="l",lwd=1.2^2)
  lines(x,GRF_W2, col="brown",type="l",lwd=1.2^2)
  lines(x,TRF_W2, col="red",type="l",lwd=1.2^2)
}
qs <- setdiff(seq(0,1,length.out = 20),c(0,1))
sapply(x, function(xx) qnorm(qs, 1 + 1*(xx > 0)))
for (i in 1) {
  QRF_W2 <- apply((results$synthetic2[[i]]$qQRF$predictions-t(sapply(x, function(xx) qnorm(qs, sd = 1+1*(xx >  0)))))^2,1,mean)
  DRF_W2 <- apply((results$synthetic2[[i]]$qDRF[,1,]-t(sapply(x, function(xx) qnorm(qs, sd = 1+1*(xx >  0)))))^2,1,mean)
  GRF_W2 <- apply((results$synthetic2[[i]]$qGRF-t(sapply(x, function(xx) qnorm(qs, sd = 1+1*(xx >  0)))))^2,1,mean)
  trf.quant <- sapply(results$synthetic2[[i]]$qTRF,unlist)
  TRF_W2 <- apply((trf.quant-t(sapply(x, function(xx) qnorm(qs, sd = 1+1*(xx >  0)))))^2,1,mean)
  if (i==1) {
    plot(x,QRF_W2,ylim=c(0,4),col="darkgreen",type="l",xlab=expression(X[1]),ylab="2-Wassertein distance",lwd=1.2^2)
  } else {
    lines(x,QRF_W2,col="darkgreen",type="l",lwd=1.2^2)
  }
  
  lines(x,DRF_W2,col="blue",type="l",lwd=1.2^2)
  lines(x,GRF_W2, col="brown",type="l",lwd=1.2^2)
  lines(x,TRF_W2, col="red",type="l",lwd=1.2^2)
}
qs <- setdiff(seq(0,1,length.out = 20),c(0,1))
for (i in 1) {
  QRF_W2 <- apply((results$synthetic3[[i]]$qQRF$predictions-t(sapply(x, function(xx) if (xx >= 0) qexp(qs, 1) else qnorm(qs))))^2,1,mean)
  DRF_W2 <- apply((results$synthetic3[[i]]$qDRF[,1,]-t(sapply(x, function(xx) if (xx >= 0) qexp(qs, 1) else qnorm(qs))))^2,1,mean)
  GRF_W2 <- apply((results$synthetic3[[i]]$qGRF-t(sapply(x, function(xx) if (xx >= 0) qexp(qs, 1) else qnorm(qs))))^2,1,mean)
  trf.quant <- sapply(results$synthetic3[[i]]$qTRF,unlist)
  TRF_W2 <- apply((trf.quant-t(sapply(x, function(xx) if (xx >= 0) qexp(qs, 1) else qnorm(qs))))^2,1,mean)
  if (i==1) {
    plot(x,QRF_W2,ylim=c(0,3.5),col="darkgreen",type="l",xlab=expression(X[1]),ylab="2-Wassertein distance",lwd=1.2^2)
  } else {
    lines(x,QRF_W2,col="darkgreen",type="l",lwd=1.2^2)
  }
  
  lines(x,DRF_W2,col="blue",type="l",lwd=1.2^2)
  lines(x,GRF_W2, col="brown",type="l",lwd=1.2^2)
  lines(x,TRF_W2, col="red",type="l",lwd=1.2^2)
}


# conditional mean non-inferiority
load("./../../experiments/univariate_comparison/results_mean_univariate_paper_final.Rdata")

mean(sapply(results.mean$synthetic1, function(l) l$mse.qrf))
mean(sapply(results.mean$synthetic1, function(l) l$mse.drf))

mean(sapply(results.mean$synthetic2, function(l) l$mse.qrf))
mean(sapply(results.mean$synthetic2, function(l) l$mse.drf))

mean(sapply(results.mean$synthetic3, function(l) l$mse.qrf))
mean(sapply(results.mean$synthetic3, function(l) l$mse.drf))


