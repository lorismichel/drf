# creating the summaries
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


# wasserstein distance synthetic 1
load("./../../experiments/univariate_comparison/results_univariate_paper_synthetic1.Rdata")
x <- c(- 50:1 / 51, 1:50 / 51)
qs <- setdiff(seq(0,1,length.out = 20),c(0,1))
sapply(x, function(xx) qnorm(qs, mean = 0.8*(xx >  0)))

QRF_W2 <- apply((results$synthetic1[[1]]$qQRF$predictions[,-c(1:5)]-t(sapply(x, function(xx) qnorm(qs, mean = 0.8*(xx >  0)))))^2,1,mean)
DRF_W2 <- apply((results$synthetic1[[1]]$qDRF[,,-c(1:5)]-t(sapply(x, function(xx) qnorm(qs, mean = 0.8*(xx >  0)))))^2,1,mean)
GRF_W2 <- apply((results$synthetic1[[1]]$qGRF-t(sapply(x, function(xx) qnorm(qs, mean = 0.8*(xx >  0))))),1,mean)
plot(QRF_W2,ylim=c(0,2),col="darkgreen",type="l")
lines(DRF_W2,col="blue",type="l")
lines(GRF_W2, col="pink",type="l")
