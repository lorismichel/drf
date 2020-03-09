require(Barycenter)
n <- seq(0,1,length.out = dim(eight[[1]])[2])
costm <- as.matrix(dist(expand.grid(n,rev(n)), diag=TRUE, upper=TRUE))
a <- matrix(eight[[1]],28*28,1)
b <- matrix(c(eight[[2]],eight[[3]],eight[[4]],eight[[5]]),28*28,4)
#Sinkhorn(a, b, costm)

#Sinkhorn Distances between the first image to the second image in the dataset eight.
#We creat costm simply using a distance matrix on the grid [0,1]x[0,1].
U <- cbind(-1+rnorm(50),-1+rnorm(50))
Y <- cbind(2+rnorm(1000),2+rnorm(1000))

costm <- as.matrix(dist(rbind(U,Y)))[1:nrow(U),-c(1:nrow(U))]
r <- matrix(rep(1/nrow(U),nrow(U)),ncol=1)
c <- matrix(rep(1/nrow(Y),nrow(Y)),nrow=1)
require(transport)
ot <- transport(a = r, b = c, costm = costm)

p <- Greenkhorn(r, c, costm, lambda = 1)$Transportplan


Yhat <- Y[sapply(1:nrow(U), function(i) sample(1:nrow(Y), 1, prob = p[i,])),]

plot(U,pch=19,cex=0.5,xlim=c(-4,4),ylim=c(-4,4))
points(Y,pch=19,cex=0.5,col="red")
for (i in 1:nrow(U)) {
  segments(x0 = U[i,1], y0 = U[i,2], x1 = Yhat[i,1], y1 = Yhat[i,2])
}

# for (i in 1:nrow(U)) {
#   segments(x0 = U[ot$from[i],1], y0 = U[ot$from[i],2], x1 = Yhat[ot$to[i],1], y1 = Yhat[ot$to[i],2],col="blue")
# }
