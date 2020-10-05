library(drf)
library(ggplot2)

# dimensions
p <- 10
n <- 2000

# CONSTRUCTION TRAINING DATA
# predictors
X  <- matrix(runif(n*p, min = -1, max = 1),ncol = p)
# responses
Y <- t(apply(X, 1, function(xx) {
  
    # copula
    normCop <- normalCopula(param=c(xx[1]), dim = 2)
    
    # margins
    mdvNorm <- mvdc(copula=normCop, margins=c("norm", "norm"),
                    paramMargins=list(list(mean = 0, sd = 1),
                                      list(mean = 0, sd = 1)))
    # gen
    rMvdc(n = 1, mvdc = mdvNorm)
}))

#TRAINING METHODS
dRF <- drf(X = X, Y = Y, splitting.rule = "FourierMMD", num_features=50)
dRF2 <- drf(X = X, Y = Y, splitting.rule = "FourierMMD", num_features=5)
giniRF <- drf(X = X, Y = Y, splitting.rule = "CART")

#############################################
#VISUAL INSPECTION
#############################################
num_test = 1
Xtest = matrix(runif(num_test*p, 0, 0), ncol=p)
Xtest[, 1] <- 0.9

weights_mrf = predict(mRF, newdata=Xtest)$weights
weights_mrf2 = predict(mRF2, newdata=Xtest)$weights
weights_gini = predict(giniRF, newdata=Xtest)$weights

library(ggplot2)
ggplot(data.frame(y1=Y[,1], y2=Y[,2], weights=as.vector(weights_mrf)), aes(x=y1, y=y2))+
  geom_point(aes(size=ifelse(weights==0, NA, weights))) + scale_size_area(max_size=2)

ggplot(data.frame(y1=Y[,1], y2=Y[,2], weights=as.vector(weights_mrf2)), aes(x=y1, y=y2))+
  geom_point(aes(size=ifelse(weights==0, NA, weights))) + scale_size_area(max_size=2)

ggplot(data.frame(y1=Y[,1], y2=Y[,2], weights=as.vector(weights_gini)), aes(x=y1, y=y2))+
  geom_point(aes(size=ifelse(weights==0, NA, weights))) + scale_size_area(max_size=2) 

#############################################
#CORRELATIONS
#############################################
predict(mRF, newdata=Xtest, type='cor')
predict(mRF2, newdata=Xtest, type='cor')
predict(giniRF, newdata=Xtest, type='cor')

#############################################
#QUANTILES OF RANDOM PROJECTION
#############################################
num_test = 21
Xtest = matrix(runif(num_test*p, -1, 1), ncol=p)
Xtest[,1] <- seq(-1, 1, length.out=num_test)
Ytest <- t(apply(X, 1, function(xx) {
  normCop <- normalCopula(param=c(xx[1]), dim = 2)
  mdvNorm <- mvdc(copula=normCop, margins=c("norm", "norm"),
                  paramMargins=list(list(mean = 0, sd = 1),
                                    list(mean = 0, sd = 1)))
  rMvdc(n = 1, mvdc = mdvNorm)
}))

alpha_seq = c(.025, .05, .3, .5, .7, .95, .975) 
nb_random_directions = 1

w <- rnorm(2)
w = c(1,-1)
w = w/sum(w^2)^0.5


yhat_mrf <- predict(mRF, newdata = Xtest, type = "functional", 
                    quantiles = alpha_seq, f = function(y) sum(w * y))$functional
yhat_mrf2 <- predict(mRF2, newdata = Xtest, type = "functional", 
                    quantiles = alpha_seq, f = function(y) sum(w * y))$functional
yhat_gini <- predict(giniRF, newdata = Xtest, type = "functional", 
                     quantiles = alpha_seq, f = function(y) sum(w * y))$functional
truth  = yhat_mrf
for(i in 1:num_test){
  for(j in 1:length(alpha_seq)){
    truth[i, j] = qnorm(alpha_seq[j], mean=0, sd=(w[1]^2+w[2]^2+2*Xtest[i, 1]*w[1]*w[2])^0.5)
  }
}
round(yhat_mrf, 2)
round(yhat_mrf2, 2)
round(yhat_gini, 2)
round(truth, 2)


alpha_seq=runif(5000)
yhat_mrf <- predict(mRF, newdata = Xtest, type = "functional", 
                    quantiles = alpha_seq, f = function(y) sum(w * y))$functional
yhat_mrf2 <- predict(mRF2, newdata = Xtest, type = "functional", 
                     quantiles = alpha_seq, f = function(y) sum(w * y))$functional
yhat_gini <- predict(giniRF, newdata = Xtest, type = "functional", 
                     quantiles = alpha_seq, f = function(y) sum(w * y))$functional
par(mfrow=c(3,1))
plot(density(yhat_mrf[10,]), xlim=c(-3,3))
plot(density(yhat_mrf2[10,]), xlim=c(-3,3))
plot(density(yhat_gini[10,]), xlim=c(-3,3))

#############################################
# PINBALL LOSS RANDOM PROJECTION
#############################################
qLoss <- function(y,yhat,alpha) {
  return(mean(alpha*pmax(y-yhat,0)+(1-alpha)*pmax(yhat-y,0)))
}
mrf_loss <- matrix(0, nrow=num_test, ncol=length(alpha_seq))
mrf2_loss <- matrix(0, nrow=num_test, ncol=length(alpha_seq))
gini_loss <- matrix(0,nrow=num_test, ncol=length(alpha_seq))

test_value = seq(-1, 1, length.out=num_test)
for(i in 1:num_test){
  for(j in 1:length(alpha_seq)){
    num_testpoint = 100
    Xtest = matrix(0, nrow=num_testpoint, ncol=p)
    Xtest[,1]= test_value[i]
    Ytest = t(apply(Xtest, 1, function(xx) {
      normCop <- normalCopula(param=c(xx[1]), dim = 2)
      mdvNorm <- mvdc(copula=normCop, margins=c("norm", "norm"),
                      paramMargins=list(list(mean = 0, sd = 1),
                                        list(mean = 0, sd = 1)))
      rMvdc(n = 1, mvdc = mdvNorm)
    })) 
    
    yhat_mrf <- predict(mRF, newdata = Xtest, type = "functional", quantiles = alpha_seq, 
                        f = function(y){sum(w*y)})$functional
    mrf_loss[i,j] <- qLoss(y = Ytest %*% w, yhat = yhat_mrf[,j], alpha = alpha_seq[j])
    
    yhat_mrf2 <- predict(mRF2, newdata = Xtest, type = "functional", quantiles = alpha_seq, f = function(y){sum(w[[1]]*y)})$functional
    mrf2_loss[i,j] <- qLoss(y = Ytest %*% w, yhat = yhat_mrf2[,j], alpha = alpha_seq[j])
    
    yhat_gini <- predict(giniRF, newdata = Xtest, type = "functional", quantiles = alpha_seq, f = function(y){sum(w[[1]]*y)})$functional
    gini_loss[i,j] <- qLoss(y = Ytest %*% w, yhat = yhat_gini[,j], alpha = alpha_seq[j])
  }
}

round(mrf_loss*100,2)
round(mrf2_loss*100,2)
round(gini_loss*100,2)
sum(mrf_loss)
sum(mrf2_loss)
sum(gini_loss)
#############################################
# CDF
#############################################
num_eval = 10
evaluation_points = expand.grid(seq(2, -2, length.out = num_eval), seq(-2, 2, length.out = num_eval))

num_test = 1
Xtest = matrix(runif(num_test*p, 0, 0), ncol=p)
Xtest[, 1] <- -0.9

weights_mrf = predict(mRF, newdata=Xtest)$weights
weights_gini = predict(giniRF, newdata=Xtest)$weights

cdf_mrf = apply(evaluation_points, 1, function(u){
    return(sum(weights_mrf*(Y[,1] < u[1] & Y[,2] < u[2])))  
})
cdf_mrf = matrix(round(cdf_mrf, 3), nrow=num_eval)

cdf_gini = apply(evaluation_points, 1, function(u){
  return(sum(weights_gini*(Y[,1] < u[1] & Y[,2] < u[2])))  
})
cdf_gini = matrix(round(cdf_gini, 3), nrow=num_eval)

library(mvtnorm)
truth = apply(evaluation_points, 1, function(u){
  return(pmvnorm(upper=c(u[1], u[2]), sigma=matrix(c(1,Xtest[,1], Xtest[,1],1), nrow=2))[1])  
})
truth = matrix(round(truth, 3), nrow=num_eval)

cdf_mrf
cdf_gini
truth

#heatmap(cdf_mrf)
#heatmap(cdf_gini)
#heatmap(truth)

#############################################
# CDF LOSS
#############################################
num_testpoint = 10000
Xtest = matrix(0, nrow=num_testpoint, ncol=p)
Xtest[,1]=-0.9
Ytest = t(apply(Xtest, 1, function(xx) {
  # copula
  normCop <- normalCopula(param=c(xx[1]), dim = 2)
  
  # margins
  mdvNorm <- mvdc(copula=normCop, margins=c("norm", "norm"),
                  paramMargins=list(list(mean = 0, sd = 1),
                                    list(mean = 0, sd = 1)))
  # gen
  rMvdc(n = 1, mvdc = mdvNorm)
}))

cdf_loss = function(phat, Y, u){
  ret = c()
  for(i in 1:nrow(Y)){
    inside = prod(Y[i,] < u)
    ret = c(ret, -inside*log(1e-5 + phat) + -(1-inside)*log(1e-5 + 1 - phat))
  }
  return(mean(ret))
}

loss_mrf = apply(evaluation_points, 1, function(u){
  phat = sum(weights_mrf*(Y[,1] < u[1] & Y[,2] < u[2]))
  return(cdf_loss(phat, Ytest, u))
})
loss_mrf = matrix(round(loss_mrf, 3), nrow=num_eval)

loss_gini = apply(evaluation_points, 1, function(u){
  phat = sum(weights_gini*(Y[,1] < u[1] & Y[,2] < u[2]))
  return(cdf_loss(phat, Ytest, u))
})
loss_gini = matrix(round(loss_gini, 3), nrow=num_eval)

loss_mrf
loss_gini

sum(loss_mrf)
sum(loss_gini)
loss_mrf/loss_gini

#############################################
#DENSITY ESTIMATE
#############################################
scaling = apply(Y, 2, sd)

kernel = function(y1, y2, sd=1){
  ret = 0
  for(k in length(y1)){
    ret = ret + dnorm((y1[k]-y2[k])/scaling[k], sd=sd)
  }
  return(ret)
}

density_estimate = function(y, weights){
  return( sum(weights*apply(Y,2,function(yy){kernel(yy, y)}) ))
}
