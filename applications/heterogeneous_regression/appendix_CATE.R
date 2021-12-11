library(ggplot2)
library(grf)
library(causalToolbox)
library(locfit)
library(drf)

#SC0 - W binary, CATE
#SC1 - W binary, confounding + linear, grf example 
#SC2 - W binary, heterogeneous + linear, grf example
#SC3 - W binary, heterogeneous + confounding + linear, grf example
params = expand.grid(n=c(800, 1600), p=c(10, 20), SC=0:3)
N_train = 100
N_test = 1000


for(sim in 1:nrow(params)){

SC = params$SC[sim]
n = params$n[sim]
p = params$p[sim]
print(c(n,p,SC))

params$mse_DRF[sim] = 0
params$mse_DRF_local[sim] = 0
params$mse_CF[sim] = 0
params$mse_XL[sim] = 0

params$ate_DRF[sim] = 0
params$ate_DRF_local[sim] = 0
params$ate_CF[sim] = 0
params$ate_XL[sim] = 0
params$ate_DML[sim] = 0

for(iter in 1:N_train){
  print(iter)

genX = function(){
  if(TRUE){
    runif(p, 0, 1)
  }
}
genW = function(x){
  if(SC==0){
    return(rbinom(1, size=1, prob=expit(4*(x[2]-0.5))))
  }
  if(SC==1 || SC==3){
    prob = (1+dbeta(x[3], 2, 4))/4
    return(rnorm(1, mean=prob, sd=(prob*(1-prob))^0.5))
    #return(rbinom(1, size=1, prob=(1+dbeta(x[3], 2, 4))/4))
  }
  if(SC==2){
    #return(rnorm(1, mean=0.5))
    return(rbinom(1, size=1, prob=0.5))
  }
}
genY = function(combined){
  w = combined[1]
  x = combined[2:(p+1)]
  if(SC==0){
    return(10*x[2]^2 + sin(5*x[1])*w + rnorm(1, sd=1))
  }
  if(SC==1){
    return((2*x[3]  - 1) + rnorm(1))
  }
  if(SC==2){
    beta = (1+1/(1+exp(-20*(x[1] - 1/3)))) * (1+1/(1+exp(-20*(x[2] - 1/3))))
    return((w - 0.5)*beta + rnorm(1))
  }
  if(SC == 3){
    beta = (1+1/(1+exp(-20*(x[1] - 1/3)))) * (1+1/(1+exp(-20*(x[2] - 1/3))))
    return((2*x[3]  - 1) + (w - 0.5)*beta + rnorm(1))
  }
}

X = t(replicate(n, genX()))
colnames(X) = paste0('V', as.character(1:p))

W = apply(X, 1, genW)
Y = apply(cbind(W, X), 1, genY)

########
X_test = t(replicate(N_test, genX()))
#X_test[,3] = seq(0,1, length.out = 100)
colnames(X_test) = colnames(X)
X_test = data.frame(X_test)

if(SC==2 || SC==3){
  truth_CATE = (1+1/(1+exp(-20*(X_test[,1] - 1/3)))) * (1+1/(1+exp(-20*(X_test[,2] - 1/3))))
  truth_ATE = mean((1+1/(1+exp(-20*(runif(1000000) - 1/3)))) * (1+1/(1+exp(-20*(runif(1000000) - 1/3)))))
} else if(SC==0){
  truth_CATE = sin(5*X_test[,1])
  truth_ATE = mean(sin(5*runif(1000000)))
} else {
  truth_CATE = rep(0, nrow(X_test))
  truth_ATE = 0
}

#############
DML_RF = function(X, Y, W){
  require(ranger)

  Wfit = ranger(w~., data=cbind(as.data.frame(X), data.frame(w=W)))
  Wtilde = W - Wfit$predictions

  Yfit = ranger(y~., data=cbind(as.data.frame(X), data.frame(y=Y)))
  Ytilde = Y -  Yfit$predictions

  lmfit = lm(Ytilde~Wtilde+0)

  return(list(Wfit = Wfit, Yfit = Yfit, theta = lmfit$coefficients[1]))
}
potential_outcomes_DML_RF = function(fit, newdata, w){
  return(predict(fit$Yfit, data=newdata)$predictions - fit$theta*predict(fit$Wfit, data=newdata)$predictions + w*fit$theta)
}


DML_fit = DML_RF(X, Y, W) #computes ATE (assumes CATE constant)
params$ate_DML[sim] = params$ate_DML[sim] + (DML_fit$theta - truth_ATE)^2 / N_train
#potential_outcomes_DML_RF(DML_fit, X_test, 1)

############
CF_fit = causal_forest(X, Y, W)
CATE_CF = predict(CF_fit, X_test)[,1] #computes CATE
qplot(truth_CATE, CATE_CF) + geom_abline(slope=1, linetype='dashed', color='red') #truth
qplot(X_test[,1], CATE_CF) + geom_line(aes(x=X_test[,1], y=truth_CATE), linetype='dashed', color='red') #truth
qplot(X_test[,1], CATE_CF-truth_CATE)
params$mse_CF[sim] = params$mse_CF[sim] + mean((CATE_CF-truth_CATE)**2) / N_train
params$ate_CF[sim] = params$ate_CF[sim] + (mean(CATE_CF) - truth_ATE)^2 / N_train

############
XL_fit = X_RF(X, as.vector(W), as.vector(Y))
CATE_XL = EstimateCate(XL_fit, X_test)
qplot(truth_CATE, CATE_XL) + geom_abline(slope=1, linetype='dashed', color='red') #truth
qplot(X_test[,1], CATE_XL) + geom_line(aes(x=X_test[,1], y=truth_CATE), linetype='dashed', color='red') #truth
qplot(X_test[,1], CATE_XL-truth_CATE)
params$mse_XL[sim] = params$mse_XL[sim] + mean((CATE_XL-truth_CATE)**2) / N_train
params$ate_XL[sim] = params$ate_XL[sim] + (mean(CATE_XL) - truth_ATE)^2 / N_train

##########
get_CATE_DRF = function(fit, newdata){
  out = predict(fit, newdata)
  ret = c()
  for(i in 1:nrow(newdata)){
    ret = c(ret, lm(out$y[,1]~out$y[,2], weights=out$weights[i,])$coefficients[2])
  }
  return(ret)
}

# DRF_fit = drf(X=X, Y=cbind(Y,W))
# CATE_DRF = get_CATE_DRF(DRF_fit, as.matrix(X_test))
#qplot(truth_CATE, CATE_DRF) + geom_abline(slope=1, linetype='dashed', color='red') #truth
#qplot(X_test[,1], CATE_DRF) + geom_line(aes(x=X_test[,1], y=truth_CATE), linetype='dashed', color='red') #truth
# params$mse_DRF[sim] = params$mse_DRF[sim] + mean((CATE_DRF-truth_CATE)**2) / N_train
# params$ate_DRF[sim] = params$ate_DRF[sim] + (mean(CATE_DRF) - truth_ATE)^2 / N_train

#DRF with local centering
Wtilde = W - regression_forest(X=X, Y=W)$predictions
Ytilde = Y - regression_forest(X=X, Y=Y)$predictions

DRF_fit = drf(X=X, Y=cbind(Ytilde, Wtilde))
CATE_DRF = get_CATE_DRF(DRF_fit, as.matrix(X_test))
qplot(truth_CATE, CATE_DRF) + geom_abline(slope=1, linetype='dashed', color='red') #truth
qplot(X_test[,1], CATE_DRF) + geom_line(aes(x=X_test[,1], y=truth_CATE), linetype='dashed', color='red') #truth
qplot(X_test[,1], CATE_DRF-truth_CATE)
params$mse_DRF_local[sim] = params$mse_DRF_local[sim] + mean((CATE_DRF-truth_CATE)**2) / N_train
params$ate_DRF_local[sim] = params$ate_DRF_local[sim] + (mean(CATE_DRF) - truth_ATE)^2 / N_train

# point = matrix(c(0.2, 0.8, 0.2, runif(p-3)), 1, p)
# #point = matrix(X[1,], 1, p)
# weights = predict(DRF_fit, newdata=as.matrix(point))$weights[1,]
# plot(X[,1], X[,3], cex=200*weights, pch=19, asp=1)
# points(point[,1], point[,3], col='red', pch=18, cex=3)
# lm(Y~W, weights=weights)
# lm(Ytilde~Wtilde, weights=weights)
# (1+1/(1+exp(-20*(point[,1] - 1/3)))) * (1+1/(1+exp(-20*(point[,2] - 1/3))))
}
}
save(params, file="./data/CATE.Rdata")

