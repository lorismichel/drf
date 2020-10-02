slope_DRF = get_coef_DRF(DRF_fit, newdata=as.matrix(X_test), 2)
n = 10000
p = 10

genX = function(){
    runif(p, 0, 1)
}
genW = function(x){
    prob = (1+dbeta(x[3], 2, 4))/4
    return(rnorm(1, mean=prob, sd=(prob*(1-prob))^0.5))
}
genY = function(combined){
  w = combined[1]
  x = combined[2:(p+1)]
  beta = (1+1/(1+exp(-20*(x[1] - 1/3)))) * (1+1/(1+exp(-20*(x[2] - 1/3))))
  return((2*x[3]  - 1) + (w - 0.5)*beta + rnorm(1))
}

X = t(replicate(n, genX()))
colnames(X) = paste0('V', as.character(1:p))

W = apply(X, 1, genW)
Y = apply(cbind(W, X), 1, genY)

########
X_test = t(replicate(2000, genX()))
#X_test[,3] = seq(0,1, length.out = 100)
colnames(X_test) = colnames(X)
X_test = data.frame(X_test)

beta = (1+1/(1+exp(-20*(X_test[,1] - 1/3)))) * (1+1/(1+exp(-20*(X_test[,2] - 1/3))))
truth_slope = beta
truth_intercept = (2*X_test[,3]  - 1) - 0.5 * beta

get_coef_DRF = function(fit, newdata, which_coef){
  out = predict(fit, newdata)
  ret = c()
  for(i in 1:nrow(newdata)){
    ret = c(ret, lm(Y~W, weights=out$weights[i,])$coefficients[which_coef])
  }
  return(ret)
}

get_coef_CF = function(fit, newdata, which_coef){
  out = get_sample_weights(fit, newdata)
  ret = c()
  for(i in 1:nrow(newdata)){
    ret = c(ret, lm(Y~W, weights=out[i,])$coefficients[which_coef])
  }
  return(ret)
}


Wtilde = W - regression_forest(X=X, Y=W)$predictions
Ytilde = Y - regression_forest(X=X, Y=Y)$predictions
DRF_fit = drf(X=X, Y=cbind(Ytilde, Wtilde))
intercept_DRF_centered = get_coef_DRF(DRF_fit, newdata=as.matrix(X_test), 1)
slope_DRF_centered = get_coef_DRF(DRF_fit, newdata=as.matrix(X_test), 2)
qplot(truth_intercept, intercept_DRF_centered) + geom_abline(slope=1, linetype='dashed', color='red')
qplot(truth_slope, slope_DRF_centered) + geom_abline(slope=1, linetype='dashed', color='red')

DRF_fit = drf(X=X, Y=cbind(Y, W))
intercept_DRF = get_coef_DRF(DRF_fit, newdata=as.matrix(X_test), 1)
slope_DRF = get_coef_DRF(DRF_fit, newdata=as.matrix(X_test), 2)
qplot(truth_intercept, intercept_DRF) + geom_abline(slope=1, linetype='dashed', color='red')
qplot(truth_slope, slope_DRF) + geom_abline(slope=1, linetype='dashed', color='red')

CF_fit = causal_forest(X, Y, W)
intercept_CF = get_coef_CF(CF_fit, newdata=as.matrix(X_test), 1)
slope_CF = get_coef_CF(CF_fit, newdata=as.matrix(X_test), 2)
qplot(truth_intercept, intercept_CF) + geom_abline(slope=1, linetype='dashed', color='red')
qplot(truth_slope, slope_CF) + geom_abline(slope=1, linetype='dashed', color='red')
