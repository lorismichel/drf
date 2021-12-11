library(drf)
library(ranger)
library(ggplot2)
library(cowplot)
library(viridis)

set.seed(22)

n = 5000
p = 20

#confounding + heterogeneity
genW = function(X){X[, 2] + rnorm(nrow(X))}
meanY <- function(W, X){
  beta = X[,1]
  expectation = X[, 2] + sin(W)*beta
  return(expectation)
}
genY = function(W, X){
  meanY(W, X) + rnorm(nrow(X), sd=1)
}

X <- matrix(runif(n*p, 0, 5), nrow=n, ncol=p)
W = genW(X)
Y = genY(W, X)


gg1 = qplot(W, Y, col=X[, 1]) + scale_color_viridis() +
  guides(color=guide_colorbar(title=expression(X[1])))+ 
  theme_light() + xlim(c(-3, 7)) + 
  theme(legend.position=c(0.15, 0.85), legend.direction='horizontal', legend.key.size = unit(0.3, 'cm'), legend.background = element_rect(colour = NA, fill = "transparent"))
gg2 = qplot(W, Y, col=X[, 2]) + scale_color_viridis() +
  guides(color=guide_colorbar(title=expression(X[2])))+ 
  theme_light() + xlim(c(-3, 7)) + 
  theme(legend.position=c(0.15, 0.85), legend.direction='horizontal', legend.key.size = unit(0.3, 'cm'), legend.background = element_rect(colour = NA, fill = "transparent"))
gg_left = plot_grid(gg1, gg2, ncol=1)
plot(gg_left)

drf_fit <- drf(Y=cbind(Y,W), X=X, num.trees=2000, min.node.size=20)
ranger_fit <- ranger(y~., data=data.frame(y=Y, w=W, x=X), num.trees=2000, min.node.size = 20)
drf_fit2 <- drf(Y=Y, X=cbind(W, X), num.trees=2000, min.node.size=20)

regress <- function(W, Y, weights, W_test){
  idx = sample(1:length(W), 1000000, prob=as.vector(weights), replace=TRUE)
  fit = smooth.spline(W[idx], Y[idx], cv=TRUE)$fit
  return(predict(fit, x=W_test)$y)
}

conditional_regression = function(x1, x2){
  test_point = matrix(c(x1, x2, rep(2.5, p-2)), 1, p)
  W_test = matrix(seq(min(W), max(W), length.out=100), ncol=1)
  
  test = Reduce(rbind, lapply(W_test, function(tmp) return(test_point)))
  
  W_sample = replicate(1000, genW(test_point))
  Y_sample = sapply(W_sample, function(w){genY(w, test_point)})
  
  ggplot(data.frame(x=W_sample, y=Y_sample), aes(x=x, y=y)) +
    geom_point(shape=16, size=0.5, color='gray')+
    geom_line(color='black', linetype='dashed', size=0.8, 
              data=data.frame(x=W_test, y=sapply(W_test, function(w){meanY(w, test_point)}))) +
    geom_line(aes(x, y), color='blue', size=0.8,
              data=data.frame(x=W_test, y=regress(W, Y, predict(drf_fit, newdata=test_point)$weights, W_test))) +
    geom_line(aes(x, y), color='red', size=0.8,  
              data=data.frame(x=W_test, y=predict(ranger_fit, data=data.frame(w=W_test, x=test))$predictions)) +
    theme_light() + xlim(-1, 6.5) + ylab('Y') + xlab('W')
}

# for(x1 in c(0.1, 0.5, 0.9)){
#   for(x2 in c(0.1, 0.5, 0.9)){
#     plot(conditional_regression(x1, x2))
#   }
#}

gg_middle = plot_grid(
  conditional_regression(1, 4) + annotate(x=0, y=7.8, size=3.8, "text", label=(expression(paste(X[1], " = 1, ", X[2], " = 4")))), 
  conditional_regression(4, 1) + annotate(x=-0, y=7.5, size=3.8, "text", label=(expression(paste(X[1], " = 4, ", X[2], " = 1")))) + ylim(c(-8, 8))
  , ncol=1)
plot(gg_middle)

causal_effect = function(obj, w_do){
  ret = rep(0, length(w_do))
  N = 2000
  samples = sample(1:n, N, replace=TRUE)
  weights = predict(obj, newdata=matrix(X[samples,], ncol=p))$weights
  for(i in 1:length(samples)){
    print(i)
    idx = sample(1:length(W), 200000, prob=as.vector(weights[i,]), replace=TRUE)
    fit = smooth.spline(W[idx], Y[idx], cv=TRUE)$fit
    for(j in 1:length(w_do)){
      ret[j] = ret[j] + predict(fit, x=w_do[j])$y/N
    }
  }
  return(ret)
}

causal_effect2 = function(obj, w_do){
  ret = rep(0, length(w_do))
  samples = sample(1:n, 2000, replace=TRUE)
  for(j in 1:length(w_do)){
    print(j)
    fn = predict(obj, data = data.frame(w=w_do[j], x=X[samples,]))$predictions
    ret[j] = mean(fn)
  }
  return(ret)
}

causal_effect3 = function(obj, w_do){
  ret = rep(0, length(w_do))
  samples = sample(1:n, 1000, replace=TRUE)
  for(j in 1:length(w_do)){
    print(j)
    out = predict(obj, newdata = cbind(w=w_do[j], x=X[samples,]))
    ret[j] = mean(as.matrix(out$weights %*% out$y))
  }
  return(ret)
}


k=200
w_do = seq(-1, 7, length.out=k)
ans = rep(0, k)
for(i in 1:k){
  ans[i] = mean(genY(w_do[i], X))
}
effect = causal_effect(drf_fit, w_do)
effect2 = causal_effect2(ranger_fit, w_do)
#effect3 = causal_effect3(drf_fit2, w_do)

gg_right = ggplot(data.frame(x=w_do, y=ans), aes(x=x, y=y)) +
  geom_line(color='black', linetype='dashed', size=0.8) +
  geom_line(aes(x, y), color='blue', size=0.8, data=data.frame(x=w_do, y=effect)) +
  geom_line(aes(x, y), color='red', size=0.8,  data=data.frame(x=w_do, y=effect2)) +
  theme_light() + xlim(-1, 6.5) + ylab('E[Y | do(W)]') + xlab('W')
plot(gg_right)
gg_right = plot_grid(NULL, gg_right, NULL, ncol=1, rel_heights=c(0.15,0.7,0.15))

plot_grid(gg_left, gg_middle, gg_right, nrow=1)
ggsave("plots/causal_effect_example.png", width=10, height=4)
