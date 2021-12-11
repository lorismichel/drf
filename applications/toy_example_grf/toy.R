library(ggplot2)
library(drf)
library(cowplot)
library(reshape2)

#set.seed(202)
#n = 150
#p = 100
#d = 2

seed = 21
#seed = seed + 1
set.seed(seed)
n = 200
p = 30
d = 2

X = matrix(rnorm(n*p), nrow=n)
Y = matrix(rnorm(n*d, sd=0.2), nrow=n)
Y[,1] = Y[,1] + X[,1]
Y[,2] = Y[,2] + X[,1]


splitcrit_GRF = matrix(0, nrow=p, ncol=n-1)
for(i in 1:p){
  ix = sort(X[,i], index.return=TRUE)$ix
  splitcrit_GRF[i, ] = sapply(1:(n-1), function(xx){
    Y_L = Y[ix[1:xx], , drop=FALSE]
    Y_R = Y[-ix[1:xx], , drop=FALSE]
    n_L = nrow(Y_L)
    n_R = n - n_L
    estimate_L = lm(Y_L[,2] ~ Y_L[,1])$coefficients[1]
    estimate_R = lm(Y_R[,2] ~ Y_R[,1])$coefficients[1]
    return( n_L*n_R/n^2 * (estimate_L - estimate_R)^2 )
    return( n_L*n_R/n^2 * (cor(Y_L)[1,2] - cor(Y_R)[1,2])^2 )
  })
}

#splitcrit_GRF = splitcrit_GRF/mean(splitcrit_GRF[2:p,which])
which = floor(0.1*n):floor(0.9*n)
gg3 = ggplot(melt(data.frame(x=which, y=t(splitcrit_GRF[2:p, which])), id='x'), aes(x=x, y=value))+
  geom_line(aes(color=variable))+
  geom_line(data = data.frame(x=which, value=splitcrit_GRF[1, which]))+
  xlab('splitting position (% of data points)') + ylab('splitting criterion') +
  ggtitle('GRF-type splitting criterion')+
  scale_x_continuous(labels = function(x){paste0(round(100*x/n), '%')})+
  theme_light()+
  theme(legend.position = "none") 
gg3



gg1 = qplot(Y[,1], Y[,2], color= ifelse(X[,1]>0, 'Yes', 'No')) + 
  geom_point(alpha=0.4) + 
  geom_smooth(method='lm', alpha=1) +
  labs(x=expression(Y["1"]), y=expression(Y["2"])) +
  guides(color=guide_legend(title = expression(X["1"] > 0))) +
  theme(legend.text=element_text(size=6))+
  xlim(c(-2.5, 2.5))+
  theme_light() #+ theme(legend.position = 'left')
gg1

#n = 100
#X = X[1:n,]
#Y = Y[1:n,]


tmp = rep(0, n^2)
for(i in 1:n){
  for(j in 1:n){
    tmp[n*(i-1) + j] = norm(Y[i, ,drop=FALSE] - Y[j, ,drop=FALSE], type='2')
  }
}
sigma = median(tmp)

K = matrix(0, nrow=n, ncol=n)
for(i in 1:n){
  for(j in 1:n){
    K[i, j] =  dnorm(norm(Y[i, ,drop=FALSE] - Y[j, ,drop=FALSE], type='2'), sd=sigma)
  }
}

splitcrit_MMD = matrix(0, nrow=p, ncol=(n-1))
for(i in 1:p){
  ix = sort(X[,i], index.return=TRUE)$ix

  left = 0
  cross = 0
  right = sum(K)
  
  for(j in 1:(n-1)){
    for(k in (j+1):n){
      right = right - 2*K[ix[j], ix[k]]
      cross = cross + K[ix[j], ix[k]]
    }
    for(k in 1:max(1, j-1)){
      cross = cross - K[ix[j], ix[k]] 
      left = left + 2*K[ix[j], ix[k]]
    }
    right = right - K[ix[j], ix[j]]
    left = left + K[ix[j], ix[j]]
    
    splitcrit_MMD[i, j] = j*(n-j)/n^2 * (left / j^2 - 2*cross/(j*(n-j)) + right / (n-j)^2)
  }
}

which = floor(0.1*n):floor(0.9*n)
#splitcrit_MMD = splitcrit_MMD/mean(splitcrit_MMD[2:p,which])
gg2 = ggplot(melt(data.frame(x=which, y=t(splitcrit_MMD[2:p, which])), id='x'), aes(x=x, y=value))+
  geom_line(aes(color=variable))+
  geom_line(data = data.frame(x=which, value=splitcrit_MMD[1, which]))+
  xlab('splitting position (% of data points)') + ylab('splitting criterion') +
  ggtitle('MMD splitting criterion') + 
  scale_x_continuous(labels = function(x){paste0(round(100*x/n), '%')}) + 
  theme_light()+
  theme(legend.position = "none")
gg2


n_test = 300
X_test = matrix(rnorm(n_test*p), nrow=n_test)
drf_fit = drf(X, Y, num.trees = 2000, mtry=10)
weights = predict(drf_fit, X_test)$weights
drf_estimates = apply(weights, 1, function(w){
  lm(Y[,2]~Y[,1], weights = w)$coefficients[1]
})
 
library(grf)
grf_fit = causal_forest(X, Y[,2], Y[,1], num.trees=2000, mtry=10)
grf_estimates =  predict(grf_fit, X_test)$predictions

plot_df = data.frame(x=X_test[,1], y=drf_estimates, method='DRF')
plot_df = rbind(plot_df, data.frame(x=X_test[,1], y=grf_estimates, method='GRF'))
gg4 = ggplot(plot_df, aes(x=x, y=y)) + 
  geom_point(aes(color=method)) +
  geom_hline(yintercept = 0, linetype='dashed', size=1) +
  labs(x=expression(X["1"]), y='estimated coefficient') +
  theme_light() + theme(legend.position = 'left')
gg4

gg = plot_grid(gg1, gg4, gg2, gg3, nrow=2, ncol=2)
gg
ggsave('~/Documents/projects/DRF/plots/toy\ example/toy_example2.png', gg, width=8, height=5)
