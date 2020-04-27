library(ggplot2)
library(mrf)
library(GGally)
library(fastDummies)

######################################################################
# load dataset
#####################################################################

load("~/Documents/projects/heterogeneity/birth_data/births.dat")
births = na.omit(births)
births = births[births$race_mother %in% c('white', 'black', 'asian'), ]
births = births[births$race_father %in% c('white', 'black', 'asian'), ]
#births = births[births$plurality==1]
attach(births)

###########################################################################
# exploratory analysis & visualization
##########################################################################
births = births[sample(1:nrow(births), 20000),]

#ggpairs(births)

qplot(birthweight, geom='density', fill=race_mother, alpha=0.3)
qplot(birthweight, geom='density', fill=(plurality==1), alpha=0.3)

qplot(birthweight, weight_mother)
qplot(weight_gain_mother, birthweight, geom='jitter', size=I(0.1), color=pregnancy_duration>37)+
  #  scale_color_viridis_c(option='magma')+
  geom_smooth()

qplot(weight_gain_mother[pregnancy_duration<37], birthweight[pregnancy_duration<37],  geom='jitter')
qplot(weight_gain_mother[pregnancy_duration>37], birthweight[pregnancy_duration>37],  geom='jitter')

qplot(age_father, binwidth=0.5)
qplot(age_mother, binwidth=0.5)
qplot(age_father, birthweight, size=I(0.1), geom='jitter')
qplot(age_mother, birthweight, size=I(0.1), geom='jitter')
qplot(age_father, age_mother, size=I(0.1), geom='jitter')

qplot(birth_interval, birthweight, geom=c('jitter', 'smooth'))
qplot(birth_order, birthweight, geom=c('jitter', 'smooth')) + geom_smooth(k=5)

qplot(birthweight, apgar_5min, geom='jitter', color=pregnancy_duration)+
  scale_color_viridis_c(option='magma')

qplot(birthweight, apgar_5min, geom=c('jitter', 'smooth'), facets=.~pregnancy_duration>36, data=births)
qplot(education_father, education_mother, geom='jitter')
qplot(education_mother, birthweight, geom='jitter')

precare_started[precare_started==100] = 0
qplot(precare_started, birthweight, geom='jitter', color=apgar_5min, size=I(0.1))
qplot(precare_started, apgar_5min, geom='jitter')
#qplot(birthweight, apgar_5min, geom='jitter', color=pregnancy_duration)

qplot(pregnancy_duration, birthweight, geom=c('jitter'), facets=.~plurality==1, data=births)+
  #geom_quantile(quantiles=0.1, method = "rqss")
  geom_smooth()#method='loess')

qplot(pregnancy_duration, birthweight, geom=c('jitter', 'smooth'), size=I(0.1), color=race_mother, data=births)
qplot(pregnancy_duration, birthweight, geom=c('jitter', 'smooth'), facets=.~precare_started!=Inf, data=births)
qplot(pregnancy_duration, birthweight, geom=c('jitter'), facets=.~delivery_method, data=births)+
  geom_smooth()
qplot(pregnancy_duration, birthweight, geom=c('jitter', 'smooth'), facets=race_father~race_mother, data=births)

qplot(age_mother, as.numeric(abnormal_conditions=='yes'), geom=c('jitter', 'smooth'))

###########################################################################
# fit mrf
###########################################################################

X = births[,c(
  'race_mother',
  'age_mother',
  'education_mother',
  'marital_status_mother',
  'height_mother',
  'BMI_mother',
  'race_father',
  'age_father',
  'education_father',
  'plurality',
  'birth_order',
  'delivery_method',
  'gender',
  'cigarettes_during_pregnancy'
)]
X$precare = births$precare!=Inf
X$race_father = factor(X$race_father)
X$race_mother = factor(X$race_mother)
X=dummy_cols(X, remove_selected_columns=TRUE)
X = as.matrix(X)

Y = births[, c('pregnancy_duration', 'birthweight')]#, 'apgar_5min'
Y = as.matrix(Y)

point_description = function(test_point){
  out = "Mother - age:"
  out = paste(out, test_point$age_mother[1], sep='')
  out = paste(out, ', race:', test_point$race_mother[1], sep='')
  out = paste(out, ', education:', c('<9 grades', '<12 grades', 'high school', 'associate degree', 'bachelor\'s degree', 'master\'s degree', 'doctorate', 'unknown')[test_point$education_mother[1]], sep='')
  out = paste(out, ', height:', test_point$height_mother[1], "in", sep='')
  out = paste(out, ', BMI:', test_point$BMI_mother[1], sep='')
  out = paste(out, ', smoking:', c('no', 'yes')[1+test_point$cigarettes_during_pregnancy[1]])
  
  out = paste(out, '\nFather - ', sep='')
  
  out = paste(out, 'age:', test_point$age_father[1], sep='')
  out = paste(out, ', race:', test_point$race_father[1], sep='')
  out = paste(out, ', education:', c('<9 grades', '<12 grades', 'high school', 'associate degree', 'bachelor\'s degree', 'master\'s degree', 'doctorate', 'unknown')[test_point$education_father[1]], sep='')
  
  out = paste(out, '\nBirth - ', sep='')
  
  out = paste(out, 'multiplicity:', test_point$plurality[1], sep='')
  out = paste(out, ', gender:', test_point$gender[1], sep='')
  out = paste(out, ', birth order:', test_point$birth_order[1], sep='')
  out = paste(out, ', method:', test_point$delivery_method[1], sep='')
  out = paste(out, ', prenatal care:', c('no', 'yes')[test_point$precare[1]!=Inf])
  
  return(out)
}

set.seed(22)
train_idx = sample(1:nrow(births), 100000, replace=FALSE)
mrf_fit = mrf(X=X[train_idx,], Y=Y[train_idx,], min.node.size = 20, splitting.rule='fourier', num_features=10)

########################################################################
# check heterogeneous regression
########################################################################
compute = function(x, y, weights, x_values){ #transform, fit smoother for mean, fit linear functions to residuals for quantiles
  idx = weights!=0
  print(sum(idx))
  x = x[idx]
  y = y[idx]
  weights = weights[idx]
  
  weights = weights * (1 - x/55)^3 #upweight leftmost points
  weights = weights * (1 + (x>40.5)*3)
  
  f_x = function(x){return(log(log(x)))}
  f_inv_x = function(x){return(exp(exp(x)))}
  f_y = f_x
  f_inv_y = f_inv_x
  
  xt = f_x(x)
  yt = f_y(y)
  
  fit = smooth.spline(yt~xt, w=weights, df=5)
  residuals = yt - predict(fit, x=data.frame(x=xt))$y$x
  
  library(quantreg)
  fit_quantile_lo = rq(residuals~xt, weights=weights, tau=0.1)
  fit_quantile_hi = rq(residuals~xt, weights=weights, tau=0.9)
  
  fitted = predict(fit, x=data.frame(x=f_x(x_values)))$y$x
  mean = f_inv_y(fitted)
  quantile_lo = f_inv_y(fitted + predict(fit_quantile_lo, newdata=data.frame(xt=f_x(x_values))))
  quantile_hi = f_inv_y(fitted + predict(fit_quantile_hi, newdata=data.frame(xt=f_x(x_values))))
  
  return(rbind(mean, quantile_lo, quantile_hi))
}

plotdf = births[train_idx, ]
for(i in sample((1:nrow(X))[-train_idx], 15)){
  #if(X[i, 7]!=2){#plurality
  # next
  #}
  test_point = X[i,]
  weights = predict(mrf_fit, newdata=matrix(test_point, nrow=1))$weights[1,]
  
  print(births[i,])
  plotdf$mrf_weights = weights
  
  x_val = seq(22, 42, length.out=200)
  out = compute(Y[train_idx,1], Y[train_idx,2], weights=weights, x_val)
  
  gg=ggplot(plotdf[plotdf$mrf_weights!=0,], aes(x=pregnancy_duration, y=birthweight))+
    geom_jitter(aes(size=ifelse(mrf_weights==0, NA, mrf_weights^0.5), color=race_mother, shape=gender), alpha=0.8)+
    scale_size_area(max_size=3) + guides(size=FALSE, alpha=FALSE)+labs(color='race mother', shape='gender baby')+
    #scale_color_viridis_c(option='magma') +
    ggtitle(point_description(births[i,])) +
    geom_line(data=data.frame(x=x_val, y=as.vector(out[1,])), aes(x=x, y=y), size=0.8, color='black') +
    geom_line(data=data.frame(x=x_val, y=out[2,]), aes(x=x, y=y), size=0.5, color='black', linetype='dashed') +
    geom_line(data=data.frame(x=x_val, y=out[3,]), aes(x=x, y=y), size=0.5, color='black', linetype='dashed') +
    theme_light()+
    theme(plot.title = element_text(size = 9, face = "italic"))+
    xlim(22,41)+
    ylim(400,4700)
  plot(gg)
  
  #cat ("Press [enter] to continue")
  #line <- readline()
}

######################################################################
# compute the causal do quantity
######################################################################
do_preg = seq(20, 42, length.out=200)
effect1 = matrix(0, nrow=3, ncol=200)
effect2 = matrix(0, nrow=3, ncol=200)

N = 500
cnt = 0
for(i in sample((1:nrow(X))[-train_idx], N)){
  cnt = cnt + 1
  print(cnt)
  
  test_point = X[i,]  
  test_point[21] = 0 
  test_point[22] = 1 #female
  #test_point[7] = 1 #not twin
  weights = predict(mrf_fit, newdata=matrix(test_point, nrow=1))$weights[1,]
  effect1 = effect1 + compute(Y[train_idx,1], Y[train_idx,2], weights=weights, do_preg)/N
  
  test_point[21] = 1 #male
  test_point[22] = 0 
  #test_point[7] = 2 #twin
  weights = predict(mrf_fit, newdata=matrix(test_point, nrow=1))$weights[1,]
  effect2 = effect2 + compute(Y[train_idx,1], Y[train_idx,2], weights=weights, do_preg)/N
}

ggplot() +
  geom_line(data=data.frame(x=do_preg, y=effect1[1,]), aes(x=x, y=y), size=0.8, color='red') +
  geom_line(data=data.frame(x=do_preg, y=effect1[2,]), aes(x=x, y=y), size=0.5, color='red', linetype='dashed') +
  geom_line(data=data.frame(x=do_preg, y=effect1[3,]), aes(x=x, y=y), size=0.5, color='red', linetype='dashed') +
  
  geom_line(data=data.frame(x=do_preg, y=effect2[1,]), aes(x=x, y=y), size=0.8, color='blue') +
  geom_line(data=data.frame(x=do_preg, y=effect2[2,]), aes(x=x, y=y), size=0.5, color='blue', linetype='dashed') +
  geom_line(data=data.frame(x=do_preg, y=effect2[3,]), aes(x=x, y=y), size=0.5, color='blue', linetype='dashed') +

  ylim(c(200, 4400)) +
  xlim(c(22,41)) +
  theme_light() +
  xlab('pregnancy length (weeks)')+
  ylab('birthweight (grams)')# + guides(color)

#################################################################################################################
# plot conditional fits
################################################################################################################
dset = births[gender=='F',]
effect1_cond = compute(dset$pregnancy_duration, dset$birthweight, weights=rep(1/nrow(dset), nrow(dset)), do_preg)
dset = births[gender=='M',]
effect2_cond = compute(dset$pregnancy_duration, dset$birthweight, weights=rep(1/nrow(dset), nrow(dset)), do_preg)

dset = births[plurality==1,]
effect1_cond = compute(dset$pregnancy_duration, dset$birthweight, weights=rep(1/nrow(dset), nrow(dset)), do_preg)
dset = births[plurality==2,]
effect2_cond = compute(dset$pregnancy_duration, dset$birthweight, weights=rep(1/nrow(dset), nrow(dset)), do_preg)

ggplot() +
  #geom_jitter(data=dset, aes(x=pregnancy_duration, y=birthweight))+
  geom_line(data=data.frame(x=do_preg, y=effect1_cond[1,]), aes(x=x, y=y), color='red') +
  geom_line(data=data.frame(x=do_preg, y=effect1_cond[2,]), aes(x=x, y=y), size=0.5, color='red', linetype='dashed') +
  geom_line(data=data.frame(x=do_preg, y=effect1_cond[3,]), aes(x=x, y=y), size=0.5, color='red', linetype='dashed') +
  
  geom_line(data=data.frame(x=do_preg, y=effect2_cond[1,]), aes(x=x, y=y), color='blue') +
  geom_line(data=data.frame(x=do_preg, y=effect2_cond[2,]), aes(x=x, y=y), size=0.5, color='blue', linetype='dashed') +
  geom_line(data=data.frame(x=do_preg, y=effect2_cond[3,]), aes(x=x, y=y), size=0.5, color='blue', linetype='dashed') +
  
  ylim(c(200, 4400))

#############################################################################################################
# explore conditional 1d fits
##########################################################################################################
compute = function(x, y, weights, x_values){ #transform, fit smoothing splines for mean, assume iid gaussian residuals
  x = x + rnorm(length(x), sd=0.5)
  
  f_x = function(x){return(log(log(x)))}
  f_y = f_x#function(x){return(log(x))}
  
  f_inv_x = function(x){return(exp(exp(x)))}
  f_inv_y = f_inv_x#function(x){return(exp(x))}
  
  xt = f_x(x)
  yt = f_y(y)
  
  fit = smooth.spline(yt~xt, w=weights, df=3)#cv=TRUE)
  #fit = smooth.spline(yt~xt, cv=TRUE)
  
  residuals = (yt - predict(fit, x=data.frame(x=xt))$y$x)
  sigmahat = sum(weights * residuals^2)^0.5
  
  fitted = predict(fit, x=data.frame(x=f_x(x_values)))$y$x
  mean = f_inv_y(fitted)
  quantile_lo = f_inv_y(fitted + qnorm(0.1)*sigmahat)
  quantile_up = f_inv_y(fitted + qnorm(0.9)*sigmahat)
  
  return(rbind(mean, quantile_lo, quantile_up))
}

compute = function(x, y, weights, x_values){ #transform, fit RF and QRF
  x = x + rnorm(length(x), sd=0.5)
  f_x = function(x){return(log(log(x)))}
  f_y = f_x#function(x){return(log(x))}
  
  f_inv_x = function(x){return(exp(exp(x)))}
  f_inv_y = f_inv_x#function(x){return(exp(x))}
  
  xt = f_x(x)
  yt = f_y(y)
  
  idx = sample(1:length(x), 10000, prob=as.vector(weights), replace=TRUE)
  fit = mrf(X=matrix(xt[idx], ncol=1), Y=matrix(yt[idx], ncol=1), min.node.size = 10, splitting.rule='fourier', num_features=10, num.trees=5000)
  
  fitted = predict(fit, type='mean', newdata=matrix(f_x(x_values), ncol=1))$mean
  mean = f_inv_y(fitted)
  weights = predict(fit, newdata=matrix(f_x(x_values), ncol=1))$weights
  fitted = predict(fit, newdata=matrix(f_x(x_values), ncol=1), type='functional', f=function(y){y[1]}, quantiles=c(0.1, 0.9))
  quantile_lo = f_inv_y(fitted$functional[,1])
  quantile_up = f_inv_y(fitted$functional[,2])

  return(rbind(mean, quantile_lo, quantile_up))
}

compute = function(x, y, weights, x_values){ #transform, fit smoothing splines for mean, assume iid gaussian residuals
  idx = sample(1:length(x), 200000, prob=as.vector(weights), replace=TRUE)
  x = x[idx]
  y = y[idx]
  x = x + rnorm(length(x), sd=0.2)
  y = y + rnorm(length(x), sd=10)
  
  f_x = function(x){return(log(log(x)))}
  f_y = f_x#function(x){return(log(x))}
  
  f_inv_x = function(x){return(exp(exp(x)))}
  f_inv_y = f_inv_x#function(x){return(exp(x))}
  
  xt = f_x(x)
  yt = f_y(y)
  
  library(Iso)
  #idx = sample(1:length(x), 200000, prob=as.vector(weights), replace=TRUE)
  fit = isoreg(xt, yt)
  fit = smooth.spline(fit$yf~sort(fit$x), cv=TRUE)
  
  residuals = (yt - predict(fit, x=data.frame(x=xt))$y$x)
  sigmahat = sum(weights * residuals^2)^0.5
  
  fitted = predict(fit, x=data.frame(x=f_x(x_values)))$y$x
  mean = f_inv_y(fitted)
  quantile_lo = f_inv_y(fitted + qnorm(0.1)*sigmahat)
  quantile_up = f_inv_y(fitted + qnorm(0.9)*sigmahat)
  
  return(rbind(mean, quantile_lo, quantile_up))
}

compute = function(x, y, weights, x_values){ #transform, fit smootherfor mean, assume gaussian residuals fit variance with smoother
  #upweight leftmost points
  weight_mask = 1 - x/55 
  weights = weights * weight_mask^3
  weights = weights / sum(weights)
  
  f_x = function(x){return(log(log(x)))}
  f_y = f_x#function(x){return(log(x))}
  f_inv_x = function(x){return(exp(exp(x)))}
  f_inv_y = f_inv_x#function(x){return(exp(x))}
  
  xt = f_x(x)
  yt = f_y(y)
  
  fit = smooth.spline(yt~xt, w=weights, df=5)#cv=TRUE)
  fitted = predict(fit, x=data.frame(x=f_x(x_values)))$y$x
  mean = f_inv_y(fitted)
  
  residuals = (yt - predict(fit, x=data.frame(x=xt))$y$x)
  #sd_res = 1.25 * abs(residuals)
  #fit_sd = lm(sd_res~xt, weights=weights)
  #sigmahat = predict(fit_sd, newdata=data.frame(xt=f_x(x_values)))
  
  #quantile_lo = f_inv_y(fitted + qnorm(0.1)*sigmahat)
  #quantile_up = f_inv_y(fitted + qnorm(0.9)*sigmahat)
  
  library(quantreg)
  fit_quantile = rq(residuals~xt, weights=weights, tau=0.1)
  quantile_lo = f_inv_y(fitted + predict(fit_quantile, newdata=data.frame(xt=f_x(x_values))))
  fit_quantile = rq(residuals~xt, weights=weights, tau=0.9)
  quantile_hi = f_inv_y(fitted + predict(fit_quantile, newdata=data.frame(xt=f_x(x_values))))
  
  return(rbind(mean, quantile_lo, quantile_hi))
}

x = pregnancy_duration# + rnorm(50000, sd=0.2)
y = birthweight
x_values = seq(18, 43, length.out=200)
weights = rep(1/length(x), length(x))
out = compute(x, y, weights, x_values)

ggplot(data.frame(x=x[weights!=0], y=y[weights!=0]), aes(x=x, y=y)) +
  geom_jitter() + 
  geom_line(data=data.frame(x=x_values, y=out[1,]), color='red')+
  geom_line(data=data.frame(x=x_values, y=out[2,]), color='red', linetype='dashed')+
  geom_line(data=data.frame(x=x_values, y=out[3,]), color='red', linetype='dashed')
