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
births = births[sample(1:nrow(births), 50000, replace=FALSE),]
#births = births[births$plurality==1]
attach(births)

###########################################################################
# exploratory analysisa & visualization
##########################################################################

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
  out = paste(test_point$age_mother[1])
  out = paste(out, test_point$race_mother[1])
  out = paste(out, test_point$education_mother[1])
  out = paste(out, test_point$cigarettes_during_pregnancy[1])
  
  out = paste(out, '\n', sep='')
  
  out = paste(out, test_point$age_father[1])
  out = paste(out, test_point$race_father[1])
  out = paste(out, test_point$education_father[1])
  
  out = paste(out, '\n', sep='')
  
  out = paste(out, test_point$plurality[1])
  out = paste(out, test_point$gender[1])
  out = paste(out, test_point$birth_order[1])
  out = paste(out, test_point$delivery_method[1])
  
  return(out)
}

mrf_fit = mrf(X=X, Y=Y, min.node.size = 20, splitting.rule='fourier', num_features=10)

########################################################################
# check heterogeneous regression
########################################################################

compute = function(x, y, weights, x_values){ #homoscedastic fit
  #   fit <- lm(log(y)~log(x), weights=weights)
  #   sigmahat = sum(weights*fit$residuals^2)^0.5
  #   mean = exp(predict(fit, newdata=data.frame(x=x_values)))
  #   quantile_lo = exp(predict(fit, newdata=data.frame(x=x_values)) + qnorm(0.1)*sigmahat)
  #   quantile_up = exp(predict(fit, newdata=data.frame(x=x_values)) + qnorm(0.9)*sigmahat)
  #   return(rbind(mean, quantile_lo, quantile_up))
  # }  
  idx = sample(1:length(x), 200000, prob=as.vector(weights), replace=TRUE)
  fit = smooth.spline(y[idx]~x[idx], cv=TRUE)
  #fit = smooth.spline(yt~xt, cv=TRUE)
  
  residuals = (y - predict(fit, x=data.frame(x=x))$y$x)
  sigmahat = sum(weights * residuals^2)^0.5
  
  fitted = predict(fit, x=data.frame(x=x_values))$y$x
  mean = fitted
  quantile_lo = fitted + qnorm(0.1)*sigmahat
  quantile_up = fitted + qnorm(0.9)*sigmahat
  return(rbind(mean, quantile_lo, quantile_up))
}

for(i in sample(nrow(X), 5)){
  #if(X[i, 7]==1){#plurality
  # next
  #}
  test_point = X[i,]
  weights = predict(mrf_fit, newdata=matrix(test_point, nrow=1))$weights[1,]
  print(births[i,])
  births$mrf_weights = weights
  
  x_val = seq(27, 42, length.out=200)
  out = compute(Y[,1], Y[,2], weights=weights, x_val)
  
  gg=ggplot(births[mrf_weights!=0], aes(x=pregnancy_duration, y=birthweight))+
    geom_jitter(aes(size=ifelse(mrf_weights==0, NA, mrf_weights^0.5), color=race_mother, shape=gender))+
    scale_size_area(max_size=2) + guides(size=FALSE)+
    #scale_color_viridis_c(option='magma') +
    ggtitle(point_description(births[i,])) +
    geom_line(data=data.frame(x=x_val, y=as.vector(out[1,])), aes(x=x, y=y), size=0.5, color='black') +
    geom_line(data=data.frame(x=x_val, y=out[2,]), aes(x=x, y=y), size=0.5, color='black', linetype='dashed') +
    geom_line(data=data.frame(x=x_val, y=out[3,]), aes(x=x, y=y), size=0.5, color='black', linetype='dashed') +
    theme_light()+
    xlim(27,42)+
    ylim(800,5000)
  plot(gg)
}

######################################################################
# compute the causal do quantity
######################################################################
do_preg = seq(27, 42, length.out=200)
effect1 = rep(0, 200)
effect2 = rep(0, 200)

N = 200
cnt = 0
for(i in sample(nrow(X), N)){
  cnt = cnt + 1
  print(cnt)
  
  test_point = X[i,]  
  test_point[21] = 0 
  test_point[22] = 1 #7 is twin
  weights = predict(mrf_fit, newdata=matrix(test_point, nrow=1))$weights[1,]
  effect1 = effect1 + compute(Y[,1], Y[,2], weights=weights, do_preg)/N
  
  test_point[21] = 1 
  test_point[22] = 0 #7 is twin
  weights = predict(mrf_fit, newdata=matrix(test_point, nrow=1))$weights[1,]
  effect2 = effect2 + compute(Y[,1], Y[,2], weights=weights, do_preg)/N
}

ggplot() +
  geom_line(data=data.frame(x=do_preg, y=effect1), aes(x=x, y=y), color='red')+
  geom_line(data=data.frame(x=do_preg, y=effect2), aes(x=x, y=y), color='blue')

#################################################################################################################
# plot conditional fits
################################################################################################################
dset = births[plurality==1,]
effect1 = compute(dset$pregnancy_duration, dset$birthweight, weights=rep(1/nrow(dset), nrow(dset)), do_preg)
dset = births[plurality!=1,]
effect2 = compute(dset$pregnancy_duration, dset$birthweight, weights=rep(1/nrow(dset), nrow(dset)), do_preg)
ggplot() +
  geom_line(data=data.frame(x=do_preg, y=effect1), aes(x=x, y=y), color='red')+
  geom_line(data=data.frame(x=do_preg, y=effect2), aes(x=x, y=y), color='blue')

dset = births[gender=='F',]
effect1 = compute(dset$pregnancy_duration, dset$birthweight, weights=rep(1/nrow(dset), nrow(dset)), do_preg)
dset = births[gender=='M',]
effect2 = compute(dset$pregnancy_duration, dset$birthweight, weights=rep(1/nrow(dset), nrow(dset)), do_preg)
ggplot() +
  geom_line(data=data.frame(x=do_preg, y=effect1), aes(x=x, y=y), color='red')+
  geom_line(data=data.frame(x=do_preg, y=effect2), aes(x=x, y=y), color='blue')

#############################################################################################################
# testing conditional 1d fits
##########################################################################################################
compute = function(x, y, weights, x_values){
  xt = log(x)
  yt = log(y)
  
  idx = weights != 0
  fit = loess(yt[idx]~xt[idx], weights=weights[idx])
  
  residuals = (yt[idx] - predict(fit, newdata=data.frame(x=xt[idx])))
  sd_res = (residuals)^2
  fit_sd = loess(sd_res~xt[idx], weights=weights[idx])
  sigmahat = (predict(fit_sd, newdata=log(x_values)))^0.5
  
  fitted = predict(fit, newdata=log(x_values))
  mean = exp(fitted)
  quantile_lo = exp(fitted + qnorm(0.1)*sigmahat)
  quantile_up = exp(fitted + qnorm(0.9)*sigmahat)
  return(rbind(mean, quantile_lo, quantile_up))
}

compute = function(x, y, weights, x_values){
  xt = log(log(x))
  yt = log(log(y))
  
  idx = sample(1:length(x), 200000, prob=as.vector(weights), replace=TRUE)
  fit = smooth.spline(yt[idx]~xt[idx], cv=TRUE)
  #fit = smooth.spline(yt~xt, cv=TRUE)
  
  residuals = (yt - predict(fit, x=data.frame(x=xt))$y$x)
  sigmahat = sum(weights * residuals^2)^0.5
  
  fitted = predict(fit, x=data.frame(x=log(log(x_values))))$y$x
  mean = exp(exp(fitted))
  quantile_lo = exp(exp(fitted + qnorm(0.1)*sigmahat))
  quantile_up = exp(exp(fitted + qnorm(0.9)*sigmahat))
  return(rbind(mean, quantile_lo, quantile_up))
}


compute = function(x, y, weights, x_values){
  xt = log(x)
  yt = log(y)
  
  idx = sample(1:length(x), 200000, prob=as.vector(weights), replace=TRUE)
  fit = smooth.spline(yt[idx]~xt[idx], cv=TRUE)
  
  residuals = (yt[idx] - predict(fit, x=data.frame(x=xt[idx]))$y$x)
  sd_res = (residuals)^2
  fit_sd = smooth.spline(sd_res~xt[idx], cv=TRUE)
  sigmahat = (predict(fit_sd, x=data.frame(x=log(x_values)))$y$x)^0.5
  
  fitted = predict(fit, x=data.frame(x=log(x_values)))$y$x
  mean = exp(fitted)
  quantile_lo = exp(fitted + qnorm(0.1)*sigmahat)
  quantile_up = exp(fitted + qnorm(0.9)*sigmahat)
  return(rbind(mean, quantile_lo, quantile_up))
}

x = pregnancy_duration
y = birthweight
x_values = seq(20, 43, length.out=200)
#weights = rep(1/length(x), length(x))
out = compute(x, y, weights, x_values)

qplot(x,y, geom='jitter') + 
  geom_line(data=data.frame(x=x_values, y=out[1,]), color='red')+
  geom_line(data=data.frame(x=x_values, y=out[2,]), color='red', linetype='dashed')+
  geom_line(data=data.frame(x=x_values, y=out[3,]), color='red', linetype='dashed')

