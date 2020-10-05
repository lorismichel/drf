library(ggplot2)
library(drf)
library(GGally)
library(fastDummies)

######################################################################
# load dataset
#####################################################################

load("../../data/birth_data/computed_data/births.dat")
births = na.omit(births)
births = births[births$race_mother %in% c('white', 'black', 'asian'), ]
births = births[births$race_father %in% c('white', 'black', 'asian'), ]

#births = births[births$plurality==1]
attach(births)

###########################################################################
# fit drf
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

set.seed(22)
train_idx = sample(1:nrow(births), 100000, replace=FALSE)
drf_fit = drf(X=X[train_idx,], Y=Y[train_idx,], min.node.size = 20, splitting.rule='FourierMMD', num.features=10)

########################################################################
# check heterogeneous regression
########################################################################
point_description = function(test_point){
  out = ''
  out = paste(out, "MOTHER - age: ", test_point$age_mother[1], sep='')
  out = paste(out, '\n    race: ', test_point$race_mother[1], sep='')
  out = paste(out, '\n    education: ', c('<9 grades', '<12 grades', 'high school', 'associate degree', 'bachelor\'s degree', 'master\'s degree', 'doctorate', 'unknown')[test_point$education_mother[1]], sep='')
  out = paste(out, '\n    height: ', test_point$height_mother[1], "in", sep='')
  out = paste(out, '\n    BMI: ', test_point$BMI_mother[1], sep='')
  out = paste(out, '\n    smoking: ', c('no', 'yes')[1+test_point$cigarettes_during_pregnancy[1]])
  
  out = paste(out, '\nFATHER - age: ', test_point$age_father[1], sep='')
  out = paste(out, '\n    race: ', test_point$race_father[1], sep='')
  out = paste(out, '\n    education: ', c('<9 grades', '<12 grades', 'high school', 'associate degree', 'bachelor\'s degree', 'master\'s degree', 'doctorate', 'unknown')[test_point$education_father[1]], sep='')
  
  out = paste(out, '\nBIRTH - multiplicity: ', test_point$plurality[1], sep='')
  out = paste(out, '\n    gender: ', c('male', 'female')[1+(test_point$gender[1]=='F')])
  out = paste(out, '\n    birth order: ', test_point$birth_order[1], sep='')
  out = paste(out, '\n    prenatal care: ', c('no', 'yes')[1+(test_point$precare[1]!=Inf)])
  out = paste(out, '\n    method: ', test_point$delivery_method[1], sep='')
  
  return(out)
}

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
levels(plotdf$gender)[levels(plotdf$gender)=="M"] <- "male"
levels(plotdf$gender)[levels(plotdf$gender)=="F"] <- "female"
set.seed(22)
for(i in sample((1:nrow(X))[-train_idx], 1500)){
  if(births[i,]$race_mother!='black' || births[i,]$plurality==1){
    next
  }
  #if(X[i, 7]!=2){#plurality
  # next
  #}
  print(i)
  test_point = X[i,]
  weights = predict(drf_fit, newdata=matrix(test_point, nrow=1))$weights[1,]
  
  print(births[i,])
  plotdf$drf_weights = weights
  
  x_val = seq(22, 42, length.out=200)
  out = compute(Y[train_idx,1], Y[train_idx,2], weights=weights, x_val)
  
  gg=ggplot(plotdf[plotdf$drf_weights!=0,], aes(x=pregnancy_duration, y=birthweight))+
    geom_jitter(aes(size=ifelse(drf_weights==0, NA, drf_weights^0.5), color=race_mother, shape=gender), alpha=0.8)+
    scale_size_area(max_size=4) + guides(size=FALSE, alpha=FALSE)+labs(color='race mother', shape='gender baby')+
    #scale_color_viridis_c(option='magma') +
    geom_line(data=data.frame(x=x_val, y=as.vector(out[1,])), aes(x=x, y=y), size=0.8, color='black') +
    geom_line(data=data.frame(x=x_val, y=out[2,]), aes(x=x, y=y), size=0.5, color='black', linetype='dashed') +
    geom_line(data=data.frame(x=x_val, y=out[3,]), aes(x=x, y=y), size=0.5, color='black', linetype='dashed') +
    theme_light()+
    #theme(plot.title = element_text(size = 9, face = "italic"))+
    coord_cartesian(xlim=c(22, 41), ylim=c(400, 4700)) +
    theme(legend.text=element_text(size=16),
          legend.title=element_text(size=18),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(size=17),
          axis.title.y = element_text(size=17))+
    annotate("text", x=21.08, y=Inf, hjust=0, vjust=1, size=4.4, label = point_description(births[i,]))+
    labs(x='pregnancy length (weeks)')+
    labs(y='birthweight (grams)')
    
  
  plot(gg)
  
  cat ("Press [enter] to continue")
  line <- readline()
}
#1646
#120894 twin or 53025 or 129085

ggsave('./cond2.png', width=21, height=12, units='cm')
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
  
  test_point[7] = 1 #not twin
  weights = predict(drf_fit, newdata=matrix(test_point, nrow=1))$weights[1,]
  effect1 = effect1 + compute(Y[train_idx,1], Y[train_idx,2], weights=weights, do_preg)/N
  
  test_point[7] = 2 #twin
  weights = predict(drf_fit, newdata=matrix(test_point, nrow=1))$weights[1,]
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
###############################################################################################################
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