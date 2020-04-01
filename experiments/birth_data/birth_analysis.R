library(ggplot2)
library(mrf)
library(GGally)

load("~/Documents/projects/heterogeneity/birth_data/births.dat")
births = na.omit(births)
#births = births[births$plurality==1]
attach(births)
#ggpairs(births)

qplot(birthweight)
qplot(plurality, birthweight)

qplot(birthweight, weight_mother)
qplot(weight_gain_mother, birthweight, geom='jitter', size=I(0.1), color=pregnancy_duration)+
  scale_color_viridis_c(option='magma')+
  geom_smooth(method='lm')

qplot(age_father, binwidth=0.5)
qplot(age_mother, binwidth=0.5)
qplot(age_father, birthweight, size=I(0.1), geom='jitter')
qplot(age_mother, birthweight, size=I(0.1), geom='jitter')
qplot(age_father, age_mother, size=I(0.1), geom='jitter')

qplot(birth_interval[birth_interval!=1000], birthweight[birth_interval!=1000], geom=c('jitter', 'smooth'))

qplot(birthweight, apgar_5min, geom='jitter')

qplot(education_father, education_mother, geom='jitter')
qplot(education_mother, birthweight, geom='jitter')

precare_started[precare_started==100] = 0
qplot(precare_started, birthweight, geom='jitter')
qplot(precare_started, apgar_5min, geom='jitter')

qplot(pregnancy_duration, birthweight, geom=c('jitter', 'smooth'))
qplot(pregnancy_duration, birthweight, geom='jitter', color=delivery_method)
qplot(pregnancy_duration, birthweight, geom=c('jitter', 'smooth'), color=gender)
qplot(pregnancy_duration, apgar_5min, geom=c('jitter', 'smooth'), color=race_mother)

qplot(age_mother, as.numeric(abnormal_conditions=='yes'), geom=c('jitter', 'smooth'))

X = births[,-c('birth_interval', 'weight_gain_mother', 'birthweight', 'apgar_5min', 'apgar_10min', 'abnormal_conditions', 'congenital_anomalies')]
X = model.matrix(~. , data=X)
X = as.matrix(X)
Y = births[, c('weight_gain_mother', 'birthweight')]#, 'apgar_5min'
Y = as.matrix(Y)

point_description = function(test_point){
  out = paste(test_point$age_mother[1])
  out = paste(out, test_point$race_mother[1])
  out = paste(out, test_point$education_mother[1])
  
  out = paste(out, '\n', sep='')
  
  out = paste(out, test_point$age_father[1])
  out = paste(out, test_point$race_father[1])
  out = paste(out, test_point$education_father[1])
  
  out = paste(out, '\n', sep='')
  
  out = paste(out, test_point$pregnancy_duration[1])
  out = paste(out, test_point$gender[1])
  out = paste(out, test_point$delivery_method[1])
  
  return(out)
}

mrf_fit = mrf(X=X, Y=Y, min.node.size = 20, splitting.rule='fourier', num_features=5)

for(i in sample(nrow(X), 10)){
  test_point = X[i,]
  weights = predict(mrf_fit, newdata=test_point)$weights[1,]
  print(births[i,])
  births$mrf_weights = weights
  gg=ggplot(births, aes(x=weight_gain_mother, y=birthweight))+
    geom_jitter(aes(size=ifelse(mrf_weights==0, NA, mrf_weights), color=race_mother))+
    scale_size_area(max_size=3) +
    #scale_color_viridis_c(option='magma') +
    ggtitle(point_description(births[i,])) +
    theme_light()
  plot(gg)
}
