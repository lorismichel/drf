library(mrf)
library(ranger)
library(hdm)
library(ggplot2)
library(ggpubr)
library(causaldrf)
library(viridis)

data('nmes_data')
data = nmes_data
dim(data)
summary(data)
data$MALE <- as.factor(data$MALE)
data$RACE3 <- as.factor(data$RACE3)
data$educate <- as.factor(data$educate)
data$marital <- as.factor(data$marital)
data$SREGION <- as.factor(data$SREGION)
data$POVSTALB <- as.numeric(data$POVSTALB)

sum(data$TOTALEXP == 0)
data = data[data$TOTALEXP != 0,]
data$TOTALEXP <- log(data$TOTALEXP)

#data$packyears <- log(data$packyears)

colnames(data)[11] = 'weight'
data$weight = data$weight/sum(data$weight)

ggplot(data, aes(x=packyears, y=TOTALEXP, color=LASTAGE>60)) +
  geom_point(alpha=0.9) + #scale_color_viridis_c() +
  geom_smooth() +
  coord_cartesian(xlim = c(0, 60), ylim=c(0, 12.5))# +
#  ylim(0, 50000)

ggplot(data, aes(x=LASTAGE, y=TOTALEXP, color=log(packyears))) +
  geom_point(alpha=0.9) + scale_color_viridis_c() + 
  geom_smooth()# +
#  ylim(0, 50000)

#drop outliers
#which = data$packyears < 120 & data$TOTALEXP < 50000
#data=data[which,]

X = model.matrix(~., data[,c(2:10)])[, -1]
Y = as.matrix(data$TOTALEXP, ncol=1)
W = as.matrix(data$packyears, ncol=1)

mrf_fit <- mrf(Y=cbind(W,Y), X=X, splitting.rule = "fourier", num_features=3, node_scaling = TRUE, min.node.size = 10)

point_description = function(test_point){
  out = paste("marital:", test_point$marital[1])
  out = paste(out, "education:", test_point$educate[1])
  out = paste(out, "poverty:", test_point$POVSTALB[1])
  out = paste(out, "region:", test_point$SREGION[1])
  out = paste(out, "belt:", test_point$beltuse[1])
  out = paste(out, "race:", test_point$RACE3[1])
  out = paste(out, "male:", test_point$MALE[1])
  out = paste(out, "start_age:", test_point$AGESMOKE[1])
  out = paste(out, "stop_age:", test_point$LASTAGE[1])
  
  return(out)
}

for(i in sample(1:nrow(X), 1, replace=FALSE)){
  i = 158
  test_point = X[i,]
  print(test_point)
  data$mrf_weights = predict(mrf_fit, newdata=test_point)$weights[1,]^0.5
  print(sum(data$mrf_weights > 0 & data$packyears < 60))
  
  gg = ggplot(data, aes(x=packyears, y=TOTALEXP, color=LASTAGE)) +
    geom_point(aes(size=ifelse(mrf_weights==0, NA, mrf_weights))) + scale_size_area(max_size=3) + scale_color_viridis_c() +
    geom_smooth(aes(weight=mrf_weights)) +
    labs(caption=point_description(data[i, ])) +
    coord_cartesian(xlim = c(0, 120), ylim=c(0, 12.5))
  print(gg)
}


causal_effect = function(obj, w_do){
  ret = rep(0, length(w_do))
  N = 200
  cnt=0
  for(i in sample(1:nrow(X), N, replace=TRUE)){
    print(cnt)
    cnt = cnt + 1
    weights = as.vector(predict(obj, newdata=matrix(X[i,], nrow=1))$weights)
    idx = sample(1:nrow(X), 100000, prob=weights, replace=TRUE)
    fit = smooth.spline(W[idx], Y[idx], cv=TRUE)$fit
    for(j in 1:length(w_do)){
      ret[j] = ret[j] + predict(fit, x=w_do[j])$y/N
    }
  }
  return(ret)
}

packyears_do = seq(0, 5, length.out = 500)
effect = causal_effect(mrf_fit, packyears_do)
pooled = predict(smooth.spline(W, Y, cv=TRUE)$fit, x=packyears_do)$y
ggplot(data.frame(x=packyears_do, y=effect, pooled=pooled)) +
       geom_line(aes(x=x, y=y), color='red') +
       geom_line(aes(x=x, y=pooled), color='blue')


#####################################################################################
X = model.matrix(~., data[,c(2:10)])[, -c(1,3)]
Y = as.matrix(data$TOTALEXP, ncol=1)
W = as.matrix(cbind(data$packyears, data$LASTAGE), ncol=2)

mrf_fit <- mrf(Y=cbind(W,Y), X=X, splitting.rule = "fourier", num_features=3, node_scaling = FALSE, min.node.size = 20)

for(i in sample(nrow(data), 5, replace=FALSE)){
  test_point = X[i, ]
  data$mrf_weights = predict(mrf_fit, newdata=test_point)$weights[1,]
  packyears = packyears_do = seq(0, 80, length.out = 200)
  
  
  gg = ggplot(data, aes(x=packyears, y=TOTALEXP, color=LASTAGE)) +
    geom_point(aes(size=ifelse(mrf_weights==0, NA, mrf_weights))) + scale_size_area(max_size=2) + 
    scale_color_viridis_c() + 
    geom_smooth(method=gam, formula=y~s(x), aes(weight=mrf_weights)) +
    labs(caption=point_description(data[i, ])) +
    xlim(c(0, 80))# + 
#    ylim(c(0, 20000))
  print(gg)
  #coord_cartesian(xlim = c(0, 120), ylim = c(0,20000))
  
#   gg = ggplot(data, aes(x=LASTAGE, y=TOTALEXP, color=packyears)) +
#     geom_point(aes(size=ifelse(mrf_weights==0, NA, mrf_weights))) + scale_size_area(max_size=2) + 
#     scale_color_viridis_c() + 
#     geom_smooth(aes(weight=mrf_weights)) +
#     labs(caption=point_description(data[i, ]))# +
# #    ylim(c(0, 20000))
#   print(gg)
}

library(mgcv)

causal_effect = function(obj, smoke_do, age){
  ret = rep(0, length(smoke_do))
  N = 200
  cnt=0
  for(i in sample(1:nrow(X), N, replace=TRUE)){
    print(cnt)
    cnt = cnt + 1
    weights = as.vector(predict(obj, newdata=matrix(X[i,], nrow=1))$weights)
    fit = gam(TOTALEXP ~ s(LASTAGE) + s(LASTAGE, by=packyears) + s(packyears) + s(packyears, by=LASTAGE), data=data, weights=weights)
    #fit = ranger(TOTALEXP ~ LASTAGE + packyears, data=data, case.weights=weights)
    
    ret = ret + predict(fit, newdata = data.frame(packyears=smoke_do, LASTAGE=age))/N
    #ret = ret + predict(fit, data = data.frame(packyears=smoke_do, LASTAGE=age))$predictions/N
  }
  return(ret)
}

packyears_do = seq(0, 5, length.out = 200)
age = 80
effect = causal_effect(mrf_fit, packyears_do, age)

#plot(packyears_do, effect, type='l', col='blue')
fit = gam(TOTALEXP ~ s(LASTAGE) + s(LASTAGE, by=packyears) + s(packyears) + s(packyears, by=LASTAGE), data=data)
#fit = ranger(TOTALEXP ~ LASTAGE + packyears, data=data)
pooled = predict(fit, newdata = data.frame(packyears=packyears_do, LASTAGE=age))
#pooled = predict(fit, data = data.frame(packyears=packyears_do, LASTAGE=age))$predictions

ggplot(data=data.frame(effect=effect, packyears=packyears_do, pooled=pooled)) +
  geom_line(aes(x=packyears, y=effect), color='red') + 
  geom_line(aes(x=packyears, y=pooled), color='blue')
