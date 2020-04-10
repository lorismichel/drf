library(mrf)
#library(grf)
library(ranger)
library(hdm)
library(ggplot2)
library(ggpubr)

data = cps2012
data=apply(data, 2, as.numeric)

data=data.frame(data)
colnames(data)

data$weight[data$female==1] = data$weight[data$female==1]/sum(data$weight[data$female==1])
data$weight[data$female==0] = data$weight[data$female==0]/sum(data$weight[data$female==0])
#pooled data
ggplot(data, aes(lnw)) +
  geom_density(adjust=3.5, alpha = 0.25, show.legend = FALSE,  aes(fill = as.factor(1-female), weight=weight)) +
  coord_cartesian(xlim=c(-0.5, 6)) +
  labs(x='log(hourly_wage)')

qplot(exp2, lnw, data=data, geom=c('jitter', 'smooth'))

ggplot(data[data$hsg==1,], aes(lnw)) +
  geom_density(adjust=3.5, alpha = 0.25, show.legend = FALSE,  aes(fill = as.factor(1-female))) +
  coord_cartesian(xlim=c(-0.5, 6)) +
  labs(x='log(hourly_wage)')

X1 = data[, -c(1,2,3,20,17,18,19)]
X1 = apply(X1, 2, as.numeric)
X1 = matrix(X1, nrow=nrow(X1))
Y1 = data[, c(2,3,17,18,19)]
Y1 = apply(Y1, 2, as.numeric)
Y1 = matrix(Y1, nrow=nrow(Y1))
mrf_fit <- mrf(X=X1, Y=Y1, splitting.rule = "fourier", num_features=10, node_scaling = FALSE, min.node.size = 20)

X2 = data[, -c(1,2,20,17,18,19)]
X2 = apply(X2, 2, as.numeric)
X2 = matrix(X2, nrow=nrow(X2))
Y2 = matrix(data[, c(2)], ncol=1)
Y2 = apply(Y2, 2, as.numeric)
Y2 = matrix(Y2, nrow=nrow(Y2))
mrf_fit2 <- mrf(X=X2, Y=Y2, splitting.rule = "fourier", num_features=10, node_scaling = FALSE, min.node.size = 20)
mrf_fit3 <- mrf(X=X2, Y=Y2, splitting.rule = "gini", node_scaling = FALSE, min.node.size = 20)

#resampled_data = data[sample(1:nrow(data), 1000000, replace=TRUE, prob=as.vector(predict(mrf_fit, newdata=test_point)$weights[1,])),]
#ggplot(resampled_data, aes(lnw, fill = as.factor(1-female))) + geom_density(alpha = 0.2)  + xlim(-0.5, 6) #+ xlim(-0.5, 6)

point_description = function(test_point){
  names = c('year', 'lnw', 'female', 'widowed', 'divorced', 'separated', 'never married', 'up to 8 grades of school', '9-11 grades of school', 'high school graduate', 
    'college graduate', 'associate degree', 'midwest', 'south', 'west', 'potential experience', 'exp2', 'exp3', 'exp4', 'weight',
    'married', 'northeast', 'college, no degree')
  out = 'marital status: '
  for(i in c(4,5,6,7,21)){
    if(test_point[i] != 0){
      out = paste(out, names[i], sep="")
    }
  }
  out = paste(out, ", education: ", sep='')
  for(i in c(8,9,10,11,12,23)){
    if(test_point[i] != 0){
      out = paste(out, names[i], sep="")
    }
  }
  out = paste(out, ", region: ", sep='')
  for(i in c(13,14,15,22)){
    if(test_point[i] != 0){
      out = paste(out, names[i], sep="")
    }
  }
  out = paste(out, ", potential experience: ", as.character(test_point[16]), sep='')

  return(out)
}

#random test_point
for(idx in sample(1:nrow(X1), 10, replace=FALSE)){
  test_point = X1[idx,]
  data$cond_weight = predict(mrf_fit, newdata=matrix(test_point, nrow=1))$weights[1,]
  data$cond_weight[data$female==1] = data$cond_weight[data$female==1]/sum(data$cond_weight[data$female==1])
  data$cond_weight[data$female==0] = data$cond_weight[data$female==0]/sum(data$cond_weight[data$female==0])
  gg1 = ggplot(data, aes(lnw, fill = as.factor(1-female), weight=cond_weight)) + geom_density(adjust=3, alpha = 0.2, show.legend = FALSE) + coord_cartesian(xlim=c(-0.5, 6))
  gg1 = gg1 + labs(caption=point_description(data[idx,]))
  plot(gg1)
  # test_point = X2[idx,]
  # test_point[1] = 1
  # data$cond_weight = predict(mrf_fit2, newdata=matrix(test_point, nrow=1))$weights[1,]
  # gg2 = ggplot(data, aes(lnw, weight=cond_weight)) + geom_density(adjust=3, alpha = 0.2, fill='hotpink2') +  coord_cartesian(xlim=c(-0.5, 6))
  # test_point[1] = 0
  # data$cond_weight = predict(mrf_fit2, newdata=matrix(test_point, nrow=1))$weights[1,]
  # gg2 = gg2 + geom_density(data=data, adjust=3, alpha=0.2, fill='steelblue3')
  # gg2 = gg2 + labs(caption=point_description(data[idx,]))
  # 
  # test_point = X2[idx,]
  # test_point[1] = 1
  # data$cond_weight = predict(mrf_fit3, newdata=matrix(test_point, nrow=1))$weights[1,]
  # gg3 = ggplot(data, aes(lnw, weight=cond_weight)) + geom_density(adjust=3, alpha = 0.2, fill='hotpink2') +  coord_cartesian(xlim=c(-0.5, 6))
  # test_point[1] = 0
  # data$cond_weight = predict(mrf_fit3, newdata=matrix(test_point, nrow=1))$weights[1,]
  # gg3 = gg3 + geom_density(data=data, adjust=3, alpha=0.2, fill='steelblue3')
  # gg3 = gg3 + labs(caption=point_description(data[idx,]))
  # 
  #  plot(ggarrange(gg1, gg2, gg3, nrow=3, ncol=1,
  #            labels=c("fourier:  (wage, gender) | covariates",
  #                     "fourier:  wage | gender, covariates",
  #                     "ranger:  wage | gender, covariates")))
}
#################################################################################
testX = matrix(rep(0, 3*ncol(data)), nrow=3)

testX[1,5] = 1 #divorced
testX[1,8] = 1 #not finished high school
testX[1,13] = 1 #south
testX[1,16] = 5 #exp

testX[2,21] = 1 #married
testX[2,23] = 1 #not finished college
testX[2,22] = 1 #northeast
testX[2,16] = 10 #exp

testX[3,7] = 1 #never married
testX[3,11] = 1 #college
testX[3,15] = 1 #west
testX[3,16] = 25 #exp
# testX = matrix(rep(0, 5*ncol(data)), nrow=5)
# testX[1,21] = 1 #married
# testX[1,11] = 1 #college
# testX[1,15] = 1 
# testX[1,16] = 5 #exp
# testX[2, ] = testX[1,]
# testX[3, ] = testX[1,]
# testX[4, ] = testX[1,]
# testX[5, ] = testX[1,]
# testX[,16] = c(1, 5, 10, 20, 30)

tmpX = testX[, -c(1,2,3,20,17,18,19)]

printdf = data.frame()
for(idx in c(1:nrow(testX))){
  test_point = tmpX[idx,]
  tmpdf = data
  tmpdf$cond_weight = predict(mrf_fit, newdata=matrix(test_point, nrow=1))$weights[1,]
  tmpdf$cond_weight[data$female==1] = tmpdf$cond_weight[data$female==1]/sum(tmpdf$cond_weight[data$female==1])
  tmpdf$cond_weight[data$female==0] = tmpdf$cond_weight[data$female==0]/sum(tmpdf$cond_weight[data$female==0])
  tmpdf$description = point_description(testX[idx,])#as.character(idx)
  printdf = rbind(printdf, tmpdf)
}
ggplot(printdf, aes(lnw)) +
  geom_density(adjust=3.5, alpha = 0.25, show.legend = FALSE,  aes(fill = as.factor(1-female), weight=cond_weight)) +
  coord_cartesian(xlim=c(-0.5, 6)) +
  facet_wrap(vars(description), nrow=nrow(testX)) +
  theme(strip.text.x = element_text(size = 10.5)) +
  labs(x='log(hourly_wage)')

##################################################################################
weights = rep(0, nrow(data))
N = 200
cnt = 1
for(idx in sample(1:nrow(X1), N, replace=FALSE)){
  print(cnt)
  cnt = cnt + 1
  test_point = X1[idx,]
  data$cond_weight = predict(mrf_fit, newdata=matrix(test_point, nrow=1))$weights[1,]
  data$cond_weight[data$female==1] = data$cond_weight[data$female==1]/sum(data$cond_weight[data$female==1])
  data$cond_weight[data$female==0] = data$cond_weight[data$female==0]/sum(data$cond_weight[data$female==0])
  weights = weights + data$cond_weight/N
}
data$cond_weight=weights
ggplot(data, aes(lnw, fill = as.factor(1-female), weight=cond_weight)) + geom_density(adjust=1.5, alpha = 0.2, show.legend = FALSE) + coord_cartesian(xlim=c(-0.5, 6))

ggplot(data, aes(lnw, fill = as.factor(1-female))) + geom_density(alpha = 0.2, adjust=1.5, show.legend = FALSE) + coord_cartesian(xlim=c(-0.5, 6))
##################################################################################
vars = c(8, 9, 10, 23, 12, 11)
weights = matrix(0, nrow=nrow(data), ncol=length(vars))

N = 100
cnt = 1
for(idx in sample(1:nrow(X1), N, replace=FALSE)){
  print(cnt)
  cnt = cnt + 1
  
  for(i in 1:length(vars)){
    datapoint = data[idx,]
    datapoint[vars] = 0
    datapoint[vars[i]] = 1
    test_point = datapoint[, -c(1,2,3,20,17,18,19)]
    test_point = apply(test_point, 2, as.numeric)
    
    
    w = predict(mrf_fit, newdata=matrix(test_point, nrow=1))$weights[1,]
    w[data$female==1] = w[data$female==1]/sum(w[data$female==1])
    w[data$female==0] = w[data$female==0]/sum(w[data$female==0])
    weights[, i] = weights[, i] + w/N
  }
}

plotdf = data.frame()
for(i in 1:length(vars)){
    tmpdf = data
    tmpdf$weights = weights[, i]
    names = c('year', 'lnw', 'female', 'widowed', 'divorced', 'separated', 'never married', 'up to 8 grades of school', '9-11 grades of school', 'high school graduate', 
              'college graduate', 'associate degree', 'midwest', 'south', 'west', 'potential experience', 'exp2', 'exp3', 'exp4', 'weight',
              'married', 'northeast', 'college, no degree')
    tmpdf$name = names[vars[i]]
    plotdf = rbind(plotdf, tmpdf)
}

ggplot(plotdf, aes(lnw)) +
  geom_density(adjust=2, alpha = 0.25, show.legend = FALSE,  aes(fill = as.factor(1-female), weight=weights)) +
  coord_cartesian(xlim=c(-0.5, 6)) +
  facet_wrap(vars(name), nrow=length(vars)) +
  theme(strip.text.x = element_text(size = 10.5)) +
  labs(x='log(hourly_wage)')
# 
# data$cond_weight = weights[,3]
# ggplot(data, aes(lnw, fill = as.factor(1-female))) +
#   geom_density(adjust=2, alpha = 0.25, show.legend = FALSE, aes(weight=cond_weight)) +
#   coord_cartesian(xlim=c(-0.5, 6)) +
#   labs(x='log(hourly_wage)')
# 
# ggplot(data[data$hsg==1,], aes(lnw)) +
#      geom_density(adjust=3.5, alpha = 0.25, show.legend = FALSE,  aes(fill = as.factor(1-female))) +
#      coord_cartesian(xlim=c(-0.5, 6)) +
#      labs(x='log(hourly_wage)')
