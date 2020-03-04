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
ggplot(data, aes(lnw, fill = as.factor(1-female))) + geom_density(alpha = 0.2, adjust=2) + coord_cartesian(xlim=c(-0.5, 6))


X1 = data[, -c(1,2,3,20)]
X1 = apply(X1, 2, as.numeric)
Y1 = data[, c(2,3)]
Y1 = apply(Y1, 2, as.numeric)
mrf_fit <- mrf(X=X1, Y=Y1, splitting.rule = "fourier", num_features=3, node_scaling = FALSE, min.node.size = 20)

X2 = data[, -c(1,2,3,20)]
X2 = cbind(X2, data$female)#put female at the end so that print works for both methods
colnames(X2)[20] = "female"
X2 = apply(X2, 2, as.numeric)
Y2 = matrix(data[, c(2)], ncol=1)
Y2 = apply(Y2, 2, as.numeric)
mrf_fit2 <- mrf(X=X2, Y=Y2, splitting.rule = "fourier", num_features=3, node_scaling = FALSE, min.node.size = 10)
mrf_fit3 <- mrf(X=X2, Y=Y2, splitting.rule = "gini", node_scaling = FALSE, min.node.size = 10)
# resampled_data = data[sample(1:nrow(data), 1000000, replace=TRUE, prob=as.vector(predict(mrf_fit, newdata=test_point)$weights[1,])),]
# ggplot(resampled_data, aes(lnw, fill = as.factor(1-female))) + geom_density(alpha = 0.2)  + xlim(-0.5, 6) #+ xlim(-0.5, 6)

point_description = function(test_point){
  out = "marital:"
  for(i in c(1,2,3,4,17)){
    if(test_point[i] != 0){
      out = paste(out, names(test_point)[i], "   ")
    }
  }
  out = paste(out, "education:")
  for(i in c(5,6,7,8,9, 19)){
    if(test_point[i] != 0){
      out = paste(out, names(test_point)[i], "   ")
    }
  }
  out = paste(out, "region:")
  for(i in c(10,11,12,18)){
    if(test_point[i] != 0){
      out = paste(out, names(test_point)[i], "   ")
    }
  }
  out = paste(out, "experience:", as.character(test_point[13]), "", as.character(test_point[14]), "", as.character(test_point[15]))

  return(out)
}

#random test_point
for(idx in sample(1:nrow(X1), 10, replace=FALSE)){
  test_point = X1[idx,]
  data$cond_weight = predict(mrf_fit, newdata=test_point)$weights[1,]
  data$cond_weight[data$female==1] = data$cond_weight[data$female==1]/sum(data$cond_weight[data$female==1])
  data$cond_weight[data$female==0] = data$cond_weight[data$female==0]/sum(data$cond_weight[data$female==0])
  gg1 = ggplot(data, aes(lnw, fill = as.factor(1-female), weight=cond_weight)) + geom_density(adjust=4, alpha = 0.2, show.legend = FALSE) + coord_cartesian(xlim=c(-0.5, 6))
  gg1 = gg1 + labs(caption=point_description(test_point))

  test_point = X2[idx,]
  test_point[20] = 1
  data$cond_weight = predict(mrf_fit2, newdata=test_point)$weights[1,]
  gg2 = ggplot(data, aes(lnw, weight=cond_weight)) + geom_density(adjust=4, alpha = 0.2, fill='hotpink2') +  coord_cartesian(xlim=c(-0.5, 6))
  test_point[20] = 0
  data$cond_weight = predict(mrf_fit2, newdata=test_point)$weights[1,]
  gg2 = gg2 + geom_density(data=data, adjust=4, alpha=0.2, fill='steelblue3')
  gg2 = gg2 + labs(caption=point_description(test_point))
  
  test_point = X2[idx,]
  test_point[20] = 1
  data$cond_weight = predict(mrf_fit3, newdata=test_point)$weights[1,]
  gg3 = ggplot(data, aes(lnw, weight=cond_weight)) + geom_density(adjust=4, alpha = 0.2, fill='hotpink2') +  coord_cartesian(xlim=c(-0.5, 6))
  test_point[20] = 0
  data$cond_weight = predict(mrf_fit3, newdata=test_point)$weights[1,]
  gg3 = gg3 + geom_density(data=data, adjust=4, alpha=0.2, fill='steelblue3')
  gg3 = gg3 + labs(caption=point_description(test_point))
  
  plot(ggarrange(gg1, gg2, gg3, nrow=3, ncol=1,
            labels=c("fourier:  (wage, gender) | covariates", 
                     "fourier:  wage | gender, covariates", 
                     "ranger:  wage | gender, covariates")))
}

#choose test_point
test_point = matrix(rep(0, 19), ncol=19)
names(test_point) = colnames(X)
test_point[1,13:16] = apply(X[,13:16], 2, median) #experience
test_point[1,4] = 1 #nevermarried, 17 is married
test_point[1,18] = 1 #ne
test_point[1,19] = 1 #sc = some college?

data$cond_weight = predict(mrf_fit, newdata=test_point)$weights[1,]
sum(data$cond_weight)
sum(data$cond_weight != 0 & data$female==1)
sum(data$cond_weight != 0 & data$female==0)

data$cond_weight[data$female==1] = data$cond_weight[data$female==1]/sum(data$cond_weight[data$female==1])
data$cond_weight[data$female==0] = data$cond_weight[data$female==0]/sum(data$cond_weight[data$female==0])
gg = ggplot(data, aes(lnw, fill = as.factor(1-female), weight=cond_weight)) + geom_density(adjust=4, alpha = 0.2) + xlim(-0.5, 6)  
print(gg + ggtitle(point_description(test_point)))
