library(mrf)
#library(grf)
library(ranger)
library(hdm)
library(ggplot2)

data = cps2012
data=apply(data, 2, as.numeric)
data=data.frame(data)
colnames(data)
data$weight[data$female==1] = data$weight[data$female==1]/sum(data$weight[data$female==1])
data$weight[data$female==0] = data$weight[data$female==0]/sum(data$weight[data$female==0])

X = data[, -c(1,2,3,20)]
X = apply(X, 2, as.numeric)
Y = data[, c(2,3)]
Y = apply(Y, 2, as.numeric)

mrf_fit <- mrf(X=X, Y=Y, splitting.rule = "fourier", num_features=3, node_scaling = FALSE, min.node.size = 20)

ggplot(data, aes(lnw, fill = as.factor(1-female))) + geom_density(alpha = 0.2, adjust=2)# + xlim(-0.5, 6)
ggplot(data, aes(x=lnw)) + 
  geom_histogram(data=subset(data, female == 1), fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(data, female == 0), fill = "blue", alpha = 0.2)

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
  out = paste(out, "exp1:", as.character(test_point[13]), "")
  out = paste(out, "exp2:", as.character(test_point[14]), "")
  out = paste(out, "exp3:", as.character(test_point[15]), "")
  
  return(out)
}

#random test_point
for(idx in sample(1:nrow(X), 10, replace=FALSE)){
  test_point = X[idx,]
  data$cond_weight = predict(mrf_fit, newdata=test_point)$weights[1,]
  data$cond_weight[data$female==1] = data$cond_weight[data$female==1]/sum(data$cond_weight[data$female==1])
  data$cond_weight[data$female==0] = data$cond_weight[data$female==0]/sum(data$cond_weight[data$female==0])
  gg = ggplot(data, aes(lnw, fill = as.factor(1-female), weight=cond_weight)) + geom_density(adjust=4, alpha = 0.2) + xlim(-0.5, 6)
  print(gg + ggtitle(point_description(test_point)))
}

#choose test_point
test_point = matrix(rep(0, 19), ncol=19)
names(test_point) = colnames(X)
test_point[1,13:16] = apply(X[,13:16],2, median) #experience
test_point[1,17] = 1 #married
test_point[1,11] = 1 #so
test_point[1,19] = 1 #sc = some college?

data$cond_weight = predict(mrf_fit, newdata=test_point)$weights[1,]
sum(data$cond_weight)
sum(data$cond_weight != 0 & data$female==1)
sum(data$cond_weight != 0 & data$female==0)

data$cond_weight[data$female==1] = data$cond_weight[data$female==1]/sum(data$cond_weight[data$female==1])
data$cond_weight[data$female==0] = data$cond_weight[data$female==0]/sum(data$cond_weight[data$female==0])
gg = ggplot(data, aes(lnw, fill = as.factor(1-female), weight=cond_weight)) + geom_density(adjust=5, alpha = 0.2) + xlim(-0.5, 6)  
print(gg + ggtitle(point_description(test_point)))
