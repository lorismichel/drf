# Soil data analysis

# libs
require(data.table)
require(mrf)
require(ggplot2)

# repro
set.seed(1)

# loading the data
soil <- fread("~/Downloads/data_for_ETH.csv")
dim(soil)


# defining responses and covariates
X <- as.matrix(soil[,-c(1:4)])
Y <- matrix(cbind(soil$ph0_30, soil$sand0_30, soil$silt0_30),ncol=3)

# fitting mrf
mRF_fourier <- mrf(X = X, Y = Y, num.trees = 2000, splitting.rule = "fourier", num_features = 3)
mRF_gini <- mrf(X = X, Y = Y, num.trees = 2000, splitting.rule = "gini")

# variable importance
plot(factor(colnames(X)),mrf::variable_importance(RF))

## summary of Y
summary(Y)

## OOB
wOOB_fourier <- mrf::get_sample_weights(forest = mRF_fourier)
wOOB_gini <- mrf::get_sample_weights(forest = mRF_gini)

# given 
getRegionProb <- function(w, y, t = c(8, 200, 200), direction = c(">", ">", ">")) {
  text <- paste0("y[,", 1:ncol(y), "]", direction, t)
  b <- c()
  for (i in 1:ncol(y)) {
    b <- cbind(b, eval(parse(text=text[i])))
  }
  ind <- apply(b, 1, function(bb) sum(bb)==length(bb))
  apply(w, 1, function(ww) sum(ww[ind]))
}

# 
cor(Y)




# look at correlation matrix
getCondCorr <- function(y, w, id1 = 1, id2 = 2){
  require(wCorr)
  ret_corr <- numeric(nrow(w))
  for(i in 1:nrow(w)){
    ret_corr[i] = weightedCorr(y[,id1], y[,id2], weights=w[i,])
  }
  return(ret_corr)
}


getCondMean <- function(y, w){
  require(wCorr)
  cond_mean <- matrix(nrow=nrow(y), ncol=ncol(y))
  for(i in 1:nrow(w)){
    cond_mean[i,] = apply(y,2,function(yy) sum(yy*w[i,]))
  }
  return(cond_mean)
}



condCorr_12 <- getCondCorr(y = Y, w = wOOB, id1 = 1, id2 = 2)
condCorr_13 <- getCondCorr(y = Y, w = wOOB, id1 = 1, id2 = 3)
condCorr_23 <- getCondCorr(y = Y, w = wOOB, id1 = 2, id2 = 3)
soil$condCorr_12 <- condCorr_12
soil$condCorr_13 <- condCorr_13
soil$condCorr_23 <- condCorr_23

condMeans <- getCondMean(y = Y, w = wOOB)
soil$condMean_1 <- condMeans[,1]
soil$condMean_2 <- condMeans[,2]
soil$condMean_3 <- condMeans[,3]

condSds <- sqrt(getCondMean(y = Y^2, w = wOOB) - (condMeans^2))
soil$condSd_1 <- condSds[,1]
soil$condSd_2 <- condSds[,2]
soil$condSd_3 <- condSds[,3]

m1 <- ggplot(soil, aes(x = X, y = Y)) +
  #geom_polygon(aes(group = group), alpha=0.3) +
  geom_point(data=soil, size=0.7, aes(x=X, y=Y, color=condMean_1)) + 
  scale_color_viridis_c(option='magma') +
  # coord_cartesian(xlim=c(-170, -60)) +
  theme_bw()

m2 <- ggplot(soil, aes(x = X, y = Y)) +
  #geom_polygon(aes(group = group), alpha=0.3) +
  geom_point(data=soil, size=0.7, aes(x=X, y=Y, color=condMean_2)) + 
  scale_color_viridis_c(option='magma') +
  # coord_cartesian(xlim=c(-170, -60)) +
  theme_bw()

m3 <- ggplot(soil, aes(x = X, y = Y)) +
  #geom_polygon(aes(group = group), alpha=0.3) +
  geom_point(data=soil, size=0.7, aes(x=X, y=Y, color=condMean_3)) + 
  scale_color_viridis_c(option='magma') +
  # coord_cartesian(xlim=c(-170, -60)) +
  theme_bw()

s1 <- ggplot(soil, aes(x = X, y = Y)) +
  #geom_polygon(aes(group = group), alpha=0.3) +
  geom_point(data=soil, size=0.7, aes(x=X, y=Y, color=condSd_1)) + 
  scale_color_viridis_c(option='magma') +
  # coord_cartesian(xlim=c(-170, -60)) +
  theme_bw()

s2 <- ggplot(soil, aes(x = X, y = Y)) +
  #geom_polygon(aes(group = group), alpha=0.3) +
  geom_point(data=soil, size=0.7, aes(x=X, y=Y, color=condSd_2)) + 
  scale_color_viridis_c(option='magma') +
  # coord_cartesian(xlim=c(-170, -60)) +
  theme_bw()

s3 <- ggplot(soil, aes(x = X, y = Y)) +
  #geom_polygon(aes(group = group), alpha=0.3) +
  geom_point(data=soil, size=0.7, aes(x=X, y=Y, color=condSd_3)) + 
  scale_color_viridis_c(option='magma') +
  # coord_cartesian(xlim=c(-170, -60)) +
  theme_bw()

c1 <- ggplot(soil, aes(x = X, y = Y)) +
  #geom_polygon(aes(group = group), alpha=0.3) +
  geom_point(data=soil, size=0.7, aes(x=X, y=Y, color=condCorr_12)) + 
  scale_color_viridis_c(option='magma') +
  # coord_cartesian(xlim=c(-170, -60)) +
  theme_bw()

c2 <- ggplot(soil, aes(x = X, y = Y)) +
  #geom_polygon(aes(group = group), alpha=0.3) +
  geom_point(data=soil, size=0.7, aes(x=X, y=Y, color=condCorr_13)) + 
  scale_color_viridis_c(option='magma') +
  # coord_cartesian(xlim=c(-170, -60)) +
  theme_bw()


c3 <- ggplot(soil, aes(x = X, y = Y)) +
  #geom_polygon(aes(group = group), alpha=0.3) +
  geom_point(data=soil, size=0.7, aes(x=X, y=Y, fill=condCorr_23)) +
  #scale_color_gradient() +
  geom_raster(aes(fill = condCorr_23)) +
  scale_color_viridis_c(option='magma') +
  # coord_cartesian(xlim=c(-170, -60)) +
  theme_bw()

require(gridExtra)
grid.arrange(m1, m2, m3,
             s1, s2, s3,
             c1, c2, c3,  
            # layout_matrix = rbind(c(1, 3, 4),
            #                       c(4, 5, 6)),
             nrow = 3, ncol=3)


# comparing with independent fits

# gini
RF_gini_1 <- mrf(X = X, Y = Y[,1,drop=FALSE], num.trees = 2000, splitting.rule = "gini")
RF_gini_2 <- mrf(X = X, Y = Y[,2,drop=FALSE], num.trees = 2000, splitting.rule = "gini")
RF_gini_3 <- mrf(X = X, Y = Y[,3,drop=FALSE], num.trees = 2000, splitting.rule = "gini")

# fourier
RF_fourier_1 <- mrf(X = X, Y = Y[,1,drop=FALSE], num.trees = 2000, splitting.rule = "fourier", num_features = 3)
RF_fourier_2 <- mrf(X = X, Y = Y[,2,drop=FALSE], num.trees = 2000, splitting.rule = "fourier", num_features = 3)
RF_fourier_3 <- mrf(X = X, Y = Y[,3,drop=FALSE], num.trees = 2000, splitting.rule = "fourier", num_features = 3)


# get weigths OOB marginally
wOOB_gini_1 <- get_sample_weights(RF_gini_1)
wOOB_gini_2 <- get_sample_weights(RF_gini_2)
wOOB_gini_3 <- get_sample_weights(RF_gini_3)

wOOB_fourier_1 <- get_sample_weights(RF_fourier_1)
wOOB_fourier_2 <- get_sample_weights(RF_fourier_2)
wOOB_fourier_3 <- get_sample_weights(RF_fourier_3)


th <- c(quantile(soil$ph0_30, 0.5),quantile(soil$sand0_30, 0.6),quantile(soil$silt0_30, 0.5))
directions <- c(">", ">", ">")

pR_gini_1 <- getRegionProb(w = wOOB_gini_1, y = Y[,1,drop=FALSE], t = th[1], direction = directions[1])
pR_gini_2 <- getRegionProb(w = wOOB_gini_1, y = Y[,2,drop=FALSE], t = th[2], direction = directions[2])
pR_gini_3 <- getRegionProb(w = wOOB_gini_1, y = Y[,3,drop=FALSE], t = th[3], direction = directions[3])

pR_fourier_1 <- getRegionProb(w = wOOB_fourier_1, y = Y[,1,drop=FALSE], t = th[1], direction = directions[1])
pR_fourier_2 <- getRegionProb(w = wOOB_fourier_1, y = Y[,2,drop=FALSE], t = th[2], direction = directions[2])
pR_fourier_3 <- getRegionProb(w = wOOB_fourier_1, y = Y[,3,drop=FALSE], t = th[3], direction = directions[3])


pR_gini <- pR_gini_1 * pR_gini_2* pR_gini_3
pR_fourier <- pR_fourier_1 * pR_fourier_2* pR_fourier_3





# plotting resulting probabilitie
soil$prob_mrf_fourier <- getRegionProb(wOOB_fourier, Y, t = th, direction = c(">",">",">"))
soil$prob_mrf_gini <- getRegionProb(wOOB_gini, Y, t = th, direction = c(">",">",">"))
soil$prob_indep_gini <- pR_gini
soil$prob_indep_fourier <- pR_fourier

pt_mrf_fourier <- ggplot(soil, aes(x = X, y = Y)) +
  #geom_polygon(aes(group = group), alpha=0.3) +
  geom_point(data=soil, size=0.7, aes(x=X, y=Y, color=prob_mrf_fourier)) + 
  scale_color_viridis_c(option='magma') +
  # coord_cartesian(xlim=c(-170, -60)) +
  theme_bw()

pt_mrf_gini <- ggplot(soil, aes(x = X, y = Y)) +
  #geom_polygon(aes(group = group), alpha=0.3) +
  geom_point(data=soil, size=0.7, aes(x=X, y=Y, color=prob_mrf_gini)) + 
  scale_color_viridis_c(option='magma') +
  # coord_cartesian(xlim=c(-170, -60)) +
  theme_bw()

pt_indep_gini <- ggplot(soil, aes(x = X, y = Y)) +
  #geom_polygon(aes(group = group), alpha=0.3) +
  geom_point(data=soil, size=0.7, aes(x=X, y=Y, color=prob_indep_gini)) + 
  scale_color_viridis_c(option='magma') +
  # coord_cartesian(xlim=c(-170, -60)) +
  theme_bw()

pt_indep_fourier <- ggplot(soil, aes(x = X, y = Y)) +
  #geom_polygon(aes(group = group), alpha=0.3) +
  geom_point(data=soil, size=0.7, aes(x=X, y=Y, color=prob_indep_fourier)) + 
  scale_color_viridis_c(option='magma') +
  # coord_cartesian(xlim=c(-170, -60)) +
  theme_bw()

require(gridExtra)
grid.arrange(pt_indep_gini, pt_indep_fourier, pt_mrf,
             pt_mrf_gini,nrow=2,ncol=2)
             # layout_matrix = rbind(c(1, 3, 4),

plot(soil$prob_mrf_fourier,soil$prob_indep_fourier,pch=19)
abline(0,1,col="blue")
plot(soil$prob_mrf_fourier,soil$prob_mrf_gini,pch=19)
abline(0,1,col="blue")
plot(soil$prob_mrf_fourier,soil$prob_indep_fourier,pch=19)
abline(0,1,col="blue")
