# mrf: multivariate random forests

A package for forest-based estimation in presence of multivariate outcomes.
Some helpful links for getting started:

The repository started as a fork from [grf](https://github.com/grf-labs/grf) itself forked from [ranger](https://github.com/imbs-hl/ranger) repository -- we owe a great deal of thanks to both the authors of the grf and the ranger authors for their useful and free package.

### Installation

The latest release of the package can be installed through CRAN:

```R
install.packages("mrf")
```

Any published release can also be installed from source:

```R
install.packages("https://raw.github.com/grf-labs/grf/master/releases/mrf_1.0.0.tar.gz", repos = NULL, type = "source")
```

Note that to install from source, a compiler that implements C++11 is required (clang 3.3 or higher, or g++ 4.8 or higher). If installing on Windows, the RTools toolchain is also required.


### Usage Examples

The following script demonstrates how to use GRF for heterogeneous treatment effect estimation. For examples
of how to use types of forest, as for quantile regression and causal effect estimation using instrumental
variables, please consult the R [documentation](https://grf-labs.github.io/grf/reference/index.html) on the relevant forest methods (quantile_forest, instrumental_forest, etc.).

```R
## examples of uses

# univariate mean shift
require(mrf)
require(ranger)
require(grf)
require(spatstat)

set.seed(0)
n <- 1000
X <- matrix(rnorm(20*n),ncol=20,nrow=n)
Y <- matrix(nrow=n, ncol=1)
for (i in 1:n) {
  Y[i,] <- ifelse(X[i,1]>0, rnorm(n = 1, mean = 0), rnorm(n = 1, mean = 2))
}

system.time({rf_ranger <- ranger(formula = y~., data = data.frame(y=Y,x=X),
                                            num.trees = 500)})
                                            
system.time({rf_grf <- regression_forest(X = X, Y = Y, 
                                            num.trees = 500)})
system.time({rf_gini <- mrf(X = X, Y = Y, 
                            num.trees = 500,
                            splitting.rule = "gini")})
system.time({rf_fourier <- mrf(X = X, Y = Y,
                                             num.trees = 500, 
                                             splitting.rule = "fourier", 
                                             num_features = 50)})

# investigate the behaviour
par(mfrow=c(5,1))
plot(X[,1],Y, main="raw data")

plot(X[,1],predict(rf_ranger, data = data.frame(x=X))$predictions, main="ranger")

wtraining_grf <- get_sample_weights(newdata =  X, forest = rf_grf)
plot(X[,1],apply(wtraining_grf, 1, function(w) sum(w*Y)), main="grf")

wtraining_fourier <- get_sample_weights(newdata =  X, forest = rf_fourier)
plot(X[,1],apply(wtraining_fourier, 1, function(w) sum(w*Y)), main="fourier")

wtraining_gini <- get_sample_weights(newdata =  X, forest = rf_gini)
plot(X[,1],apply(wtraining_gini, 1, function(w) sum(w*Y)), main="gini")


# univariate sd shift
require(grf)
require(ranger)

set.seed(0)
n <- 1000
X <- matrix(rnorm(20*n),ncol=20,nrow=n)
Y <- matrix(nrow=n, ncol=1)
for (i in 1:n) {
  Y[i,] <- ifelse(X[i,1]>0, rnorm(n = 1, sd = 1), rnorm(n = 1, sd = 2))
}

system.time({rf_ranger <- ranger(formula = y~., data = data.frame(y=Y,x=X),
                                 num.trees = 500, quantreg = TRUE)})
system.time({rf_grf <- quantile_forest(X = X, Y = Y, 
                                          num.trees = 500)})                                 
system.time({rf_gini <- mrf(X = X, Y = Y, 
                                          num.trees = 500,
                                          splitting.rule = "gini")})
system.time({rf_fourier <- mrf(X = X, Y = Y,
                                             num.trees = 500, 
                                             splitting.rule = "fourier", 
                                             num_features = 50)})

# investigate the behaviour
require(spatstat)
wtraining_gini <- get_sample_weights(newdata =  X,forest = rf_gini)
wtraining_fourier <- get_sample_weights(newdata =  X,forest = rf_fourier)

l_gini <- apply(wtraining_gini, 1, function(w) ewcdf(x = Y[,1], weights = w))
l_fourier <- apply(wtraining_fourier, 1, function(w) ewcdf(x = Y[,1], weights = w))
par(mfrow=c(3,1))

par(mfrow=c(5,1))
plot(X[,1], Y, main="raw data",pch=19)
plot(X[,1], predict(rf_ranger, data = data.frame(x=X),type = "quantiles", quantiles = 0.8)$predictions,pch=19,main="ranger", ylim=c(0.5,2.5))
plot(X[,1], predict(rf_grf, X, quantiles = 0.8)[,1],main="grf",pch=19, ylim=c(0.5,2.5))
plot(X[,1],unlist(lapply(l_fourier, function(ll) quantile(ll, 0.8))),pch=19, main="fourier", ylim=c(0.5,2.5))
plot(X[,1],unlist(lapply(l_gini, function(ll) quantile(ll, 0.8))),pch=19, main="gini", ylim=c(0.5,2.5))



# shift in the dependence gaussian
require(mrf)
require(ranger)

set.seed(0)
n <- 1000
X <- matrix(rnorm(20*n),ncol=20,nrow=n)
Y <- matrix(nrow=n, ncol=2)
for (i in 1:n) {
  Y[i,] <- if (X[i,1]>0) MASS::mvrnorm(n = 1, mu = c(0,0), Sigma = (diag(0.5,2)+matrix(0.5,ncol=2,nrow=2))) else MASS::mvrnorm(n = 1, mu = c(0,0), Sigma = (diag(1.5,2)+matrix(-0.5,ncol=2,nrow=2)))
}
#Y <- cbind(Y, matrix(rnorm(8*n),ncol=8))

system.time({list.forests <- list();
             for (i in 1:2) {
               list.forests[[i]] <- ranger(formula = y~., data = data.frame(y=Y[,i], x=X),num.trees = 500, quantreg = TRUE)}
            })
system.time({rf_gini <- mrf(X = X, Y = Y,  num.trees = 500, splitting.rule = "gini")})
system.time({rf_fourier <- mrf(X = X, Y = Y,
                                num.trees = 500, 
                                splitting.rule = "fourier", 
                                num_features = 100)})
rf_gini
rf_fourier

# investigate the behaviour
par(mfrow=c(2,1))
plot(Y[X[,1]>0,],pch=19)
plot(Y[X[,1]<=0,],pch=19)

require(spatstat)
wtraining_gini <- get_sample_weights(newdata =  X,forest = rf_gini)
wtraining_fourier <- get_sample_weights(newdata =  X,forest = rf_fourier)

Ysample1_gini <- Y[sample(1:n, prob = wtraining_gini[,1]/sum(wtraining_gini[,1]), size = 1000, replace = TRUE),]
Ysample2_gini <- Y[sample(1:n, prob = wtraining_gini[,2]/sum(wtraining_gini[,2]), size = 1000, replace = TRUE),]

plot(Ysample1_gini[,1:2], pch=19)
plot(Ysample2_gini[,1:2], pch=19)
cor(Ysample1_gini)
cor(Ysample2_gini)

Ysample1_fourier <- Y[sample(1:n, prob = wtraining_fourier[,1]/sum(wtraining_fourier[,1]), size = 1000, replace = TRUE),]
Ysample2_fourier <- Y[sample(1:n, prob = wtraining_fourier[,2]/sum(wtraining_fourier[,2]), size = 1000, replace = TRUE),]

cor(Ysample1_fourier)
cor(Ysample2_fourier)
plot(Ysample1_fourier[,1:2], pch=19)
plot(Ysample2_fourier[,1:2], pch=19)
```
