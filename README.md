# Distributional Random Forests <a href='https://github.com/lorismichel/drf/blob/master/experiments/DRFlogo.png'><img src='https://github.com/lorismichel/drf/blob/master/DRFlogo.png' align="right" height="160" /></a>

[![Build Status](https://travis-ci.com/lorismichel/drf.svg?branch=master)](https://travis-ci.com/lorismichel/drf)

A package for forest-based conditional distribution estimation of a possibly multivariate response. The estimated distribution is in a simple form which allows for simple and fast computation of different functionals of the conditional distributions such as, for example, conditional quantiles, conditional correlations or conditional probability statements. One can do a heterogeneity adjustment with DRF by obtaining the weighting function which describes the relevance of each training point for a given test point and which can further be used as an input to some other method.

This repository started as a fork from the [grf](https://github.com/grf-labs/grf) repository, which is itself forked from [ranger](https://github.com/imbs-hl/ranger) repository. We sincerely thank the authors of both repositories for their useful and free packages.

### Installation

The latest release of the package can be installed through CRAN (soon):

```R
install.packages("drf")
```

The development version can be installed from github

```R
devtools::install_github("lorismichel/drf",subdir = "r-package/drf")
```

Another installation possibility is to clone the repo, and then within the r-package folder run

```R
Rscript build_package.R
```

Note that to install from source, a compiler that implements C++11 is required (clang 3.3 or higher, or g++ 4.8 or higher). If installing on Windows, the RTools toolchain is also required.


### Usage Example
```R
require(drf)

# generate data
n = 1000
p = 10
d = 2
X <- matrix(rnorm(n*p), ncol=p)
Y <- matrix(rnorm(n*d), ncol=d)
Y[,1] = Y[,1] + X[,1] #mean shift of Y1 based on X1
Y[,2] = Y[,2] * X[,2] #variance shift of Y2 based on X2

# fit model
fit <- drf(X = X, Y = Y, num.trees = 2000, splitting.rule = "FourierMMD") #those are the default values
fit #prints variable importance

#generate test data
X_test <- matrix(rnorm(100*p), ncol=p)

# estimated conditional distribution represented via weights
predict(fit, newdata = X_test)

# many distributional functionals are implemented and do not need to be manually computed from the weights  
predict(fit, newdata = X_test, functional = "mean")

# covariance matrix at a fixed test point
predict(fit, newdata = rep(0, p), functional = "cov")$cov[1,,]

# we can transform the response beforehand to obtain more complicated quantities 
predict(fit, newdata = X_test, functional = "quantile", quantiles=c(0.1, 0.9), transformation = function(y) c(sin(y[1]), y[1]*y[2], y[2]^2))

```
