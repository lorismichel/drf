# distributional random forests ![logoDRF](https://https://github.com/lorismichel/drf/tree/master/experiments/copula_examples/plots/PLOT_COPULA_HSIC_SC_1.png)

[![Build Status](https://travis-ci.com/lorismichel/drf.svg?branch=master)](https://travis-ci.com/lorismichel/drf)

A package for forest-based conditional distribution estimation in presence of potentially multivariate outcomes. 

This repository started as a fork from [grf](https://github.com/grf-labs/grf) itself forked from [ranger](https://github.com/imbs-hl/ranger) repository -- we owe a great deal of thanks to both the authors of the drf and the ranger authors for their useful and free package.

### Installation

The latest release of the package can be installed through CRAN:

```R
install.packages("drf")
```

Any published release can also be installed from source:

```R
install.packages("https://raw.github.com/lorismichel/drf/master/releases/drf_1.0.0.tar.gz", repos = NULL, type = "source")
```

Note that to install from source, a compiler that implements C++11 is required (clang 3.3 or higher, or g++ 4.8 or higher). If installing on Windows, the RTools toolchain is also required.


### Usage Examples

The following script demonstrates how to use DRF for conditional distribution estimation. 

```R
## example with a bivariate response.

require(drf)

# API check
require(drf)

# data
X <- matrix(rnorm(2*200), ncol=2)
Y <- X + matrix(rnorm(2*200), ncol=2)
Xtest <- matrix(rnorm(2*200), ncol=2)

# fit prediction
d <- drf(X = X, Y = Y, num.trees = 500, splitting.rule = "FourierMMD", num.features = 10)

# weights from the conditional distribution
predict(d, newdata = Xtest)

# cond mean
predict(d, newdata = X, functional = "mean")
predict(d, newdata = X, functional = "mean", transformation = function(y) c(y[1],y[2]^2))

# cond sd
predict(d, newdata = X, functional = "sd")
predict(d, newdata = X, functional = "sd", transformation = function(y) c(y[1],y[2]^2))

# cond quantiles
predict(d, newdata = X, functional = "quantile", quantiles = c(0.1, 0.9))
predict(d, newdata = X, functional = "quantile", transformation = function(y) c(y[1],y[2]^2), 
        quantiles = c(0.1, 0.9))

# cor
predict(d, newdata = X, functional = "cor")
predict(d, newdata = X, functional = "cor",
        transformation = function(y) c(y[1], y[2]^2))

# cov
predict(d, newdata = X, functional = "cov")
predict(d, newdata = X, functional = "cov",
        transformation = function(y) c(y[1], y[2]^2))

# normalPredictionScore
predict(d, newdata = X, functional = "normalPredictionScore")
predict(d, newdata = X, functional = "normalPredictionScore",
        transformation = function(y) c(y[1], y[2]^2))

# cdf
predict(d, newdata = X, functional = "cdf")
predict(d, newdata = X, functional = "cdf",
        transformation = function(y) c(y[1]))
```
