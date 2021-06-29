# API check
require(drf)

# data
X <- matrix(rnorm(2*200), ncol=2)
Y <- matrix(rnorm(2*200), ncol=2)

# fit prediction
d <- drf(X = X, Y = Y, num.trees = 500, splitting.rule = "fastMMD")


# weights
predict(d, newdata = X)

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
