genData <- function(dataset = "synthetic1", n = 5000, p = 10, meanShift = 1, sdShift = 1) {
  
  if (dataset == "synthetic1") {
    x <- runif(n,-1,1)
    y <- rnorm(n, mean = meanShift*(x > 0))
    X <- matrix(runif(n * p), ncol = p)
    X <- cbind(x,X)
    return(list(y=y,X=X))
  } else if (dataset == "synthetic2") {
    x <- runif(n,-1,1)
    y <- rnorm(n, sd = 1 + sdShift*(x > 0))
    X <- matrix(runif(n * p), ncol = p)
    X <- cbind(x,X)
    return(list(y=y,X=X))
  } else if (dataset == "synthetic3") {
    x <- runif(n,-1,1)
    y <- ifelse(x >= 0, rexp(n = n, 1), rnorm(n, 1, 1))
    X <- matrix(runif(n * p), ncol = p)
    X <- cbind(x,X)
    return(list(y=y,X=X))
  } else if (dataset == "synthetic4") {
    x <- runif(n)
    y <- sin(4*pi*x) + ifelse(x>=.5, rnorm(n), rnorm(n, sd=2))
    X <- matrix(runif(n * p), ncol = p)
    X <- cbind(x,X)
    return(list(y=y,X=X))
  } else if (dataset == "friedman1") {
    d <- mlbench::mlbench.friedman1(n,1)
    return(list(y=d$y,X=d$x))
  } else if (dataset == "friedman2") {
    d <- mlbench::mlbench.friedman2(n,1)
    return(list(y=d$y,X=d$x))
  } else if (dataset == "friedman3") {
    d <- mlbench::mlbench.friedman3(n,1)
    return(list(y=d$y,X=d$x))
  } else if (dataset == "Abalone") {
    data("abalone", package = "AppliedPredictiveModeling")
    response <- "Rings"
    abalone[[response]] <- as.numeric(abalone[[response]])
    return(list(y=abalone$Rings, X=model.matrix(~.-Rings-1, data = abalone)))
  } else if (dataset == "Boston") {
    data("BostonHousing2", package = "mlbench")
    response <- "cmedv"
    BostonHousing2[[response]] <- as.numeric(BostonHousing2[[response]])
    return(list(y=BostonHousing2$cmedv,X=model.matrix(~crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + b + lstat-1, data = BostonHousing2)))
  } else if (dataset == "BigMac") {
    data("BigMac2003", package = "alr3")
    response <- "BigMac"
    BigMac2003[[response]] <- as.numeric(BigMac2003[[response]])
    return(list(y=BigMac2003$BigMac,X=model.matrix(~Bread + Rice + FoodIndex + Bus + Apt + TeachGI + 
                                                     TeachNI + TaxRate + TeachHours-1, BigMac2003)))
  } else if (dataset == "Ozone") {
    data("Ozone", package = "mlbench")
    Ozone <- subset(Ozone, complete.cases(Ozone))
    Ozone <- as.data.frame(lapply(Ozone, function(x) {
      x <- x[, drop = TRUE]
      if (is.factor(x)) return(as.ordered(x))
      x
    }))
    response <- "V4"
    Ozone[[response]] <- as.numeric(Ozone[[response]])
    return(list(y=Ozone$V4 ,X=model.matrix(~V1 + V2 + V3 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13-1, Ozone)))
  }
}

