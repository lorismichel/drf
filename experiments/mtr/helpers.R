# helpers for mtr

# libs 
require(foreign)
require(KernelKnn)
require(KRLS)
require(parallel)

# atp1d and atp2d
# The Airline Ticket Price dataset concerns the prediction of airline ticket prices. The rows
# are a sequence of time-ordered observations over several days. Each sample in this dataset
# represents a set of observations from a specific observation date and departure date pair. The
# input variables for each sample are values that may be useful for prediction of the airline ticket
# prices for a specific departure date. The target variables in these datasets are the next day
# (ATP1D) price or minimum price observed over the next 7 days (ATP7D) for 6 target flight
# preferences: (1) any airline with any number of stops, (2) any airline non-stop only, (3) Delta
# Airlines, (4) Continental Airlines, (5) Airtrain Airlines, and (6) United Airlines. The input
# variables include the following types: the number of days between the observation date and
# the departure date (1 feature), the boolean variables for day-of-the-week of the observation
# date (7 features), the complete enumeration of the following 4 values: (1) the minimum price,
# mean price, and number of quotes from (2) all airlines and from each airline quoting more
# than 50% of the observation days (3) for non-stop, one-stop, and two-stop flights, (4) for the
# current day, previous day, and two days previous. The result is a feature set of 411 variables.
# For specific details on how these datasets are constructed please consult Groves and Gini
# (2015). The nature of these datasets is heterogeneous with a mixture of several types of
# variables including boolean variables, prices, and counts.


# edm
# The Electrical Discharge Machining dataset (Karalic and Bratko 1997) represents a two-target
# regression problem. The task is to shorten the machining time by reproducing the behaviour
# of a human operator that controls the values of two variables. Each of the target variables
# takes 3 distinct numeric values ({−1, 0, 1}) and there are 16 continuous input variables.

# sf1 and sf2
# The Solar Flare dataset (Lichman 2013) has 3 target variables that correspond to the number
# of times 3 types of solar flare (common, moderate, severe) are observed within 24 h. There
# are two versions of this dataset. SF1 contains data from year 1969 and SF2 from year 1978.

# jura
# The Jura (Goovaerts 1997) dataset consists of measurements of concentrations of seven heavy
# metals (cadmium, cobalt, chromium, copper, nickel, lead, and zinc), recorded at 359 locations
# 123
# Mach Learn (2016) 104:55–98 83
# in the topsoil of a region of the Swiss Jura. The type of land use (Forest, Pasture, Meadow,
# Tillage) and rock type (Argovian, Kimmeridgian, Sequanian, Portlandian, Quaternary) were
# also recorded for each location. In a typical scenario (Goovaerts 1997; Álvarez and Lawrence
# 2011), we are interested in the prediction of the concentration of metals that are more expensive to measure 
# (primary variables) using measurements of metals that are cheaper to sample
# (secondary variables). In this study, cadmium, copper and lead are treated as target variables
# while the remaining metals along with land use type, rock type and the coordinates of each
# location are used as predictive features


# wq
# The Water Quality dataset (Dzeroski et al. 2000) has 14 target attributes that refer to the
# relative representation of plant and animal species in Slovenian rivers and 16 input attributes
# that refer to physical and chemical water quality parameters.

# enb
# The Energy Building dataset (Tsanas and Xifara 2012) concerns the prediction of the heating
# load and cooling load requirements of buildings (i.e. energy efficiency) as a function of eight
# building parameters such as glazing area, roof area, and overall height, amongst others.


# slumb
# The Concrete Slump dataset (Yeh 2007) concerns the prediction of three properties of concrete (slump, flow and compressive strength) as a function of the content of seven concrete
# ingredients: cement,fly ash, blast furnace slag, water, superplasticizer, coarse aggregate, and
# fine aggregate.

# andro
# The Andromeda dataset (Hatzikos et al. 2008) concerns the prediction of future values for
# six water quality variables (temperature, pH, conductivity, salinity, oxygen, turbidity) in
# Thermaikos Gulf of Thessaloniki, Greece. Measurements of the target variables are taken
# from under-water sensors with a sampling interval of 9 seconds and then averaged to get a
# single measurement for each variable over each day. The specific dataset that we use here
# corresponds to using a window of 5 days (i.e. features attributes correspond to the values of
# the six water quality variables up to 5 days in the past) and a lead of 5 days (i.e. we predict the values of each variable 6 days ahead)


# edm
# The Electrical Discharge Machining dataset (Karalic and Bratko 1997) represents a two-target
# regression problem. The task is to shorten the machining time by reproducing the behaviour
# of a human operator that controls the values of two variables. Each of the target variables
# takes 3 distinct numeric values ({−1, 0, 1}) and there are 16 continuous input variables.


# wq
# The Water Quality dataset (Dzeroski et al. 2000) has 14 target attributes that refer to the
# relative representation of plant and animal species in Slovenian rivers and 16 input attributes
# that refer to physical and chemical water quality parameters.
require(copula)


loadMTRdata <- function(dataset.name = "atp1d", path = '~/Downloads/mtr-datasets/') {
  if (!dataset.name %in% c("example1", "example2", "air1", "air2", "wage", "births1", "births2")) {
    dataset <- read.arff(file = paste0(path, dataset.name, ".arff"))
  }
  
  if (dataset.name == "air1") {
    load(paste0(path, 'air_data_benchmark.Rdata'))
    set.seed(0)
    #ids <- 1:nrow(X)
    ids <- sample(1:nrow(X), size = 10000, replace = FALSE)
    return(list(X = as.matrix(X[ids,]), X.knn = scale(as.matrix(X[ids,])), X.gauss = scale(as.matrix(X[ids,])), Y = as.matrix(Y[ids,])))
  } else if (dataset.name == "air2") {
    load(paste0(path, 'air_data_benchmark2.Rdata'))
    set.seed(0)
    #ids <- 1:nrow(X)
    ids <- sample(1:nrow(X), size = 5000, replace = FALSE)
    return(list(X = as.matrix(X[ids,]), X.knn = scale(as.matrix(X[ids,])), X.gauss = scale(as.matrix(X[ids,])), Y = as.matrix(Y[ids,])))
  } else if (dataset.name == "wage") {
    load(paste0(path, 'wage_benchmark.Rdata'))
    set.seed(0)
    ids <- sample(1:nrow(X), size = 10000, replace = FALSE)
    X <- X[ids,]
    Y <- Y[ids,]
    nb.unique <- apply(X, 2,function(x) length(unique(x)))
    X <- X[,nb.unique != 1]
    #ids <- 1:nrow(X)
    
    return(list(X = as.matrix(X), X.knn = scale(as.matrix(X)), X.gauss = scale(as.matrix(X)), Y = as.matrix(Y)))
  } else if (dataset.name == "births1") {
    load(paste0(path, 'births_benchmark1.Rdata'))
    set.seed(0)
    nb.unique <- apply(X, 2,function(x) length(unique(x)))
    X <- X[,nb.unique != 1]
    #ids <- 1:nrow(X)
    ids <- sample(1:nrow(X), size = 10000, replace = FALSE)
    return(list(X = as.matrix(X[ids,]), X.knn = scale(as.matrix(X[ids,])), X.gauss = scale(as.matrix(X[ids,])), Y = as.matrix(Y[ids,])))
  } else if (dataset.name == "births2") {
    load(paste0(path, 'births_benchmark2.Rdata'))
    set.seed(0)
    nb.unique <- apply(X, 2,function(x) length(unique(x)))
    X <- X[,nb.unique != 1]
    #ids <- 1:nrow(X)
    ids <- sample(1:nrow(X), size = 10000, replace = FALSE)
    return(list(X = as.matrix(X[ids,]), X.knn = scale(as.matrix(X[ids,])), X.gauss = scale(as.matrix(X[ids,])), Y = as.matrix(Y[ids,])))
  } else if (dataset.name == "enb") {
    names.dataset <- c("Relative Compactness",
                       "Surface Area",
                       "Wall Area",
                       "Roof Area",
                       "Overall Height",
                       "Orientation",
                       "Glazing Area",
                       "Glazing Area Distribution",
                       "Heating Load",
                       "Cooling Load")
    
    names(dataset) <- names.dataset
    
    return(list(X = as.matrix(dataset[,-c(9:10)]), X.knn = scale(as.matrix(dataset[,-c(9:10)])), X.gauss = scale(as.matrix(dataset[,-c(9:10)])), Y = as.matrix(dataset[,c(9:10)])))
  } else if (dataset.name == "oes97") {
    
  } else if (dataset.name == "oes10") {
    
  } else if (dataset.name == "sf1") {
    dataset$area_largest <- NULL
    dataset$`c-class` <-  as.numeric(dataset$`c-class`)
    dataset$`m-class` <-  as.numeric(dataset$`m-class`)
    dataset$`x-class` <-  as.numeric(dataset$`x-class`)
    dataset <- data.frame(dataset, stringsAsFactors = TRUE)
    X <- model.matrix(~.-1, data = dataset[,-c(10:12)])
    Y <- dataset[,c(10:12)]
    return(list(X = X, X.knn = X, X.gauss = X, Y = Y))
  } else if (dataset.name == "sf2") {
    dataset$area_largest <- NULL
    dataset$`c-class` <-  as.numeric(dataset$`c-class`)
    dataset$`m-class` <-  as.numeric(dataset$`m-class`)
    dataset$`x-class` <-  as.numeric(dataset$`x-class`)
    dataset <- data.frame(dataset, stringsAsFactors = TRUE)
    X <- model.matrix(~.-1, data = dataset[,-c(10:12)])
    Y <- dataset[,c(10:12)]
    return(list(X = X, X.knn = X, X.gauss = X, Y = Y))
  } else if (dataset.name == "scpf") {
      dataset <- na.omit(dataset)
      # could we do it better?
      return(list(X = as.matrix(dataset[,c("daysUntilLastIssue", "latitude", "longitude", "distanceFromCenter", "city=Oakland",
                                           "city=Chicago", "city=NH", "city=Richmond")]), X.knn = scale(as.matrix(dataset[,c("daysUntilLastIssue", "latitude", "longitude", "distanceFromCenter", "city=Oakland",
                                                                                                                             "city=Chicago", "city=NH", "city=Richmond")])), X.gauss = scale(as.matrix(dataset[,c("daysUntilLastIssue", "latitude", "longitude", "distanceFromCenter", "city=Oakland",
                                                                                                                                                                                                                  "city=Chicago", "city=NH", "city=Richmond")])), Y = as.matrix(dataset[,c(24:26)])))
  } else if (dataset.name == "osales") {
    # could we do it better?
    nb.NA <- apply(dataset,2,function(x) sum(is.na(x)))
    dataset <- dataset[,nb.NA == 0]
    return(list(X = as.matrix(dataset[,c(names(dataset)[!grepl(names(dataset), pattern = "Outcome")])]), Y = as.matrix(dataset[,names(dataset)[grepl(names(dataset), pattern = "Outcome")]])))
  } else if (dataset.name == "RF1") {
    # could we do it better?
    dataset <- na.omit(dataset)
    return(list(X = as.matrix(dataset[,c(names(dataset)[!grepl(names(dataset), pattern = "48H")])]), X.knn = scale(as.matrix(dataset[,c(names(dataset)[!grepl(names(dataset), pattern = "48H")])])), X.gauss = scale(as.matrix(dataset[,c(names(dataset)[!grepl(names(dataset), pattern = "48H")])])), Y = as.matrix(dataset[,names(dataset)[grepl(names(dataset), pattern = "48H")]])))
  } else if (dataset.name == "RF2") {
    # could we do it better?
    dataset <- na.omit(dataset)
    return(list(X = as.matrix(dataset[,c(names(dataset)[!grepl(names(dataset), pattern = "48H")])]), X.knn = scale(as.matrix(dataset[,c(names(dataset)[!grepl(names(dataset), pattern = "48H")])])), X.gauss = scale(as.matrix(dataset[,c(names(dataset)[!grepl(names(dataset), pattern = "48H")])])), Y = as.matrix(dataset[,names(dataset)[grepl(names(dataset), pattern = "48H")]])))
  } else if (dataset.name == "slump") {
    return(list(X = as.matrix(dataset[,1:7]), X.knn = scale(as.matrix(dataset[,1:7])), X.gauss = scale(as.matrix(dataset[,1:7])), Y = as.matrix(dataset[,-c(1:7)])))
  } else if (dataset.name == "andro") {
    return(list(X = as.matrix(dataset[,-c(31:36)]), X.knn = scale(as.matrix(dataset[,-c(31:36)])), X.gauss = scale(as.matrix(dataset[,-c(31:36)])), Y = as.matrix(dataset[,c(31:36)])))
  } else if (dataset.name == "edm") {
    return(list(X = as.matrix(dataset[,-c(17:18)]), X.knn = scale(as.matrix(dataset[,-c(17:18)])), X.gauss = scale(as.matrix(dataset[,-c(17:18)])), Y = as.matrix(dataset[,c(17:18)])))
  } else if (dataset.name == "wq") {
    return(list(X = as.matrix(dataset[,-c(17:30)]), X.knn = scale(as.matrix(dataset[,-c(17:30)])), X.gauss = scale(as.matrix(dataset[,-c(17:30)])), Y = as.matrix(dataset[,c(17:30)])))
  } else if (dataset.name == "atp1d") {
    w.s <- apply(dataset, 2, function(x) length(unique(x)))
    dataset <- dataset[,w.s!=1]
    return(list(X = as.matrix(dataset[,c(1:370)]), X.knn = scale(as.matrix(dataset[,c(1:370)])), X.gauss = scale(as.matrix(dataset[,c(1:370)])), Y = as.matrix(dataset[,c(371:376)])))
  } else if (dataset.name == "atp7d") {
    w.s <- apply(dataset, 2, function(x) length(unique(x)))
    dataset <- dataset[,w.s!=1]
    return(list(X = as.matrix(dataset[,c(1:370)]), X.knn = scale(as.matrix(dataset[,c(1:370)])), X.gauss = scale(as.matrix(dataset[,c(1:370)])), Y = as.matrix(dataset[,c(371:376)])))
  } else if (dataset.name == "jura") {
    return(list(X = as.matrix(dataset[,-c(16:18)]), X.knn = scale(as.matrix(dataset[,-c(16:18)])), X.gauss = scale(as.matrix(dataset[,-c(16:18)])), Y = as.matrix(dataset[,c(16:18)])))
  } else if (dataset.name %in% c("example1", "example2")) {
    # PARAMS
    # choice of SC
    if (dataset.name == "example1") {
      SC <- 0
    } else {
      SC <- 1
    }
    
    
    # dimensions
    d <- 10
    n <- 5000
    
    
    # CONSTRUCTION
    
    # predictors
    X  <- matrix(runif(n*d, min = -1, max = 1),ncol = d)
    # responses
    Y <- t(apply(X, 1, function(xx) {
      
      if (SC == 0) {
        
        # copula
        normCop <- normalCopula(param=c(xx[1]), dim = 2)
        
        # margins
        mdvNorm <- mvdc(copula=normCop, margins=c("norm", "norm"),
                        paramMargins=list(list(mean = 0, sd = 1),
                                          list(mean = 0, sd = 1)))
        # gen
        rMvdc(n = 1, mvdc = mdvNorm)
      }  else if (SC == 1) {
        
        # copula
        tCop <- tCopula(xx[2],  df=ifelse(xx[1] <= (-1 + 2/3), 1, ifelse(xx[1]<= (-1 + 4/3), 3, 10)))
        # margins
        if (xx[3]<=0) {
          margins <- c("norm", "norm")
          paramMargins <- list(list(mean = 0, sd = 1),
                               list(mean = 0, sd = 1))
        } else {
          margins <- c("norm", "exp")
          paramMargins <- list(list(mean = 0, sd = 1),
                               list(rate = 1))
        }
        mdvT <- mvdc(copula=tCop, margins=margins,
                     paramMargins=paramMargins)
        # gen
        rMvdc(n = 1, mvdc = mdvT)
        
      }
    }))
    colnames(Y) <- c("Y1", "Y2")
    return(list(X = X, Y = Y, X.knn = scale(X), X.gauss = scale(X)))
  }
}

kFoldCV <- function(n, k = 10) {
  ids <- sample(cut(1:n, breaks = k, labels = FALSE))
  return(lapply(1:k, function(i) which(ids == i)))
}

RMSE_t <- function(Y, Yhat) {
  return(sqrt(apply((Y-Yhat) * (Y-Yhat),2,mean)))
}

qLoss <- function(y,yhat,alpha) {
  return(mean(alpha*pmax(y-yhat,0)+(1-alpha)*pmax(yhat-y,0)))
}

generateRandomDirection <- function(dim = 2, nb = 1) {
  w <- list()
  for (i in 1:nb) {
    x <- rnorm(n = dim, mean = 0, sd = 1)
    w[[i]] <- x/sqrt(sum(x^2))
  }
  return(w)
}


## competitors
KNN <- function(X, Y) {
  return(list(X = X, Y = Y))
}

GaussKernel <- function(X, Y) {
  return(list(X = X, Y = Y))
}

ResRF <- function(X, Y, fit.mean = FALSE) {
  
  # list of forest
  if (fit.mean) {
    forest.list <- apply(Y, 2, function(y) ranger::ranger(y~., data = data.frame(X=X,y=y)))
    
    # compute residuals (OOB)
    residuals <- sapply(1:ncol(Y), function(i) Y[,i]-forest.list[[i]]$predictions)
  } else {
    forest.list <- NULL
    residuals <- Y
  }
   
  cov.res <- cov(residuals)
  cor.res <- cor(residuals)
  return(list(forest.list = forest.list, X = X, Y = Y, residuals = residuals, 
              cov.res = cov.res, cor.res = cor.res, fit.mean = fit.mean))
}

predictKNN <- function(object,
                       newdata, 
                       k = 10, 
                       type = "functional", 
                       f = function(y) y[1], 
                       quantiles = NULL) {
  
  indices <- knn.index.dist(data = object$X, 
                            TEST_data = newdata, 
                            k = min(k, nrow(object$X)-1))$test_knn_idx
  
  if (type == "mean") {
    
    means <- t(apply(indices, 1, function(ind) apply(object$Y, 2, function(y) mean(y[ind]))))
    
    return(list(mean=means))
    
  } else if (type == "functional") {

    f.vals <- t(apply(object$Y, 1, f))
    if (is.null(quantiles)) {
      funs <- apply(indices, 1, function(ids) mean(f.vals[ids]))
    } else {
      if (length(quantiles)>1) {
        funs <- t(apply(indices, 1, function(ids) quantile(f.vals[ids], probs = quantiles)))
      } else {
        funs <- t(apply(indices, 1, function(ids) apply(f.vals[ids,], 2, function(x) quantile(x, probs = quantiles))))
      }
      
    }
    
    return(list(functional=funs))
    
  } else if (type == "cov") {
    
    cov.mat <- array(1, dim = c(nrow(indices), ncol(object$Y), ncol(object$Y)))
    for (i in 1:nrow(indices)) {
      cov.mat[i,,] <- cov(object$Y[indices[i,],])
    }
    
    return(list(cov = cov.mat))
    
  } else if (type == "cor") {
    
    cor.mat <- array(1, dim = c(nrow(indices), ncol(object$Y), ncol(object$Y)))
    for (i in 1:nrow(indices)) {
      cor.mat[i,,] <- cor(object$Y[indices[i,],])
    }
   
    return(list(cor = cor.mat))
    
  } else if (type == "normalPredictionScore") {
    
    n <- nrow(object$Y)
    d <- ncol(object$Y)
    
    means <- t(apply(indices, 1, function(ind) apply(object$Y, 2, function(y) mean(y[ind]))))
    
    funs <- lapply(1:nrow(indices), function(i) {
      
      inv.cov <- tryCatch(solve(cov(object$Y[indices[i,],]) + 10^{-3}*diag(ncol(object$Y))), error = function(cond) solve(diag(diag(cov(object$Y[indices[i,],])),ncol(object$Y)))) 
      
      return(function(y) (n/(n+1))*((n-d)/(d*(n-1)))*as.numeric((y-means[i,])%*%inv.cov%*%(y-means[i,])))
    })
    
    return(list(normalPredictionScore=funs))
    
  } else if (type == "ecdf") {
    
    if (!require(spatstat)) {
      stop("spatstat package missing.")
    }
    
    f.vals <- apply(object$Y, 1, f)
    
    funs <- lapply(1:nrow(indices), function(i) {
      return(function(y) spatstat::ewcdf(x = f.vals[indices[i,]], weights = rep(1/length(indices[i,]), length(indices[i,])))(y))
    })
    
    return(list(ecdf=funs))
  } 
  
}



predictResRF <- function(object,
                         newdata, 
                         type = "functional", 
                         f, 
                         quantiles = NULL) {
  
  
  
  if (type == "mean") {
    
    means <- sapply(object$forest.list, function(rf) predict(rf, data.frame(X = newdata))$predictions)
    
    return(list(mean=means))
    
  } else if (type == "functional") {
    
    #means.w <- sapply(object$forest.list, function(rf) predict(rf, data.frame(X = newdata))$predictions)%*%w
    #sds.w <- t(w)%*%object$cov.res%*%w
    
    #funs <- sapply(quantiles, function(q) qnorm(p = q, mean = means.w, sd = sds.w))
    if (object$fit.mean) {
      means <- sapply(object$forest.list, function(rf) predict(rf, data.frame(X = newdata))$predictions)
    } else {
      means <- matrix(0, nrow=nrow(newdata), ncol=ncol(object$Y))
    }
    
    qq <- matrix(apply(apply(object$residuals, 1, f),1, function(x) quantile(x, probs=quantiles)),nrow=1)
    
   
    return(list(functional=qq[rep(1,nrow(newdata)),]))
  } else if (type == "cov") {
    
    cov.mat <- array(1, dim = c(nrow(newdata), ncol(object$Y), ncol(object$Y)))
    for (i in 1:nrow(newdata)) {
      cov.mat[i,,] <- object$cov.res
    }
    
    return(list(cov = cov.res))
    
  } else if (type == "cor") {
    
    cor.mat <- array(1, dim = c(nrow(newdata), ncol(object$Y), ncol(object$Y)))
    for (i in 1:nrow(newdata)) {
      cor.mat[i,,] <- object$cor.res
    }
    
    return(list(cor = cor.mat))
    
  } else if (type == "normalPredictionScore") {
    
    n <- nrow(object$Y)
    d <- ncol(object$Y)
    
    means <- sapply(object$forest.list, function(rf) predict(rf, data.frame(X = newdata))$predictions)
    
    inv.cov <- solve(object$cov.res)
    
    funs <- lapply(1:nrow(newdata), function(i) {
      
      return(function(y) (n/(n+1))*((n-d)/(d*(n-1)))*as.numeric((y-means[i,])%*%inv.cov%*%(y-means[i,])))
    })
    
    return(list(normalPredictionScore=funs))
  }
  
}


predictGaussKernel <- function(object,
                       newdata, 
                       sigma = 1, 
                       type = "functional", 
                       f = function(y) y[1], 
                       quantiles = NULL) {
  
  w <- t(apply(newdata, 1, function(x) exp(-1 * rowSums((x-object$X)*(x-object$X))/sigma)))
  rw <- rowSums(w)
  # if problem, uniform weights
  ids.0 <- which(rw == 0)
  w[ids.0,] <- 1/ncol(w)
  rw[ids.0] <- 1
  w <- w 
  
  
  if (type == "mean") {
    
    means <- t(apply(w, 1, function(ww) apply(object$Y, 2, function(y) sum(y*ww))))
    
    return(list(mean=means))
    
  } else if (type == "functional") {
    
    f.vals <- t(apply(object$Y, 1, f))
    
    if (is.null(quantiles)) {
      funs <- apply(w, 1, function(ww) sum(f.vals*ww))
    } else {
      if (length(quantiles)>1) {
        funs <- t(apply(w, 1, function(ww) spatstat::weighted.quantile(f.vals, w = ww, probs = quantiles)))
      } else {
        funs <- t(apply(w, 1, function(ww) apply(f.vals, 2, function(x) spatstat::weighted.quantile(x, w = ww, probs = quantiles))))
      }
      
    }
    
    return(list(functional=funs))
  } else if (type == "cor") {
    
    if (!require(wCorr)) {
      stop("wCorr package missing.")
    }
    
    cor.mat <- array(1, dim = c(nrow(w), ncol(object$Y), ncol(object$Y)))
    for (id1 in 1:ncol(object$Y)) {
      for (id2 in id1:ncol(object$Y)) {
        if (id1 != id2) {
          cor.mat[,id2,id1] <- cor.mat[,id1,id2] <- sapply(1:nrow(w), 
                                                           function(i) {
                                                             weightedCorr(object$Y[,id1], 
                                                                          object$Y[,id2], 
                                                                          method = "pearson", 
                                                                          weights=as.numeric(w[i,]))
                                                           })
        }
      }
    }
    
    return(list(cor = cor.mat))
    
  } else if (type == "cov") {
    
    if (!require(wCorr)) {
      stop("wCorr package missing.")
    }
    
    means <- t(apply(w, 1, function(ww) ww%*%object$Y))
    means2 <- t(apply(w, 1, function(ww) ww%*%(object$Y^2)))
    sds <- sqrt(means2-(means)^2)
    cov.mat <- array(1, dim = c(nrow(w), ncol(object$Y), ncol(object$Y)))
    for (id1 in 1:ncol(object$Y)) {
      for (id2 in id1:ncol(object$Y)) {
        if (id1 != id2) {
          cov.mat[,id2,id1] <- cov.mat[,id1,id2] <- sapply(1:nrow(w), 
                                                           function(i) {
                                                             sds[i,id2]*sds[i,id1]*weightedCorr(object$Y[,id1], 
                                                                                                object$Y[,id2], 
                                                                                                method = "pearson", 
                                                                                                weights=as.numeric(w[i,]))
                                                           })
        }
      }
    }
    
    return(list(cov = cov.mat))
    
  }  else if (type == "normalPredictionScore") {
    
    if (!require(wCorr)) {
      stop("wCorr package missing.")
    }
    
    means <- t(apply(w, 1, function(ww) ww%*%object$Y))
    means2 <- t(apply(w, 1, function(ww) ww%*%(object$Y^2)))
    sds <- sqrt(means2-(means)^2)
    covs <- array(1, dim = c(nrow(w), ncol(object$Y), ncol(object$Y)))
    for (id1 in 1:ncol(object$Y)) {
      for (id2 in id1:ncol(object$Y)) {
        if (id1 != id2) {
          covs[,id2,id1] <- covs[,id1,id2] <- sapply(1:nrow(w), 
                                                     function(i) {
                                                       sds[i,id2]*sds[i,id1]*weightedCorr(object$Y[,id1], 
                                                                                          object$Y[,id2], 
                                                                                          method = "pearson", 
                                                                                          weights=as.numeric(w[i,]))
                                                     })
        }
      }
    }
    
    n <- nrow(object$Y)
    d <- ncol(object$Y)
    
    funs <- lapply(1:nrow(w), function(i) {
      
      
      inv.cov <- tryCatch(solve(covs[i,,]+10^{-3}*diag(1, ncol(object$Y))), error = function(cond) solve(diag(diag(covs[i,,]),ncol(object$Y)))) 
      
      return(function(y) (n/(n+1))*((n-d)/(d*(n-1)))*as.numeric((y-means[i,])%*%inv.cov%*%(y-means[i,])))
    })
    
    return(list(normalPredictionScore=funs))
    
  } else if (type == "ecdf") {
    
    if (!require(spatstat)) {
      stop("spatstat package missing.")
    }
    
    f.vals <- apply(object$Y, 1, f)
    
    funs <- lapply(1:nrow(w), function(i) {
      return(function(y) spatstat::ewcdf(x = f.vals, weights = as.numeric(w[i,]))(y))
    })
    
    return(list(ecdf=funs))
  } 
    
  
  
}


hyperParamSelection <-      function(Y, 
                                     X.knn,
                                     X.gauss,
                                     k = 10, 
                                     alpha_seq = c(.005, .025, .05, .3, .5, .7, .95, .975, .995), 
                                     seed = 0,
                                     ...) {
  
  if (length(alpha_seq) <= 1) {
    stop("alpha_seq should be at least of length 2.")
  }
  
  Y <- scale(Y)
  
  # repro
  set.seed(seed)
  
  # create folds
  folds <- kFoldCV(n = nrow(X.knn), k = k)
  
  # properties of the simulations
  #w <- generateRandomDirection(dim = ncol(Y), nb = nb_random_directions)
  w <- lapply(1:ncol(Y), FUN = function(i) {w <- rep(0,ncol(Y)); w[i] <- 1; return(w)})
  knn_loss1 <- matrix(0,nrow=ncol(Y), ncol=length(alpha_seq))
  knn_loss2 <- matrix(0,nrow=ncol(Y), ncol=length(alpha_seq))
  knn_loss3 <- matrix(0,nrow=ncol(Y), ncol=length(alpha_seq))

  gauss_loss1 <- matrix(0,nrow=ncol(Y), ncol=length(alpha_seq))
  gauss_loss2 <- matrix(0,nrow=ncol(Y), ncol=length(alpha_seq))
  gauss_loss3 <- matrix(0,nrow=ncol(Y), ncol=length(alpha_seq))
  gauss_loss4 <- matrix(0,nrow=ncol(Y), ncol=length(alpha_seq))
  gauss_loss5 <- matrix(0,nrow=ncol(Y), ncol=length(alpha_seq))
  
  # CV loop
  for (kk in 1:k) {
    
    print(paste0("CV loop: ", kk))
  
    comp.knn <- KNN(X = X.knn[-folds[[kk]],], Y = Y[-folds[[kk]],])
    comp.gauss <- GaussKernel(X = X.gauss[-folds[[kk]],], Y = Y[-folds[[kk]],])
    
    # loop over projections
    for (i in 1:length(w)) {
      
      yhat_knn1 <- predictKNN(comp.knn, newdata = X.knn[folds[[kk]],], k = 5, type = "functional", 
                             quantiles = alpha_seq, f = function(y) sum(w[[i]]*y))$functional
      yhat_knn2 <- predictKNN(comp.knn, newdata = X.knn[folds[[kk]],], k = 10, type = "functional", 
                             quantiles = alpha_seq, f = function(y) sum(w[[i]]*y))$functional
      yhat_knn3 <- predictKNN(comp.knn, newdata = X.knn[folds[[kk]],], k = sqrt(nrow(X.knn)*(k-1)/k), type = "functional", 
                             quantiles = alpha_seq, f = function(y) sum(w[[i]]*y))$functional
      
      yhat_gauss1 <- predictGaussKernel(comp.gauss, newdata = X.gauss[folds[[kk]],], sigma = 0.1, type = "functional", 
                                       quantiles = alpha_seq, f = function(y) sum(w[[i]]*y))$functional
      yhat_gauss2 <- predictGaussKernel(comp.gauss, newdata = X.gauss[folds[[kk]],], sigma = 0.5, type = "functional", 
                                       quantiles = alpha_seq, f = function(y) sum(w[[i]]*y))$functional
      yhat_gauss3 <- predictGaussKernel(comp.gauss, newdata = X.gauss[folds[[kk]],], sigma = 1, type = "functional", 
                                       quantiles = alpha_seq, f = function(y) sum(w[[i]]*y))$functional
      yhat_gauss4 <- predictGaussKernel(comp.gauss, newdata = X.gauss[folds[[kk]],], sigma = 2, type = "functional", 
                                       quantiles = alpha_seq, f = function(y) sum(w[[i]]*y))$functional
      yhat_gauss5 <- predictGaussKernel(comp.gauss, newdata = X.gauss[folds[[kk]],], sigma = 10, type = "functional", 
                                        quantiles = alpha_seq, f = function(y) sum(w[[i]]*y))$functional
      
      for (j in 1:length(alpha_seq)) {
        knn_loss1[i,j] <- knn_loss1[i,j] +  qLoss(y = Y[folds[[kk]],] %*% w[[i]],
                                                yhat = yhat_knn1[,j], alpha = alpha_seq[j])/k
        knn_loss2[i,j] <- knn_loss2[i,j] +  qLoss(y = Y[folds[[kk]],] %*% w[[i]],
                                                yhat = yhat_knn2[,j], alpha = alpha_seq[j])/k
        knn_loss3[i,j] <- knn_loss3[i,j] +  qLoss(y = Y[folds[[kk]],] %*% w[[i]],
                                                yhat = yhat_knn3[,j], alpha = alpha_seq[j])/k
       
        gauss_loss1[i,j] <- gauss_loss1[i,j] +  qLoss(y = Y[folds[[kk]],] %*% w[[i]],
                                                    yhat = yhat_gauss1[,j], alpha = alpha_seq[j])/k
        gauss_loss2[i,j] <- gauss_loss2[i,j] +  qLoss(y = Y[folds[[kk]],] %*% w[[i]],
                                                    yhat = yhat_gauss2[,j], alpha = alpha_seq[j])/k
        gauss_loss3[i,j] <- gauss_loss3[i,j] +  qLoss(y = Y[folds[[kk]],] %*% w[[i]],
                                                    yhat = yhat_gauss3[,j], alpha = alpha_seq[j])/k
        gauss_loss4[i,j] <- gauss_loss4[i,j] +  qLoss(y = Y[folds[[kk]],] %*% w[[i]],
                                                    yhat = yhat_gauss4[,j], alpha = alpha_seq[j])/k
        gauss_loss5[i,j] <- gauss_loss5[i,j] +  qLoss(y = Y[folds[[kk]],] %*% w[[i]],
                                                    yhat = yhat_gauss5[,j], alpha = alpha_seq[j])/k}
    }
  }
  
  return(list(knn=list(knn_loss1 = knn_loss1,
                   knn_loss2 = knn_loss2, 
                   knn_loss3 = knn_loss3),
              gauss=list(gauss_loss1 = gauss_loss1,
                   gauss_loss2 = gauss_loss2,
                   gauss_loss3 = gauss_loss3,
                   gauss_loss4 = gauss_loss4,
                   gauss_loss5 = gauss_loss5)))
  
}



runRandomPinballAnalysis <- function(X, 
                                     Y, 
                                     X.knn,
                                     X.gauss,
                                     param.knn,
                                     param.gauss,
                                     k = 10, 
                                     alpha_seq = c(.005, .025, .05, .1, .3, .5, .7, .9, .95, .975, .995), 
                                     nb_random_directions = 10,
                                     seed = 0,
                                     ...) {
  
  #if (length(alpha_seq) <= 1) {
  #  stop("alpha_seq should be at least of length 2.")
  #}
  
  # scaling the responses
  Y <- scale(Y)
  
  # repro
  set.seed(seed)
  
  # create folds
  folds <- kFoldCV(n = nrow(X), k = k)
  
  # properties of the simulations
  w <- generateRandomDirection(dim = ncol(Y), nb = nb_random_directions)
  
  # quantile loss matrices
  mrf_loss <- matrix(0,nrow=nb_random_directions, ncol=length(alpha_seq))
  gini_loss <- matrix(0,nrow=nb_random_directions, ncol=length(alpha_seq))
  res_loss <- matrix(0,nrow=nb_random_directions, ncol=length(alpha_seq))
  knn_loss <- matrix(0,nrow=nb_random_directions, ncol=length(alpha_seq))
  gauss_loss <- matrix(0,nrow=nb_random_directions, ncol=length(alpha_seq))
  
  # CV loop
  for (kk in 1:k) {
    
    print(paste0("CV loop: ", kk))
    
    # two tree methods
    print("training forests.")
    mRF <- mrf(X = X[-folds[[kk]],], Y = Y[-folds[[kk]],], splitting.rule = "fourier", ...)
    giniRF <- mrf(X = X[-folds[[kk]],], Y = Y[-folds[[kk]],], splitting.rule = "gini")
    
    print("training global res.")
    resRF <- ResRF(X = X[-folds[[kk]],], Y = Y[-folds[[kk]],])
    print("training knn.")
    comp.knn <- KNN(X = X.knn[-folds[[kk]],], Y = Y[-folds[[kk]],])
    print("training gauss.")
    comp.gauss <- GaussKernel(X = X.gauss[-folds[[kk]],], Y = Y[-folds[[kk]],])
    
    print("predict forests.")
    yhat_mrf <- predict(mRF, newdata = X[folds[[kk]],], type = "functional", 
                        quantiles = c(alpha_seq,0.1), f = function(y) sapply(1:length(w), function(i) sum(w[[i]]*y)))$functional
    yhat_gini <- predict(giniRF, newdata = X[folds[[kk]],], type = "functional", 
                         quantiles = c(alpha_seq,0.1), f = function(y) sapply(1:length(w), function(i) sum(w[[i]]*y)))$functional
    # loop over projections
    print("predict res.")
    yhat_res   <- predictResRF(resRF, newdata = X[folds[[kk]],], type = "functional", 
                               quantiles = alpha_seq, f = function(y) sapply(1:length(w), function(i) sum(w[[i]]*y)))$functional
    
    print("predict knn.")
    yhat_knn   <-   predictKNN(comp.knn, newdata = X.knn[folds[[kk]],], k = param.knn, type = "functional", 
                 quantiles = alpha_seq, f = function(y) sapply(1:length(w), function(i) sum(w[[i]]*y)))$functional
    
    print("predict gauss.")
    yhat_gauss   <-  predictGaussKernel(comp.gauss, newdata = X.gauss[folds[[kk]],], sigma = param.gauss, type = "functional", 
                         quantiles = alpha_seq, f = function(y) sapply(1:length(w), function(i) sum(w[[i]]*y)))$functional
    # for (i in 1:length(w)) {
    #   
    #   yhat_knn <- predictKNN(comp.knn, newdata = X.knn[folds[[kk]],], k = param.knn, type = "functional", 
    #                        quantiles = alpha_seq, f = function(y) sum(w[[i]]*y))$functional
    #   
    #   yhat_gauss <- predictGaussKernel(comp.gauss, newdata = X.gauss[folds[[kk]],], sigma = param.gauss, type = "functional", 
    #                        quantiles = alpha_seq, f = function(y) sum(w[[i]]*y))$functional
    #   
      # define the qs
      #mrf_q[i,folds[[kk]],] <- yhat_mrf
      #gini_q[i,folds[[kk]],] <- yhat_gini
      #res_q[i,folds[[kk]],] <- yhat_res
      #knn_q[i,folds[[kk]],] <- yhat_knn
      #gauss_q[i,folds[[kk]],] <- yhat_gauss
      
      # define the us
      #funs <- predict(mRF, newdata = X[folds[[kk]],], type = "ecdf", f = function(y) sum(w[[i]]*y))$ecdf
      #mrf_u[i,folds[[kk]]] <- sapply(1:length(folds[[kk]]), function(j) funs[[j]](sum(w[[i]]*as.numeric(Y[folds[[kk]][j],]))))
      
      #funs <- predict(giniRF, newdata = X[folds[[kk]],], type = "ecdf", f = function(y) sum(w[[i]]*y))$ecdf
      #gini_u[i,folds[[kk]]] <- sapply(1:length(folds[[kk]]), function(j) funs[[j]](sum(w[[i]]*as.numeric(Y[folds[[kk]][j],]))))
      
      #funs <- predictKNN(comp.knn, newdata = X.knn[folds[[kk]],], type = "ecdf", k = param.knn, f = function(y) sum(w[[i]]*y))$ecdf
      #knn_u[i,folds[[kk]]] <- sapply(1:length(folds[[kk]]), function(j) funs[[j]](sum(w[[i]]*as.numeric(Y[folds[[kk]][j],]))))
      
      #funs <- predictGaussKernel(comp.gauss, newdata = X.gauss[folds[[kk]],], type = "ecdf", sigma = param.gauss, f = function(y) sum(w[[i]]*y))$ecdf
      #gauss_u[i,folds[[kk]]] <- sapply(1:length(folds[[kk]]), function(j) funs[[j]](sum(w[[i]]*as.numeric(Y[folds[[kk]][j],]))))
      
      j <- 1
      for (i in 1:length(w)) {
          mrf_loss[i,j] <- mrf_loss[i,j] +  qLoss(y = Y[folds[[kk]],] %*% w[[i]],
                                                  yhat = yhat_mrf[[i]][,1], alpha = alpha_seq[j])/k
          gini_loss[i,j] <- gini_loss[i,j] + qLoss(y = Y[folds[[kk]],] %*% w[[i]], 
                                                   yhat = yhat_gini[[i]][,1], alpha = alpha_seq[j])/k
          res_loss[i,j] <- res_loss[i,j] + qLoss(y = Y[folds[[kk]],] %*% w[[i]], 
                                                   yhat = yhat_res[,i], alpha = alpha_seq[j])/k
          knn_loss[i,j] <- knn_loss[i,j] +  qLoss(y = Y[folds[[kk]],] %*% w[[i]],
                                                  yhat = yhat_knn[,i], alpha = alpha_seq[j])/k
          gauss_loss[i,j] <- gauss_loss[i,j] +  qLoss(y = Y[folds[[kk]],] %*% w[[i]],
                                                  yhat = yhat_gauss[,i], alpha = alpha_seq[j])/k
      }  
  }
  
  return(list(mrf_loss = mrf_loss, gini_loss = gini_loss, res_loss = res_loss,
              knn_loss = knn_loss, gauss_loss = gauss_loss,
              #mrf_u = mrf_u, gini_u = gini_u, knn_u = knn_u, gauss_u = gauss_u,
              w = w))
              #mrf_q = mrf_q, gini_q = gini_q, knn_q = knn_q, gauss_q = gauss_q, res_q = res_q))
  
}

runRandomPinballNLAnalysis <- function(X, 
                                       Y,
                                       X.knn,
                                       X.gauss,
                                       param.knn,
                                       param.gauss,
                                       k = 10, 
                                       alpha_seq = c(.005, .025, .05, .3, .5, .7, .95, .975, .995), 
                                       seed = 0,
                                       ...) {
  
  # repro
  set.seed(seed)
  
  # create folds
  folds <- kFoldCV(n = nrow(X), k = k)
  
  # properties of the simulations
  mrf_loss <- matrix(0,nrow=ncol(Y)^2, ncol=length(alpha_seq))
  gini_loss <- matrix(0,nrow=ncol(Y)^2, ncol=length(alpha_seq))
  knn_loss <- matrix(0,nrow=ncol(Y)^2, ncol=length(alpha_seq))
  gauss_loss <- matrix(0,nrow=ncol(Y)^2, ncol=length(alpha_seq))
  
  # CV loop
  for (kk in 1:k) {
    
    print(paste0("CV loop: ", kk))
    
    
    mRF <- mrf(X = X[-folds[[kk]],], Y = Y[-folds[[kk]],], splitting.rule = "fourier", ...)
    giniRF <- mrf(X = X[-folds[[kk]],], Y = Y[-folds[[kk]],], splitting.rule = "gini")
    
    comp.knn <- KNN(X = X.knn[-folds[[kk]],], Y = Y[-folds[[kk]],])
    comp.gauss <- GaussKernel(X = X.gauss[-folds[[kk]],], Y = Y[-folds[[kk]],])
    
    pairs <- expand.grid(1:ncol(Y),1:ncol(Y))
    pairs <- pairs[1:(nrow(pairs)/2),]
    
    for (i in 1:nrow(pairs)) {
      
      yhat_mrf <- predict(mRF, newdata = X[folds[[kk]],], type = "functional", 
                          quantiles = alpha_seq, f = function(y) y[pairs[i,1]]*y[pairs[i,2]])$functional
      yhat_gini <- predict(giniRF, newdata = X[folds[[kk]],], type = "functional", 
                           quantiles = alpha_seq, f = function(y) y[pairs[i,1]]*y[pairs[i,2]])$functional
      yhat_knn <- predictKNN(comp.knn, newdata = X.knn[folds[[kk]],], k = param.knn, type = "functional", 
                          quantiles = alpha_seq, f = function(y) y[pairs[i,1]]*y[pairs[i,2]])$functional
      yhat_gauss <- predictGaussKernel(comp.gauss, newdata = X.gauss[folds[[kk]],], sigma = param.gauss, type = "functional", 
                           quantiles = alpha_seq, f = function(y) y[pairs[i,1]]*y[pairs[i,2]])$functional
      
      for (j in 1:length(alpha_seq)) {
        mrf_loss[i,j] <- mrf_loss[i,j] +  qLoss(y = Y[folds[[kk]],pairs[i,1]]*Y[folds[[kk]],pairs[i,2]],
                                                yhat = yhat_mrf[,j], alpha = alpha_seq[j])/k
        gini_loss[i,j] <- gini_loss[i,j] + qLoss(y = Y[folds[[kk]],pairs[i,1]]*Y[folds[[kk]],pairs[i,2]], 
                                                 yhat = yhat_gini[,j], alpha = alpha_seq[j])/k
        knn_loss[i,j] <- knn_loss[i,j] +  qLoss(y = Y[folds[[kk]],pairs[i,1]]*Y[folds[[kk]],pairs[i,2]],
                                               yhat = yhat_knn[,j], alpha = alpha_seq[j])/k
        gauss_loss[i,j] <- gauss_loss[i,j] + qLoss(y = Y[folds[[kk]],pairs[i,1]]*Y[folds[[kk]],pairs[i,2]], 
                                                 yhat = yhat_gauss[,j], alpha = alpha_seq[j])/k
        
      }
    }
  }
  
  return(list(mrf_loss = mrf_loss, gini_loss = gini_loss,
              knn_loss = knn_loss, gauss_loss = gauss_loss))
  
}


runNormalCoverage <- function(X, 
                              Y, 
                              param.knn,
                              param.gauss,
                              X.knn,
                              X.gauss,
                              k = 10, 
                              alpha = 0.05,
                              seed = 0,
                              ...) {
  
  # repro
  set.seed(seed)
  
  # create folds
  folds <- kFoldCV(n = nrow(X), k = k)
  
  scores_CV_mrf <- rep(NA, nrow(X))
  scores_CV_gini <- rep(NA, nrow(X))
  scores_CV_res <- rep(NA, nrow(X))
  scores_CV_knn <- rep(NA, nrow(X))
  scores_CV_gauss <- rep(NA, nrow(X))
  #scores_CV_mrf_global <- rep(NA, nrow(X))
  #scores_CV_gini_global <- rep(NA, nrow(X))
  
  # areas_CV_mrf <- rep(NA, nrow(X))
  # areas_CV_gini <- rep(NA, nrow(X))
  # areas_CV_mrf_global <- rep(NA, nrow(X))
  # areas_CV_gini_global <- rep(NA, nrow(X))
  
  # CV loop
  for (kk in 1:k) {
    print(paste0("CV loop: ", kk))
    
    # fit the two types of forests
    mRF <- mrf(X = X[-folds[[kk]],], Y = Y[-folds[[kk]],], splitting.rule = "fourier", ...)
    giniRF <- mrf(X = X[-folds[[kk]],], Y = Y[-folds[[kk]],], splitting.rule = "gini")
    resRF <- ResRF(X = X[-folds[[kk]],], Y = Y[-folds[[kk]],])
    comp.knn <- KNN(X = X.knn[-folds[[kk]],], Y = Y[-folds[[kk]],])
    comp.gauss <- GaussKernel(X = X.gauss[-folds[[kk]],], Y = Y[-folds[[kk]],])
      
    # get the global covariance matrix of residuals
    #residuals.mrf.oof <- Y[folds[[kk]],]-predict(mRF, newdata = X[folds[[kk]],], type = "mean")$mean
    #residuals.gini.oof <- Y[folds[[kk]],]-predict(giniRF, newdata = X[folds[[kk]],], type = "mean")$mean
    #cov.mrf.residuals <- cov(residuals.mrf.oof)
    #cov.gini.residuals <- cov(residuals.gini.oof)
    
    # get the function for pred
    preds_mrf <- predict(mRF, newdata = X[folds[[kk]],], type = "normalPredictionScore")
    preds_gini <- predict(giniRF, newdata = X[folds[[kk]],], type = "normalPredictionScore")
    preds_res <- predictResRF(resRF, X[folds[[kk]],], type = "normalPredictionScore")
    preds_knn <- predictKNN(comp.knn, newdata = X.knn[folds[[kk]],],k = param.knn, type = "normalPredictionScore")
    preds_gauss <- predictGaussKernel(comp.gauss, sigma = param.gauss,newdata = X.gauss[folds[[kk]],], type = "normalPredictionScore")
    
    #preds_mrf_global <- predict(mRF, newdata = X[folds[[kk]],], cov.residuals = cov.mrf.residuals, type = "normalPredictionScore_global")
    #preds_gini_global <- predict(giniRF, newdata = X[folds[[kk]],], cov.residuals = cov.gini.residuals, type = "normalPredictionScore_global")
    
    # get the scores
    scores_mrf <- sapply(1:nrow(Y[folds[[kk]],]), function(i) preds_mrf$normalPredictionScore[[i]](Y[folds[[kk]],][i,]))
    scores_gini <- sapply(1:nrow(Y[folds[[kk]],]), function(i) preds_gini$normalPredictionScore[[i]](Y[folds[[kk]],][i,]))
    scores_res <- sapply(1:nrow(Y[folds[[kk]],]), function(i) preds_res$normalPredictionScore[[i]](Y[folds[[kk]],][i,]))
    scores_knn <- sapply(1:nrow(Y[folds[[kk]],]), function(i) preds_knn$normalPredictionScore[[i]](Y[folds[[kk]],][i,]))
    scores_gauss <- sapply(1:nrow(Y[folds[[kk]],]), function(i) preds_gauss$normalPredictionScore[[i]](Y[folds[[kk]],][i,]))
    
    #scores_mrf_global <- sapply(1:nrow(Y[folds[[kk]],]), function(i) preds_mrf_global$normalPredictionScore_global[[i]](Y[folds[[kk]],][i,]))
    #scores_gini_global <- sapply(1:nrow(Y[folds[[kk]],]), function(i) preds_gini_global$normalPredictionScore_global[[i]](Y[folds[[kk]],][i,]))
    
    # get the area
    #Y.grid <- expand.grid(lapply(1:ncol(Y), function(i) seq(min(Y[,i]), max(Y[,i]), length.out = 100)))
    
    #vals_gini <- lapply(1:nrow(Y[folds[[kk]],]),function(i) apply(Y.grid, 1, function(x) preds_gini$normalPredictionScore[[i]](as.numeric(x))))
    #vals_mrf <- lapply(1:nrow(Y[folds[[kk]],]),function(i) apply(Y.grid, 1, function(x) preds_mrf$normalPredictionScore[[i]](as.numeric(x))))
    #vals_mrf_global <- lapply(1:nrow(Y[folds[[kk]],]),function(i) apply(Y.grid, 1, function(x) preds_mrf_global$normalPredictionScore[[i]](as.numeric(x))))
    #vals_gini_global <- lapply(1:nrow(Y[folds[[kk]],]),function(i) apply(Y.grid, 1, function(x) preds_gini_global$normalPredictionScore[[i]](as.numeric(x))))
    
    
    # save CV scores
    #scores_CV_mrf_global[folds[[kk]]] <- scores_mrf_global
    #scores_CV_gini_global[folds[[kk]]] <- scores_gini_global
    scores_CV_mrf[folds[[kk]]] <- scores_mrf
    scores_CV_gini[folds[[kk]]] <- scores_gini
    scores_CV_res[folds[[kk]]] <- scores_res
    scores_CV_knn[folds[[kk]]] <- scores_knn
    scores_CV_gauss[folds[[kk]]] <- scores_gauss
    
    #areas_CV_mrf_global[folds[[kk]]] <- as.numeric(lapply(vals_mrf_global, function(v) mean(v <= qchisq(p = 1-alpha, df = ncol(Y)))))
    #areas_CV_gini_global[folds[[kk]]] <- as.numeric(lapply(vals_gini_global, function(v) mean(v <= qchisq(p = 1-alpha, df = ncol(Y)))))
    #areas_CV_mrf[folds[[kk]]] <- as.numeric(lapply(vals_mrf, function(v) mean(v <= qchisq(p = 1-alpha, df = ncol(Y)))))
    #areas_CV_gini[folds[[kk]]] <- as.numeric(lapply(vals_gini, function(v) mean(v <= qchisq(p = 1-alpha, df = ncol(Y)))))
  }

  n <- ((k-1)/k)*nrow(Y)
  d <- ncol(Y)
  
  return(list(#Y.grid = Y.grid, 
              #areas_CV_mrf_global = areas_CV_mrf_global,
              #areas_CV_gini_global = areas_CV_gini_global,
              #areas_CV_mrf = areas_CV_mrf,
              #areas_CV_gini = areas_CV_gini,
              #scores_CV_mrf_global = scores_CV_mrf_global, 
              #scores_CV_gini_global = scores_CV_gini_global,
              scores_CV_mrf = scores_CV_mrf, 
              scores_CV_gini = scores_CV_gini,
              scores_CV_res = scores_CV_res,
              scores_CV_knn = scores_CV_knn, 
              scores_CV_gauss = scores_CV_gauss,
              #coverage_mrf_global = mean(scores_CV_mrf_global <= qf(p = 1-alpha, df1 = d, df2 = n-d)),
              #coverage_gini_global = mean(scores_CV_gini_global <= qf(p = 1-alpha, df1 = d, df2 = n-d)),
              coverage_mrf = mean(scores_CV_mrf <= qf(p = 1-alpha, df1 = d, df2 = n-d)),
              coverage_gini = mean(scores_CV_gini <= qf(p = 1-alpha, df1 = d, df2 = n-d)),
              coverage_res = mean(scores_CV_res <= qf(p = 1-alpha, df1 = d, df2 = n-d)),
              coverage_knn = mean(scores_CV_knn <= qf(p = 1-alpha, df1 = d, df2 = n-d)),
              coverage_gauss = mean(scores_CV_gauss <= qf(p = 1-alpha, df1 = d, df2 = n-d)),
              cutoff = qf(p = 1-alpha, df1 = d, df2 = n-d),
              folds = folds
              ))
  
}

permRF <- function(y, X, nrep = 100) {
  
  obs.p <- ranger::ranger(y~., data = data.frame(y, x=X))$prediction.error
  null.p <- sapply(1:nrep, function(i) {
    rf <- ranger::ranger(y~., data = data.frame(sample(y), x=X))
    return(rf$prediction.error)
  })
  
  return(sum(null.p <= obs.p)/(nrep+1))
}

makeSummaries <- function(dataset, path="./experiments/mtr/data/", nrep = 100) {
  
  infos <- load(paste0(path, dataset, ".Rdata"))
  
  
  
  # look at the quantile loss
  heter0 <- sapply(1:20, function(i) {x <- rbind(res_pinball$mrf_loss[i,],
                                                 res_pinball$gini_loss[i,],
                                                 res_pinball$res_loss[i,],
                                                 res_pinball$knn_loss[i,],
                                                 res_pinball$gauss_loss[i,])
  apply(apply(x, 2, function(xx) {v <- rep(0, length(xx)); v[which.min(xx)] <- 1; v}),1,mean)})
  heter0 <- table(apply(heter0, 2, which.max))/200
  
  heter1 <- lapply(1:20, function(i) {x <- rbind(res_pinball$mrf_loss[i,],
                                         res_pinball$gini_loss[i,],
                                         res_pinball$res_loss[i,],
                                         res_pinball$knn_loss[i,],
                                         res_pinball$gauss_loss[i,])
                 apply(x, 2, function(xx) {v <- rep(0, length(xx)); v[which.min(xx)] <- 1; v})})
  heter1 <- Reduce(heter1, f = function(x,y) x+y)
  heter1 <- heter1/20
  
  heter2 <- lapply(1:20, function(i) {x <- rbind(res_pinball$mrf_loss[i,],
                                                 res_pinball$gini_loss[i,],
                                                 res_pinball$res_loss[i,],
                                                 res_pinball$knn_loss[i,],
                                                 res_pinball$gauss_loss[i,])
  apply(x, 2, function(xx) {rank(xx)})})
  heter2 <- Reduce(heter2, f = function(x,y) x+y)
  heter2 <- heter2/20
  # vec.mse.u.mrf <- list()
  # vec.mse.u.gini <- list()
  # vec.mse.u.knn <- list()
  # vec.mse.u.gauss <- list()
  # # look at prediction in the random pinball
  # for (i in 1:nrow(res_pinball$mrf_u)) {
  #   vec.mse.u.mrf[[i]] <- (ranger::ranger(u~., data = data.frame(u=res_pinball$mrf_u[i,],x=d$X))$predictions-res_pinball$mrf_u[i,])^2
  #   vec.mse.u.gini[[i]] <- (ranger::ranger(u~., data = data.frame(u=res_pinball$gini_u[i,],x=d$X))$predictions-res_pinball$gini_u[i,])^2
  #   vec.mse.u.knn[[i]] <- (ranger::ranger(u~., data = data.frame(u=res_pinball$knn_u[i,],x=d$X))$predictions-res_pinball$knn_u[i,])^2
  #   vec.mse.u.gauss[[i]] <- (ranger::ranger(u~., data = data.frame(u=res_pinball$gauss_u[i,],x=d$X))$predictions-res_pinball$gauss_u[i,])^2
  # }
  # 
  # # computing the Us
  # u.res <- pf(q = res_coverage$scores_CV_res, df1 = ncol(d$Y), df2 = nrow(d$Y)-ncol(d$Y))
  # u.mrf <- pf(q = res_coverage$scores_CV_mrf, df1 = ncol(d$Y), df2 = nrow(d$Y)-ncol(d$Y))
  # u.gini <- pf(q = res_coverage$scores_CV_gini, df1 = ncol(d$Y), df2 = nrow(d$Y)-ncol(d$Y))
  # u.knn <- pf(q = res_coverage$scores_CV_knn, df1 = ncol(d$Y), df2 = nrow(d$Y)-ncol(d$Y))
  # u.gauss <- pf(q = res_coverage$scores_CV_gauss, df1 = ncol(d$Y), df2 = nrow(d$Y)-ncol(d$Y))
  # 
  # mse.res <- ranger::ranger(u~., data = data.frame(u=u.res,x=d$X))$prediction.error
  # mse.mrf <- ranger::ranger(u~., data = data.frame(u=u.mrf,x=d$X))$prediction.error
  # mse.gini <- ranger::ranger(u~., data = data.frame(u=u.gini,x=d$X))$prediction.error
  # mse.knn <- ranger::ranger(u~., data = data.frame(u=u.knn,x=d$X))$prediction.error
  # mse.gauss <- ranger::ranger(u~., data = data.frame(u=u.knn,x=d$X))$prediction.error
  # 
  # #p.null <- sapply(1:nrep, function(i) ranger::ranger(u~., data = data.frame(u=runif(nrow(d$X)),x=d$X))$prediction.error)
  # 
  # #p.res <- (sum(p.null <= mse.res)+1) / (length(p.null)+1)
  # #p.mrf <- (sum(p.null <= mse.mrf)+1) / (length(p.null)+1)
  # #p.gini <- (sum(p.null <= mse.gini)+1) / (length(p.null)+1)
  # #p.knn <- (sum(p.null <= mse.knn)+1) / (length(p.null)+1)
  # #p.gauss <- (sum(p.null <= mse.knn)+1) / (length(p.null)+1)
  # #pf(q = res_coverage$scores_CV_gauss, df1 = d, df2 = n-d)
  # 
  # 
  # ## random pinball
  # 
  # mean.pinball.mrf <-   mean(res_pinball$mrf_loss - res_pinball$mrf_loss<=0)
  # mean.pinball.gini <-  mean(res_pinball$mrf_loss - res_pinball$gini_loss<=0)
  # mean.pinball.res <-   mean(res_pinball$mrf_loss - res_pinball$res_loss<=0)
  # mean.pinball.knn <-   mean(res_pinball$mrf_loss - res_pinball$knn_loss<=0)
  # mean.pinball.gauss <- mean(res_pinball$mrf_loss - res_pinball$gauss_loss<=0)
  # 
  # # max
  # max.pinball.mrf <-  max(apply(res_pinball$mrf_loss, 1, mean))
  # max.pinball.gini <- max(apply(res_pinball$gini_loss, 1, mean))
  # max.pinball.res <- max(apply(res_pinball$res_loss, 1, mean))
  # max.pinball.knn <- max(apply(res_pinball$knn_loss, 1, mean))
  # max.pinball.gauss <- max(apply(res_pinball$gauss_loss, 1, mean))
  # 
  # ## random pinball (NL)
  # 
  # # mean
  # mean.pinball.nl.mrf <- mean(res_pinball_nl$mrf_loss)
  # mean.pinball.nl.gini <- mean(res_pinball_nl$gini_loss)
  # mean.pinball.nl.knn <- mean(res_pinball_nl$knn_loss)
  # mean.pinball.nl.gauss <- mean(res_pinball_nl$gauss_loss)
  # 
  # # max
  # max.pinball.nl.mrf <- max(apply(res_pinball_nl$mrf_loss, 1, max))
  # max.pinball.nl.gini <- max(apply(res_pinball_nl$mrf_loss, 1, max))
  # # max.pinball.nl.res <- max(apply(res_pinball_nl$res_loss, 1, max))
  # max.pinball.nl.knn <- max(apply(res_pinball_nl$mrf_loss, 1, max))
  # max.pinball.nl.gauss <-max(apply(res_pinball_nl$mrf_loss, 1, max))
  # 
  # ## coverage
  # 
  # # compute the p-values
  # #p.val.mrf <- permRF(y = res_coverage$scores_CV_mrf, X = d$X, nrep = 100)
  # #p.val.mrf.global <-permRF(y = res_coverage$scores_CV_gini_global, X = d$X, nrep = 100)
  # #p.val.gini <-permRF(y = res_coverage$scores_CV_gini, X = d$X, nrep = 100)
  # #p.val.gini.global <-permRF(y = res_coverage$scores_CV_gini_global, X = d$X, nrep = 100)
  # 
  
  return(list(
    heter0 = heter0,
    heter1 = heter1,
    heter2 = heter2
    # mean.pinball.mrf = mean.pinball.mrf,
    # mean.pinball.gini = mean.pinball.gini,
    # mean.pinball.res = mean.pinball.res,
    # mean.pinball.knn = mean.pinball.knn,
    # mean.pinball.gauss = mean.pinball.gauss,
    # 
    # max.pinball.mrf = max.pinball.mrf,
    # max.pinball.gini = max.pinball.gini,
    # max.pinball.res = max.pinball.res,
    # max.pinball.knn = max.pinball.knn,
    # max.pinball.gauss = max.pinball.gauss,
    # 
    # mean.pinball.nl.mrf = mean.pinball.nl.mrf,
    # mean.pinball.nl.gini = mean.pinball.nl.gini,
    # mean.pinball.nl.knn = mean.pinball.nl.knn,
    # mean.pinball.nl.gauss = mean.pinball.nl.gauss,
    # 
    # max.pinball.nl.mrf = max.pinball.nl.mrf,
    # max.pinball.nl.gini = max.pinball.nl.gini,
    # max.pinball.nl.knn = max.pinball.nl.knn,
    # max.pinball.nl.gauss = max.pinball.nl.gauss,
    # 
    # coverage.mrf = res_coverage$coverage_mrf, 
    # coverage.gini = res_coverage$coverage_gini, 
    # coverage.res = res_coverage$coverage_res,
    # coverage.knn = res_coverage$coverage_knn,
    # coverage.gauss = res_coverage$coverage_gauss,
    # #coverage.mrf.global = res_coverage$coverage_mrf_global, 
    # #coverage.gini.global = res_coverage$coverage_gini_global 
    # mse.res = mse.res,
    # mse.mrf = mse.mrf,
    # mse.gini = mse.gini,
    # mse.knn = mse.knn,
    # mse.gauss = mse.gauss,
    # vec.mse.u.mrf = vec.mse.u.mrf,
    # vec.mse.u.gini = vec.mse.u.gini,
    # vec.mse.u.knn = vec.mse.u.knn,
    # vec.mse.u.gauss = vec.mse.u.gauss
    # #p.val.mrf = p.val.mrf, 
    # #p.val.mrf.global = p.val.mrf.global,
    # #p.val.gini = p.val.gini,
    # #p.val.gini.global = p.val.gini.global)
  ))
}

W2unif <- function(s) {
  uq <- qunif(p = c(1:length(s))/(length(s)+1))
  
  return(mean((s-uq)^2))
}

# y_train <- d$Y[1:100,]
# y_test <- d$Y[1:10,]
# w_train <- matrix(runif(nrow(y_train)*nrow(y_train)),nrow=nrow(y_train),ncol=nrow(y_train))
# w_test_train <- matrix(runif(nrow(y_test)*nrow(y_train)),nrow=nrow(y_test),ncol=nrow(y_train))
# w_train <- w_train / rowSums(w_train)
# w_test_train <- w_test_train / rowSums(w_test_train)
# k <- function(y1,y2) {exp(-sum((y1-y2)^2))}

# function to compute the RKHS MSE
computeRKHSgaussMSE <- function(y_test, y_train, w_test, k) {
  
  # training kernel
  k_train <- apply(y_train, 1, function(y1) apply(y_train, 1, function(y2) k(y1,y2)))
  # cross testing-training kernel
  k_test_train <- t(apply(y_test, 1, function(y1) apply(y_train, 1, function(y2) k(y1,y2))))
  
  # compute the values (biased)
  vals <- sapply(1:nrow(y_test), function(i) k(y_test[i,],y_test[i,]) - 2*sum(k_test_train[i,]*w_test[i,]) + t(w_test[i,])%*%(k_train%*%w_test[i,]))
  
  return(vals)
}

runRKHSanalysis <- function(X, 
                            Y,
                            k = 10, 
                            seed = 0,
                            ...) {
  
  # repro
  set.seed(seed)
  
  # create folds
  folds <- kFoldCV(n = nrow(X), k = k)
  
  # properties of the simulations
  mrf_mse <- numeric(nrow(X))
  gini_mse <- numeric(nrow(X))
  
  # CV loop
  for (kk in 1:k) {
    
    print(paste0("CV loop: ", kk))
    
    
    mRF <- mrf(X = X[-folds[[kk]],], Y = Y[-folds[[kk]],], splitting.rule = "fourier", ...)
    giniRF <- mrf(X = X[-folds[[kk]],], Y = Y[-folds[[kk]],], splitting.rule = "gini")
    
    w_mrf <- predict(mRF, type = "weights", newdata = X[folds[[kk]],])$weights
    mrf_mse[folds[[kk]]] <- computeRKHSgaussMSE(y_test = Y[folds[[kk]],], y_train = Y[-folds[[kk]],],w_test = w_mrf, k = function(y1,y2) {exp(-sum((y1-y2)^2))})
    
    w_gini <- predict(giniRF, type = "weights", newdata = X[folds[[kk]],])$weights
    gini_mse[folds[[kk]]] <- computeRKHSgaussMSE(y_test = Y[folds[[kk]],], y_train = Y[-folds[[kk]],],w_test = w_gini, k = function(y1,y2) {exp(-sum((y1-y2)^2))})
    
  }
  
  return(list(mrf_mse = mrf_mse, gini_mse = gini_mse))
  
}


runCovAnalysis <- function(X, 
                                       Y,
                                       seed = 0,
                           k = 5,
                                       ...) {
  
  # repro
  set.seed(seed)
  
  # create folds
  folds <- kFoldCV(n = nrow(X), k = k)
  
  # properties of the simulations
  mrf_loss <- matrix(0,nrow=ncol(Y)^2, ncol=nrow(X))
  gini_loss <- matrix(0,nrow=ncol(Y)^2, ncol=nrow(X))
  knn_loss <- matrix(0,nrow=ncol(Y)^2, ncol=nrow(X))
  gauss_loss <- matrix(0,nrow=ncol(Y)^2, ncol=nrow(X))
  
  # CV loop
  for (kk in 1:k) {
    
    print(paste0("CV loop: ", kk))
    
    
    mRF <- mrf(X = X[-folds[[kk]],], Y = Y[-folds[[kk]],], splitting.rule = "fourier", ...)
    giniRF <- mrf(X = X[-folds[[kk]],], Y = Y[-folds[[kk]],], splitting.rule = "gini")
    
    #comp.knn <- KNN(X = X.knn[-folds[[kk]],], Y = Y[-folds[[kk]],])
    #comp.gauss <- GaussKernel(X = X.gauss[-folds[[kk]],], Y = Y[-folds[[kk]],])
    
    pairs <- expand.grid(1:ncol(Y),1:ncol(Y))
    pairs <- pairs[1:(nrow(pairs)/2),]
    
    for (i in 1:nrow(pairs)) {
      
      # yhat_mrf <- predict(mRF, newdata = X[folds[[kk]],], type = "functional", 
      #                     quantiles = NULL, f = function(y) y[pairs[i,1]]*y[pairs[i,2]])$functional
      # yhat_gini <- predict(giniRF, newdata = X[folds[[kk]],], type = "functional", 
      #                      quantiles = NULL, f = function(y) y[pairs[i,1]]*y[pairs[i,2]])$functional
      
      yhat_mrf <- predict(mRF, newdata = X[folds[[kk]],], type = "cov")$cov
      yhat_gini <- predict(giniRF, newdata = X[folds[[kk]],], type = "cov")$cov

      
        mrf_loss[i,folds[[kk]]] <- (Y[folds[[kk]],pairs[i,1]]*Y[folds[[kk]],pairs[i,2]]-yhat_mrf[,pairs[i,1],pairs[i,2]])^2
        gini_loss[i,folds[[kk]]] <- (Y[folds[[kk]],pairs[i,1]]*Y[folds[[kk]],pairs[i,2]]-yhat_gini[,pairs[i,1],pairs[i,2]])^2
        
    
    }
  }
  
  return(list(mrf_loss = mrf_loss, gini_loss = gini_loss))
  
}
