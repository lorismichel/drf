#' Distributional Random Forests
#'
#' Trains a distributional random forest that can be used to estimate
#' statistical functional F(P(Y | X)) for possibly multivariate response Y.
#'
#' @param X The covariates used in the regression. Can be either a matrix of numerical values, or a data.frame with characters and factors. In the latter case,
#'   one-hot-encoding will be implicitely used.
#' @param Y The (multivariate) outcome. A matrix or data.frame of numeric values.
#' @param num.trees Number of trees grown in the forest. Default is 500.
#' @param splitting.rule a character value. The type of splitting rule used, can be either "CART" or "FourierMMD".
#' @param num.features a numeric value, in case of "FourierMMD", the number of random features to sample.
#' @param bandwidth a numeric value, the bandwidth of the Gaussian kernel used in case of "FourierMMD".
#' @param node.scaling a boolean value, should the responses be scaled or not by node.
#' @param sample.weights (experimental) Weights given to an observation in estimation.
#'                       If NULL, each observation is given the same weight. Default is NULL.
#' @param clusters Vector of integers or factors specifying which cluster each observation corresponds to.
#'  Default is NULL (ignored).
#' @param equalize.cluster.weights If FALSE, each unit is given the same weight (so that bigger
#'  clusters get more weight). If TRUE, each cluster is given equal weight in the forest. In this case,
#'  during training, each tree uses the same number of observations from each drawn cluster: If the
#'  smallest cluster has K units, then when we sample a cluster during training, we only give a random
#'  K elements of the cluster to the tree-growing procedure. When estimating average treatment effects,
#'  each observation is given weight 1/cluster size, so that the total weight of each cluster is the
#'  same. Note that, if this argument is FALSE, sample weights may also be directly adjusted via the
#'  sample.weights argument. If this argument is TRUE, sample.weights must be set to NULL. Default is
#'  FALSE.
#' @param sample.fraction Fraction of the data used to build each tree.
#'                        Note: If honesty = TRUE, these subsamples will
#'                        further be cut by a factor of honesty.fraction. Default is 0.5.
#' @param mtry Number of variables tried for each split. Default is
#'             \eqn{\sqrt p + 20} where p is the number of variables.
#' @param min.node.size A target for the minimum number of observations in each tree leaf. Note that nodes
#'                      with size smaller than min.node.size can occur, as in the original randomForest package.
#'                      Default is 5.
#' @param honesty Whether to use honest splitting (i.e., sub-sample splitting). Default is TRUE.
#'  For a detailed description of honesty, honesty.fraction, honesty.prune.leaves, and recommendations for
#'  parameter tuning, see the grf reference for more information
#'  \href{https://grf-labs.github.io/grf/REFERENCE.html#honesty-honesty-fraction-honesty-prune-leaves}{algorithm reference}.
#' @param honesty.fraction The fraction of data that will be used for determining splits if honesty = TRUE. Corresponds
#'                         to set J1 in the notation of the paper. Default is 0.5 (i.e. half of the data is used for
#'                         determining splits).
#' @param honesty.prune.leaves If TRUE, prunes the estimation sample tree such that no leaves
#'  are empty. If FALSE, keep the same tree as determined in the splits sample (if an empty leave is encountered, that
#'  tree is skipped and does not contribute to the estimate). Setting this to FALSE may improve performance on
#'  small/marginally powered data, but requires more trees (note: tuning does not adjust the number of trees).
#'  Only applies if honesty is enabled. Default is TRUE.
#' @param alpha A tuning parameter that controls the maximum imbalance of a split. Default is 0.05.
#' @param imbalance.penalty A tuning parameter that controls how harshly imbalanced splits are penalized. Default is 0.
#' @param ci.group.size The forest will grow ci.group.size trees on each subsample.
#'                      In order to provide confidence intervals, ci.group.size must
#'                      be at least 2. Default is 2.
#' @param compute.oob.predictions Whether OOB predictions on training set should be precomputed. Default is TRUE.
#' @param num.threads Number of threads used in training. By default, the number of threads is set
#'                    to the maximum hardware concurrency.
#' @param seed The seed of the C++ random number generator.
#'
#' @return A trained distributional random forest object.
#'
#' @examples
#' \dontrun{
#' # Train a distributional random forest with CART splitting rule.
#' n <- 500
#' p <- 10
#' X <- matrix(rnorm(n * p), n, p)
#' Y <- X + matrix(rnorm(n * p), ncol=p)
#' drf.forest <- drf(X = X, Y = Y)
#'
#' # Predict conditional correlation.
#' X.test <- matrix(0, 101, p)
#' X.test[, 1] <- seq(-2, 2, length.out = 101)
#' cor.pred <- predict(drf.forest, X.test, functional = "cor")
#'
#' # Predict on out-of-bag training samples.
#' cor.oob.pred <- predict(r.forest,  functional = "cor")
#' 
#' # Train a distributional random forest with "FourierMMD" splitting rule.
#' n <- 500
#' p <- 10
#' X <- matrix(rnorm(n * p), n, p)
#' Y <- X + matrix(rnorm(n * p), ncol=p)
#' drf.forest <- drf(X = X, Y = Y, splitting.rule = "FourierMMD", num.features = 10)
#'
#' # Predict conditional correlation.
#' X.test <- matrix(0, 101, p)
#' X.test[, 1] <- seq(-2, 2, length.out = 101)
#' cor.pred <- predict(drf.forest, X.test, functional = "cor")
#'
#' # Predict on out-of-bag training samples.
#' cor.oob.pred <- predict(r.forest,  functional = "cor")
#'
#' }
#'
#' @export
#' @useDynLib drf
#' @importFrom Rcpp evalCpp
#' @importFrom utils modifyList
drf <-               function(X, Y,
                              num.trees = 500,
                              splitting.rule = "FourierMMD",
                              num.features = 10,
                              bandwidth = 1.0,
                              node.scaling = FALSE,
                              sample.weights = NULL,
                              clusters = NULL,
                              equalize.cluster.weights = FALSE,
                              sample.fraction = 0.5,
                              mtry = min(ceiling(sqrt(ncol(X)) + 20), ncol(X)),
                              min.node.size = 5,
                              honesty = TRUE,
                              honesty.fraction = 0.5,
                              honesty.prune.leaves = TRUE,
                              alpha = 0.05,
                              imbalance.penalty = 0,
                              ci.group.size = 2,
                              compute.oob.predictions = TRUE,
                              num.threads = NULL,
                              seed = stats::runif(1, 0, .Machine$integer.max)) {
  
  # initial checks for X and Y
  if (is.data.frame(X)) {
    
    if (any(apply(X, 2, class) %in% c("factor", "character"))) {
      
      any.factor.or.character <- TRUE
      X.mat <- as.matrix(fastDummies::dummy_cols(X, remove_selected_columns = TRUE))
    } else {
      
      any.factor.or.character <- FALSE
      X.mat <- as.matrix(X)
    }
    
    mat.col.names <- colnames(X.mat)
  } else {
    X.mat <- X
    mat.col.names <- NULL
    any.factor.or.character <- FALSE
  }
  
  if (is.data.frame(Y)) {
    
    if (any(apply(Y, 2, class) %in% c("factor", "character"))) {
      stop("Y should only contain numeric variables.")
    }
    Y <- as.matrix(Y)
  }
  
  
  validate_X(X.mat)
  validate_sample_weights(sample.weights, X.mat)
  #Y <- validate_observations(Y, X)
  clusters <- validate_clusters(clusters, X.mat)
  samples.per.cluster <- validate_equalize_cluster_weights(equalize.cluster.weights, clusters, sample.weights)
  num.threads <- validate_num_threads(num.threads)

  all.tunable.params <- c("sample.fraction", "mtry", "min.node.size", "honesty.fraction",
                          "honesty.prune.leaves", "alpha", "imbalance.penalty")

  data <- create_data_matrices(X.mat, outcome = scale(Y), sample.weights = sample.weights)

  args <- list(num.trees = num.trees,
               clusters = clusters,
               samples.per.cluster = samples.per.cluster,
               sample.fraction = sample.fraction,
               mtry = mtry,
               min.node.size = min.node.size,
               honesty = honesty,
               honesty.fraction = honesty.fraction,
               honesty.prune.leaves = honesty.prune.leaves,
               alpha = alpha,
               imbalance.penalty = imbalance.penalty,
               ci.group.size = ci.group.size,
               compute.oob.predictions = compute.oob.predictions,
               num.threads = num.threads,
               seed = seed,
               num_features = num.features, 
               bandwidth = bandwidth,
               node_scaling = ifelse(node.scaling, 1, 0))

   if (splitting.rule == "CART") {
     ##forest <- do.call(gini_train, c(data, args))
     forest <- do.call.rcpp(gini_train, c(data, args))
     ##forest <- do.call(gini_train, c(data, args))
   } else if (splitting.rule == "FourierMMD") {
     forest <- do.call.rcpp(fourier_train, c(data, args))
   } else {
     stop("splitting rule not available.")
   }
   
   class(forest) <- c("drf")
   forest[["ci.group.size"]] <- ci.group.size
   forest[["X.orig"]] <- X.mat
   forest[["Y.orig"]] <- Y
   forest[["sample.weights"]] <- sample.weights
   forest[["clusters"]] <- clusters
   forest[["equalize.cluster.weights"]] <- equalize.cluster.weights
   forest[["tunable.params"]] <- args[all.tunable.params]
   forest[["mat.col.names"]] <- mat.col.names
   forest[["any.factor.or.character"]] <- any.factor.or.character
   
   forest
}

#' Predict with a drf forest
#'
#'
#' @param object The trained drf forest.
#' @param newdata Points at which predictions should be made. If NULL, makes out-of-bag
#'                predictions on the training set instead (i.e., provides predictions at
#'                Xi using only trees that did not use the i-th training example). Note
#'                that this matrix (or vector) should have the number of columns as the training
#'                matrix, and that the columns must appear in the same order.
#' @param functional which type of statistical functional. One option between "mean", "sd", "quantile", "cor", "cov", "ecdf" or "normalPredictionScore".
#' @param transformation a function giving a transformation of the responses, by default if NULL, the identity \code{function(y) y} is used.
#' @param num.threads Number of threads used in training. If set to NULL, the software
#'                    automatically selects an appropriate amount.
#' @param ... additional parameters.
#'
#' @return a list containing an entry with the same name as the functional selected. 
#'
#' @examples
#' \dontrun{
#' # Train a distributional random forest with CART splitting rule.
#' n <- 500
#' p <- 10
#' X <- matrix(rnorm(n * p), n, p)
#' Y <- X + matrix(rnorm(n * p), ncol=p)
#' drf.forest <- drf(X = X, Y = Y)
#'
#' # Predict conditional correlation.
#' X.test <- matrix(0, 101, p)
#' X.test[, 1] <- seq(-2, 2, length.out = 101)
#' cor.pred <- predict(drf.forest, X.test, functional = "cor")
#'
#' # Predict on out-of-bag training samples.
#' cor.oob.pred <- predict(r.forest,  functional = "cor")
#' 
#' # Train a distributional random forest with "FourierMMD" splitting rule.
#' n <- 500
#' p <- 10
#' X <- matrix(rnorm(n * p), n, p)
#' Y <- X + matrix(rnorm(n * p), ncol=p)
#' drf.forest <- drf(X = X, Y = Y, splitting.rule = "FourierMMD", num.features = 10)
#'
#' # Predict conditional correlation.
#' X.test <- matrix(0, 101, p)
#' X.test[, 1] <- seq(-2, 2, length.out = 101)
#' cor.pred <- predict(drf.forest, X.test, functional = "cor")
#'
#' # Predict on out-of-bag training samples.
#' cor.oob.pred <- predict(r.forest,  functional = "cor")
#'
#' }
#'
#' @method predict drf
#' @export
#' 
predict.drf <- function(object, 
                        newdata = NULL,
                        transformation = NULL,
                        functional = NULL,
                        num.threads = NULL,
                        ...) {
  
  # if the newdata is a data.frame we should be careful about the non existing levels
  if (is.data.frame(newdata)) {
    
    if (object$any.factor.or.character) {
      newdata.mat <- as.matrix(fastDummies::dummy_cols(.data = newdata, remove_selected_columns = TRUE))
    } else {
      newdata.mat <- as.matrix(newdata)
    }
    
    col.to.add <- setdiff(object$mat.col.names, colnames(newdata.mat))
    
    for (col in col.to.add) {
      newdata.mat[,col] <- 0
    }
    
    newdata.mat <- newdata.mat[,object$mat.col.names]
  } else {
    
    newdata.mat <- newdata
  }
  
  
  # support vector as input 
  if (is.null(dim(newdata.mat))) {
    newdata.mat <- matrix(newdata.mat, 1)
  }
  
  # get the weights which are used in a second step
  w <- get_sample_weights(forest = object, 
                          newdata = newdata.mat, 
                          num.threads = num.threads)
  
  
  if (!is.null(transformation) && !(functional %in% c("mean", "quantile", "sd", 
                                                      "cor", "cov",
                                                      "normalPredictionScore", "cdf"))) {
    stop("transformation not available.")
  }
  
  
  if (is.null(transformation)) {
    transformation <- function(y) y
  }
  
  if (is.null(functional)) {
    
    # return the weights 
    return(list(weights = w, 
                y = object$Y.orig))
    
  } else if (functional %in% c("mean",
                               "quantile", 
                               "sd")) {
    
    
    # get the additional parameters
    add.param <- list(...)
    
    if (functional == "quantile" && is.null(add.param$quantiles)) {
      stop("additional parameter quantiles should be provided when functional is quantile.")
    }
    
    # compute the functional on the training set
    functional.t <- t(apply(object$Y.orig, 
                            1, 
                            function(yy) transformation(yy)))
    
    # check length one (R size management)
    if (length(transformation(object$Y.ori[1,])) == 1) {
      functional.t <- t(functional.t)
    }
      
      # in case of quantile regression
      if (!is.null(add.param$quantiles)) {
        
        functional.val <- lapply(1:ncol(functional.t), function(j) t(apply(w, 1, function(ww) spatstat::weighted.quantile(x = functional.t[ww!=0, j], 
                                                                                                                          w = ww[ww!=0], 
                                                                                                                          probs = add.param$quantiles))))
        quantile.array <- array(dim = c(nrow(w), length(functional.t), length(add.param$quantiles)))                                                                      
      
        for (i in 1:length(functional.val)) {
        
        if (length(add.param$quantile) == 1) {
          functional.val[[i]] <- t(functional.val[[i]])
        }
        #colnames(functional.val[[i]]) <- paste("q=", round(add.param$quantiles, 2), sep="")
        quantile.array[,i,] <- functional.val[[i]]
      }
        
        
        return(list(quantile = quantile.array))
        
      }
    }
    
    # if no quantiles provided then conditional mean or sd, possibly multi-dimensional
    if (functional == "mean") {
      
      functional.mean <- t(apply(w, 1, function(ww) ww%*%functional.t))
      
      # check length one (R size management)
      if (length(transformation(object$Y.ori[1,])) == 1) {
        
        functional.mean <- t(functional.mean)
      }
      
      return(list(mean = functional.mean))
      
    } else if (functional == "sd") {
      
      functional.mean <- t(apply(w, 1, function(ww) ww%*%functional.t))
      functional.mean2 <- t(apply(w, 1, function(ww) ww%*%(functional.t)^2))
      
      # check length one (R size management)
      if (length(transformation(object$Y.ori[1,])) == 1) {
        
        
        functional.mean <- t(functional.mean)
        functional.mean2 <- t(functional.mean2)
      }
      
      return(list(sd = sqrt(functional.mean2-(functional.mean)^2)))
    
  } else if (functional == "cor") {
    
    # compute the functional on the training set
    functional.t <- t(apply(object$Y.orig, 
                            1, 
                            function(yy) transformation(yy)))
    
    # check length one (R size management)
    if (length(transformation(object$Y.ori[1,])) == 1) {
      stop("cor available only for multi-dimensional transformation.")
    }
    
    cor.mat <- array(1, dim = c(nrow(w), ncol(functional.t), ncol(functional.t)))
    
    for (i in 1:nrow(w)) {
      cor.mat[i,,] <- stats::cov.wt(x = functional.t, wt = as.numeric(w[i,]), cor = TRUE)$cor
    } 
    
    return(list(cor = cor.mat))
    
  } else if (functional == "cov") {
    
    # compute the functional on the training set
    functional.t <- t(apply(object$Y.orig, 
                            1, 
                            function(yy) transformation(yy)))
    
    # check length one (R size management)
    if (length(transformation(object$Y.ori[1,])) == 1) {
      stop("cor available only for multi-dimensional transformation.")
    }
    
    cov.mat <- array(1, dim = c(nrow(w), ncol(functional.t), ncol(functional.t)))
    
    for (i in 1:nrow(w)) {
      cov.mat[i,,] <- stats::cov.wt(x = functional.t, wt = as.numeric(w[i,]))$cov
    }
    
    return(list(cov = cov.mat))
    
  }  else if (functional == "normalPredictionScore") {
    
    # compute the functional on the training set
    functional.t <- t(apply(object$Y.orig, 
                            1, 
                            function(yy) transformation(yy)))
    
    # check length one (R size management)
    if (length(transformation(object$Y.ori[1,])) == 1) {
      stop("cor available only for multi-dimensional transformation.")
    }
    
    means <- t(apply(w, 1, function(ww) ww%*%functional.t))
    
    covs <- array(1, dim = c(nrow(w), ncol(functional.t), ncol(functional.t)))
    
    for (i in 1:nrow(w)) {
      covs[i,,] <- stats::cov.wt(x = functional.t, wt = as.numeric(w[i,]))$cov
    } 
    
    # dims
    n <- nrow(object$Y.orig)
    d <- ncol(object$Y.orig)
    
    funs <- lapply(1:nrow(w), function(i) {
                    inv.cov <- solve(covs[i,,])
                    
                    return(function(y) (n/(n+1))*((n-d)/(d*(n-1)))*as.numeric((y-means[i,])%*%inv.cov%*%(y-means[i,])))
                  })
    
    return(list(normalPredictionScore = funs))
       
  } else if (functional == "cdf") {
    
    functional.t <- apply(object$Y.orig, 
                            1, 
                            function(yy) transformation(yy))
    
    # check length one (R size management)
    if (length(transformation(object$Y.ori[1,])) != 1) {
      stop("multi-dimensional ecdf not available.")
    } else {
      functional.t <- t(functional.t)
    }
    
    funs <- lapply(1:nrow(w), function(i) {
      return(function(y) spatstat::ewcdf(x = functional.t, weights = as.numeric(w[i,]))(y))
    })
    
    return(list(cdf = funs))
  } 
}
