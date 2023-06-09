#' Distributional Random Forests
#'
#' Trains a Distributional Random Forest which estimates the full conditional distribution \eqn{P(Y | X)}
#' for possibly multivariate response Y and predictors X. The conditional distribution estimate is represented
#' as a weighted distribution of the training data. The weights can be conveniently used in the downstream analysis
#' to estimate any quantity of interest \eqn{\tau(P(Y | X))}.
#' @param X The covariates used in the regression. Can be either a matrix of numerical values, or a data frame with columns of any data type.
#' @param Y The (multivariate) outcome variable. Needs to be a matrix or a data frame consisting of numeric values.
#' @param num.trees Number of trees grown in the forest. Default is 500.
#' @param splitting.rule A character value. The type of the splitting rule used, can be either "FourierMMD" (MMD splitting criterion with FastMMD approximation for speed) or "CART" (sum of standard CART criteria over the components of Y).
#' @param num.features A numeric value, in case of "FourierMMD", the number of random features to sample.
#' @param bandwidth A numeric value, the bandwidth of the Gaussian kernel used in case of "FourierMMD", the value is set to NULL by default and the median heuristic is used.
#' @param response.scaling A boolean value, should the responses be standardized before fitting the forest. Default is TRUE.
#' @param node.scaling A boolean value, should the responses be standardized in every node of every tree. Default is FALSE.
#' @param sample.weights (experimental) Weights given to an observation in estimation.
#'                       If NULL, each observation is given the same weight. Default is NULL.
#' @param sample.fraction Fraction of the data used to build each tree.
#'                        Note: If honesty = TRUE, these subsamples will
#'                        further be cut by a factor of honesty.fraction. Default is 0.5.
#' @param mtry Number of variables tried for each split. Default is
#'             \eqn{\sqrt p + 20}, where p is the number of predictors.
#' @param min.node.size A target for the minimum number of observations in each tree leaf. Note that nodes
#'                      with size smaller than min.node.size can occur, as in the original randomForest package.
#'                      Default is 5.
#' @param honesty Whether to use honest splitting (i.e., sub-sample splitting). Default is TRUE.
#'  For a detailed description of honesty, honesty.fraction, honesty.prune.leaves, and recommendations for
#'  parameter tuning, see the \href{https://grf-labs.github.io/grf/REFERENCE.html#honesty-honesty-fraction-honesty-prune-leaves}{GRF reference}
#'  for more information (the original source).
#' @param honesty.fraction The fraction of data that will be used for determining splits if honesty = TRUE. Default is 0.5 (i.e. half of the data is used for
#'                         determining splits and the other half for populating the nodes of the tree).
#' @param honesty.prune.leaves If TRUE, prunes the estimation sample tree such that no leaves
#'  are empty. If FALSE, keeps the same tree as determined in the splits sample (if an empty leave is encountered, that
#'  tree is skipped and does not contribute to the estimate). Setting this to FALSE may improve performance on
#'  small/marginally powered data, but requires more trees (note: tuning does not adjust the number of trees).
#'  Only applies if honesty is enabled. Default is TRUE.
#' @param alpha A tuning parameter that controls the maximum imbalance of a split. Default is 0.05, meaning a child node will contain at most 5\% of observations in the parent node.
#' @param imbalance.penalty A tuning parameter that controls how harshly imbalanced splits are penalized. Default is 0.
#' @param compute.oob.predictions Whether OOB predictions on training set should be precomputed. Default is TRUE.
#' @param num.threads Number of threads used in training. By default, the number of threads is set
#'                    to the maximum hardware concurrency.
#' @param seed The seed of the C++ random number generator.
#' @param compute.variable.importance boolean, should the variable importance be computed in the object.
#'
#' @return A trained Distributional Random Forest object.
#'
#' @examples
#' library(drf)
#'
#' n = 10000
#' p = 20
#' d = 3
#' 
#' # Generate training data
#' X = matrix(rnorm(n * p), nrow=n)
#' Y = matrix(rnorm(n * d), nrow=n)
#' Y[, 1] = Y[, 1] + X[, 1]
#' Y[, 2] = Y[, 2] * X[, 2]
#' Y[, 3] = Y[, 3] * X[, 1] + X[, 2]
#' 
#' # Fit DRF object
#' drf.forest = drf(X, Y)
#' 
#' # Generate test data
#' X_test = matrix(rnorm(10 * p), nrow=10)
#' 
#' out = predict(drf.forest, newdata=X_test)
#' # Compute E[Y_1 | X] for all data in X_test directly from
#' # the weights representing the estimated distribution
#' out$weights %*% out$y[,1]
#' 
#' out = predict(drf.forest, newdata=X_test,
#'               functional='mean')
#' # Compute E[Y_1 | X] for all data in X_test using built-in functionality
#' out[,1]
#' 
#' out = predict(drf.forest, newdata=X_test,
#'               functional='quantile',
#'               quantiles=c(0.25, 0.75),
#'               transformation=function(y){y[1] * y[2] * y[3]})
#' # Compute 25% and 75% quantiles of Y_1*Y_2*Y_3, conditionally on X = X_test[1, ]
#' out[1,,]
#' 
#' out = predict(drf.forest, newdata=X_test,
#'               functional='cov',
#'               transformation=function(y){matrix(1:6, nrow=2) %*% y})
#' # Compute 2x2 covariance matrix for (1*Y_1 + 3*Y_2 + 5*Y_3, 2*Y_1 + 4*Y_2 + 6*Y_3),
#' # conditionally on X = X_test[1, ]
#' out[1,,]
#' 
#' out = predict(drf.forest, newdata=X_test,
#'               functional='custom',
#'               custom.functional=function(y, w){c(sum(y[, 1] * w), sum(y[, 2] * w))})
#' # Compute E[Y_1, Y_2 | X] for all data in X_test by providing custom functional that
#' # computes it from the weights
#' out
#'
#' @export
#' @useDynLib drf
#' @importFrom Rcpp evalCpp
#' @importFrom utils modifyList
drf <-               function(X, Y,
                              num.trees = 500,
                              splitting.rule = "FourierMMD",
                              num.features = 10,
                              bandwidth = NULL,
                              response.scaling = TRUE,
                              node.scaling = FALSE,
                              sample.weights = NULL,
                              sample.fraction = 0.5,
                              mtry = min(ceiling(sqrt(ncol(X)) + 20), ncol(X)),
                              min.node.size = 15,
                              honesty = TRUE,
                              honesty.fraction = 0.5,
                              honesty.prune.leaves = TRUE,
                              alpha = 0.05,
                              imbalance.penalty = 0,
                              compute.oob.predictions = TRUE,
                              num.threads = NULL,
                              seed = stats::runif(1, 0, .Machine$integer.max),
                              compute.variable.importance = FALSE) {
  
  # initial checks for X and Y
  if (is.data.frame(X)) {
    
    if (is.null(names(X))) {
      stop("the regressor should be named if provided under data.frame format.")
    }
    
    if (any(apply(X, 2, class) %in% c("factor", "character"))) {
      any.factor.or.character <- TRUE
      X.mat <- as.matrix(fastDummies::dummy_cols(X, remove_selected_columns = TRUE))
    } else {
      any.factor.or.character <- FALSE
      X.mat <- as.matrix(X)
    }
    
    mat.col.names.df <- names(X)
    mat.col.names <- colnames(X.mat)
  } else {
    X.mat <- X
    mat.col.names <- NULL
    mat.col.names.df <- NULL
    any.factor.or.character <- FALSE
  }
  
  if (is.data.frame(Y)) {
    
    if (any(apply(Y, 2, class) %in% c("factor", "character"))) {
      stop("Y should only contain numeric variables.")
    }
    Y <- as.matrix(Y)
  }
  
  if (is.vector(Y)) {
    Y <- matrix(Y,ncol=1)
  }
  
  
  #validate_X(X.mat)
  
  if (inherits(X, "Matrix") && !(inherits(X, "dgCMatrix"))) {
        stop("Currently only sparse data of class 'dgCMatrix' is supported.")
    }
  
  validate_sample_weights(sample.weights, X.mat)
  #Y <- validate_observations(Y, X)
  
  # set legacy GRF parameters
  clusters <- vector(mode = "numeric", length = 0)
  samples.per.cluster <- 0
  equalize.cluster.weights <- FALSE
  ci.group.size <- 1
  
  num.threads <- validate_num_threads(num.threads)
  
  all.tunable.params <- c("sample.fraction", "mtry", "min.node.size", "honesty.fraction",
                          "honesty.prune.leaves", "alpha", "imbalance.penalty")
  
  # should we scale or not the data
  if (response.scaling) {
    Y.transformed <- scale(Y)
  } else {
    Y.transformed <- Y
  }
  
  data <- create_data_matrices(X.mat, outcome = Y.transformed, sample.weights = sample.weights)
  
  # bandwidth using median heuristic by default
  if (is.null(bandwidth)) {
    bandwidth <- medianHeuristic(Y.transformed)
  }
  
  
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
  forest[["is.df.X"]] <- is.data.frame(X)
  forest[["Y.orig"]] <- Y
  forest[["sample.weights"]] <- sample.weights
  forest[["clusters"]] <- clusters
  forest[["equalize.cluster.weights"]] <- equalize.cluster.weights
  forest[["tunable.params"]] <- args[all.tunable.params]
  forest[["mat.col.names"]] <- mat.col.names
  forest[["mat.col.names.df"]] <- mat.col.names.df
  forest[["any.factor.or.character"]] <- any.factor.or.character
  
  if (compute.variable.importance) {
    forest[['variable.importance']] <- variableImportance(forest, h = bandwidth)
  }
  
  forest
}
