#' Predict from Distributional Random Forests object
#'
#' Obtain predictions from a DRF forest object. For any point \eqn{x} in the predictor space, it returns the estimate of the conditional distribution 
#' \eqn{P(Y | X=x)} represented as a weighted distribution \eqn{\sum_i w_i y_i} of the training observations \eqn{{y_i}}. 
#' Additionally, this function also provides support to directly obtain estimates of certain target quantities \eqn{\tau(P(Y | X))}, 
#' such as e.g. conditional quantiles, variances or correlations.
#'
#' @param object Trained DRF forest object.
#' @param newdata Points at which predictions should be made. If NULL, returns out-of-bag
#'                predictions on the training set (i.e., for every training point \eqn{X_i}, provides predictions using only trees which did not use
#'                this point for tree construction). Can be either a data frame, matrix or a vector. Each row represents a data point of interest and 
#'                the number and ordering of columns is assumed the be the same as in the training set.
#' @param functional Optional. String indicating the statistical functional that we want to compute from the weights. One option between:
#' \itemize{
#'  \item{"mean"}{ - Conditional mean, the returned value is a matrix \code{mean} of dimension \code{n} x \code{f},
#'  where \code{n} denotes the number of observations in \code{newdata} and \code{f} the dimension of the \code{transformation}.}
#'  \item{"sd"}{ - Conditional standard deviation for each component of the (transformed) response, the returned value is
#'  a matrix of dimension \code{n} x \code{f}, where \code{n} denotes the number of observations in \code{newdata} and \code{f} the dimension of the \code{transformation}.}
#'  \item{"quantile"}{ - Conditional quantiles. It requires additional parameter \code{quantiles} containing the list of quantile levels we want to compute. 
#'  The returned value is an array of dimension \code{n} x \code{f}  x \code{q}, where \code{n} denotes the number of observations in \code{newdata}, 
#'  \code{f} the dimension of the \code{transformation} and \code{q} the number of desired quantiles.}
#'  \item{"cor"}{ - Conditional correlation matrix, the returned value is an array of dimension \code{n} x \code{f} x \code{f},
#'  where \code{n} denotes the number of observations in \code{newdata} and \code{f} the dimension of the \code{transformation}.}
#'  \item{"cov"}{ - Conditional covariance matrix, the returned value is an array of dimension \code{n} x \code{f}  x \code{f},
#'  where \code{n} denotes the number of observations in \code{newdata}, \code{f} the dimension of the \code{transformation}.}
#'  \item{"custom"}{ - A custom function provided by the user, the returned value is a matrix of dimension \code{n} x \code{f},
#'  where \code{n} denotes the number of observations in \code{newdata} and \code{f} the dimension of the output of the function \code{custom.functional} provided by the user.}
#' }
#' @param transformation An optional transformation function that is applied to the responses before computing the target functional. It helps to extend the functionality to a much wider range of targets. 
#'                       The responses are not transformed by default, i.e. the identity function \eqn{f(y) = y} is used.
#' @param custom.functional A user-defined function when \code{functional} is set to "custom". This should be a function \code{f(y,w)} which for a single test point
#'                          takes the \code{n} x \code{f} matrix \code{y} and the corresponding \code{n}-dimensional vector of weights \code{w} and returns the quantity of interest given as a list of values.
#'                          \code{n} denotes the number of training observations and \code{f} the dimension of the \code{transformation}.
#' @param num.threads Number of threads used for computing. If set to NULL, the software automatically selects an appropriate amount.
#' @param ... additional parameters.
#'
#' @return If functional equals NULL, returns a list containing the matrix of training responses as well as the matrix of weights, whose number of rows corresponds the number of rows of "newdata" and the number of columns corresponds to the number of training data points.
#' If functional is specified, the desired quantity is returned, in the format described above.
#' 
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
#' @method predict drf
#' @export
#'
predict.drf <- function(object,
                        newdata = NULL,
                        functional = NULL,
                        transformation = NULL,
                        custom.functional = NULL,
                        num.threads = NULL,
                        ...) {
  
  # if the newdata is a data.frame we should be careful about the non existing levels
  if (!is.null(newdata) && is.data.frame(newdata)) {
    
    if (is.data.frame(newdata) && !object$is.df.X) {
      stop("data.frame for newdata is accepted only if it was used for training data.")
    }
    if (ncol(newdata) != length(object$mat.col.names.df)) {
      stop("newdata should have the same dimension as the training data.")
    }
    
    names(newdata) <- object$mat.col.names.df
    
    # check if factor or not
    if (!object$any.factor.or.character) {
      newdata.mat <- as.matrix(newdata)
    } else {
      newdata.mat <- as.matrix(fastDummies::dummy_cols(.data = newdata,
                                                       remove_selected_columns = TRUE))
      
      # define the modifications of the columns to do
      col.to.remove <- setdiff(colnames(newdata.mat), object$mat.col.names)
      col.to.add <- setdiff(object$mat.col.names, colnames(newdata.mat))
      
      # col to remove
      newdata.mat <- newdata.mat[,!(colnames(newdata.mat)%in%col.to.remove), drop = FALSE]
      
      # col to add
      prev.nb.col <- ncol(newdata.mat)
      prev.col.names <- colnames(newdata.mat)
      
      for (col in col.to.add) {
        newdata.mat <- cbind(newdata.mat, 0)
      }
      
      colnames(newdata.mat) <- c(prev.col.names, col.to.add)
      
      newdata.mat <- newdata.mat[, object$mat.col.names]
    }
  } else if (!is.null(newdata)) {
    newdata.mat <- newdata
  }
  
  # support vector as input
  if (!is.null(newdata) && is.null(dim(newdata.mat))) {
    newdata.mat <- matrix(newdata.mat, 1)
  } else if (is.null(newdata)) {
    newdata.mat <- NULL
  }
  
  
  if(!is.null(transformation) && is.null(functional)){
    stop("If functional is NULL, transformation should not be specified.")
  }
    
  if (is.null(transformation)) {
    transformation <- function(y) y
  }
  # compute the transformation on the training set
  Y_transformed <- t(apply(object$Y.orig, 1, function(yy) transformation(yy)))
  
  # check length one (R size management)
  if (length(transformation(object$Y.orig[1,])) == 1) {
    Y_transformed <- t(Y_transformed)
  }
  
  
  # get the weights which are used in the second step
  w <- get_sample_weights(forest = object,
                          newdata = newdata.mat,
                          num.threads = num.threads)
  
  
  if (is.null(functional)) {
    # return the weights
    return(list(weights = w, y = object$Y.orig))
  }
  
  if (functional == "quantile") {
    # get the additional parameters
    add.param <- list(...)
    
    if (is.null(add.param$quantiles)) {
      stop("Additional parameter 'quantiles' should be provided when the functional is quantile.")
    }
    
    functional.val <- lapply(1:ncol(Y_transformed), function(j) t(apply(w, 1, function(ww) weighted.quantile(x = Y_transformed[ww!=0, j],
                                                                                                            w = ww[ww!=0],
                                                                                                            probs = add.param$quantiles))))
    quantile.array <- array(dim = c(nrow(w), ncol(Y_transformed), length(add.param$quantiles)),
                            dimnames = list(NULL, NULL, paste("q=", round(add.param$quantiles, 2), sep="")))
    
    for (i in 1:length(functional.val)) {
      
      if (length(add.param$quantiles) == 1) {
        functional.val[[i]] <- t(functional.val[[i]])
      }
      #colnames(functional.val[[i]]) <- paste("q=", round(add.param$quantiles, 2), sep="")
      quantile.array[,i,] <- functional.val[[i]]
    }
      
    return(quantile.array)
  }
  
  if (functional == "custom") {
    custom <- apply(w, 1, function(ww) custom.functional(Y_transformed, ww))
    
    if(is.null(dim(custom))) return(matrix(custom, ncol=1))
    return(t(custom))
  }
  
  if (functional == "mean") {
    functional.mean <- t(apply(w, 1, function(ww) ww %*% Y_transformed))
    
    # check length one (R size management)
    if (length(transformation(object$Y.orig[1,])) == 1) {
      
      functional.mean <- t(functional.mean)
    }
    
    #colnames(functional.mean) <- colnames(object$Y.orig)
    
    return(functional.mean)
  }
  
  if (functional == "sd") {
    functional.mean <- t(apply(w, 1, function(ww) ww %*% Y_transformed))
    functional.mean2 <- t(apply(w, 1, function(ww) ww %*% (Y_transformed)^2))
    
    # check length one (R size management)
    if (length(transformation(object$Y.orig[1,])) == 1) {
      
      
      functional.mean <- t(functional.mean)
      functional.mean2 <- t(functional.mean2)
    }
    
    functional.sd <- sqrt(functional.mean2-(functional.mean)^2)
    #colnames(functional.sd) <- colnames(object$Y.orig)
    
    return(functional.sd)
  }
  
  if (functional == "cor") {
    cor.mat <- array(1, dim = c(nrow(w), ncol(Y_transformed), ncol(Y_transformed)),
                     dimnames = list(NULL, NULL, NULL))
    
    for (i in 1:nrow(w)) {
      cor.mat[i,,] <- stats::cov.wt(x = Y_transformed, wt = as.numeric(w[i,]), cor = TRUE)$cor
    }
    
    return(cor.mat)
  }
  
  if (functional == "cov") {
    cov.mat <- array(1, dim = c(nrow(w), ncol(Y_transformed), ncol(Y_transformed)),
                     dimnames = list(NULL, NULL, NULL))
    
    for (i in 1:nrow(w)) {
      cov.mat[i,,] <- stats::cov.wt(x = Y_transformed, wt = as.numeric(w[i,]))$cov
    }
    
    return(cov.mat)
  }
  
  if (functional == "normalPredictionScore") {
    # check length one (R size management)
    if (length(transformation(object$Y.orig[1,])) == 1) {
      stop("cor available only for multi-dimensional transformation.")
    }
    
    means <- t(apply(w, 1, function(ww) ww %*% Y_transformed))
    
    covs <- array(1, dim = c(nrow(w), ncol(Y_transformed), ncol(Y_transformed)))
    
    for (i in 1:nrow(w)) {
      covs[i,,] <- stats::cov.wt(x = Y_transformed, wt = as.numeric(w[i,]))$cov
    }
    
    # dims
    n <- nrow(object$Y.orig)
    d <- ncol(object$Y.orig)
    
    funs <- lapply(1:nrow(w), function(i) {
      inv.cov <- solve(covs[i,,])
      
      return(function(y) (n/(n+1))*((n-d)/(d*(n-1)))*as.numeric((y-means[i,])%*%inv.cov%*%(y-means[i,])))
    })
    
    return(funs)
  }
  
  if (functional == "MQ") {
    u <- list(...)$u
    
    if (!is.matrix(u) || ncol(u)!=ncol(object$Y.orig)) {
      stop("Incompatible u with the response y.")
    }
    
    # compute the cost between the provided u's and the y's
    costm <- t(apply(object$Y.orig, 1, function(yy) apply(u, 1, function(uu) {
      sum((yy-uu)^2)
    })))
    
    # get the transport solution
    info.mq <- apply(w,
                     1,
                     function(ww) {
                       ids.in <- which(ww!=0)
                       tr <- transport::transport(costm[ids.in,], a = ww[ids.in], b = rep(1/nrow(u),nrow(u)), fullreturn = TRUE)
                       ids.y <- apply(tr$primal, 2, function(x) sample(1:length(x), size = 1, replace = FALSE, prob = x))
                       return(list(ids.y=ids.y, ids.in=ids.in))
                     })
    
    # get one version of the multimap
    yhat <- lapply(info.mq, function(info) {object$Y.orig[info$ids.in,,drop=F][info$ids.y,,drop=F]})
    return(list(yhat = yhat, u = u))
    
  } 
  
  stop("Functional not implemented!")
}