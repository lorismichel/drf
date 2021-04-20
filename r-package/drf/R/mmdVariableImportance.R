#' Variable importance based on MMD
#' 
#' compute an mmd-based variable importance for the drf fit.
#'
#' @param object an S3 object of class drf.
#' @param h the bandwidth parameter, default to NULL using then the median heuristic.
#' @param response.scaling a boolean value indicating if the responses should be scaled globally beforehand.
#' @param type the type of importance, could be either "raw", the plain MMD values, "relative", the ratios to the observed MMD or "difference", the excess to the observed MMD
#' 
#' @return a vector of variable importance values.
#' 
#' @export
variableImportance <- function(object,
                                  h = NULL,
                                  response.scaling = TRUE,
                                  type = "difference") {

  # apply the response scaling if necessary  
  if (response.scaling) {
    Y.transformed <- scale(object$Y.orig)
  } else {
    Y.transformed <- object$Y.orig
  }
  
  # Gaussian kernel function with bandwidth h
  if (is.null(h)) {
    h <- medianHeuristic(Y.transformed)
  }
  
  k <- function(x,y) exp(-sum((x-y)^2) / (2*h^2))

  # store variable importance (with the non-permuted one also)
  perm.mmd <- rep(NA, ncol(object$X.orig)+1)

  # permute variables and compute MMD
  for (j in 0:ncol(object$X.orig)) {

    # permute
    X.perm <- object$X.orig
    
    if (j != 0) {
      X.perm[,j] <- sample(X.perm[,j], replace = FALSE)
    }

    # get the weights
    preds <- predict.drf(object, newdata = X.perm)

    # get the kernel on training
    k.train <- exp(as.matrix(-stats::dist(Y.transformed)^2/(2*h^2)))

    # three terms in the MMD
    simple.term <- apply(Y.transformed, 1, function(y) k(y,y))
    cross.term <- sapply(1:nrow(Y.transformed), function(i) sum(preds$weights[i,] * apply(Y.transformed,1,function(y) k(Y.transformed[i,],y))))
    inner.term <- sapply(1:nrow(Y.transformed), function(i) sapply(1:nrow(Y.transformed), function(j) t(preds$weights[i,]) %*% k.train %*% preds$weights[j,]))
    # average across training points
    perm.mmd[j+1] <- mean(simple.term - 2 * cross.term + inner.term)
  }

  # return values
  if (type == "raw") {
    names(perm.mmd) <- c(0:ncol(object$X.orig))
  } else if (type == "relative") {
    perm.mmd <- perm.mmd / perm.mmd[1]
    perm.mmd <- perm.mmd[-1]
    names(perm.mmd) <- c(1:ncol(object$X.orig))
  } else if (type == "difference") {
    perm.mmd <- perm.mmd - perm.mmd[1]
    perm.mmd <- perm.mmd[-1]
    names(perm.mmd) <- c(1:ncol(object$X.orig))
  }

  return(perm.mmd)
}
