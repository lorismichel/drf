#' MMD variable importance
#' compute an mmd based variable importance
#'
#' @param object a drf object
#' @param h the bandwidth parameter, default to 1.
#' @param type the type of importance
#'
#' @export
mmdVariableImportance <- function(object,
                                  h = 1,
                                  type = "raw") {

  # Gaussian kernel function with bandwith
  k <- function(x,y) exp(-sum((x-y)^2) / h)

  # stored importance
  perm.mmd <- rep(NA, ncol(object$X.orig)+1)

  # permute variables and compute MMD
  for (j in 0:ncol(object$X.orig)) {

    # permute
    X.perm <- object$X.orig
    if (j != 0) {
      X.perm[,j] <- sample(X.perm[,j], replace = FALSE)
    }

    # get the weights
    preds <- predict(object, newdata = X.perm)

    # get the kernel on training
    k.train <- apply(object$Y.orig, 1, function(y1) apply(object$Y.orig, 1, function(y2) k(y1,y2)))

    # three terms
    simple.term <- apply(object$Y.orig, 1, function(y) k(y,y))
    cross.term <- sapply(1:nrow(object$Y.orig), function(i) sum(preds$weights[i,] * apply(object$Y.orig,1,function(y) k(object$Y.orig[i,],y))))
    inner.term <- sapply(1:nrow(object$Y.orig), function(i) t(preds$weights[i,]) %*% k.train %*% preds$weights[i,])
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
