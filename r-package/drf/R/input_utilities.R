validate_X <- function(X) {
  # vectors should be converted to matrix at this point
  valid.classes <- c("matrix", "data.frame", "dgCMatrix")
  
  if (!inherits(X, valid.classes)) {
    stop(paste(
      "For X, the only supported data input types are:",
      "`matrix`, `data.frame`, `dgCMatrix` (sparse Matrix)"
    ))
  }
  if (any(0 %in% dim(X))) {
    stop("Feature matrix X must have non-zero dimensions.")
  }
  
  # data.frame requires names
  if(is.data.frame(X)){
    
    if (any(colnames(X) == "") || is.null(colnames(X))) {
      stop("Feature matrix X has to be named if provided as data.frame.")
    }
    
    # Inputs have to be numeric, categorical (char, factor), or bool
    if(!all(sapply(X, function(x){
      is.numeric(x) || is.factor(x) || is.character(x)
    }))){
      stop(paste(
        "Feature matrix X may only contain data of type `numeric`, `factor`, ",
        "or `character` if provided as data.frame."))
    }
  }
  
  if(is.matrix(X)){
    # Only accept numeric (not character matrices and such) because only for
    # data.frame dummies will be made.
    if(!is.numeric(X)){
      stop("Feature matrix X has to be all numeric if provided as matrix.")
    }
  }
  
  # May contain NA 
  # if (any(is.na(X))) {
  #   stop("The feature matrix X contains at least one NA.")
  # }
  
}

validate_Y <- function(Y, n) {
  
  # vectors should be converted to matrix at this point
  if (!inherits(Y, c("matrix", "data.frame"))) {
    stop("For outcome Y, the only supported types are: `matrix` or `data.frame`.")
  }
  
  if (any(0 %in% dim(Y))) {
    stop("Outcome Y must have non-zero dimensions.")
  }
  
  if (NROW(Y) != n) {
    stop("Outcome Y does not have as many rows as X.")
  }
  
  # sapply works for data.frame and matrix (over cols)
  if(!all(sapply(Y, is.numeric))){
    stop(paste(
      "Outcome Y must be numeric. DRF does not ",
      "currently support non-numeric observations."
    ))
  }
  
  if (anyNA(Y)) {
    stop("Outcome Y contains at least one NA value.")
  }
  
}

validate_num_threads <- function(num.threads) {
  if (is.null(num.threads)) {
    num.threads <- 0
  } else if (!is.numeric(num.threads) | num.threads < 0) {
    stop("Error: Invalid value for num.threads")
  }
  num.threads
}

validate_boost_error_reduction <- function(boost.error.reduction) {
  if (boost.error.reduction < 0 || boost.error.reduction > 1) {
    stop("boost.error.reduction must be between 0 and 1")
  }
  boost.error.reduction
}

validate_ll_vars <- function(linear.correction.variables, num.cols) {
  if (is.null(linear.correction.variables)) {
    linear.correction.variables <- 1:num.cols
  }
  if (min(linear.correction.variables) < 1) {
    stop("Linear correction variables must take positive integer values.")
  } else if (max(linear.correction.variables) > num.cols) {
    stop("Invalid range of correction variables.")
  } else if (!is.vector(linear.correction.variables) |
             !all(linear.correction.variables == floor(linear.correction.variables))) {
    stop("Linear correction variables must be a vector of integers.")
  }
  linear.correction.variables
}

validate_ll_lambda <- function(lambda) {
  if (lambda < 0) {
    stop("Lambda cannot be negative.")
  } else if (!is.numeric(lambda) | length(lambda) > 1) {
    stop("Lambda must be a scalar.")
  }
  lambda
}

validate_ll_path <- function(lambda.path) {
  if (is.null(lambda.path)) {
    lambda.path <- c(0, 0.001, 0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 1, 10)
  } else if (min(lambda.path) < 0) {
    stop("Lambda values cannot be negative.")
  } else if (!is.numeric(lambda.path)) {
    stop("Lambda values must be numeric.")
  }
  lambda.path
}

validate_newdata <- function(newdata, X) {
  if (ncol(newdata) != ncol(X)) {
    stop("newdata must have the same number of columns as the training matrix.")
  }
  validate_X(newdata)
}

validate_sample_weights <- function(sample.weights, X) {
  if (!is.null(sample.weights)) {
    if (length(sample.weights) != nrow(X)) {
      stop("sample.weights has incorrect length")
    }
    # Checks from grf@4aaf2d510469c4f277f0293e241db62bca1d2892
    if (anyNA(sample.weights) || any(sample.weights < 0) || any(is.infinite(sample.weights))) {
      stop("sample.weights must be nonnegative and without missing values")
    }
  }
}

#' @importFrom Matrix Matrix cBind
#' @importFrom methods new
create_data_matrices <- function(X, outcome = NULL, sample.weights = FALSE) {
  default.data <- matrix(nrow = 0, ncol = 0)
  sparse.data <- new("dgCMatrix", Dim = c(0L, 0L))
  out <- list()
  
  if (!is.null(outcome)) {
    out[["outcome.index"]] <- NCOL(X) + 1:(1+NCOL(outcome)-1)
  }
  
  if (!identical(sample.weights, FALSE)) {
    # sample.weight.index is required as input to gini/fourier_train, regardless
    # of whether sample weights are specified or not
    out[["sample.weight.index"]] <- NCOL(X) + NCOL(outcome) + 1
    if (is.null(sample.weights)) {
      out[["use.sample.weights"]] <- FALSE
    } else {
      out[["use.sample.weights"]] <- TRUE
    }
  } else {
    sample.weights = NULL
  }
  
  if (inherits(X, "dgCMatrix") && NCOL(X) > 1) {
    sparse.data <- cbind(X, outcome, sample.weights)
  } else {
    X <- as.matrix(X)
    default.data <- as.matrix(cbind(X, outcome, sample.weights))
  }
  out[["train.matrix"]] <- default.data
  out[["sparse.train.matrix"]] <- sparse.data
  
  out
}

# Call the drf Rcpp bindings (argument_names) with R argument.names
#
# All the bindings argument names (C++) have underscores: sample_weights, train_matrix, etc.
# On the R side each variable name is written as sample.weights, train.matrix, etc.
# This function simply replaces the underscores in the passed argument names with dots.
do.call.rcpp = function(what, args, quote = FALSE, envir = parent.frame()) {
  names(args) = gsub("\\.", "_", names(args))
  do.call(what, args, quote, envir)
}
