#' Print a DRF forest object.
#' @param x The tree to print.
#' @param decay.exponent A tuning parameter that controls the importance of split depth.
#' @param max.depth The maximum depth of splits to consider.
#' @param ... Additional arguments (currently ignored).
#'
#' @method print drf
#' @export
print.drf <- function(x, decay.exponent = 2, max.depth = 4, ...) {
  
  var.importance <- variable_importance(x, decay.exponent, max.depth)
  var.importance <- c(round(var.importance, 3))
  if (is.null(x$mat.col.names)) {
    names(var.importance) <- 1:length(var.importance)
  } else {
    names(var.importance) <- x$mat.col.names
  }
  
  main.class <- class(x)[1]
  num.samples <- nrow(x$X.orig)

  cat("DRF forest object\n")
  cat("Number of trees:", x[["_num_trees"]], "\n")
  cat("Number of training samples:", num.samples, "\n")

  cat("Variable importance:", "\n")
  print(var.importance)
}

#' Print a DRF tree object.
#' @param x The tree to print.
#' @param ... Additional arguments (currently ignored).
#'
#' @method print drf_tree
#' @export
print.drf_tree <- function(x, ...) {
  cat("DRF tree object", "\n")
  cat("Number of training samples:", x$num_samples, "\n")
  cat("Variable splits:", "\n")

  # Add the index of each node as an attribute for easy access.
  nodes <- lapply(1:length(x$nodes), function(i) {
    node <- x$nodes[[i]]
    node$index <- i
    return(node)
  })

  # Perform DFS to print the nodes (mimicking a stack with a list).
  frontier <- nodes[1]
  frontier[[1]]$depth <- 0
  while (length(frontier) > 0) {
    # Pop the first node off the stack.
    node <- frontier[[1]]
    frontier <- frontier[-1]

    output <- paste(rep("  ", node$depth), collapse = "")
    output <- paste(output, "(", node$index, ")", sep = "")

    if (node$is_leaf) {
      leaf_stats_text <- ""
      if(!is.null(node$leaf_stats)){
        leaf_stats_text <- paste(paste(names(node$leaf_stats), unname(node$leaf_stats), sep = ": ", collapse = " "))
      }
      output <- paste(output, "* num_samples:", length(node$samples), "", leaf_stats_text)
    } else {
      split.var <- node$split_variable
      split.var.name <- x$columns[split.var]
      output <- paste(output, "split_variable:", split.var.name, " split_value:", signif(node$split_value))

      left_child <- nodes[node$left_child]
      left_child[[1]]$depth <- node$depth + 1

      right_child <- nodes[node$right_child]
      right_child[[1]]$depth <- node$depth + 1

      frontier <- c(left_child, right_child, frontier)
    }
    cat(output, "\n")
  }
}
