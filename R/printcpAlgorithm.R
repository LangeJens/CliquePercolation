#' print.cpAlgorithm
#' 
#' Print method for objects of class \code{cpAlgorithm}.
#'
#' @param x An object of class \code{cpAlgorithm}; see also
#'   \link{cpAlgorithm}
#' @param ... currently ignored
#' 
#' @author Jens Lange, \email{lange.jens@@outlook.com}
#'
#' @export

print.cpAlgorithm <- function(x, ...) {
  cat("\nResults of clique percolation community detection algorithm\n\n")
  cat("--------------------\n\n")
  cat("User-specified Settings\n\n")
  cat("method =", x$method, "\n")
  cat("k =", x$k)
  if (x$method != "unweighted") {
    cat("\nI =", x$I)
  }
  cat("\n\n")
  cat("--------------------\n\n")
  cat("Results\n\n")
  cat("Number of communities:", length(x$list.of.communities.numbers), "\n")
  cat("Number of shared nodes:", length(x$shared.nodes.numbers), "\n")
  cat("Number of isolated nodes:", length(x$isolated.nodes.numbers), "\n\n")
  cat("--------------------\n\n")
  cat("For details, use summary() (see ?summary.cpAlgorithm).")
}
