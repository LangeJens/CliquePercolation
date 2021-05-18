#' print.cpPermuteEntropy
#' 
#' Print method for objects of class \code{cpPermuteEntropy}.
#'
#' @param x An object of class \code{cpPermuteEntropy}; see also
#'   \link{cpPermuteEntropy}
#' @param ... currently ignored
#' 
#' @author Jens Lange, \email{lange.jens@@outlook.com}
#'
#' @export

print.cpPermuteEntropy <- function(x, ...) {
  cat("\nConfidence intervals for entropy values of random permutations of original network\n\n")
  cat("--------------------\n\n")
  cat("User-specified Settings\n\n")
  cat("n =", x$Settings$n, "\n")
  cat("interval =", x$Settings$interval, "\n")
  cat("CFinder =", x$Settings$CFinder, "\n")
  cat("ncores =", x$Settings$ncores, "\n")
  cat("seed =", x$Settings$seed, "\n")
  cat("\n\n")
  cat("--------------------\n\n")
  cat("Confidence intervals\n\n")
  print(round(x$Confidence.Interval, 3), row.names = FALSE)
  cat("\n\n")
  cat("--------------------\n\n")
  cat("Extracted rows from cpThreshold object\n\n")
  if (nrow(x$Extracted.Rows != 0)) {
    print(round(x$Extracted.Rows, 3), row.names = FALSE)
  } else {cat("NULL")}
  cat("\n")
}
