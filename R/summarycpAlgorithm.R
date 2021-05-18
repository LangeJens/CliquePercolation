#' summary.cpAlgorithm
#' 
#' Summary method for objects of class \code{cpAlgorithm}.
#'
#' @param object An object of class \code{cpAlgorithm}; see also
#'   \link{cpAlgorithm}
#' @param details A string or vector indicating about which part of
#'   the results more information is requested; default is
#'   \code{c("communities.labels","shared.nodes.labels","isolated.nodes.labels")};
#'   see Details
#' @param ... currently ignored
#' 
#' @return
#'   Prints information depending on \code{details}.
#' 
#' @details
#'   The function extracts information from an object produced by \code{cpAlgorithm}.
#'   To do so, the user has to specify in \code{details} which information is requested.
#'   It is possible to extract information about the communities with either
#'   numbers (\code{communities.numbers}) or labels (\code{communities.labels})
#'   as identifiers of the nodes. Moreover, it is possible to extract information about
#'   shared nodes with either numbers (\code{shared.nodes.numbers}) or labels
#'   (\code{shared.nodes.labels}) as identifiers of the nodes. Finally, it is possible
#'   to extract information about isolated nodes with either numbers
#'   (\code{isolated.nodes.numbers}) or labels (\code{isolated.nodes.labels})
#'   as identifiers of the nodes. Any combination of these options can be specified
#'   in \code{details}.
#' 
#' @examples
#' ## Example for unweighted networks
#' 
#' # create qgraph object
#' W <- matrix(c(0,1,1,1,0,0,0,0,
#'               0,0,1,1,0,0,0,0,
#'               0,0,0,0,0,0,0,0,
#'               0,0,0,0,1,1,1,0,
#'               0,0,0,0,0,1,1,0,
#'               0,0,0,0,0,0,1,0,
#'               0,0,0,0,0,0,0,1,
#'               0,0,0,0,0,0,0,0), nrow = 8, ncol = 8, byrow = TRUE)
#' colnames(W) <- letters[1:8]
#' rownames(W) <- letters[1:8]
#' W <- Matrix::forceSymmetric(W)
#' W <- qgraph::qgraph(W)
#'
#' # run clique percolation for unweighted networks
#' results <- cpAlgorithm(W = W, k = 3, method = "unweighted")
#' 
#' # print results overview
#' results
#' 
#' # extract details about the communities
#' summary(results, details = "communities.labels")
#' 
#' # extract information about shared and isolated nodes
#' summary(results, details = c("shared.nodes.numbers", "isolated.nodes.labels"))
#' 
#' @author Jens Lange, \email{lange.jens@@outlook.com}
#'
#' @export

summary.cpAlgorithm <- function(object, details = c("communities.labels",
                                                    "shared.nodes.labels",
                                                    "isolated.nodes.labels"), ...) {
  
  ###error messages if details is not one of the possible specifications
  check_details <- all(details %in% c("communities.labels",
                                      "communities.numbers",
                                      "shared.nodes.labels",
                                      "shared.nodes.numbers",
                                      "isolated.nodes.labels",
                                      "isolated.nodes.numbers"))
  if (check_details == FALSE) {
    stop("details must be 'communities.labels', 'communities.numbers',
         'shared.nodes.labels', 'shared.nodes.numbers',
         'isolated.nodes.labels', and/or 'isolated.nodes.numbers'.")
  }
  
  #displaying the communities with numbers as identifiers of nodes
  if ("communities.numbers" %in% details) {
    cat("\n--------------------\n")
    cat("Communities (numbers as identifiers of nodes)")
    cat("\n--------------------\n\n")
    if (length(object$list.of.communities.numbers) > 0) {
      for (i in 1:length(object$list.of.communities.numbers)) {
        cat("Community", i, ":", object$list.of.communities.numbers[[i]], "\n")
      }
    } else {cat("NULL")}
    cat("\n")
  }
  
  #displaying the communities with labels as identifiers of nodes
  if ("communities.labels" %in% details) {
    cat("\n--------------------\n")
    cat("Communities (labels as identifiers of nodes)")
    cat("\n--------------------\n\n")
    if (length(object$list.of.communities.labels) > 0) {
      for (i in 1:length(object$list.of.communities.labels)) {
        cat("Community", i, ":", object$list.of.communities.labels[[i]], "\n")
      }
    } else {cat("NULL")}
    cat("\n")
  }
  
  #displaying the shared nodes with numbers as identifiers of nodes
  if ("shared.nodes.numbers" %in% details) {
    cat("\n--------------------\n")
    cat("Shared nodes (numbers as identifiers of nodes)")
    cat("\n--------------------\n\n")
    if (length(object$shared.nodes.numbers) > 0) {
      cat(object$shared.nodes.numbers, "\n")
    } else {cat("NULL")}
    cat("\n")
  }
  
  #displaying the shared nodes with labels as identifiers of nodes
  if ("shared.nodes.labels" %in% details) {
    cat("\n--------------------\n")
    cat("Shared nodes (labels as identifiers of nodes)")
    cat("\n--------------------\n\n")
    if (length(object$shared.nodes.labels) > 0) {
      cat(object$shared.nodes.labels, "\n")
    } else {cat("NULL")}
    cat("\n")
  }
  
  #displaying the isolated nodes with numbers as identifiers of nodes
  if ("isolated.nodes.numbers" %in% details) {
    cat("\n--------------------\n")
    cat("Isolated nodes (numbers as identifiers of nodes)")
    cat("\n--------------------\n\n")
    if (length(object$isolated.nodes.numbers) > 0) {
      cat(object$isolated.nodes.numbers, "\n")
    } else {cat("NULL")}
    cat("\n")
  }
  
  #displaying the isolated nodes with labels as identifiers of nodes
  if ("isolated.nodes.labels" %in% details) {
    cat("\n--------------------\n")
    cat("Isolated nodes (labels as identifiers of nodes)")
    cat("\n--------------------\n\n")
    if (length(object$isolated.nodes.labels) > 0) {
      cat(object$isolated.nodes.labels, "\n")
    } else {cat("NULL")}
    cat("\n")
  }
  
}
