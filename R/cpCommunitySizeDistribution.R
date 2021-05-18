#' Plotting Clique Percolation Community Size Distribution
#' 
#' Function for plotting the frequency distribution of community sizes from clique
#' percolation community detection and testing for power-law.
#' 
#' @param list.of.communities List object taken from results of cpAlgorithm
#'    function; see also \link{cpAlgorithm}
#' @param color.line string indicating the color of the line in the plot as described
#'    in \link[graphics]{par}; default is \code{"#bc0031"}
#' @param test.power.law Logical indicating whether fit of power-law should be tested; default
#'    is FALSE; see Details
#'    
#' @return
#'   The function primarily plots the community size distribution. Additionally, it returns
#'   a list with a data frame containing all community sizes and their frequencies
#'   (\code{size.distribution}). If \code{test.power.law = TRUE}, a test of fit of a power-law
#'   distribution is also returned as a list object with results from fit_power_law (see
#'   also \link[igraph]{fit_power_law}).
#' 
#' @details
#'   The function takes the results of cpAlgorithm (see also \link{cpAlgorithm}),
#'   that is, either the \code{list.of.communities.numbers} or the
#'   \code{list.of.communities.labels} and plots the community size distribution. If there
#'   are no communities, no plot can be generated. An error is printed indicating this.
#'   
#'   If \code{test.power.law = TRUE}, test of a fit of a power-law is performed with the
#'   function fit_power_law (see also \link[igraph]{fit_power_law}). Fit is tested for
#'   the entire distribution from the smallest community size onward (i.e., typically k
#'   as specified in cpAlgorithm). Moreover, test uses the \code{plfit} implementation of
#'   fit_power_law. For other arguments, default values are used.
#' 
#' @examples
#' # create qgraph object; 150 nodes; 1/7 of all edges are different from zero
#' W <- matrix(c(0), nrow = 150, ncol = 150, byrow = TRUE)
#' set.seed(4186)
#' W[upper.tri(W)] <- sample(c(rep(0,6),1), length(W[upper.tri(W)]), replace = TRUE)
#' rand_w <- stats::rnorm(length(which(W == 1)), mean = 0.3, sd = 0.1)
#' W[which(W == 1)] <- rand_w
#' W <- Matrix::forceSymmetric(W)
#' W <- qgraph::qgraph(W, DoNotPlot = TRUE)
#' 
#' # run clique percolation for weighted networks
#' cp.results <- cpAlgorithm(W, k = 3, method = "weighted", I = 0.38)
#' 
#' # plot community size distribution with blue line
#' cp.size.dist <- cpCommunitySizeDistribution(cp.results$list.of.communities.numbers,
#'                                             color.line = "#0000ff")
#' # test for power-law distribution
#' cp.size.dist <- cpCommunitySizeDistribution(cp.results$list.of.communities.numbers,
#'                                             color.line = "#0000ff",
#'                                             test.power.law = TRUE)
#' cp.size.dist$fit.power.law
#' 
#' @author Jens Lange, \email{lange.jens@@outlook.com} 
#' 
#' @export cpCommunitySizeDistribution

cpCommunitySizeDistribution <- function(list.of.communities, color.line = "#bc0031",
                                        test.power.law = FALSE){
  
  #if there are no communities...
  #print error
  if (length(list.of.communities) == 0) {
    stop("No communities. Thus, no size distribution is plotted.")
  }
  
  #if there is at least one community...
  #plot size distribution
  if (length(list.of.communities) > 0) {
    size_communities <- c()
    for (i in 1:length(list.of.communities)) {
      size_communities[i] <- length(list.of.communities[[i]])
    }
    size_freq <- as.data.frame(table(size_communities))
    names(size_freq) <- c("size","frequency")
    size_freq[, c(1:2)] <- lapply(size_freq[, c(1:2)], as.character)
    size_freq[, c(1:2)] <- lapply(size_freq[, c(1:2)], as.numeric)
    
    graphics::plot(1,type='n',xlim=c(1,max(size_freq$size)),ylim=c(0,max(size_freq$frequency)),
                   xlab='Community Size', ylab='Frequency', cex.lab = 1.5, cex.axis = 1.5)
    graphics::grid(col = "lightgray", lty = "solid")
    graphics::points(x = size_freq$size, y = size_freq$frequency, col = color.line,
                     pch = 16, cex = 1.5)
    graphics::lines(x = size_freq$size, y = size_freq$frequency, col = color.line, lwd = 3)
    
    if (test.power.law == FALSE) {
      invisible(list(size.distribution = size_freq))
    } else {
      size_vector <- rep(size_freq$size, size_freq$frequency)
      fit_pl <- igraph::fit_power_law(size_vector, xmin = min(size_vector),
                                      implementation = "plfit")
      invisible(list(size.distribution = size_freq,
                     fit.power.law = fit_pl))
    }
  }
  
}
