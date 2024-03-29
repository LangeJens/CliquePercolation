#' Confidence Intervals Of Entropy Based On Random Permutations Of Network
#' 
#' Function for determining confidence intervals of entropy values calculated for
#' community partition from clique percolation based on randomly permuted networks
#' of original network.
#' 
#' @param W A qgraph object or a symmetric matrix; see also \link[qgraph]{qgraph}
#' @param cpThreshold.object A cpThreshold object; see also \link{cpThreshold}
#' @param n number of permutations (default is 100)
#' @param interval requested confidence interval (larger than zero and smaller 1;
#'    default is 0.95)
#' @param CFinder logical indicating whether clique percolation for weighted networks
#'    should be performed as in CFinder ; see also \link{cpAlgorithm}
#' @param ncores Numeric.
#'    Number of cores to use in computing results.
#'    Defaults to \code{parallel::detectCores() / 2} or half of your
#'    computer's processing power.
#'    Set to \code{1} to not use parallel computing
#' @param seed Numeric.
#'    Set seed for reproducible results.
#'    Defaults to \code{NULL}
#'    
#' @return
#'   A list object with the following elements:
#'   \describe{
#'   \item{Confidence.Interval}{a data frame with lower and upper bound of
#'      confidence interval for each \code{k}}
#'   \item{Extracted.Rows}{rows extracted from \code{cpThreshold.object} that are
#'      larger than the upper bound of the specified confidence interval for
#'      each \code{k}}
#'   \item{Settings}{user-specified settings}
#' }
#' 
#' @details
#'   The function generates \code{n} random permutations of the network
#'   specified in \code{W}. For each randomly permuted network, it runs \code{cpThreshold}
#'   (see \link{cpThreshold} for more information) with \code{k} and \code{I} values
#'   extracted from the cpThreshold object specified in \code{cpThreshold.object}.
#'   Across permutations, the confidence intervals of the entropy values are determined
#'   for each \code{k} separately.
#' 
#'   The confidence interval of the entropy values is determined separately for each \code{k}.
#'   This is because larger \code{k} have to produce less communities on average,
#'   which will decrease entropy. Comparing confidence intervals of smaller \code{k} to
#'   those of larger \code{k} would therefore be disadvantageous for larger \code{k}.
#' 
#'   In the output, one can check the confidence intervals of each \code{k}. Moreover,
#'   a data frame is produced that takes the cpThreshold object that was specified in
#'   \code{cpThreshold.object} and removes all rows that do not exceed the upper bound of the
#'   confidence interval of the respective \code{k}.
#' 
#' @examples
#' ## Example with fictitious data
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
#' W <- Matrix::forceSymmetric(W)
#' W <- qgraph::qgraph(W)
#' 
#' # create cpThreshold object
#' cpThreshold.object <- cpThreshold(W = W, method = "unweighted", k.range = c(3,4),
#'                                   threshold = "entropy")
#' 
#' # run cpPermuteEntropy with 100 permutations and 95% confidence interval
#' \donttest{
#' results <- cpPermuteEntropy(W = W, cpThreshold.object = cpThreshold.object,
#'                             n = 100, interval = 0.95, ncores = 1, seed = 4186)
#' 
#' # check results
#' results
#' }
#' 
#' ## Example with Obama data set (see ?Obama)
#' 
#' # get data
#' data(Obama)
#' 
#' # estimate network
#' net <- qgraph::EBICglasso(qgraph::cor_auto(Obama), n = nrow(Obama))
#' 
#' # create cpThreshold object
#' \donttest{
#' threshold <- cpThreshold(net, method = "weighted",
#'                          k.range = 3:4,
#'                          I.range = seq(0.1, 0.5, 0.01),
#'                          threshold = "entropy")
#' }
#'                           
#' # run cpPermuteEntropy with 50 permutations and 99% confidence interval
#' \donttest{
#' permute <- cpPermuteEntropy(net, cpThreshold.object = threshold,
#'                             interval = 0.99, n = 50, ncores = 1, seed = 4186)
#' 
#' # check results
#' permute
#' }
#' 
#' @author Jens Lange, \email{lange.jens@@outlook.com} 
#' 
#' @export cpPermuteEntropy

cpPermuteEntropy <- function(W, cpThreshold.object, n = 100, interval = 0.95,
                             CFinder = FALSE, ncores, seed = NULL) {
  
  ###check whether W is a qgraph object
  ###if not check whether matrix is symmetric and convert to qgraph object
  if (!isTRUE(methods::is(W, "qgraph"))) {
    
    #error if W is a matrix but not symmetric
    if (isSymmetric(W) == FALSE) {
      stop("If W is a matrix, it must be symmetric.")
    }
    
    #converts matrix to qgraph if input is not a qgraph object
    W <- qgraph::qgraph(W, DoNotPlot = TRUE)
    
    #### OLD ERROR BEGIN ####
    
    #stop("W (network object) must be a qgraph object.")
    
    #### OLD ERROR END ####
    
  }
  
  #error message if n is not larger than zero
  if (n <= 0) {
    stop("n must be larger than zero.")
  }
  
  #error message if interval is not larger than zero and smaller than 1
  if (!(interval < 1 & interval > 0)) {
    stop("interval must be larger than zero and smaller than 1.")
  }
  
  #set default for missing processing cores
  if(missing(ncores)){
    ncores <- ceiling(parallel::detectCores() / 2)
  }
  
  #for n random permutations of graph, the function runs cpThreshold for k.range and I.range
  #random permutations generated by permuting the upper triangle of the weights matrix and forcing it symmetric
  #for each random permutation, the maximum entropy is extracted for each k and saved in data frame
  #information about parameters in original cpThreshold function is extracted from cpThreshold.object
  if (CFinder == FALSE) {
    if ("Intensity"%in%names(cpThreshold.object)) {method <- "weighted"} else {method <- "unweighted"}
  }
  if (CFinder == TRUE) {
    method <- "weighted.CFinder"
  }
  k.range <- sort(unique(cpThreshold.object$k))
  I.range <- sort(unique(cpThreshold.object$Intensity), decreasing = TRUE)
  
  #initialize iterations
  iterations <- as.list(1:n)
  
  #parallel processing
  cl <- parallel::makeCluster(ncores)
  
  #set seed for reproducibility
  if(!is.null(seed)){
    parallel::clusterSetRNGStream(cl = cl, iseed = seed)
  }
  
  #export variables
  parallel::clusterExport(cl = cl,
                          varlist = c("W", "k.range", "I.range",
                                      "method", "cpThreshold"),
                          envir=environment())
  
  #let user know permutations has started
  message("Permutating...\n", appendLF = FALSE)
  
  #Permutate
  perms <- pbapply::pblapply(
    X = iterations, cl = cl,
    FUN = function(X, W, k.range, I.range, method){
      Wmat <- qgraph::getWmat(W)
      Wmat[upper.tri(Wmat)] <- sample(Wmat[upper.tri(Wmat)])
      Wmat <- Matrix::forceSymmetric(Wmat, "U")
      graph <- qgraph::qgraph(Wmat, DoNotPlot = TRUE)
      
      utils::capture.output(
        results_cpThreshold <- cpThreshold(graph, method = method, k.range = k.range,
                                           I.range = I.range, threshold = "entropy")
      ) #capture.output suppresses display of progress bar from cpThreshold
      
      max_ent <- matrix(nrow = length(k.range), ncol = 2)
      colnames(max_ent) <- c("k", "max")
      
      for (j in 1:length(k.range)) {
        max_ent[j,] <- c(k.range[j], max(results_cpThreshold[results_cpThreshold$k == k.range[j], ]$Entropy.Threshold))
      }
      
      return(max_ent)
      
    },
    W = W, k.range = k.range, I.range = I.range, method = method
  )
  
  parallel::stopCluster(cl)
  
  max_ent <- as.data.frame(do.call(rbind, perms))
  
  #### OLD CODE BEGIN ####
  
  #max_ent <- data.frame(k = numeric(),
  #                      max = numeric())
  
  #counter <- 1
  
  #progress_bar_permute <- utils::txtProgressBar(min = 0, max = n,
  #                                              style = 3) #progress bar definition
  #progress_bar_permute_counter <- 0 #counter for progress bar
  
  #for (i in 1:n) {
  #  Wmat <- qgraph::getWmat(W)
  #  Wmat[upper.tri(Wmat)] <- sample(Wmat[upper.tri(Wmat)])
  #  Wmat <- Matrix::forceSymmetric(Wmat, "U")
  #  graph <- qgraph::qgraph(Wmat, DoNotPlot = TRUE)
    
  #  utils::capture.output(
  #    results_cpThreshold <- cpThreshold(graph, method = method, k.range = k.range,
  #                                        I.range = I.range, threshold = "entropy")
  #  ) #capture.output suppresses display of progress bar from cpThreshold
    
  #  for (j in k.range) {
  #    max_ent[counter, "k"] <- j
  #    max_ent[counter, "max"] <- max(results_cpThreshold[results_cpThreshold$k == j, ]$Entropy.Threshold)
  #    counter <- counter + 1
  #  }
    
    #progress bar update
  #  progress_bar_permute_counter <- progress_bar_permute_counter + 1
  #  utils::setTxtProgressBar(progress_bar_permute, progress_bar_permute_counter)
  #}
  
  #close progress bar
  #close(progress_bar_permute)
  
  #### OLD CODE END ####
  
  
  #determine confidence interval based on normal distribution of extracted entropies for each k
  #extract rows of cpThreshold.object with entropy larger than upper confidence for each k
  results <- data.frame(k = numeric(),
                        l = numeric(),
                        u = numeric())
  extracted_rows <- list()
  counter2 <- 1
  for (k in k.range) {
    m <- mean(max_ent[max_ent$k == k, "max"])
    std <- stats::sd(max_ent[max_ent$k == k, "max"])
    lower_ci <- m - stats::qnorm((1 + interval)/2) * std/sqrt(n)
    upper_ci <- m + stats::qnorm((1 + interval)/2) * std/sqrt(n)
    results[counter2, "k"] <- k
    results[counter2, "l"] <- lower_ci
    results[counter2, "u"] <- upper_ci
    
    rows <- cpThreshold.object[cpThreshold.object$k == k & cpThreshold.object$Entropy.Threshold > upper_ci, ]
    extracted_rows[[counter2]] <- rows
    
    counter2 <- counter2 + 1
  }
  names(results) <- c("k",
                      paste(interval*100, "% CI lower", sep = ""),
                      paste(interval*100, "% CI upper", sep = ""))
  extracted_rows <- Reduce(rbind, extracted_rows)
  
  returned_object <- list(Confidence.Interval = results,
                          Extracted.Rows = extracted_rows,
                          Settings = list(n = n,
                                          interval = interval,
                                          CFinder = CFinder,
                                          ncores = ncores,
                                          seed = seed))
  
  class(returned_object) <- "cpPermuteEntropy"
  
  return(returned_object)
}
