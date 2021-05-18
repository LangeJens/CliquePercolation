#' Optimizing \code{k} And \code{I} For Clique Percolation Community Detection
#' 
#' Function for determining threshold value(s) (ratio of largest to second largest
#' community sizes, chi, entropy) of ranges of \code{k} and \code{I} values to help deciding
#' for optimal \code{k} and \code{I} values.
#' 
#' @param W A qgraph object or a symmetric matrix; see also \link[qgraph]{qgraph}
#' @param method A string indicating the method to use 
#'    (\code{"unweighted"}, \code{"weighted"}, or \code{"weighted.CFinder"}).
#'    See \link{cpAlgorithm} for more information
#' @param k.range integer or vector of \code{k} value(s) for which threshold(s) are determined
#'    See \link{cpAlgorithm} for more information
#' @param I.range integer or vector of \code{I} value(s) for which threshold(s) are determined
#'    See \link{cpAlgorithm} for more information
#' @param threshold A string or vector indicating which threshold(s) to determine
#'    (\code{"largest.components.ratio", "chi", "entropy"}); see Details
#' 
#' @return
#'   A data frame with columns for \code{k}, \code{I} (if \code{method = "weighted"}
#'   or \code{method = "weighted.CFinder"}), number of communities, number of isolated
#'   nodes, and results of the specified threshold(s).
#' 
#' @details
#'   Optimizing \code{k} (clique size) and \code{I} (Intensity threshold) in clique percolation
#'   community detection is a difficult task. Farkas et al. (2007) recommend to look at the
#'   ratio of the largest to second largest community sizes
#'   (\code{threshold = "largest.components.ratio"}) for very large networks or
#'   the variance of the community sizes when removing the community size of the largest
#'   community (\code{threshold = "chi"}) for somewhat smaller networks. These thresholds were
#'   derived from percolation theory. If \code{I} for a certain \code{k} is too high, no
#'   community will be identified. If \code{I} is too low, a giant community with all nodes
#'   emerges. Just above this \code{I}, the distribution of community sizes often follows a
#'   power law, which constitutes a broad community sizes distribution. Farkas et al. (2007)
#'   point out, that for such \code{I}, the ratio of the largest to second largest community
#'   sizes is approximately 2, constituting one way to optimize \code{I} for each possible
#'   \code{k}. For somewhat smaller networks, the ratio can be rather unstable. Instead,
#'   Farkas et al. (2007, p.8) propose to look at the variance of the community sizes after
#'   removing the largest community. The idea is that when \code{I} is rather low, one giant
#'   community and multiple equally small ones occur. Then, the variance of the community
#'   sizes of the small communities (removing the giant community) is low. When \code{I}
#'   is high, only a few equally small communities will occur. Then, the variance of the
#'   community sizes (after removing the largest community) will also be low. In between,
#'   the variance will at some point be maximal, namely when the community size
#'   distribution is maximally broad (power law-distributed). Thus, the maximal variance
#'   could be used to optimize \code{I} for various \code{k}.
#' 
#'   For very small networks, optimizing \code{k} and \code{I} based on the distribution of the
#'   community sizes will be impossible, as too few communities will occur. Another possible
#'   threshold for such networks is based on the entropy of the community sizes
#'   (\code{threshold = "entropy"}). Entropy can be interpreted as an indicator of how
#'   surprising the respective solution is. The formula used here is based on Shannon
#'   Information, namely
#'   \deqn{-\sum_{i=1}^N p_i * \log_2 p_i}
#'   with \eqn{p_i} being the probability that a node is part of community \eqn{i}. For instance,
#'   if there are two communities, one of size 5 and one of size 3, the result would be
#'   \deqn{-((5/8 * \log_2 5/8) + (3/8 * \log_2 3/8)) = 1.46}
#'   When calculating entropy, the isolated nodes identified by clique percolation are treated as
#'   a separate community. If there is only one community or only isolated nodes, entropy is
#'   zero, indicating that the surprisingness is low. As compared to the ratio and chi 
#'   thresholds, entropy favors communities that are equal in size. Thus, it should not be 
#'   used for larger networks for which a broader community size distribution is preferred.
#'   Note that the entropy threshold has not been validated for clique percolation as of now.
#'   Initial simulation studies indicate that it consistently detects surprising community
#'   partitions in smaller networks especially if there are cliques of larger \code{k}.
#' 
#'   Ratio thresholds can be determined only if there are at least two communities. Chi threshold
#'   can be determined only if there are at least three communities. If there are not enough
#'   communities for the respective threshold, their values are NA in the data frame.
#'   Entropy can always be determined.
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
#' W <- Matrix::forceSymmetric(W)
#' W <- qgraph::qgraph(W)
#' 
#' # determine entropy threshold for k = 3 and k = 4
#' results <- cpThreshold(W = W, method = "unweighted", k.range = c(3,4), threshold = "entropy")
#' 
#' ## Example for weighted networks; three large communities with I = 0.3, 0.2, and 0.1, respectively
#' 
#' # create qgraph object
#' W <- matrix(c(0,0.10,0,0,0,0,0.10,0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0.10,0,0,0,0,0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0,0.10,0,0,0,0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0.10,0,0,0.10,0.20,0,0,0,0,0.20,0.20,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0.10,0,0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0.10,0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0.20,0,0,0,0,0.20,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0,0.20,0,0,0,0.20,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0.20,0,0,0.20,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0.20,0,0.20,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0.20,0.20,0.30,0,0,0,0,0.30,0.30,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.20,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.30,0,0,0,0,0.30,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.30,0,0,0,0.30,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.30,0,0,0.30,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.30,0,0.30,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.30,0.30,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.30,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 22, ncol = 22, byrow = TRUE) 
#' W <- Matrix::forceSymmetric(W)
#' W <- qgraph::qgraph(W, layout = "spring", edge.labels = TRUE)
#' 
#' # determine ratio, chi, and entropy thresholds for k = 3 and I from 0.3 to 0.09
#' results <- cpThreshold(W = W, method = "weighted", k.range = 3,
#'                        I.range = c(seq(0.3, 0.09, by = -0.01)),
#'                        threshold = c("largest.components.ratio","chi","entropy"))
#'
#' ## Example with Obama data set (see ?Obama)
#' 
#' # get data
#' data(Obama)
#' 
#' # estimate network
#' net <- qgraph::EBICglasso(qgraph::cor_auto(Obama), n = nrow(Obama))
#' 
#' # determine entropy threshold for k from 3 to 4 and I from 0.1 to 0.5
#' threshold <- cpThreshold(net, method = "weighted",
#'                          k.range = 3:4,
#'                          I.range = seq(0.1, 0.5, 0.01),
#'                          threshold = "entropy")
#' 
#' @references
#' Farkas, I., Abel, D., Palla, G., & Vicsek, T. (2007). Weighted network modules.
#' \emph{New Journal of Physics, 9}, 180-180. http://doi.org/10.1088/1367-2630/9/6/180
#' 
#' @author Jens Lange, \email{lange.jens@@outlook.com} 
#' 
#' @export cpThreshold

cpThreshold <- function(W, method = c("unweighted","weighted","weighted.CFinder"), k.range, I.range,
                        threshold = c("largest.components.ratio","chi","entropy")){
  
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
  ###error message if method is not "unweighted", "weighted", or "weighted.CFinder"
  if (method != "unweighted" & method != "weighted" & method != "weighted.CFinder") {
    stop("method must be 'unweighted', 'weighted', or 'weighted.CFinder' (depending on the network).")
  }
  ###error messages if threshold is not "largest.components.ratio", "chi", and/or "entropy"
  check_threshold <- all(threshold %in% c("largest.components.ratio","chi","entropy"))
  if (check_threshold == FALSE) {
    stop("threshold must be 'largest.components.ratio', 'chi', and/or 'entropy'.")
  }
  
  #function for chi formula
  formula_chi <- function(size_comm){
    size_comm_sort <- sort(size_comm)
    size_comm_sort_not_max <- size_comm_sort[-length(size_comm_sort)]
    value_chi <- c()
    for (v in 1:length(size_comm_sort_not_max)) {
      denominator <- sum(size_comm_sort_not_max[-v])^2
      value_chi[v] <- (size_comm_sort_not_max[v]^2)/denominator 
    }
    result_formula_chi <- sum(value_chi)
    return(result_formula_chi)
  }
  
  #function for entropy formula
  formula_entropy <- function(size_all){
    value_ent <- c()
    for (w in 1:length(size_all)) {
      value_ent[w] <- -((size_all[w] / sum(size_all)) * log2((size_all[w] / sum(size_all))))
    }
    result_formula_ent <- sum(value_ent)
    return(result_formula_ent)
  }
  
  #if input is weighted network...
  #all threshold strategies can be implemented and they depend on k.range and I.range
  if (method == "weighted" | method == "weighted.CFinder") {
    ratio <- c()
    chi <- c()
    entropy <- c()
    I_cp <- c()
    k_cp <- c()
    community <- c()
    isolated <- c()
    count <- 1
    progress_bar <- utils::txtProgressBar(min = 0, max = length(k.range)*length(I.range),
                                          style = 3) #progress bar definition
    progress_bar_counter <- 0 #counter for progress bar
    for (k in k.range) {
      for (i in I.range) {
        results <- cpAlgorithm(W, k = k, method = method, I = as.numeric(as.character(i)))
        #if there are at least two communities...
        #ratio threshold can be determined if requested
        if (length(results$list.of.communities.numbers) > 1 & "largest.components.ratio" %in% threshold) {
          size_dist_lcr <- c()
          for (j in 1:length(results$list.of.communities.numbers)) {
            size_dist_lcr[j] <- length(results$list.of.communities.numbers[[j]])
          }
          distinct_sizes_lcr <- length(size_dist_lcr)
          ratio[count] <- sort(size_dist_lcr)[distinct_sizes_lcr] / sort(size_dist_lcr)[distinct_sizes_lcr - 1]
        }
        #if there are fewer than two communities...
        #ratio threshold cannot be determined even if requested
        if (length(results$list.of.communities.numbers) < 2 & "largest.components.ratio" %in% threshold) {
          ratio[count] <- NA
        }
        #if there are at least three communities...
        #chi threshold can be determined if requested
        if (length(results$list.of.communities.numbers) > 2 & "chi" %in% threshold) {
          size_dist_chi <- c()
          for (j in 1:length(results$list.of.communities.numbers)) {
            size_dist_chi[j] <- length(results$list.of.communities.numbers[[j]])
          }
          chi[count] <- formula_chi(size_dist_chi)
        }
        #if there are fewer than three communities...
        #chi threshold cannot be determined even if requested
        if (length(results$list.of.communities.numbers) < 3 & "chi" %in% threshold) {
          chi[count] <- NA
        }
        #entropy can always be calculated
        #when all nodes are isolated, entropy is simply zero, because all nodes are isolated and the formula is 1 * log2(1) = 0
        #entropy formula needs vector with sizes of each community and number of isolated nodes
        #when there are shared nodes, probabilities in formula would add up to value > 1
        #to circumvent that, shared nodes are divided between their communities (e.g., two communities share node then each community gets only 0.5 instead of 1)
        #code below creates vector with all necessary sizes and subtracts respective values from communities with shared nodes
        if ("entropy" %in% threshold) {
          if (length(results$list.of.communities.numbers) > 0) {
            size_dist_ent <- c()
            for (j in 1:length(results$list.of.communities.numbers)) {
              size_dist_ent[j] <- length(results$list.of.communities.numbers[[j]])
            }
          } else {size_dist_ent <- c()}
          if (length(results$shared.nodes.numbers) > 0) {
            for (s in results$shared.nodes.numbers) {
              counter <- c()
              for (c in 1:length(results$list.of.communities.numbers)) {
                if (s %in% results$list.of.communities.numbers[[c]]) {
                  counter <- c(counter,c)
                }
              }
              for (a in counter) {
                size_dist_ent[a] <- size_dist_ent[a] - (1 - 1/length(counter))
              }
            }
          } else {size_dist_ent <- size_dist_ent}
          if (length(results$isolated.nodes.numbers) > 0) {
            isolated_ent <- length(results$isolated.nodes.numbers)
          } else {isolated_ent <- c()}
          size_combined <- c(size_dist_ent,isolated_ent)
          entropy[count] <- formula_entropy(size_combined)
        }
        I_cp[count] <- i
        k_cp[count] <- k
        community[count] <- length(results$list.of.communities.numbers)
        isolated[count] <- length(results$isolated.nodes.numbers)
        count <- count + 1
        
        #progress bar update
        progress_bar_counter <- progress_bar_counter + 1
        utils::setTxtProgressBar(progress_bar, progress_bar_counter)
      }
    }
  }
  
  #if input is unweighted network...
  #all threshold strategies can be implemented and they depend only on k.range
  if (method == "unweighted") {
    ratio <- c()
    chi <- c()
    entropy <- c()
    k_cp <- c()
    community <- c()
    isolated <- c()
    count <- 1
    progress_bar <- utils::txtProgressBar(min = 0, max = length(k.range),
                                          style = 3) #progress bar definition
    progress_bar_counter <- 0 #counter for progress bar
    for (k in k.range) {
      results <- cpAlgorithm(W, k = k, method = method)
      #if there are at least two communities...
      #ratio threshold can be determined if requested
      if (length(results$list.of.communities.numbers) > 1 & "largest.components.ratio" %in% threshold) {
        size_dist_lcr <- c()
        for (j in 1:length(results$list.of.communities.numbers)) {
          size_dist_lcr[j] <- length(results$list.of.communities.numbers[[j]])
        }
        distinct_sizes_lcr <- length(size_dist_lcr)
        ratio[count] <- sort(size_dist_lcr)[distinct_sizes_lcr] / sort(size_dist_lcr)[distinct_sizes_lcr - 1]
      }
      #if there are fewer than two communities...
      #ratio threshold cannot be determined even if requested
      if (length(results$list.of.communities.numbers) < 2 & "largest.components.ratio" %in% threshold) {
        ratio[count] <- NA
      }
      #if there are at least three communities...
      #chi threshold can be determined if requested
      if (length(results$list.of.communities.numbers) > 2 & "chi" %in% threshold) {
        size_dist_chi <- c()
        for (j in 1:length(results$list.of.communities.numbers)) {
          size_dist_chi[j] <- length(results$list.of.communities.numbers[[j]])
        }
        chi[count] <- formula_chi(size_dist_chi)
      }
      #if there are fewer than three communities...
      #chi threshold cannot be determined even if requested
      if (length(results$list.of.communities.numbers) < 3 & "chi" %in% threshold) {
        chi[count] <- NA
      }
      #entropy can always be calculated
      #when all nodes are isolated, entropy is simply zero, because all nodes are isolated and the formula is 1 * log2(1) = 0
      #entropy formula needs vector with sizes of each community and number of isolated nodes
      #when there are shared nodes, probabilities in formula would add up to value > 1
      #to circumvent that, shared nodes are divided between their communities (e.g., two communities share node then each community gets only 0.5 instead of 1)
      #code below creates vector with all necessary sizes and subtracts respective values from communities with shared nodes
      if ("entropy" %in% threshold) {
        if (length(results$list.of.communities.numbers) > 0) {
          size_dist_ent <- c()
          for (j in 1:length(results$list.of.communities.numbers)) {
            size_dist_ent[j] <- length(results$list.of.communities.numbers[[j]])
          }
        } else {size_dist_ent <- c()}
        if (length(results$shared.nodes.numbers) > 0) {
          for (s in results$shared.nodes.numbers) {
            counter <- c()
            for (c in 1:length(results$list.of.communities.numbers)) {
              if (s %in% results$list.of.communities.numbers[[c]]) {
                counter <- c(counter,c)
              }
            }
            for (a in counter) {
              size_dist_ent[a] <- size_dist_ent[a] - (1 - 1/length(counter))
            }
          }
        } else {size_dist_ent <- size_dist_ent}
        if (length(results$isolated.nodes.numbers) > 0) {
          isolated_ent <- length(results$isolated.nodes.numbers)
        } else {isolated_ent <- c()}
        size_combined <- c(size_dist_ent,isolated_ent)
        entropy[count] <- formula_entropy(size_combined)
      }
      k_cp[count] <- k
      community[count] <- length(results$list.of.communities.numbers)
      isolated[count] <- length(results$isolated.nodes.numbers)
      count <- count + 1
      
      #progress bar update
      progress_bar_counter <- progress_bar_counter + 1
      utils::setTxtProgressBar(progress_bar, progress_bar_counter)
    }
  }
  
  #close progress bar
  close(progress_bar)
  
  #return data frame with respective values and add thresholds depending on request
  
  #first, create data set with information about simulation parameters depending on method
  
  if (method == "weighted" | method == "weighted.CFinder") {
    data <- data.frame(cbind(k_cp,I_cp,community,isolated))
    names(data) <- c("k","Intensity","Number.of.Communities","Number.of.Isolated.Nodes")
  }
  
  if (method == "unweighted") {
    data <- data.frame(cbind(k_cp,community,isolated))
    names(data) <- c("k","Number.of.Communities","Number.of.Isolated.Nodes")
  }
  
  #second, add required thresholds
  
  if ("largest.components.ratio" %in% threshold) {
    data <- cbind(data, Ratio.Threshold = ratio)
  }
  
  if ("chi" %in% threshold) {
    data <- cbind(data, Chi.Threshold = chi)
  }
  
  if ("entropy" %in% threshold) {
    data <- cbind(data, Entropy.Threshold = entropy)
  }
  
  return(data)
  
}
