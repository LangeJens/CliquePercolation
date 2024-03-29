% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cpCommunityGraph.R
\name{cpCommunityGraph}
\alias{cpCommunityGraph}
\title{Plotting Clique Percolation Community Network}
\usage{
cpCommunityGraph(
  list.of.communities,
  node.size.method = c("proportional", "normal"),
  max.node.size = 10,
  ...
)
}
\arguments{
\item{list.of.communities}{List object taken from results of cpAlgorithm
function; see also \link{cpAlgorithm}}

\item{node.size.method}{String indicating how node size is plotted (\code{"proportional"}
or \code{"normal"}); see Details}

\item{max.node.size}{Integer indicating size of the node representing the
largest community, if \code{node.size.method = "proportional"}}

\item{...}{any additional argument from qgraph; see also \link[qgraph]{qgraph}}
}
\value{
The function primarily plots the community network. Additionally, it returns a list with
  the weights matrix (\code{community.weights.matrix}) of the community network.
}
\description{
Function for plotting a network with nodes representing communities from clique percolation
community detection and edges representing the number of shared nodes of the communities.
}
\details{
The function takes the results of cpAlgorithm (see also \link{cpAlgorithm}),
  that is, either the \code{list.of.communities.numbers} or the
  \code{list.of.communities.labels} and plots the community network. Each node represents
  a community. Edges connecting two nodes represent the number of shared nodes between
  the two communities.
  
  The nodes can be plotted proportional to the sizes of the communities
  (\code{node.size.method = "proportional"}). The node representing the largest community
  is then plotted with the size specified in \code{max.node.size}. All other nodes
  are plotted relative to this largest node. Alternatively, all nodes can have the
  same size (\code{node.size.method = "normal"}).
  
  For the plotting, all isolated nodes will be ignored. If there are less than two
  communities in the list, plotting the network is useless. Therefore, an error
  is printed in this case.
}
\examples{
## Example with fictitious data

# create qgraph object
W <- matrix(c(0,1,1,0,0,0,0,
              0,0,1,0,0,0,0,
              0,0,0,1,1,1,0,
              0,0,0,0,1,1,0,
              0,0,0,0,0,1,0,
              0,0,0,0,0,0,1,
              0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE)
W <- Matrix::forceSymmetric(W)
W <- qgraph::qgraph(W)

# run clique percolation for unweighted networks
cp.results <- cpAlgorithm(W = W, k = 3, method = "unweighted")

# plot community network; proportional; maximum size is 7
cp.network1 <- cpCommunityGraph(cp.results$list.of.communities.numbers,
                                node.size.method = "proportional",
                                max.node.size = 7)
                                       
# plot community network; proportional; maximum size is 7
# change shape of nodes to triangle via qgraph argument
cp.network2 <- cpCommunityGraph(cp.results$list.of.communities.numbers,
                                node.size.method = "proportional",
                                max.node.size = 7,
                                shape = "triangle")
                                
## Example with Obama data set (see ?Obama)

# get data
data(Obama)

# estimate network
net <- qgraph::EBICglasso(qgraph::cor_auto(Obama), n = nrow(Obama))

# run clique percolation algorithm with specific k and I
cpk3I.16 <- cpAlgorithm(net, k = 3, I = 0.16, method = "weighted")

# plot community network; normal
Obama.network <- cpCommunityGraph(cpk3I.16$list.of.communities.numbers,
                                  node.size.method = "proportional",
                                  theme = "colorblind")

}
\author{
Jens Lange, \email{lange.jens@outlook.com}
}
