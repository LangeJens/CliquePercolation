
[![CRAN
Version](http://www.r-pkg.org/badges/version/CliquePercolation)](https://cran.r-project.org/package=CliquePercolation)
[![Downloads](https://cranlogs.r-pkg.org/badges/CliquePercolation)](https://cran.r-project.org/package=CliquePercolation)
[![Travis build
status](https://travis-ci.com/LangeJens/CliquePercolation.svg?branch=master)](https://travis-ci.com/LangeJens/CliquePercolation)

# CliquePercolation

**CliquePercolation** is an **R** package that entails multiple
functions related to the clique percolation community detection
algorithms for undirected, unweighted networks and for undirected,
weighted networks. As various sciences analyze structural properties of
networks, the **CliquePercolation** package can be useful in many fields
such as physics, biology, psychology, computer science, or sociology.

## Installation

To use **CliquePercolation**, you first need to install **R** (find
download page [here](https://cran.r-project.org/)). Moreover, I
recommend to use **R** via *RStudio*, an environment that facilitates
the use of **R** in multiple ways (find download page
[here](https://rstudio.com/)).

The developmental version of **CliquePercolation** is available on
GitHub. You can download it from GitHub directly with the help of the
**devtools** package in **R**:

``` r
install.packages("devtools")
devtools::install_github("LangeJens/CliquePercolation", build_vignettes = TRUE)
```

You can download the current stable version from CRAN:

``` r
install.packages("CliquePercolation")
```

## Community Guidelines

If you have recommendations for improving **CliquePercolation**, want to
report issues or problems with the package, or need support, you can
either open an
[issue](https://github.com/LangeJens/CliquePercolation/issues) on GitHub
or contact me directly via e-mail (<lange.jens@outlook.com>). If you
consider contributing to **CliquePercolation**, you can also submit a
[pull request](https://github.com/LangeJens/CliquePercolation/pulls).

## Illustrative Examples

The following includes two examples – one for an undirected, weighted
network and one for an undirected, unweighted network – to illustrate
parts of the package with real data. For a more extensive description of
(a) the clique percolation algorithms for undirected, weighted as well
as undirected, unweighted networks and (b) the workflow of the package
with multiple, fictitious examples, see the package vignette, e.g., by
running `vignette("CliquePercolation")` after installing and loading
(`library(CliquePercolation)`) the package. Furthermore, the
documentation of each function covers various other standalone examples
with fictitious and real data that can be used to verify their
functionality. An overview of all functions and links to their
documentations are available by running `?'CliquePercolation'`.

### The Obama network

The package includes a data set with 10 evaluative reactions toward
Barack Obama from 5,914 participants (for details see `?Obama`). This
data set can be used to estimate a Gaussian Graphical Model (i.e., a
regularized partial correlation network), which is an undirected,
weighted network. Subsequently, the **CliquePercolation** package allows
analyzing the community structure of the network.

First, we will load the data, estimate the network with the package
**bootnet**, and plot the network with the package **qgraph**.

``` r
#load data
data(Obama)

#estimate network
net <- bootnet::estimateNetwork(Obama, default = "EBICglasso", missing = "pairwise")

#plot network
graph <- plot(net, layout = "spring")
```

<img src="README_files/figure-gfm/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

Second, we optimize *k* and *I* values for running the clique
percolation algorithm. We do so by determining the entropy values of
community partitions for a range of *k* and *I* values and then applying
a permutation test to determine which of these combinations produce
community partitions that are more surprising than already expected by
chance.

``` r
#determine entropy for a range of k and I values
threshold <- cpThreshold(graph, method = "weighted",
                         k.range = 3:4,
                         I.range = seq(0.1, 0.5, 0.01),
                         threshold = "entropy")

#use permutation test to determine which k and I are optimal
permute <- cpPermuteEntropy(graph, cpThreshold.object = threshold,
                            ncores = 2, seed = 4186)
permute
#> 
#> Confidence intervals for entropy values of random permutations of original network
#> 
#> --------------------
#> 
#> User-specified Settings
#> 
#> n = 100 
#> interval = 0.95 
#> CFinder = FALSE 
#> ncores = 2 
#> seed = 4186 
#> 
#> 
#> --------------------
#> 
#> Confidence intervals
#> 
#>  k 95% CI lower 95% CI upper
#>  3        1.409        1.519
#>  4        1.233        1.350
#> 
#> 
#> --------------------
#> 
#> Extracted rows from cpThreshold object
#> 
#>  k Intensity Number.of.Communities Number.of.Isolated.Nodes Entropy.Threshold
#>  3      0.14                     3                        0             1.539
#>  3      0.15                     3                        2             1.985
#>  3      0.16                     3                        2             1.985
#>  3      0.17                     3                        2             1.985
#>  3      0.18                     2                        4             1.571
#>  4      0.14                     2                        3             1.539
#>  4      0.15                     2                        4             1.571
```

Third, we run the clique percolation algorithm with the optimized
values. That is, *k = 3* and *I = 0.14* produce the highest entropy with
the smallest number of isolated nodes that is more surprising than
already expected by chance.

``` r
#run clique percolation algorithm with optimal k and I
cpk3I.14 <- cpAlgorithm(graph, k = 3, I = 0.14, method = "weighted")

#print results overview
cpk3I.14
#> 
#> Results of clique percolation community detection algorithm
#> 
#> --------------------
#> 
#> User-specified Settings
#> 
#> method = weighted 
#> k = 3
#> I = 0.14
#> 
#> --------------------
#> 
#> Results
#> 
#> Number of communities: 3 
#> Number of shared nodes: 3 
#> Number of isolated nodes: 0 
#> 
#> --------------------
#> 
#> For details, use summary() (see ?summary.cpAlgorithm).

#show detailed summary of results
summary(cpk3I.14)
#> 
#> --------------------
#> Communities (labels as identifiers of nodes)
#> --------------------
#> 
#> Community 1 : Mor Led Car Kno Int Hns 
#> Community 2 : Hns Ang Afr 
#> Community 3 : Led Car Hop Prd 
#> 
#> 
#> --------------------
#> Shared nodes (labels as identifiers of nodes)
#> --------------------
#> 
#> Led Car Hns 
#> 
#> 
#> --------------------
#> Isolated nodes (labels as identifiers of nodes)
#> --------------------
#> 
#> NULL
```

Finally, we plot the community partition with three communities and
three shared nodes onto the original network.

``` r
#plot community partition on original network plot
coloredGraph <- cpColoredGraph(graph,
                               list.of.communities = cpk3I.14$list.of.communities.labels,
                               layout = "spring", theme = "colorblind")
```

<img src="README_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

The plot indicates that feelings and beliefs partly split up in
separate, yet overlapping communities.

### The immunoglobulin network

The package also includes an undirected, unweighted network of
interactions of 1,316 amino-acids (for details see `?immuno`), which we
can also analyze with the **CliquePercolation** package.

First, we will load the data and plot the network with the package
**qgraph**.

``` r
#load data
data(immuno)

#plot network
graph <- qgraph::qgraph(immuno, layout = "spring", vsize = 1)
```

<img src="README_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

Second, we optimize *k* for running the clique percolation algorithm. We
do so by determining the ratio of the largest to the second largest
community and the chi values for a range of *k* values to determine
whether a community partition leads to a ratio larger than 2 (and if
yes, which is the first community partition to exceed this threshold)
and/or to the highest chi value with a low number of isolated nodes.

``` r
#determine k for clique percolation
threshold <- cpThreshold(graph, method = "unweighted",
                         k.range = 4:5,
                         threshold = c("largest.components.ratio", "chi"))

threshold
#>   k Number.of.Communities Number.of.Isolated.Nodes Ratio.Threshold
#> 1 4                    17                        9        1.311404
#> 2 5                   219                      102        1.038462
#>   Chi.Threshold
#> 1   0.234150780
#> 2   0.005933637
```

Third, we run the clique percolation algorithm with the optimized value.
That is, *k = 4* produces the highest chi with a low number of isolated
nodes.

``` r
#run clique percolation algorithm with optimal k
cpk4 <- cpAlgorithm(graph, k = 4, method = "unweighted")

#print results overview
cpk4
#> 
#> Results of clique percolation community detection algorithm
#> 
#> --------------------
#> 
#> User-specified Settings
#> 
#> method = unweighted 
#> k = 4
#> 
#> --------------------
#> 
#> Results
#> 
#> Number of communities: 17 
#> Number of shared nodes: 56 
#> Number of isolated nodes: 9 
#> 
#> --------------------
#> 
#> For details, use summary() (see ?summary.cpAlgorithm).
```

Finally, we plot the community graph, namely a network in which each
node represents a community and edges indicate the number of nodes the
communities share. Larger nodes represent larger communities.

``` r
#plot community graph
communityGraph <- cpCommunityGraph(cpk4$list.of.communities.numbers,
                                   node.size.method = "proportional",
                                   max.node.size = 10,
                                   theme = "colorblind",
                                   layout = "spring",
                                   repulsion = 0.98)
```

<img src="README_files/figure-gfm/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

The plot indicates that the interactions of amino-acids in
immunoglobulin are structurally organized in three blocks of
interrelated communities. All three blocks are dominated by a few large
communities.
