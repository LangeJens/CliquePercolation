# CliquePercolation

**CliquePercolation** is an **R** package that entails multiple functions related to the clique percolation community detection algorithms for undirected, unweighted networks and for undirected, weighted networks. As various sciences analyze structural properties of networks, the **CliquePercolation** package can be useful in many fields such as physics, biology, psychology, computer science, or sociology. For an extensive description of (a) the clique percolation algorithms for undirected, unweighted as well as undirected, weighted networks and (b) the workflow of the package with multiple examples, see the package vignette, e.g., by running `vignette("CliquePercolation")` after installing the package. Furthermore, the documentation of each function covers various other standalone examples that can be used to verify their functionality. An overview of all functions and links to their documentations are available by running `?'CliquePercolation'`.

## Installation

To use **CliquePercolation**, you first need to install **R** (find download page [here](https://cran.r-project.org/)). Moreover, I recommend to use **R** via *RStudio*, an environment that facilitates the use of **R** in multiple ways (find download page [here](https://rstudio.com/)).

The developmental version of **CliquePercolation** is available on GitHub. You can download it from GitHub directly with the help of the **devtools** package in **R**:


```r
install.packages("devtools")
devtools::install_github("LangeJens/CliquePercolation", build_vignettes = TRUE)
```


You can download the current stable version from CRAN:


```r
install.packages("CliquePercolation")
```

## Community Guidelines

If you have recommendations for improving **CliquePercolation**, want to report issues or problems with the package, or need support, you can either open an [issue](https://github.com/LangeJens/CliquePercolation/issues) on GitHub or contact me directly via e-mail (<lange.jens@outlook.com>). If you consider contributing to **CliquePercolation**, you can also submit a [pull request](https://github.com/LangeJens/CliquePercolation/pulls).