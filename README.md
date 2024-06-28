
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clap <img src="man/figures/logo.png" align="right" width = 150 />

<!-- badges: start -->
<!-- badges: end -->

## Overview

The issue of overlapping regions in multidimensional data arises when
different classes or clusters share similar feature representations,
making it challenging to delineate distinct boundaries between them
accurately. This package provides methods for detecting and visualizing
these overlapping regions using partitional clustering techniques based
on nearest neighbor distances.

## Installation

You can install the development version of clap from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pridiltal/clap")
```

## Installation

You can install the development version of clap from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pridiltal/clap")
```

## Example

``` r
library(clap)
class1 <- matrix(rnorm(100, mean = 0, sd = 1), ncol = 2) + matrix(rep(c(1, 1), each = 50), ncol = 2)
class2 <- matrix(rnorm(100, mean = 0, sd = 1), ncol = 2) + matrix(rep(c(-1, -1), each = 50), ncol = 2)
datanew <- rbind(class1, class2)
training <- data.frame(datanew,
class = factor(c(rep(1, 50), rep(2, 50))))

# Plot the dummy data to visualize overlaps
p <- ggplot2::ggplot(training, ggplot2::aes(x = X1, y = X2, color = class)) +
ggplot2::geom_point() +
ggplot2::labs(title = "Dummy Data with Overlapping Classes")
print(p)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r

# Perform clustering
cluster_result <- perform_clustering(training, class_column = class)
# Compute cluster composition
composition <- compute_cluster_composition(cluster_result)
# Extract IDs to numeric vector
ids_vector <- extract_ids_vector(composition)
# Subset data based on extracted IDs
overlapdata <- training[ids_vector, ]
# Plot overlapping data points
p2 <- p + ggplot2::geom_point(data = overlapdata, ggplot2::aes(X1, X2), colour = "black")
(p2)
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="100%" />
