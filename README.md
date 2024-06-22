
# clap

<!-- badges: start -->
<!-- badges: end -->

The goal of clap is to ...

## Installation

You can install the development version of clap from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pridiltal/clap")
```

## Example

This is a basic example which shows you how to solve a common problem:

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

