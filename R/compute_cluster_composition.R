#' Compute cluster composition and filter based on percentage
#'
#' @param x An object of class 'clap' returned by perform_clustering function,
#'        containing members (list of clusters), cluster_df (data frame of cluster assignments),
#'        and the original dataset.
#' @return  filtered data frame summarizing cluster composition with class 'clap'.
#' @import dplyr
#' @example
#' class1 <- matrix(rnorm(100, mean = 0, sd = 1), ncol = 2) + matrix(rep(c(1, 1), each = 50), ncol = 2)
#' class2 <- matrix(rnorm(100, mean = 0, sd = 1), ncol = 2) + matrix(rep(c(-1, -1), each = 50), ncol = 2)
#' datanew <- rbind(class1, class2)
#' training <- data.frame(datanew, class = factor(c(rep(1, 50), rep(2, 50))))
#'
#' # Plot the dummy data to visualize overlaps
#' p <- ggplot(training, aes(x = X1, y = X2, color = class)) +
#' geom_point() +
#' labs(title = "Dummy Data with Overlapping Classes")
#' print(p)
#'
#' # Compute cluster composition
#' composition <- compute_cluster_composition(cluster_result)
#' @export
compute_cluster_composition <- function(x) {

  # Check if the input is of class 'clap'
  if (!inherits(x, "clap")) {
    stop("Input must be an object of class 'clap' returned by perform_clustering function.")
  }

  data <- x$data
  cluster_df <- x$cluster_df

  # Convert cluster_df to data frame if it's not already
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  # Merge with the original dataset
  merged_data <- data |>
    dplyr::mutate(ID = dplyr::row_number()) |>
    dplyr::inner_join(cluster_df, by = "ID")

  # Compute composition with IDs
  composition <- merged_data |>
    dplyr::group_by(Cluster, class) |>
    dplyr::summarise(Count = dplyr::n(), IDs = paste(ID, collapse = ", ")) |>
    dplyr::ungroup() |>
    dplyr::group_by(Cluster) |>
    dplyr::mutate(Percentage = Count / sum(Count) * 100)

  # Assign class 'clap' to the resulting composition
  class(composition) <- c("clap", class(composition))

  # Return filtered composition
  return(composition)
}
