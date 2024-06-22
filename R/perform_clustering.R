#' Perform clustering based on nearest neighbor distances
#'
#' @details This function first removes the specified class column from the data, calculates the nearest neighbor distances, and then performs clustering using a radius based on the maximum nearest neighbor distance.
#'
#' @param data A numeric matrix or data frame of data points.
#' @param class_column A character string or unquoted name specifying the name of the column containing class labels.
#' @return An object of class 'clap' containing:
#' \describe{
#'   \item{members}{A list of clusters with their respective data point IDs.}
#'   \item{cluster_df}{A data frame with cluster assignments for each data point.}
#'   \item{data}{The original dataset.}
#' }
#' @importFrom mclust partuniq
#' @importFrom FNN get.knnx
#' @importFrom stats dist
#' @importFrom dplyr select
#' @importFrom rlang enquo
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   # Generate dummy data
#'   class1 <- matrix(rnorm(100, mean = 0, sd = 1), ncol = 2) +
#'     matrix(rep(c(1, 1), each = 50), ncol = 2)
#'   class2 <- matrix(rnorm(100, mean = 0, sd = 1), ncol = 2) +
#'     matrix(rep(c(-1, -1), each = 50), ncol = 2)
#'   datanew <- rbind(class1, class2)
#'   training <- data.frame(datanew, class = factor(c(rep(1, 50), rep(2, 50))))
#'
#'   # Plot the dummy data to visualize overlaps
#'   p <- ggplot2::ggplot(training, ggplot2::aes(x = X1, y = X2, color = class)) +
#'     ggplot2::geom_point() +
#'     ggplot2::labs(title = "Dummy Data with Overlapping Classes")
#'   print(p)
#'
#'   # Perform clustering
#'   cluster_result <- perform_clustering(training, class_column = class)
#' }
#'
#' @export
perform_clustering <- function(data, class_column = NULL) {

  originaldata <- data

  class_column <- rlang::enquo(class_column)
  data <- data |>
    dplyr::select(-!!class_column)

  # Calculate the distance matrix for the data
  distance_matrix <- as.matrix(stats::dist(data))

  # Function to find the nearest neighbor distance for each point
  nearest_neighbor_distance <- function(distances) {
    # Exclude the distance to itself (which is zero)
    min(distances[distances != 0])
  }

  # Apply the function to each row of the distance matrix
  nn_distances <- apply(
    distance_matrix, 1,
    nearest_neighbor_distance
  )

  # Define the clustering radius
  radius <- max(nn_distances)

  # Perform clustering initialization using mclust package
  initial_clusters <- mclust::partuniq(data)
  unique_clusters <- unique(initial_clusters)

  # Initialize cluster members and exemplars
  num_points <- nrow(data)
  members <- vector("list", length = num_points)
  exemplars <- integer()
  exemplars[1] <- 1
  members[[1]] <- 1

  # Perform clustering based on nearest neighbors
  for (i in 2:num_points) {
    knn_result <- FNN::get.knnx(data[exemplars, , drop = FALSE],
                                data[i, , drop = FALSE], k = 1)
    nearest_exemplar_index <- knn_result$nn.index[1, 1]
    nearest_distance <- knn_result$nn.dist[1, 1]

    if (nearest_distance < radius) {
      exemplar <- exemplars[nearest_exemplar_index]
      members[[exemplar]] <- c(members[[exemplar]], i)
    } else {
      exemplars <- c(exemplars, i)
      members[[i]] <- i
    }
  }

  #  # Remove NULL entries from members
  members <- Filter(Negate(is.null), members)

  # Extract exemplars and assign names to clusters
  exemplars <- sapply(members, function(x) x[[1]])
  names(members) <- exemplars

  # Convert clusters to a data frame
  cluster_list <- lapply(names(members), function(x) {
    data.frame(ID = members[[x]], Cluster = as.numeric(x))
  })
  cluster_df <- do.call(rbind, cluster_list)

  # Create the final result as a list and assign class attribute 'clap' to the result
  result <- structure(list(
    members = members,
    cluster_df = cluster_df,
    data = originaldata
  ), class = "clap")

  # Return the result
  return(result)
}
