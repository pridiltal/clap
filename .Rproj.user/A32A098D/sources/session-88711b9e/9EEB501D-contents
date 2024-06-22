#' Compute nearest neighbor distances for data points
#'
#' @param datanew2 A numeric matrix or data frame of data points.
#' @return A numeric vector of nearest neighbor distances.
#' @export
compute_nn_distances <- function(datanew2) {
  # Compute the pairwise distance matrix
  distance_matrix <- as.matrix(dist(datanew2))

  # Function to find the nearest neighbor distance for each point
  nearest_neighbor_distance <- function(distances) {
    # Exclude the distance to itself (which is zero)
    min(distances[distances != 0])
  }

  # Apply the function to each row of the distance matrix
  nn_distances <- apply(distance_matrix, 1, nearest_neighbor_distance)

  # Return nearest neighbor distances
  nn_distances
}
