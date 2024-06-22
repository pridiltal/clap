#' Perform clustering based on nearest neighbor distances
#'
#' @param data A numeric matrix or data frame of data points.
#' @param nn_distances A numeric vector of nearest neighbor distances.
#' @return A list containing members (list of clusters) and cluster_df
#' (data frame of cluster assignments).
#' @importFrom mclust partuniq
#' @importFrom FNN get.knnx
#' @export
perform_clustering <- function(data, nn_distances) {
  n <- nrow(data)
  radius <- max(nn_distances)

  # Perform clustering initialization
  cl <- mclust::partuniq(data)
U <- unique(cl)

   m <- length(U)

  members <- rep(list(NULL), n)
  exemplars <- 1
  members[[1]] <- 1

  # Perform clustering based on nearest neighbors
  for (i in 2:n) {
    KNN <- FNN::get.knnx(data = data[exemplars, , drop = FALSE], query = data[i, , drop = FALSE], k = 1)
    m <- KNN$nn.index[1, 1]
    d <- KNN$nn.dist[1, 1]
    if (d < radius) {
      l <- exemplars[m]
      members[[l]] <- c(members[[l]], i)
      next
    }
    exemplars <- c(exemplars, i)
    members[[i]] <- i
  }

  # Remove NULL entries from members
  members <- members[!sapply(members, is.null)]

  # Extract exemplars and assign names to clusters
  exemplars <- sapply(members, function(x) x[[1]])
  names(members) <- exemplars

  # Convert clusters to a data frame
  cluster_list <- lapply(names(members), function(x) data.frame(ID = members[[x]], Cluster = as.numeric(x)))
  cluster_df <- do.call(rbind, cluster_list)

  # Return results
  list(
    members = members,
    cluster_df = cluster_df
  )
}
