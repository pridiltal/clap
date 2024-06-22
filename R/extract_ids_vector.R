#' Extract and convert IDs to numeric vector
#'
#' @param composition A data frame containing filtered composition data and class 'clap'.
#' @return A numeric vector of IDs.
#' @importFrom dplyr filter
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
#' cluster_result <- perform_clustering(training, class_column = class)
#' # Compute cluster composition
#' composition <- compute_cluster_composition(cluster_result)
#' # Extract IDs to numeric vector
#' ids_vector <- extract_ids_vector(composition)
#' overlapdata <- training[ids_vector, ]
#' p2 <- p+
#' geom_point(data = overlapdata, aes(X1, X2), colour= "black")
#' print(p2)
#'
#' @export
extract_ids_vector <- function(composition) {

  # Check if the input is of class 'clap'
  if (!inherits(composition, "clap")) {
    stop("Input must be an object of class 'clap' returned by compute_cluster_composition function.")
  }

  # Filter composition
  filtered_composition <- composition |>
    dplyr::filter(Percentage <= 50)
  # Extract IDs to a vector
  ids_vector <- unlist(strsplit(filtered_composition$IDs, ", "))
  ids_vector <- as.numeric(ids_vector)

  # Return IDs vector
  return(ids_vector)
}
