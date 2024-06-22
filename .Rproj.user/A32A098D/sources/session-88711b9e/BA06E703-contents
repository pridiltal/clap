#' Extract and convert IDs to numeric vector
#'
#' @param filtered_composition A data frame containing filtered composition data.
#' @return A numeric vector of IDs.
#' @export
extract_ids_vector <- function(composition) {

  # Filter composition
  filtered_composition <- composition %>%
    filter(Percentage <= 50)
  # Extract IDs to a vector
  ids_vector <- unlist(strsplit(filtered_composition$IDs, ", "))
  ids_vector <- as.numeric(ids_vector)

  # Return IDs vector
  ids_vector
}
