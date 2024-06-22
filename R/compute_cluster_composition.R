#' Compute cluster composition and filter based on percentage
#'
#' @param data A data frame containing the original dataset.
#' @param cluster_df A data frame containing cluster assignments.
#' @return A filtered data frame summarizing cluster composition.
#' @export
compute_cluster_composition <- function(data, cluster_df) {
  # Merge with the original dataset
  merged_data <- data %>%
    mutate(ID = row_number()) %>%
    inner_join(cluster_df, by = "ID")

  # Compute composition with IDs
  composition <- merged_data %>%
    group_by(Cluster, class) %>%
    summarise(Count = n(), IDs = paste(ID, collapse = ", ")) %>%
    ungroup() %>%
    group_by(Cluster) %>%
    mutate(Percentage = Count / sum(Count) * 100)



  # Return filtered composition
  return(composition)
}
