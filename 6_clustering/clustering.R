
# if the exist furnace is gas or oil then code consider the furnace efficiency in clustering process if its electricity then not considering efficiency cause the electric heater's efficiency is 1 

furnace_type <- homes$exist_furnace[1]

if (furnace_type %in% c("Natural Gas", "Oil")) {
  set.seed(123)
  
  # Function to cluster homes for a specific period
  cluster_homes <- function(cluster_subset, start_cluster_id) {
    
    # Handle cases with only 1 row
    if (nrow(cluster_subset) == 1) {
      cluster_subset$cluster <- start_cluster_id
      cluster_subset$distance_to_centroid <- 0
      return(list(clustered_data = cluster_subset, representative_home = cluster_subset))
    }
    
    # Handle cases with 2 to 10 rows
    if (nrow(cluster_subset) <= 10) {
      cluster_subset$cluster <- start_cluster_id:(start_cluster_id + nrow(cluster_subset) - 1)
      cluster_subset$distance_to_centroid <- rep(0, nrow(cluster_subset))
      # Return all homes as representatives
      representative_homes <- cluster_subset
      return(list(clustered_data = cluster_subset, representative_home = representative_homes))
    }
    
    # For all other cases (more than 10 rows), proceed with clustering
    
    # 1. Select and scale the relevant columns for clustering
    columns_to_scale <- c("energy_heating_gj", "furnace_efficiency", "total_hdd")
    scaled_data <- scale(cluster_subset[columns_to_scale])
    
    # 2. Perform the clustering
    clusters_result <- kmeans(scaled_data, centers = 10)  # Fixed at 10 clusters
    
    # Attach cluster labels to the original data, adjusted by start_cluster_id
    cluster_subset$cluster <- clusters_result$cluster + start_cluster_id - 1
    
    # Calculate the distance of each home to its cluster centroid in the scaled space
    cluster_subset$distance_to_centroid <- sqrt(rowSums((scaled_data - clusters_result$centers[cluster_subset$cluster - start_cluster_id + 1,])^2))
    
    return(list(clustered_data = cluster_subset, kmeans_result = clusters_result))
  }
  
  
  # Prepare the data for clustering  ### In the below filter section I eliminate following condition, energy_cooling_gj != 0
  cluster_data <- homes %>%
    filter(!is.na(energy_heating_gj), !is.na(furnace_efficiency), !is.na(total_hdd)) %>%
    select(period, energy_heating_gj, furnace_efficiency, total_hdd)
  
  # Create an empty data frame to store the results
  results <- data.frame()
  
  # Iterate over each period and cluster the homes
  unique_periods <- unique(cluster_data$period)
  
  # Starting cluster ID
  current_cluster_id <- 1
  
  for(period in unique_periods) {
    cat("Processing period:", period, "\n")
    
    period_data <- cluster_data %>% filter(period == !!period)
    
    # Check if there's any NA, NaN, or Inf value in the data
    if(any(sapply(period_data, function(col) any(is.na(col) | is.nan(col) | is.infinite(col))))) {
      cat("Found NA/NaN/Inf values in period:", period, "\n")
      print(summary(period_data))
      next
    }
    
    clustering_output <- cluster_homes(period_data, current_cluster_id)
    clustered_period_data <- clustering_output$clustered_data
    
    # Print the clustering result details for this period
    cat("Clustering details for period:", period, "\n")
    print(clustering_output$kmeans_result)
    
    results <- rbind(results, clustered_period_data)
    
    # Update the cluster ID for the next iteration
    current_cluster_id <- max(clustered_period_data$cluster) + 1
  }
  # After clustering is done, match back the cluster assignments and distances to the original 'homes' dataframe
  homes <- left_join(homes, results %>% select(period, energy_heating_gj, furnace_efficiency, total_hdd, cluster, distance_to_centroid), by = c("period", "energy_heating_gj", "furnace_efficiency", "total_hdd"))
  
  # Add cluster size information
  homes <- homes %>%
    group_by(cluster) %>%
    mutate(cluster_size = n())
  
  # Select the home closest to the centroid for each cluster from the updated 'homes' dataframe
  representative_homes <- homes %>% 
    filter(cluster %in% unique(results$cluster)) %>% 
    group_by(cluster) %>% 
    slice(which.min(distance_to_centroid)) %>%
    ungroup()
  
  # 'representative_homes' now contains representative homes for each cluster.
  
  # After generating 'representative_homes', check if all periods are present
  expected_periods <- c("Before 1946", "1946–1960", "1961–1977", "1978–1983", "1984–1995", "1996–2000", "2001–2005", "2006–2010", "2011–2015", "2016–2020")
  missing_periods <- setdiff(expected_periods, unique(representative_homes$period))
  
  if (length(missing_periods) > 0) {
    cat("Warning: No representative homes found for the following periods:\n")
    print(missing_periods)
  }
  
} else if (furnace_type == "Electricity") {
  set.seed(123)
  
  # Function to cluster homes for a specific period
  cluster_homes <- function(cluster_subset, start_cluster_id) {
    
    # Handle cases with only 1 row
    if (nrow(cluster_subset) == 1) {
      cluster_subset$cluster <- start_cluster_id
      cluster_subset$distance_to_centroid <- 0
      return(list(clustered_data = cluster_subset, representative_home = cluster_subset))
    }
    
    # Handle cases with 2 to 10 rows
    if (nrow(cluster_subset) <= 10) {
      cluster_subset$cluster <- start_cluster_id:(start_cluster_id + nrow(cluster_subset) - 1)
      cluster_subset$distance_to_centroid <- rep(0, nrow(cluster_subset))
      # Return all homes as representatives
      representative_homes <- cluster_subset
      return(list(clustered_data = cluster_subset, representative_home = representative_homes))
    }
    
    # For all other cases (more than 10 rows), proceed with clustering
    
    # 1. Select and scale the relevant columns for clustering
    columns_to_scale <- c("energy_heating_gj", "total_hdd")
    scaled_data <- scale(cluster_subset[columns_to_scale])
    
    # 2. Perform the clustering
    clusters_result <- kmeans(scaled_data, centers = 10)  # Fixed at 10 clusters
    
    # Attach cluster labels to the original data, adjusted by start_cluster_id
    cluster_subset$cluster <- clusters_result$cluster + start_cluster_id - 1
    
    # Calculate the distance of each home to its cluster centroid in the scaled space
    cluster_subset$distance_to_centroid <- sqrt(rowSums((scaled_data - clusters_result$centers[cluster_subset$cluster - start_cluster_id + 1,])^2))
    
    return(list(clustered_data = cluster_subset, kmeans_result = clusters_result))
  }
  
  
  # Prepare the data for clustering  ### In the below filter section I eliminate following condition, energy_cooling_gj != 0
  cluster_data <- homes %>%
    filter(!is.na(energy_heating_gj), !is.na(total_hdd)) %>%
    select(period, energy_heating_gj, total_hdd)
  
  # Create an empty data frame to store the results
  results <- data.frame()
  
  # Iterate over each period and cluster the homes
  unique_periods <- unique(cluster_data$period)
  
  # Starting cluster ID
  current_cluster_id <- 1
  
  for(period in unique_periods) {
    cat("Processing period:", period, "\n")
    
    period_data <- cluster_data %>% filter(period == !!period)
    
    # Check if there's any NA, NaN, or Inf value in the data
    if(any(sapply(period_data, function(col) any(is.na(col) | is.nan(col) | is.infinite(col))))) {
      cat("Found NA/NaN/Inf values in period:", period, "\n")
      print(summary(period_data))
      next
    }
    
    clustering_output <- cluster_homes(period_data, current_cluster_id)
    clustered_period_data <- clustering_output$clustered_data
    
    # Print the clustering result details for this period
    cat("Clustering details for period:", period, "\n")
    print(clustering_output$kmeans_result)
    
    results <- rbind(results, clustered_period_data)
    
    # Update the cluster ID for the next iteration
    current_cluster_id <- max(clustered_period_data$cluster) + 1
  }
  # After clustering is done, match back the cluster assignments and distances to the original 'homes' dataframe
  homes <- left_join(homes, results %>% select(period, energy_heating_gj, total_hdd, cluster, distance_to_centroid), by = c("period", "energy_heating_gj", "total_hdd"))
  
  # Add cluster size information
  homes <- homes %>%
    group_by(cluster) %>%
    mutate(cluster_size = n())
  
  # Select the home closest to the centroid for each cluster from the updated 'homes' dataframe
  representative_homes <- homes %>% 
    filter(cluster %in% unique(results$cluster)) %>% 
    group_by(cluster) %>% 
    slice(which.min(distance_to_centroid)) %>%
    ungroup()
  
  # 'representative_homes' now contains representative homes for each cluster.
  
  # After generating 'representative_homes', check if all periods are present
  expected_periods <- c("Before 1946", "1946–1960", "1961–1977", "1978–1983", "1984–1995", "1996–2000", "2001–2005", "2006–2010", "2011–2015", "2016–2020")
  missing_periods <- setdiff(expected_periods, unique(representative_homes$period))
  
  if (length(missing_periods) > 0) {
    cat("Warning: No representative homes found for the following periods:\n")
    print(missing_periods)
  }
  
} else {
  stop("Unknown furnace type in the first row")
}

