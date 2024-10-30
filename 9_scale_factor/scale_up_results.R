
homes_heating <- homes_heating %>%
  # Group by period and compute the total cluster_size for each period
  group_by(period) %>%
  mutate(total_cluster_size_for_period = sum(cluster_size, na.rm = TRUE)) %>%
  ungroup() %>%
  # Join with vintage_values_all_periods to get the value for each period
  left_join(vintage_values_all_periods, by = c("period" = "period")) %>%
  # Compute the scale_factor
  mutate(scale_factor = cluster_size * value / total_cluster_size_for_period) %>%
  # Compute the scale_factor_final
  mutate(scale_factor_final = scale_factor)

homes_heating
