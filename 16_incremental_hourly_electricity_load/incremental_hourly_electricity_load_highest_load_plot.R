
# Load required libraries
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

# Define the list of provinces ("ON", "BC", "MB", "NB", "NF", "NS", "AB", "PE", "QC", "SK")
provinces <- c("ON", "BC", "MB", "NB", "NF", "NS", "AB", "PE", "QC", "SK")

# Initialize an empty tibble to store the percentage increases
percentage_increases_with_NG <- tibble(
  province = character(),
  percentage_increase_average_load = numeric(),
  percentage_increase_average_10_percentage_peak_load = numeric(),
  percentage_increase_average_highest_peak_load = numeric()
)


# Loop over each province
for (province in provinces) {
  # Load electricity load data
  elec_load_path <- sprintf("data/elec_load/%s.csv", province)
  elec_load <- read_csv(elec_load_path)
  
  # Process elec_load
  elec_load <- elec_load %>%
    mutate(LST = as.POSIXct(LST, format="%Y-%m-%d %H:%M"),
           load = as.numeric(load),
           year = year(LST),
           hour_of_year = yday(LST) * 24 + hour(LST)) %>%
    filter(year == 2015)
  
  # Calculate average load and thresholds for peak hours
  avg_load_2015 <- mean(elec_load$load, na.rm = TRUE)
  threshold_90 <- quantile(elec_load$load, 0.9, na.rm = TRUE)
  threshold_9999 <- quantile(elec_load$load, 0.9999, na.rm = TRUE)
  
  peak_hours_data_90 <- elec_load %>% filter(load >= threshold_90)
  peak_hours_data_9999 <- elec_load %>% filter(load >= threshold_9999)
  
  avg_load_peak_hours_90 <- mean(peak_hours_data_90$load)
  avg_load_peak_hours_9999 <- mean(peak_hours_data_9999$load)
  
  # Load RDS file for "with_NG"
  rds_path <- sprintf("Results_paper/Results_Costmood/%s/total_incremental_hourly_electricity_load_%s_with_NG.rds", province, province)
  incremental_data <- readRDS(rds_path)
  
  # Join with elec_load and calculate aggregated load
  aggregated_data <- elec_load %>%
    left_join(incremental_data, by = c("hour_of_year" = "hour")) %>%
    mutate(aggregated_load = load + total_aggregated_heating_cooling)
  
  # Calculate averages and differences for aggregated data
  avg_aggregated_load <- mean(aggregated_data$aggregated_load, na.rm = TRUE)
  threshold_aggregated_90 <- quantile(aggregated_data$aggregated_load, 0.9, na.rm = TRUE)
  threshold_aggregated_9999 <- quantile(aggregated_data$aggregated_load, 0.9999, na.rm = TRUE)
  
  peak_hours_data_aggregated_90 <- aggregated_data %>% filter(aggregated_load >= threshold_aggregated_90)
  peak_hours_data_aggregated_9999 <- aggregated_data %>% filter(aggregated_load >= threshold_aggregated_9999)
  
  avg_load_peak_hours_aggregated_90 <- mean(peak_hours_data_aggregated_90$aggregated_load)
  avg_load_peak_hours_aggregated_9999 <- mean(peak_hours_data_aggregated_9999$aggregated_load)
  
  load_difference_90 <- avg_aggregated_load - avg_load_2015
  peak_load_difference_90 <- avg_load_peak_hours_aggregated_90 - avg_load_peak_hours_90
  load_difference_percentage_90 <- (load_difference_90 / avg_load_2015) * 100
  peak_load_difference_percentage_90 <- (peak_load_difference_90 / avg_load_peak_hours_90) * 100
  
  load_difference_9999 <- avg_aggregated_load - avg_load_2015
  peak_load_difference_9999 <- avg_load_peak_hours_aggregated_9999 - avg_load_peak_hours_9999
  load_difference_percentage_9999 <- (load_difference_9999 / avg_load_2015) * 100
  peak_load_difference_percentage_9999 <- (peak_load_difference_9999 / avg_load_peak_hours_9999) * 100
  
  # Append the calculated percentages to the tibble for the current province
  percentage_increases_with_NG <- percentage_increases_with_NG %>%
    add_row(province = province,
            percentage_increase_average_load = load_difference_percentage_90,
            percentage_increase_average_10_percentage_peak_load = peak_load_difference_percentage_90,
            percentage_increase_average_highest_peak_load = peak_load_difference_percentage_9999)
  
  # Define median hour for annotation placement
  median_hour <- median(aggregated_data$hour_of_year)
  adjusted_x_position_peak_90 <- median_hour + 800
  adjusted_x_position_peak_9999 <- median_hour + 2800
  
  # Construct the ggplot
  combined_elec_load_plot <- ggplot() +
    geom_line(data = elec_load, aes(x = hour_of_year, y = load), size = 1, alpha = 0.3, color = "#1E90FF") +
    geom_hline(yintercept = avg_load_2015, size = 0.5, linetype="dashed", color = "#4682B4") +
    geom_hline(yintercept = avg_load_peak_hours_90, size = 0.5, linetype="dotted", color = "#FF6347") +
    geom_hline(yintercept = avg_load_peak_hours_9999, size = 0.5, linetype="dotdash", color = "#FF6347") +
    geom_line(data = aggregated_data, aes(x = hour_of_year, y = aggregated_load), size = 1, alpha = 0.3, color = "#FF4500") +
    geom_hline(yintercept = avg_aggregated_load, size = 0.5, linetype="dashed", color = "#4682B4") +
    geom_hline(yintercept = avg_load_peak_hours_aggregated_90, size = 0.5, linetype="dotted", color = "#FF6347") +
    geom_hline(yintercept = avg_load_peak_hours_aggregated_9999, size = 0.5, linetype="dotdash", color = "#FF6347") +
    geom_segment(aes(x = median_hour, y = avg_load_2015, xend = median_hour, yend = avg_aggregated_load),
                 arrow = arrow(type = "closed", length = unit(0.05, "inches")),
                 linetype = "dashed", color = "#008080", size = 0.5) +
    annotate("text", x = median_hour - 200, y = (avg_aggregated_load + avg_load_2015) / 2,
             label = paste("% Increase in Average Load on a year - H&C: ", round(load_difference_percentage_90, 2), "%"),
             color = "#008080", size = 2.1, hjust = 0, fontface = "bold") +
    geom_segment(aes(x = adjusted_x_position_peak_90, y = avg_load_peak_hours_90, 
                     xend = adjusted_x_position_peak_90, yend = avg_load_peak_hours_aggregated_90),
                 arrow = arrow(type = "closed", length = unit(0.05, "inches")),
                 linetype = "dashed", color = "#6a5acd", size = 0.5) +
    annotate("text", x = adjusted_x_position_peak_90 + 800, y = (avg_load_peak_hours_aggregated_90 + avg_load_peak_hours_90) / 2, 
             label = paste("% Increase in Average Load on the top 10% of Peak Hours- H&C: ", round(peak_load_difference_percentage_90, 2), "%"), 
             color = "#6a5acd", size = 2.1, hjust = 1, fontface = "bold") +
    geom_segment(aes(x = adjusted_x_position_peak_9999, y = avg_load_peak_hours_9999, 
                     xend = adjusted_x_position_peak_9999, yend = avg_load_peak_hours_aggregated_9999),
                 arrow = arrow(type = "closed", length = unit(0.05, "inches")),
                 linetype = "dashed", color = "#6a5acd", size = 0.5) +
    annotate("text", x = adjusted_x_position_peak_9999 - 100, y = (avg_load_peak_hours_aggregated_9999 + avg_load_peak_hours_9999) / 2 + 1600, 
             label = paste("% Increase in Average Load on the Very Highest Load Hours- H&C: ", round(peak_load_difference_percentage_9999, 2), "%"), 
             color = "#6a5acd", size = 2.1, hjust = 1, fontface = "bold") +
    guides(color = FALSE) +
    labs(title = sprintf("Electricity Load and Aggregated Load with Heat Pump for %s", province),
         subtitle = "Comparison of Average Load During Peak Hours and Throughout the Year",
         x = "Hour of Year",
         y = "Load (MW)") +
    theme_light() +
    theme(legend.position = "bottom", legend.title = element_blank(),
          axis.text.x = element_text(size = 4, face = "bold"),
          axis.text.y = element_text(size = 2, face = "bold"),
          axis.title.x = element_text(size = 6),  # Set the x-axis label font size
          axis.title.y = element_text(size = 6),  # Set the y-axis label font size
          plot.title = element_text(size = 8),    # Set the plot title font size
          plot.subtitle = element_text(size = 6)) # Set the plot subtitle font size
  
  # Save the plot
  plot_path <- sprintf("Results_paper/Results_Costmood/%s/combined_elec_load_plot_%s_with_NG_highest_load.png", province, province)
  ggsave(plot_path, combined_elec_load_plot, width = 4.88, height = 3.88, units = "in", dpi = 300)
}

write_csv(percentage_increases_with_NG, "Results_paper/Results_Costmood/percentage_increases_with_NG.csv")

##################
#########################
################################

# Load required libraries
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

# Define the list of provinces 
provinces <- c("ON", "BC", "MB", "NB", "NF", "NS", "AB", "PE", "QC", "SK")

# Initialize an empty tibble to store the percentage increases
percentage_increases_no_NG <- tibble(
  province = character(),
  percentage_increase_average_load = numeric(),
  percentage_increase_average_10_percentage_peak_load = numeric(),
  percentage_increase_average_highest_peak_load = numeric()
)

# Loop over each province
for (province in provinces) {
  # Load electricity load data
  elec_load_path <- sprintf("data/elec_load/%s.csv", province)
  elec_load <- read_csv(elec_load_path)
  
  # Process elec_load
  elec_load <- elec_load %>%
    mutate(LST = as.POSIXct(LST, format="%Y-%m-%d %H:%M"),
           load = as.numeric(load),
           year = year(LST),
           hour_of_year = yday(LST) * 24 + hour(LST)) %>%
    filter(year == 2015)
  
  # Calculate average load and thresholds for peak hours
  avg_load_2015 <- mean(elec_load$load, na.rm = TRUE)
  threshold_90 <- quantile(elec_load$load, 0.9, na.rm = TRUE)
  threshold_9999 <- quantile(elec_load$load, 0.9999, na.rm = TRUE)
  
  peak_hours_data_90 <- elec_load %>% filter(load >= threshold_90)
  peak_hours_data_9999 <- elec_load %>% filter(load >= threshold_9999)
  
  avg_load_peak_hours_90 <- mean(peak_hours_data_90$load)
  avg_load_peak_hours_9999 <- mean(peak_hours_data_9999$load)
  
  # Load RDS file for "no_NG"
  rds_path <- sprintf("Results_paper/Results_Costmood/%s/total_incremental_hourly_electricity_load_%s_no_NG.rds", province, province)
  incremental_data <- readRDS(rds_path)
  
  # Join with elec_load and calculate aggregated load
  aggregated_data <- elec_load %>%
    left_join(incremental_data, by = c("hour_of_year" = "hour")) %>%
    mutate(aggregated_load = load + total_aggregated_heating_cooling)
  
  # Calculate averages and differences for aggregated data
  avg_aggregated_load <- mean(aggregated_data$aggregated_load, na.rm = TRUE)
  threshold_aggregated_90 <- quantile(aggregated_data$aggregated_load, 0.9, na.rm = TRUE)
  threshold_aggregated_9999 <- quantile(aggregated_data$aggregated_load, 0.9999, na.rm = TRUE)
  
  peak_hours_data_aggregated_90 <- aggregated_data %>% filter(aggregated_load >= threshold_aggregated_90)
  peak_hours_data_aggregated_9999 <- aggregated_data %>% filter(aggregated_load >= threshold_aggregated_9999)
  
  avg_load_peak_hours_aggregated_90 <- mean(peak_hours_data_aggregated_90$aggregated_load)
  avg_load_peak_hours_aggregated_9999 <- mean(peak_hours_data_aggregated_9999$aggregated_load)
  
  load_difference_90 <- avg_aggregated_load - avg_load_2015
  peak_load_difference_90 <- avg_load_peak_hours_aggregated_90 - avg_load_peak_hours_90
  load_difference_percentage_90 <- (load_difference_90 / avg_load_2015) * 100
  peak_load_difference_percentage_90 <- (peak_load_difference_90 / avg_load_peak_hours_90) * 100
  
  load_difference_9999 <- avg_aggregated_load - avg_load_2015
  peak_load_difference_9999 <- avg_load_peak_hours_aggregated_9999 - avg_load_peak_hours_9999
  load_difference_percentage_9999 <- (load_difference_9999 / avg_load_2015) * 100
  peak_load_difference_percentage_9999 <- (peak_load_difference_9999 / avg_load_peak_hours_9999) * 100
  
  # Append the calculated percentages to the tibble for the current province
  percentage_increases_no_NG <- percentage_increases_no_NG %>%
    add_row(province = province,
            percentage_increase_average_load = load_difference_percentage_90,
            percentage_increase_average_10_percentage_peak_load = peak_load_difference_percentage_90,
            percentage_increase_average_highest_peak_load = peak_load_difference_percentage_9999)
  
  # Define median hour for annotation placement
  median_hour <- median(aggregated_data$hour_of_year)
  adjusted_x_position_peak_90 <- median_hour + 800
  adjusted_x_position_peak_9999 <- median_hour + 2800
  
  # Construct the ggplot
  combined_elec_load_plot <- ggplot() +
    geom_line(data = elec_load, aes(x = hour_of_year, y = load), size = 1, alpha = 0.3, color = "#1E90FF") +
    geom_hline(yintercept = avg_load_2015, size = 0.5, linetype="dashed", color = "#4682B4") +
    geom_hline(yintercept = avg_load_peak_hours_90, size = 0.5, linetype="dotted", color = "#FF6347") +
    geom_hline(yintercept = avg_load_peak_hours_9999, size = 0.5, linetype="dotdash", color = "#FF6347") +
    geom_line(data = aggregated_data, aes(x = hour_of_year, y = aggregated_load), size = 1, alpha = 0.3, color = "#FF4500") +
    geom_hline(yintercept = avg_aggregated_load, size = 0.5, linetype="dashed", color = "#4682B4") +
    geom_hline(yintercept = avg_load_peak_hours_aggregated_90, size = 0.5, linetype="dotted", color = "#FF6347") +
    geom_hline(yintercept = avg_load_peak_hours_aggregated_9999, size = 0.5, linetype="dotdash", color = "#FF6347") +
    geom_segment(aes(x = median_hour, y = avg_load_2015, xend = median_hour, yend = avg_aggregated_load),
                 arrow = arrow(type = "closed", length = unit(0.05, "inches")),
                 linetype = "dashed", color = "#008080", size = 0.5) +
    annotate("text", x = median_hour - 200, y = (avg_aggregated_load + avg_load_2015) / 2,
             label = paste("% Increase in Average Load on a year - H&C: ", round(load_difference_percentage_90, 2), "%"),
             color = "#008080", size = 2.1, hjust = 0, fontface = "bold") +
    geom_segment(aes(x = adjusted_x_position_peak_90, y = avg_load_peak_hours_90, 
                     xend = adjusted_x_position_peak_90, yend = avg_load_peak_hours_aggregated_90),
                 arrow = arrow(type = "closed", length = unit(0.05, "inches")),
                 linetype = "dashed", color = "#6a5acd", size = 0.5) +
    annotate("text", x = adjusted_x_position_peak_90 + 800, y = (avg_load_peak_hours_aggregated_90 + avg_load_peak_hours_90) / 2, 
             label = paste("% Increase in Average Load on the top 10% of Peak Hours- H&C: ", round(peak_load_difference_percentage_90, 2), "%"), 
             color = "#6a5acd", size = 2.1, hjust = 1, fontface = "bold") +
    geom_segment(aes(x = adjusted_x_position_peak_9999, y = avg_load_peak_hours_9999, 
                     xend = adjusted_x_position_peak_9999, yend = avg_load_peak_hours_aggregated_9999),
                 arrow = arrow(type = "closed", length = unit(0.05, "inches")),
                 linetype = "dashed", color = "#6a5acd", size = 0.5) +
    annotate("text", x = adjusted_x_position_peak_9999 - 100, y = (avg_load_peak_hours_aggregated_9999 + avg_load_peak_hours_9999) / 2 + 1600, 
             label = paste("% Increase in Average Load on the Very Highest Load Hours- H&C: ", round(peak_load_difference_percentage_9999, 2), "%"), 
             color = "#6a5acd", size = 2.1, hjust = 1, fontface = "bold") +
    guides(color = FALSE) +
    labs(title = sprintf("Electricity Load and Aggregated Load with Heat Pump for %s", province),
         subtitle = "Comparison of Average Load During Peak Hours and Throughout the Year",
         x = "Hour of Year",
         y = "Load (MW)") +
    theme_light() +
    theme(legend.position = "bottom", legend.title = element_blank(),
          axis.text.x = element_text(size = 4, face = "bold"),
          axis.text.y = element_text(size = 2, face = "bold"),
          axis.title.x = element_text(size = 6),  # Set the x-axis label font size
          axis.title.y = element_text(size = 6),  # Set the y-axis label font size
          plot.title = element_text(size = 8),    # Set the plot title font size
          plot.subtitle = element_text(size = 6)) # Set the plot subtitle font size
  
  # Save the plot
  plot_path <- sprintf("Results_paper/Results_Costmood/%s/combined_elec_load_plot_%s_no_NG_highest_load.png", province, province)
  ggsave(plot_path, combined_elec_load_plot, width = 4.88, height = 3.88, units = "in", dpi = 300)
}

write_csv(percentage_increases_no_NG, "Results_paper/Results_Costmood/percentage_increases_no_NG.csv")
