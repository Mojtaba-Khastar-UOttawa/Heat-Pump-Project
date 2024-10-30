# Electricity Load Data: 
# find the prov
load_prov <- representative_homes$PROVINCE[1]

# Define the path pattern to the data files and call Electricity Load Data based on MW
path_pattern <- "data/elec_load/%s.csv"

# Use a conditional approach to select the appropriate file based on the province
if (load_prov == "AB") {
  elec_load <- read_csv(sprintf(path_pattern, "AB"))
} else if (load_prov == "BC") {
  elec_load <- read_csv(sprintf(path_pattern, "BC"))
} else if (load_prov == "MB") {
  elec_load <- read_csv(sprintf(path_pattern, "MB"))
} else if (load_prov == "NB") {
  elec_load <- read_csv(sprintf(path_pattern, "NB"))
} else if (load_prov == "NF") {
  elec_load <- read_csv(sprintf(path_pattern, "NF"))
} else if (load_prov == "NS") {
  elec_load <- read_csv(sprintf(path_pattern, "NS"))
} else if (load_prov == "ON") {
  elec_load <- read_csv(sprintf(path_pattern, "ON"))
} else if (load_prov == "PE") {
  elec_load <- read_csv(sprintf(path_pattern, "PE"))
} else if (load_prov == "QC") {
  elec_load <- read_csv(sprintf(path_pattern, "QC"))
} else if (load_prov == "SK") {
  elec_load <- read_csv(sprintf(path_pattern, "SK"))
} else {
  stop("Province code not found or not supported.")
}

# Convert LST to a date-time object and load to numeric
elec_load$LST <- as.POSIXct(elec_load$LST, format="%Y-%m-%d %H:%M")
elec_load$load <- as.numeric(elec_load$load)

# Extract year and hour of the year
elec_load <- elec_load %>%
  mutate(year = year(LST),
         hour_of_year = yday(LST) * 24 + hour(LST))

# Filter the data for the year 2015
last_year_data <- elec_load %>% filter(year == 2015)

# Calculate average load for the entire year 2015
avg_load_2015 <- mean(last_year_data$load, na.rm = TRUE)
avg_electricity_load <- mean(last_year_data$load, na.rm = TRUE)
# 1. Identify the top 10% of hours with the highest electricity load in 2015

# Calculate the threshold for the top 10% of loads
threshold <- quantile(last_year_data$load, 0.9, na.rm = TRUE)
# Filter the data to only these hours
peak_hours_data <- last_year_data %>% filter(load >= threshold)

# 2. Calculate the average load during these hours
avg_load_peak_hours <- mean(peak_hours_data$load)


# Join the datasets on hour_of_year (or hour) (exist electricity load + hp elec use)
aggregated_data <- last_year_data %>%
  left_join(optimal_plot_data, by = c("hour_of_year" = "hour"))

# Join the datasets on hour_of_year (or hour) (exist electricity load + hp elec use for cooling)
aggregated_data <- aggregated_data %>%
  left_join(optimal_plot_data_c, by = c("hour_of_year" = "hour"))

# Create a new column that aggregate the electricity load and hp_elec_use
aggregated_data <- aggregated_data %>%
  mutate(aggregated_load = load + hp_elec_use_hourly) %>%
  mutate(aggregated_load_c = load + hp_elec_use_c_hourly) %>%
  mutate(aggregated_load_t = load + hp_elec_use_hourly + hp_elec_use_c_hourly)

# Calculate the average aggregated load for the entire year 2015
avg_electricity_load <- mean(aggregated_data$load, na.rm = TRUE)
avg_aggregated_load <- mean(aggregated_data$aggregated_load, na.rm = TRUE)
avg_aggregated_load_c <- mean(aggregated_data$aggregated_load_c, na.rm = TRUE)
avg_aggregated_load_t <- mean(aggregated_data$aggregated_load_t, na.rm = TRUE)

# Calculate the threshold for the top 10% of loads
threshold_aggregated <- quantile(aggregated_data$aggregated_load, 0.9, na.rm = TRUE)
threshold_aggregated_c <- quantile(aggregated_data$aggregated_load_c, 0.9, na.rm = TRUE)
threshold_aggregated_t <- quantile(aggregated_data$aggregated_load_t, 0.9, na.rm = TRUE)

# Filter the data to only these hours
peak_hours_data_aggregated <- aggregated_data %>% filter(aggregated_load >= threshold)
peak_hours_data_aggregated_c <- aggregated_data %>% filter(aggregated_load_c >= threshold)
peak_hours_data_aggregated_t <- aggregated_data %>% filter(aggregated_load_t >= threshold)

# Calculate the average load during these hours
avg_load_peak_hours_aggregated <- mean(peak_hours_data_aggregated$aggregated_load)
avg_load_peak_hours_aggregated_c <- mean(peak_hours_data_aggregated_c$aggregated_load_c)
avg_load_peak_hours_aggregated_t <- mean(peak_hours_data_aggregated_t$aggregated_load_t)

## Combined plot
# Calculate the difference
load_difference <- avg_aggregated_load - avg_electricity_load
peak_load_difference <- avg_load_peak_hours_aggregated - avg_load_peak_hours

load_difference_c <- avg_aggregated_load_c - avg_electricity_load
peak_load_difference_c <- avg_load_peak_hours_aggregated_c - avg_load_peak_hours

load_difference_t <- avg_aggregated_load_t - avg_electricity_load
peak_load_difference_t <- avg_load_peak_hours_aggregated_t - avg_load_peak_hours

# Calculate the difference in percentage
load_difference_percentage <- (load_difference / avg_electricity_load) * 100
peak_load_difference_percentage <- (peak_load_difference / avg_load_peak_hours) * 100

load_difference_percentage_c <- (load_difference_c / avg_electricity_load) * 100
peak_load_difference_percentage_c <- (peak_load_difference_c / avg_load_peak_hours) * 100

load_difference_percentage_t <- (load_difference_t / avg_electricity_load) * 100
peak_load_difference_percentage_t <- (peak_load_difference_t / avg_load_peak_hours) * 100

# Print the result
print(paste("Difference between Average aggregated Load with HP and Average Electricity Load:", round(load_difference_percentage, 2), "%"))
print(paste("Difference between Average aggregated peak Load with HP and Average Electricity peak Load:", round(peak_load_difference_percentage, 2), "%"))

print(paste("Difference between Average aggregated Load just for cooling and Average Electricity Load:", round(load_difference_percentage_c, 2), "%"))
print(paste("Difference between Average aggregated peak Load just for cooling and Average Electricity peak Load:", round(peak_load_difference_percentage_c, 2), "%"))

print(paste("Difference between Average aggregated Load H&C and Average Electricity Load:", round(load_difference_percentage_t, 2), "%"))
print(paste("Difference between Average aggregated peak Load H&C and Average Electricity peak Load:", round(peak_load_difference_percentage_t, 2), "%"))

###
# Plot the electricity load for 2015, aggregated load Heating and cooling, average load during these peak hours, and visualize the difference in average load during the peak hours and the entire year.

# Define median hour for annotation placement
median_hour_t <- median(aggregated_data$hour_of_year)

# Adjust the x-coordinate for the peak hours text annotation
adjusted_x_position_peak_t <- median_hour_t + 800 # Modify this value as needed

# Plot the electricity load for 2015, aggregated load for Heating and Cooling (H&C), and average load during peak hours
combined_elec_load_plot_t <- ggplot() +
  geom_line(data = last_year_data, aes(x = hour_of_year, y = load), size = 1, alpha = 0.3, color = "#1E90FF") +   
  geom_hline(yintercept = avg_load_2015, size = 1, linetype="dashed", color = "#4682B4") +
  geom_hline(yintercept = avg_load_peak_hours, size = 1, linetype="dashed", color = "#FF6347") +
  geom_line(data = aggregated_data, aes(x = hour_of_year, y = aggregated_load_t), size = 1, alpha = 0.3, color = "#FF4500") + 
  geom_hline(yintercept = avg_aggregated_load_t, size = 1, linetype="dashed", color = "#4682B4") +
  geom_hline(yintercept = avg_load_peak_hours_aggregated_t, size = 1, linetype="dashed", color = "#FF6347") +
  geom_segment(aes(x = median_hour_t, y = avg_electricity_load, xend = median_hour_t, yend = avg_aggregated_load_t),
               arrow = arrow(type = "closed", length = unit(0.15, "inches")),
               linetype = "dashed", color = "#008080", size = 0.8) +
  annotate("text", x = median_hour_t + 700, y = (avg_aggregated_load_t + avg_electricity_load) / 2, 
           label = paste("Percentage Increase in Average Load on a year - H&C: ", round(load_difference_percentage_t, 2), "%"), 
           color = "#008080", size = 5, hjust = 0) +
  geom_segment(aes(x = adjusted_x_position_peak_t, y = avg_load_peak_hours, 
                   xend = adjusted_x_position_peak_t, yend = avg_load_peak_hours_aggregated_t),
               arrow = arrow(type = "closed", length = unit(0.15, "inches")),
               linetype = "dashed", color = "#6a5acd", size = 0.8) +
  annotate("text", x = adjusted_x_position_peak_t - 700, y = (avg_load_peak_hours_aggregated_t + avg_load_peak_hours) / 2, 
           label = paste("Percentage Increase in Average Load on the top 10% of Peak Hours- H&C: ", round(peak_load_difference_percentage_t, 2), "%"), 
           color = "#6a5acd", size = 5, hjust = 1) +
  guides(color = FALSE) + # Remove the color guide to clean up the legend
  labs(title = "Electricity Load and Aggregated Load with Heat Pump Electricity Usage for Heating and Cooling by Hour of Year",
       subtitle = "This Plot Presented for Single-attached Houses in Ontario",
       x = "Hour of Year",
       y = "Load (MW)",
       caption = "Electricity Load depicted in light blue, Aggregated Load for Heating and Cooling in orange. Dashed lines represent averages.") +
  theme_light() +
  theme(legend.position = "bottom", legend.title = element_blank()) # Move legend to bottom and remove title

# Print the plot
print(combined_elec_load_plot_t)
# Save the plot with the specified dimensions
ggsave("figures_tables/ON-SD-Gas-Bgas/combined_elec_load_plot_t.png", plot = combined_elec_load_plot_t, width = 16, height = 16)
