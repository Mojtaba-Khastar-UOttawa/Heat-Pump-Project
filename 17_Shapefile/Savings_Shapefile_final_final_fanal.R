# -1500 to +2500 

library(sf)
library(ggplot2)
library(dplyr)
library(readr)

# Load the shapefile FSA
canada_shape_fsa <- read_sf("data/Shapefiles/FSA/lfsa000b21a_e.shp")

# Define provinces and file patterns 

provinces <- c("ON", "BC", "AB", "MB", "NB", "NF", "NS", "PE", "QC", "SK")
folder_patterns <- c("Single_Attached_Electricity_electricity", 
                     "Single_Attached_Natural_Gas_electricity", 
                     "Single_Attached_Oil_electricity", 
                     "Single_Detached_Electricity_electricity", 
                     "Single_Detached_Natural_Gas_electricity", 
                     "Single_Detached_Oil_electricity")

# Initialize an empty list to store data from each file
all_data <- list()

# Loop through each province and folder pattern
for (prov in provinces) {
  for (pattern in folder_patterns) {
    folder_name <- paste0(prov, "_", pattern)
    file_path <- paste0("Results/", folder_name, "/mac_various_", folder_name, ".rds")
    if (file.exists(file_path)) {
      # Load the data and add to the list
      data <- readRDS(file_path)
      all_data <- append(all_data, list(data))
    }
  }
}

# Combine all data into one tibble
combined_data <- bind_rows(all_data)

# Filter data for the specific scenario "ctax170_+rebate"
filtered_combined_data <- combined_data[combined_data$scenario == "ctax170_+rebate", ]

# Selecting the 'id', 'saving', 'scale_factor', 'prov', and 'fsa' columns
filtered_combined_data <- filtered_combined_data[, c("id", "saving", "scale_factor", "prov", "fsa")]

# Filter to ignore values outside the range of -10000 to 10000
filtered_combined_data <- filtered_combined_data %>%
  filter(saving >= -2500 & saving <= 2500)

# Calculate Weighted Savings and Sum by FSA
fsa_weighted_savings <- filtered_combined_data %>%
  mutate(weighted_saving = saving * scale_factor) %>%
  group_by(fsa) %>%
  summarise(
    total_weighted_saving = sum(weighted_saving, na.rm = TRUE),
    total_scale_factor = sum(scale_factor, na.rm = TRUE),
    weighted_average_saving = total_weighted_saving / total_scale_factor
  )

# Merge with Shapefile
fsa_map_data <- left_join(canada_shape_fsa, fsa_weighted_savings, by = c("CFSAUID" = "fsa"))

# Assign a gray color to NA values for FSAs without data
na.value_color <- "gray"

# Define the colors from the RdYlGn palette
colors <- RColorBrewer::brewer.pal(11, "RdYlGn")  # Expanded to 11 colors for finer control

# Adjust these values to ensure 0 is exactly at the yellow transition
# -1500 to 0 (red to yellow), and 0 to 2500 (yellow to green)
values_for_colors <- c(-1500, -750, 0, 1250, 2500)

# Normalize the values to the range [0, 1]
normalized_values <- (values_for_colors - min(values_for_colors)) / (max(values_for_colors) - min(values_for_colors))

# Create a Color Map using a Custom Scale for Weighted Average Saving
plot_canada_weighted_average_saving <- ggplot(data = fsa_map_data) +
  geom_sf(aes(fill = weighted_average_saving)) +
  scale_fill_gradientn(
    colors = colors,
    values = normalized_values,  # Adjust these based on your recalculated values
    limits = c(-1500, 2500),  # Explicitly set limits for the color scale
    na.value = na.value_color,
    guide = guide_colourbar(title.position = "top", title.hjust = 0.5, barwidth = 10, barheight = 0.5),
    labels = scales::comma
  ) +
  labs(title = "Average Annual Monetary Savings per Household by FSA",
       fill = "Range of Average Savings ($)") +
  theme_light() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 8),
    legend.text = element_text(face = "bold", size = 8),
    plot.title = element_text(face = "bold", size = 10),
    plot.margin = unit(c(4, 2, 4, 3), "points")
  )


# Print and Save the Plot
print(plot_canada_weighted_average_saving)
ggsave("figures_tables/map/Canada-Weighted-Average-Saving-f_final333333334567.png", 
       plot = plot_canada_weighted_average_saving, width = 12, height = 10, units = "in", dpi = 800)



