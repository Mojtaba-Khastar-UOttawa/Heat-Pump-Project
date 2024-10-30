library(sf)
library(ggplot2)
library(dplyr)
library(readr)

# Load the shapefile FSA
canada_shape_fsa <- read_sf("data/Shapefiles/FSA/lfsa000b21a_e.shp")

# Define provinces and file patterns 

provinces <- c("ON", "BC", "AB", "MB", "NB", "NF", "NS", "PE", "QC", "SK")
folder_patterns <- c("Single_Attached_Electricity_electricity", 
                     "Single_Attached_Natural_Gas_gas", 
                     "Single_Attached_Oil_electricity", 
                     "Single_Detached_Electricity_electricity", 
                     "Single_Detached_Natural_Gas_gas", 
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

# Selecting the 'id', 'ghg', 'scale_factor', 'prov', and 'fsa' columns
filtered_combined_data <- filtered_combined_data[, c("id", "ghg", "scale_factor", "prov", "fsa")]

# Calculate Weighted GHG Savings and Sum by FSA
fsa_weighted_ghgs <- filtered_combined_data %>%
  mutate(weighted_ghg = ghg * scale_factor) %>%
  group_by(fsa) %>%
  summarise(
    total_weighted_ghg = sum(weighted_ghg, na.rm = TRUE),
    total_scale_factor = sum(scale_factor, na.rm = TRUE),
    weighted_average_ghg = total_weighted_ghg / total_scale_factor
  )

# Merge with Shapefile
fsa_map_data <- left_join(canada_shape_fsa, fsa_weighted_ghgs, by = c("CFSAUID" = "fsa"))

# Assign a gray color to NA values for FSAs without data
na.value_color <- "gray"

# Filter out negative GHG values by setting them to NA
fsa_map_data$weighted_average_ghg[fsa_map_data$weighted_average_ghg < 0] <- NA

# Create a Color Map with Gradient Scale for Weighted Average GHG Saving
plot_canada_weighted_average_ghg <- ggplot(data = fsa_map_data) +
  geom_sf(aes(fill = weighted_average_ghg)) +
  scale_fill_gradient(
    low = "#98fb98", 
    high = "#006400", # A deep, vivid green
    na.value = na.value_color, 
    guide = guide_colourbar(title.position = "top", title.hjust = 0.5, barwidth = 10, barheight = 0.5)
  ) +
  labs(title = "Average Annual GHG Savings per Household by FSA",
       fill = "Range of Average GHG Savings (Ton)") +
  theme_light() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 8),
    legend.text = element_text(face = "bold", size = 8),
    plot.title = element_text(face = "bold", size = 10),
    plot.margin = unit(c(4, 2, 4, 3), "points")  # Adjust margin around the entire plot
  )

# Print and Save the Plot
print(plot_canada_weighted_average_ghg)
ggsave("figures_tables/map/Canada-Weighted-Average-GHG-withng-f1.png", 
       plot = plot_canada_weighted_average_ghg, width = 12, height = 10, units = "in", dpi = 800)