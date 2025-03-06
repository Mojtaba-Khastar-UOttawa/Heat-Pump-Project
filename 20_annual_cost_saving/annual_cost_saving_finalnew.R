
rm(list = ls())
gc()


# Load required libraries
library(dplyr)
library(readr)
library(openxlsx)
library(ggplot2)
library(scales)
library(tidyr)
library(ggthemes)  # For additional themes and scales

# Define the list of provinces
provinces <- c("ON", "BC", "MB", "NB", "NF", "NS", "AB", "PE", "QC", "SK")

# Define the desired order of scenarios
scenario_order <- c("base", 
                    "Rebate5K", 
                    "Rebate5K+lowinterest",
                    "Rebate10K", 
                    "Rebate10K+lowinterest",
                    "Rebate5K+ctax95", 
                    "Rebate10K+ctax95", 
                    "Rebate5K+ctax170", 
                    "Rebate10K+ctax170")

# Initialize an empty data frame to store results
all_data <- data.frame()

# Loop over each province and categorize data into NG, ELEC, and OIL
for (province in provinces) {
  
  # Define energy source categories
  heating_sources <- list(
    NG_ngb = c("Single_Detached_Natural_Gas_gas", "Single_Attached_Natural_Gas_gas"),
    NG_elecb = c("Single_Detached_Natural_Gas_electricity", "Single_Attached_Natural_Gas_electricity"),
    ELEC = c("Single_Detached_Electricity_electricity", "Single_Attached_Electricity_electricity"),
    OIL = c("Single_Detached_Oil_electricity", "Single_Attached_Oil_electricity")
  )
  
  # Loop over each energy category
  for (heating_type in names(heating_sources)) {
    
    # Initialize an empty list to store tibbles
    tibble_list <- list()
    
    # Process files for the current energy type
    for (file_name in heating_sources[[heating_type]]) {
      file_path <- paste0("Results_paper/Results_Costmood/", province, "_", file_name, "/mac_various_", province, "_", file_name, ".rds")
      
      # Read the file if it exists
      if (file.exists(file_path)) {
        tibble_list[[file_name]] <- readRDS(file_path)
      }
    }
    
    # Skip to the next energy type if no files are found
    if (length(tibble_list) == 0) next
    
    # Combine all tibbles in the list into one
    province_data <- bind_rows(tibble_list)
    
    # Filter out Inf values and only include defined scenarios
    province_data <- province_data %>%
      filter(!is.infinite(saving)) %>%
      filter(scenario %in% scenario_order)
    
    # Compute median savings for each scenario
    median_savings <- province_data %>%
      group_by(scenario) %>%
      summarise(median_saving = median(saving, na.rm = TRUE)) %>%
      mutate(province = paste0(province, "_", heating_type))  # Rename province to include energy type
    
    # Append to the main dataset
    all_data <- bind_rows(all_data, median_savings)
  }
}


# Separate "province" into two columns: real_province, heating_type
all_data <- all_data %>%
  separate(col = province, 
           into = c("real_province", "old_heating"), 
           sep = "_", 
           extra = "merge")  
# For instance, "ON_NG_elecb" -> real_province="ON", old_heating="NG_elecb"

# Keep scenario factor the same
scenario_order <- c("base", "Rebate5K", "Rebate5K+lowinterest", 
                    "Rebate10K", "Rebate10K+lowinterest",
                    "Rebate5K+ctax95", "Rebate10K+ctax95", 
                    "Rebate5K+ctax170", "Rebate10K+ctax170")
all_data$scenario <- factor(all_data$scenario, levels = scenario_order)

# Define shape for each scenario (same as before)
scenario_shapes <- c("base" = 1, 
                     "Rebate5K" = 2, 
                     "Rebate5K+lowinterest" = 3,
                     "Rebate10K" = 4, 
                     "Rebate10K+lowinterest" = 5,
                     "Rebate5K+ctax95" = 6, 
                     "Rebate10K+ctax95" = 7, 
                     "Rebate5K+ctax170" = 8, 
                     "Rebate10K+ctax170" = 9)

# Make the plot
plot <- ggplot(all_data, 
               aes(x = old_heating, 
                   y = median_saving,
                   shape = scenario,
                   color = scenario)) +
  geom_point(size = 3, alpha=0.8) +
  scale_shape_manual(values = scenario_shapes) +
  theme_light() +
  labs(x = "Previous Heating System",
       y = "Median Annual Cost Saving ($)",
       title = "Median Annual Cost Saving for Different Scenarios",
       subtitle = "Faceted by Province") +
  # 5) Facet by real_province so each province has a little gap
  facet_wrap(~ real_province, scales = "free_x", nrow = 1) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size=9),
    axis.text.y  = element_text(size=9),
    strip.text   = element_text(face="bold", size=10),  # Province labels
    panel.spacing.x = unit(1, "lines")                  # Gap between province panels
  )

# Save the updated plot
ggsave("Results_paper/Results_Costmood/Median_Saving_Scenarios_Faceted.png", 
       plot = plot, width = 12, height = 5, dpi = 300)

print(plot)


rm(list = ls())
gc()



###########
############
#################

## percentage of households with positive saving
# Load required libraries
library(dplyr)
library(readr)
library(openxlsx)
library(ggplot2)
library(scales)
library(tidyr)
library(ggthemes)

# 1) Define the list of provinces
provinces <- c("ON", "BC", "MB", "NB", "NF", "NS", "AB", "PE", "QC", "SK")

# 2) Define the desired order of scenarios
scenario_order <- c("base", 
                    "Rebate5K", 
                    "Rebate5K+lowinterest",
                    "Rebate10K", 
                    "Rebate10K+lowinterest",
                    "Rebate5K+ctax95", 
                    "Rebate10K+ctax95", 
                    "Rebate5K+ctax170", 
                    "Rebate10K+ctax170")

# 3) Initialize an empty data frame to store final results
all_data <- data.frame()

# 4) Loop over each province & heating sources
for (province in provinces) {
  
  # Heating source categories (same as before)
  heating_sources <- list(
    NG_ngb   = c("Single_Detached_Natural_Gas_gas",
                 "Single_Attached_Natural_Gas_gas"),
    NG_elecb = c("Single_Detached_Natural_Gas_electricity",
                 "Single_Attached_Natural_Gas_electricity"),
    ELEC     = c("Single_Detached_Electricity_electricity",
                 "Single_Attached_Electricity_electricity"),
    OIL      = c("Single_Detached_Oil_electricity",
                 "Single_Attached_Oil_electricity")
  )
  
  for (heating_type in names(heating_sources)) {
    
    tibble_list <- list()
    
    # 4a) Read the .rds files for that province & heating combination
    for (file_name in heating_sources[[heating_type]]) {
      file_path <- paste0("Results_paper/Results_Costmood/",
                          province, "_", file_name,
                          "/mac_various_", province, "_", file_name, ".rds")
      
      if (file.exists(file_path)) {
        tibble_list[[file_name]] <- readRDS(file_path)
      }
    }
    
    if (length(tibble_list) == 0) next
    
    # Combine all tibbles for that province+heating into one
    province_data <- bind_rows(tibble_list)
    
    # Filter out infinite values & keep only defined scenarios
    province_data <- province_data %>%
      filter(!is.infinite(saving)) %>%
      filter(scenario %in% scenario_order)
    
    # 4b) For each scenario, compute the sum of scale_factor_final 
    #     for rows with positive saving & the total sum of scale_factor_final
    #     Then we get the % of homes (weighted by scale_factor) that have saving>0
    positive_saving_stats <- province_data %>%
      group_by(scenario) %>%
      summarise(
        sum_scale_factor_positive = sum(ifelse(saving > 0, scale_factor_final, 0), na.rm=TRUE),
        total_scale_factor        = sum(scale_factor_final, na.rm=TRUE)
      ) %>%
      mutate(
        # If there's no scale factor, set 0 to avoid dividing by zero
        percentage_positive_saving = ifelse(
          total_scale_factor > 0,
          (sum_scale_factor_positive / total_scale_factor) * 100,
          0
        ),
        # Re-label province to combine real province + heating type
        province = paste0(province, "_", heating_type)
      ) %>%
      ungroup()
    
    # 4c) Bind these results into our master all_data
    #     Instead of 'median_saving', we store 'percentage_positive_saving'
    all_data <- bind_rows(all_data, positive_saving_stats)
  }
}

# 5) Convert scenario to factor with the defined levels
all_data$scenario <- factor(all_data$scenario, levels = scenario_order)

# 6) Define shapes for each scenario
scenario_shapes <- c("base"                 = 1, 
                     "Rebate5K"             = 2, 
                     "Rebate5K+lowinterest" = 3,
                     "Rebate10K"            = 4, 
                     "Rebate10K+lowinterest"= 5,
                     "Rebate5K+ctax95"      = 6, 
                     "Rebate10K+ctax95"     = 7, 
                     "Rebate5K+ctax170"     = 8, 
                     "Rebate10K+ctax170"    = 9)

##############################################################################
## 7) IMPROVE THE FIGURE: 
##    We'll separate the "province" field into real_province & old_heating,
##    then facet by province, exactly like in your example code.
##############################################################################

all_data <- all_data %>%
  separate(col = province,
           into = c("real_province", "old_heating"),
           sep = "_",
           extra = "merge")  # e.g. "ON_NG_ngb" -> real_province="ON", old_heating="NG_ngb"

plot <- ggplot(all_data,
               aes(x = old_heating,
                   y = percentage_positive_saving,
                   shape = scenario,
                   color = scenario)) +
  geom_point(size = 3, alpha=0.8) +
  scale_shape_manual(values = scenario_shapes) +
  theme_light() +
  labs(x = "Previous Heating System",
       y = "Percentage of Homes with Positive Saving (%)",
       title = "Percentage of Households that Save Money by Adopting a Heat Pump",
       subtitle = "Faceted by Province, Weighted by scale_factor_final") +
  facet_wrap(~ real_province, scales = "free_x", nrow = 1) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size=9),
    axis.text.y  = element_text(size=9),
    strip.text   = element_text(face="bold", size=10),  
    panel.spacing.x = unit(1, "lines"),  # Gap between province panels
    legend.position = "right"
  )

# 8) Save the updated plot
ggsave("Results_paper/Results_Costmood/Percentage_Positive_Saving_Faceted.png", 
       plot = plot, width = 12, height = 5, dpi = 300)

# 9) Print the plot
print(plot)


rm(list = ls())
gc()

