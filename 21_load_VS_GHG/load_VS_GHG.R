# -------------------------------
# Load required libraries
# -------------------------------
library(dplyr)
library(readr)
library(openxlsx)
library(ggplot2)
library(scales)
library(ggrepel)

# -------------------------------
# Define provinces
# -------------------------------
provinces <- c("ON", "BC", "MB", "NB", "NF", "NS", "AB", "PE", "QC", "SK")

# -------------------------------
# Define a function to process the RDS files for a given province and backup type
# -------------------------------
process_province_data <- function(province, backup_type = c("NG", "Electric")) {
  backup_type <- match.arg(backup_type)
  tibble_list <- list()
  
  # Choose file names based on backup type
  if(backup_type == "NG"){
    file_names <- c("Single_Detached_Natural_Gas_gas",
                    "Single_Detached_Electricity_electricity",
                    "Single_Detached_Oil_electricity",
                    "Single_Attached_Natural_Gas_gas",
                    "Single_Attached_Electricity_electricity",
                    "Single_Attached_Oil_electricity")
  } else if(backup_type == "Electric"){
    file_names <- c("Single_Detached_Natural_Gas_electricity",
                    "Single_Detached_Electricity_electricity",
                    "Single_Detached_Oil_electricity",
                    "Single_Attached_Natural_Gas_electricity",
                    "Single_Attached_Electricity_electricity",
                    "Single_Attached_Oil_electricity")
  }
  
  # Loop over each file name for this province
  for(file_name in file_names){
    file_path <- paste0("Results_paper/Results_Costmood/", province, "_", file_name, 
                        "/homes_heating_", province, "_", file_name, ".rds")
    if(file.exists(file_path)){
      dat <- readRDS(file_path)
      
      # Rename furnace column if necessary:
      # - If "furnace_ghg" exists, then leave as is.
      # - Otherwise, if "resistance_ghg" exists, rename it to "furnace_ghg".
      # - Otherwise, if "oil_furnace_ghg" exists, rename it to "furnace_ghg".
      if(!"furnace_ghg" %in% colnames(dat)){
        if("resistance_ghg" %in% colnames(dat)){
          dat <- dat %>% rename(furnace_ghg = resistance_ghg)
        } else if("oil_furnace_ghg" %in% colnames(dat)){
          dat <- dat %>% rename(furnace_ghg = oil_furnace_ghg)
        }
      }
      
      # Always select these columns (if available):
      cols_to_keep <- c("ac_ghg", "furnace_ghg", "hp_ghg", 
                        "oldac_ghg", "oldfurnace_ghg",
                        "saving", "ghg", "ghg_wold", "id", "fsa", "prov", "scale_factor_final")
      available_cols <- intersect(cols_to_keep, colnames(dat))
      dat <- dat %>% select(all_of(available_cols))
      
      # Multiply each of the key emissions columns by scale_factor_final to get scaled values
      if("ac_ghg" %in% colnames(dat)){
        dat <- dat %>% mutate(scaled_ac_ghg = ac_ghg * scale_factor_final)
      }
      if("furnace_ghg" %in% colnames(dat)){
        dat <- dat %>% mutate(scaled_furnace_ghg = furnace_ghg * scale_factor_final)
      }
      if("hp_ghg" %in% colnames(dat)){
        dat <- dat %>% mutate(scaled_hp_ghg = hp_ghg * scale_factor_final)
      }
      if("oldac_ghg" %in% colnames(dat)){
        dat <- dat %>% mutate(scaled_oldac_ghg = oldac_ghg * scale_factor_final)
      }
      if("oldfurnace_ghg" %in% colnames(dat)){
        dat <- dat %>% mutate(scaled_oldfurnace_ghg = oldfurnace_ghg * scale_factor_final)
      }
      
      tibble_list[[file_name]] <- dat
    }
  }
  
  # If no files were found, return NULL
  if(length(tibble_list) == 0) return(NULL)
  
  # Combine all the tibbles into one for this province
  total_homes_heating <- bind_rows(tibble_list)
  return(total_homes_heating)
}

# -------------------------------
# Loop over provinces and backup types to compute GHG saving percentages
# -------------------------------
# Create an empty data frame to store the summary results
results <- data.frame()
# Optionally, store detailed data (if you need it later)
results_details <- list()

for(province in provinces){
  # --- Process for NG backup ---
  dat_ng <- process_province_data(province, backup_type = "NG")
  if(!is.null(dat_ng)){
    # Calculate total baseline emissions (AC + furnace) and post retrofit emissions (HP)
    baseline_emissions_ng <- sum(dat_ng$scaled_ac_ghg, dat_ng$scaled_furnace_ghg, na.rm = TRUE)
    post_retrofit_emissions_ng <- sum(dat_ng$scaled_hp_ghg, na.rm = TRUE)
    ghg_saving_ng <- ((baseline_emissions_ng - post_retrofit_emissions_ng) / baseline_emissions_ng) * 100
    
    results <- rbind(results, data.frame(province = province,
                                         backup = "NG",
                                         ghg_saving = ghg_saving_ng))
    results_details[[paste0(province, "_NG")]] <- dat_ng
  }
  
  # --- Process for Electric backup ---
  dat_elec <- process_province_data(province, backup_type = "Electric")
  if(!is.null(dat_elec)){
    baseline_emissions_elec <- sum(dat_elec$scaled_ac_ghg, dat_elec$scaled_furnace_ghg, na.rm = TRUE)
    post_retrofit_emissions_elec <- sum(dat_elec$scaled_hp_ghg, na.rm = TRUE)
    ghg_saving_elec <- ((baseline_emissions_elec - post_retrofit_emissions_elec) / baseline_emissions_elec) * 100
    
    results <- rbind(results, data.frame(province = province,
                                         backup = "Electric",
                                         ghg_saving = ghg_saving_elec))
    results_details[[paste0(province, "_Electric")]] <- dat_elec
  }
}

# -------------------------------
# Load the Excel files that contain the percentage increase in highest peak load
# -------------------------------
perc_increase_ng <- read_csv("Results_paper/Results_Costmood/percentage_increases_with_NG.csv")
perc_increase_elec <- read_csv("Results_paper/Results_Costmood/percentage_increases_no_NG.csv")

# For each, select the province and the column with the x-axis value.
perc_increase_ng <- perc_increase_ng %>% 
  select(province, percentage_increase_average_highest_peak_load) %>% 
  rename(load_increase = percentage_increase_average_highest_peak_load)

perc_increase_elec <- perc_increase_elec %>% 
  select(province, percentage_increase_average_highest_peak_load) %>% 
  rename(load_increase = percentage_increase_average_highest_peak_load)

# -------------------------------
# Merge the load-increase values with the results data frame
# -------------------------------
# For NG backup, join with perc_increase_ng; for Electric backup, join with perc_increase_elec.
results <- results %>%
  mutate(load_increase = NA_real_)  # initialize new column

results <- results %>%
  left_join(
    perc_increase_ng %>% rename(load_increase_NG = load_increase),
    by = "province"
  ) %>%
  left_join(
    perc_increase_elec %>% rename(load_increase_Elec = load_increase),
    by = "province"
  ) %>%
  mutate(load_increase = ifelse(backup == "NG", load_increase_NG, load_increase_Elec)) %>%
  select(province, backup, ghg_saving, load_increase)

# -------------------------------
# Plot the data
# -------------------------------
# In the final plot, each province will have two points (one for NG and one for Electric backup)
# that are connected by a line.
results <- results %>%
  mutate(point_label = ifelse(backup == "NG", "NG b", "Electric b")) %>%
  group_by(province) %>%
  mutate(point_label = if(n_distinct(ghg_saving) == 1 & n_distinct(load_increase) == 1) {
    ifelse(backup == "NG", NA, point_label)
  } else { point_label }) %>%
  ungroup()

# Define a manual shape mapping for provinces
province_shapes <- c("ON" = 16,  # solid circle
                     "BC" = 17,  # solid triangle
                     "MB" = 15,  # solid square
                     "NB" = 18,  # solid diamond
                     "NF" = 8,   # star
                     "NS" = 3,   # plus
                     "AB" = 7,   # inverted triangle
                     "PE" = 4,   # cross
                     "QC" = 9,   # diamond plus
                     "SK" = 10)  # circle plus

# Build the plot
p <- ggplot(results, aes(x = load_increase, y = ghg_saving, group = province, color = province, shape = province)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_shape_manual(values = province_shapes) +
  # Add labels for the endpoints using ggrepel for clarity
  geom_text_repel(aes(label = point_label), 
                  nudge_y = 2,    # adjust the vertical nudge as needed
                  nudge_x = 0,    # adjust the horizontal nudge as needed
                  show.legend = FALSE,
                  segment.size = 0.2) +
  labs(x = "Percentage Increase in Highest Peak Load",
       y = "GHG Saving (%)",
       title = "GHG Saving vs. Peak Load Increase by Province",
       subtitle = "Comparing Heat Pump with NG Backup and Electric Backup") +
  theme_minimal() +
  theme_light() +
  scale_x_continuous(labels = function(x) paste0(x, "%"))

# Save the plot
ggsave("Results_paper/Results_Costmood/ghg_saving_vs_load_increase.png", 
       plot = p, width = 10, height = 6)

# Optionally, view the plot
print(p)

