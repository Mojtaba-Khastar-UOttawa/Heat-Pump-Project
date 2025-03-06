# Load required libraries
library(dplyr)
library(readr)
library(openxlsx)

#### If the natural gas remains as fuel for backup heating for homes which has gas furnace 

# Define the list of provinces
provinces <- c("ON", "BC", "MB", "NB", "NF", "NS", "AB", "PE", "QC", "SK")

# Loop over each province
for (province in provinces) {
  # Initialize an empty list to store tibbles
  tibble_list <- list()
  
  # Construct file paths
  file_names <- c("Single_Detached_Natural_Gas_gas",
                  "Single_Detached_Electricity_electricity",
                  "Single_Detached_Oil_electricity",
                  "Single_Attached_Natural_Gas_gas",
                  "Single_Attached_Electricity_electricity",
                  "Single_Attached_Oil_electricity")
  
  for (file_name in file_names) {
    file_path <- paste0("Results_paper/Results_Costmood/", province, "_", file_name, "/incremental_hourly_electricity_load_", province, "_", file_name, ".rds")
    
    # Read the file if it exists
    if (file.exists(file_path)) {
      tibble_list[[file_name]] <- readRDS(file_path)
    }
  }
  
  # Skip to the next province if no files are found
  if (length(tibble_list) == 0) next
  
  # Extract and sum the 'aggregated_heating_cooling' column
  total_agg <- Reduce(`+`, lapply(tibble_list, `[[`, "aggregated_heating_cooling"))
  total_oldac <- Reduce(`+`, lapply(tibble_list, `[[`, "oldac_elec_use_hourly"))
  total_res  <- Reduce(`+`, lapply(tibble_list, `[[`, "resistance_elec_use_hourly"))
  
  # Create a new tibble with hour plus the summed columns
  final_tibble <- tibble(
    hour = tibble_list[[1]]$hour,
    total_aggregated_heating_cooling = total_agg,
    total_oldac_elec_use_hourly = total_oldac,
    total_resistance_elec_use_hourly = total_res
  )
  
  # Create a directory for the province in the Results folder
  province_dir <- paste0("Results_paper/Results_Costmood/", province)
  if (!dir.exists(province_dir)) {
    dir.create(province_dir)
  }
  
  # Define file names
  file_base <- paste0(province_dir, "/total_incremental_hourly_electricity_load_", province, "_with_NG")
  
  # Save the tibble in different formats
  saveRDS(final_tibble, file = paste0(file_base, ".rds"))
  write.csv(final_tibble, file = paste0(file_base, ".csv"), row.names = FALSE)
  write.xlsx(final_tibble, file = paste0(file_base, ".xlsx"))
}

##################################

### If backup heating is electrical for any homes 
# Define the list of provinces
provinces <- c("ON", "BC", "MB", "NB", "NF", "NS", "AB", "PE", "QC", "SK")

# Loop over each province
for (province in provinces) {
  # Initialize an empty list to store tibbles
  tibble_list <- list()
  
  # Construct file paths
  file_names <- c("Single_Detached_Natural_Gas_electricity",
                  "Single_Detached_Electricity_electricity",
                  "Single_Detached_Oil_electricity",
                  "Single_Attached_Natural_Gas_electricity",
                  "Single_Attached_Electricity_electricity",
                  "Single_Attached_Oil_electricity")
  
  for (file_name in file_names) {
    file_path <- paste0("Results_paper/Results_Costmood/", province, "_", file_name, "/incremental_hourly_electricity_load_", province, "_", file_name, ".rds")
    
    # Read the file if it exists
    if (file.exists(file_path)) {
      tibble_list[[file_name]] <- readRDS(file_path)
    }
  }
  
  # Skip to the next province if no files are found
  if (length(tibble_list) == 0) next
  
  # Extract and sum the 'aggregated_heating_cooling' column
  total_agg <- Reduce(`+`, lapply(tibble_list, `[[`, "aggregated_heating_cooling"))
  total_oldac <- Reduce(`+`, lapply(tibble_list, `[[`, "oldac_elec_use_hourly"))
  total_res  <- Reduce(`+`, lapply(tibble_list, `[[`, "resistance_elec_use_hourly"))
  
  # Create a new tibble with hour plus the summed columns
  final_tibble <- tibble(
    hour = tibble_list[[1]]$hour,
    total_aggregated_heating_cooling = total_agg,
    total_oldac_elec_use_hourly = total_oldac,
    total_resistance_elec_use_hourly = total_res
  )
  
  # Create a directory for the province in the Results folder
  province_dir <- paste0("Results_paper/Results_Costmood/", province)
  if (!dir.exists(province_dir)) {
    dir.create(province_dir)
  }
  
  # Define file names
  file_base <- paste0(province_dir, "/total_incremental_hourly_electricity_load_", province, "_no_NG")
  
  # Save the tibble in different formats
  saveRDS(final_tibble, file = paste0(file_base, ".rds"))
  write.csv(final_tibble, file = paste0(file_base, ".csv"), row.names = FALSE)
  write.xlsx(final_tibble, file = paste0(file_base, ".xlsx"))
}
