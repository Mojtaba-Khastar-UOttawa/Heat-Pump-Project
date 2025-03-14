

incremental_hourly_electricity_load <- optimal_plot_data %>%
  left_join(optimal_plot_data_c, by = c("hour" = "hour"))

incremental_hourly_electricity_load <- incremental_hourly_electricity_load %>%
  mutate(aggregated_heating_cooling = hp_elec_use_hourly + hp_elec_use_c_hourly) %>%
  mutate(aggregated_heating_cooling_with_positive_saving = hp_elec_use_hourly_with_positive_saving + hp_elec_use_c_hourly_with_positive_saving)

# Add resistance_elec_use_hourly if furnace_type == "Electricity", else 0
if (furnace_type == "Electricity") {
  resistance_data <- homes_heating_hourly %>%
    group_by(hour) %>% 
    # According to previous code, divide by 3 due to triplication, then convert GJ to MW
    summarise(resistance_elec_use_hourly = ((sum(resistance_elec_use_hourly * scale_factor_final, na.rm = TRUE))/3)*0.2777777778)
} else {
  # For non-electric furnace, set resistance_elec_use_hourly = 0
  resistance_data <- tibble(hour = 1:8760, resistance_elec_use_hourly = 0)
}

# Merge resistance data into incremental data
incremental_hourly_electricity_load <- incremental_hourly_electricity_load %>%
  left_join(resistance_data, by = "hour")

# Extracting province, existing furnace, and house type from the first row
first_row <- homes %>% slice(1)
prov <- first_row$PROVINCE[1]
exist_furnace <- gsub(" ", "_", first_row$exist_furnace[1])
house_type <- gsub(" ", "_", first_row$house_type[1])  

# Define the directory path within the main directory
dir_path <- paste0("Results_paper/Results_Costmood/", prov, "_", house_type, "_", exist_furnace, "_", hp_backup, "/")

print(prov)
print(house_type)
print(exist_furnace)
print(hp_backup)
print(dir_path)

# Check if the directory exists and create it if it doesn't
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

# Creating a valid file name with the path
file_name_with_path <- paste0(dir_path, "incremental_load_", prov, "_", house_type, "_", exist_furnace, "_", hp_backup, ".xlsx")

# Save the incremental_load dataframe to an Excel file in the specified directory
write_xlsx(incremental_hourly_electricity_load, file_name_with_path)
