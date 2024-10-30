

# Extracting variables from the first row
first_row <- homes %>% slice(1)
prov <- first_row$PROVINCE[1]
exist_furnace <- gsub(" ", "_", first_row$exist_furnace[1])
house_type <- gsub(" ", "_", first_row$house_type[1])

# Assuming hp_backup is already defined in your environment
# hp_backup <- "gas" or "electricity" based on your console output

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

# Dynamically create file names and save the tibbles
saveRDS(representative_homes, file.path(dir_path, paste0("representative_homes_", prov, "_", house_type, "_", exist_furnace, "_", hp_backup, ".rds")))
saveRDS(homes_heating, file.path(dir_path, paste0("homes_heating_", prov, "_", house_type, "_", exist_furnace, "_", hp_backup, ".rds")))
saveRDS(aggregated_data, file.path(dir_path, paste0("aggregated_data_", prov, "_", house_type, "_", exist_furnace, "_", hp_backup, ".rds")))
saveRDS(mac_various, file.path(dir_path, paste0("mac_various_", prov, "_", house_type, "_", exist_furnace, "_", hp_backup, ".rds")))
saveRDS(incremental_hourly_electricity_load, file.path(dir_path, paste0("incremental_hourly_electricity_load_", prov, "_", house_type, "_", exist_furnace, "_", hp_backup, ".rds")))


