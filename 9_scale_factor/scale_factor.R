#### line 80 and 200 NF vs other provinces 
# Calculate scale factor based on heat type

# First Part: Data Gathering

# Function to process the data
process_heat_type_data <- function(filepath, new_colname) {
  data <- read_excel(filepath, skip = 10, range = cell_cols(c("B", NA)))
  names(data)[1] <- "heat_type"
  
  cleaned_heat_type_data <- data %>%
    mutate(row_num = row_number()) %>% 
    filter(!(row_num < 21 | row_num == 31)) %>%
    select(heat_type, `2020`) %>%
    rename(!!new_colname := `2020`)
  
  return(cleaned_heat_type_data)
}

# Provinces and housing types
provinces <- c("NL", "PE", "NS", "NB", "QC", "MB", "SK", "AB", "BC", "TR", "ON")
housing_types <- c("Detached", "Attached", "Apartment", "Mobile")
file_nums <- 22:25  # File numbers based on the provided paths

all_heat_type_data <- list()

# Loop through each province
for (prov in provinces) {
  province_data <- list()
  
  # Loop through each housing type
  for (i in seq_along(housing_types)) {
    file_path <- paste0("data/nrcan_pop_data/", prov, "/heat_type/res_", tolower(prov), "_e_", file_nums[i], ".xls")
    colname <- paste0(prov, "_", housing_types[i], "_2020")
    province_data[[housing_types[i]]] <- process_heat_type_data(file_path, colname)
  }
  
  # Combine the datasets for each province and add to the all_data list
  combined_data <- reduce(province_data, left_join, by = "heat_type")
  all_heat_type_data[[prov]] <- combined_data
}

# Combine the datasets for all provinces
final_heat_type_data <- reduce(all_heat_type_data, left_join, by = "heat_type")
final_heat_type_data

# Second Part: Compute Scale Factor

compute_heat_type_scale_factor <- function(province, house_type, type, data) {
  colname <- paste0(province, "_", house_type, "_2020")
  selected_data <- data %>% select(heat_type, !!colname)
  
  if (type == "gas") {
    categories <- c("Natural Gas – Normal Efficiency", 
                    "Natural Gas – Medium Efficiency", 
                    "Natural Gas – High Efficiency", 
                    "Natural Gas/Electric")
  } else if (type == "oil") {
    categories <- c("Heating Oil – Normal Efficiency", 
                    "Heating Oil – Medium Efficiency", 
                    "Heating Oil – High Efficiency", 
                    "Wood/Heating Oil", 
                    "Heating Oil/Electric")
  } else if (type == "elec") {
    categories <- c("Electric", "Wood/Electric")
  } else {
    stop("Invalid type provided!")
  }
  
  scale_factor_heat_type <- sum(selected_data[[colname]][selected_data$heat_type %in% categories], na.rm = TRUE)
  
  # Convert the scale factor from percentage to range [0, 1]
  scale_factor_heat_type <- scale_factor_heat_type / 100
  
  return(scale_factor_heat_type)
}


# Extract data from the first row of representative_homes
chosen_province <- representative_homes$PROVINCE[2] ## for all provinces exclude NF
#chosen_province <- representative_homes$prov[2] ## for NF province 
chosen_house_type_raw <- representative_homes$house_type[1]
chosen_heating_type_raw <- representative_homes$exist_furnace[1]

# Convert house type to the expected format
chosen_house_type <- switch(chosen_house_type_raw,
                            "Single Attached" = "Attached",
                            "Single Detached" = "Detached",
                            "Apartment" = "Apartment",
                            "Mobile" = "Mobile")

# Convert heating type to the expected format
chosen_heating_type <- switch(chosen_heating_type_raw,
                              "Natural Gas" = "gas",
                              "Electricity" = "elec",
                              "Oil" = "oil")

# Use the extracted and converted values in the compute_heat_type_scale_factor function
scale_factor_heat_type <- compute_heat_type_scale_factor(chosen_province, chosen_house_type, chosen_heating_type, final_heat_type_data)

# Output the scale factor
scale_factor_heat_type

###

# Calculate scale factor based on vintage

# First Part: Data Gathering

# Function to process vintage file
process_vintage_file <- function(filepath, prov) {
  data <- read_excel(filepath, skip = 10, range = cell_cols(c("B", NA)))
  names(data)[1] <- "periods"
  
  cleaned_vintage_data <- data %>%
    mutate(row_num = row_number()) %>% 
    filter(!(row_num %in% c(1, 3, 14:27, 29) | row_num >= 40)) %>%
    select(periods, `2020`)
  
  # Rename the second column based on file and province
  type_suffix <- ifelse(grepl("_e_16", filepath), "detached", "apartment")
  names(cleaned_vintage_data)[2] <- paste0("2020_", prov, "_", type_suffix)
  
  return(cleaned_vintage_data)
}

# Rename periods function
rename_periods <- function(data) {
  data$periods <- sub("^Total .*", "Total Housing Stock (thousands)", data$periods)
  return(data)
}

# Provinces
provinces <- c("NL", "PE", "NS", "NB", "QC", "MB", "SK", "AB", "BC", "TR", "ON")

# Initialize final data
final_vintage_data <- data.frame(periods = character())

# Iterate through each province
for(prov in provinces) {
  
  # File paths for current province
  filepath_16 <- paste0("data/nrcan_pop_data/", prov, "/vintage/res_", tolower(prov), "_e_16.xls")
  filepath_17 <- paste0("data/nrcan_pop_data/", prov, "/vintage/res_", tolower(prov), "_e_17.xls")
  
  # Process both files for current province
  pp_16 <- process_vintage_file(filepath_16, prov)
  pp_17 <- process_vintage_file(filepath_17, prov)
  
  # Separate data for each type of house
  pp_detached <- pp_16[1:11, ]
  pp_attached <- pp_16[12:22, ]
  pp_apartment <- pp_17[1:11, ]
  pp_mobile <- pp_17[12:22, ]
  
  pp_detached <- rename_periods(pp_detached)
  pp_attached <- rename_periods(pp_attached)
  pp_apartment <- rename_periods(pp_apartment)
  pp_mobile <- rename_periods(pp_mobile)
  
  # Join data for current province
  prov_data <- left_join(pp_detached, pp_attached, by = "periods")
  names(prov_data)[3] <- paste0("2020_", prov, "_attached")
  prov_data <- left_join(prov_data, pp_apartment, by = "periods")
  prov_data <- left_join(prov_data, pp_mobile, by = "periods")
  names(prov_data)[5] <- paste0("2020_", prov, "_mobile")
  
  # Rename to remove the .x suffix if it exists
  colnames(prov_data) <- gsub("\\.x$", "", colnames(prov_data))
  
  # Join with final data
  if(nrow(final_vintage_data) == 0) {
    final_vintage_data <- prov_data
  } else {
    final_vintage_data <- left_join(final_vintage_data, prov_data, by = "periods")
  }
  
  # Again, rename to remove the .x suffix if it exists
  colnames(final_vintage_data) <- gsub("\\.x$", "", colnames(final_vintage_data))
}

final_vintage_data

# Second Part: Compute Scale Factor for all periods

get_vintage_values_all_periods <- function(province, house_type, data, scale_factor) {
  # Construct the column name based on the province and house type
  colname <- paste0("2020_", province, "_", tolower(house_type))
  
  # Create a tibble with periods and their corresponding vintage values
  vintage_values <- tibble(
    periods = data$periods,
    value = as.numeric(data[[colname]]) * 1000 * scale_factor
  )
  
  return(vintage_values)
}

# Extract data from the first row of representative_homes
chosen_province <- representative_homes$PROVINCE[2] ## for all provinces exclude NF
#chosen_province <- representative_homes$prov[2] ## for NF province 
chosen_house_type_raw <- representative_homes$house_type[1]

# Convert house type to the expected format
chosen_house_type <- switch(chosen_house_type_raw,
                            "Single Attached" = "Attached",
                            "Single Detached" = "Detached",
                            "Apartment" = "Apartment",
                            "Mobile" = "Mobile")

# Use the extracted and converted values in the get_vintage_values_all_periods function
vintage_values_all_periods <- get_vintage_values_all_periods(chosen_province, chosen_house_type, final_vintage_data, scale_factor_heat_type)
vintage_values_all_periods <- rename(vintage_values_all_periods, period = periods)
vintage_values_all_periods

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
file_name_with_path <- paste0(dir_path, "vintage_values_all_periods_", prov, "_", house_type, "_", exist_furnace, "_", hp_backup,".xlsx")

# Save the percentage_share dataframe to an Excel file in the specified directory
write_xlsx(vintage_values_all_periods, file_name_with_path)
