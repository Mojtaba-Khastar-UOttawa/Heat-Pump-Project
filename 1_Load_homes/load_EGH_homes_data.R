# load_EGH_homes_data.R

load_EGH_homes_data <- function(province_code) {
  # Map province codes to province names (as used in your file paths)
  province_names <- list(
    "AB" = "alberta",
    "BC" = "british columbia",
    "ON" = "ontario",
    "MB" = "manitoba",
    "NB" = "new brunswick",
    "NF" = "newfoundland and labrador",
    "NS" = "nova scotia",
    "PE" = "prince edward island",
    "QC" = "quebec",
    "SK" = "saskatchewan"
  )
  
  # Get the province name based on the code
  province_name <- province_names[[province_code]]
  
  # Define the base path
  base_path <- paste0("Scripts_Poster_1/Load_homes/", province_code, "/EGH_data/")
  
  # List of file suffixes
  file_suffixes <- c(
    "_d without e_all_tsv_0-100_rating_clouddb_new.csv",
    "_d with e_all_tsv_0-100_rating_clouddb_new.csv",
    "_d with e_all_tsv_gj_rating_clouddb_new.csv",
    "_d without e_all_tsv_gj_rating_clouddb_new.csv"
  )
  
  # Initialize an empty list to store dataframes
  homes_list <- list()
  
  # Load and store each file
  for (i in seq_along(file_suffixes)) {
    file_name <- paste0(province_name, file_suffixes[i])
    file_path <- file.path(base_path, file_name)
    if (file.exists(file_path)) {
      # Read the CSV file without specifying col_names or col_types
      homes_list[[i]] <- read_csv(file_path, show_col_types = FALSE)
    } else {
      warning(paste("File not found:", file_path))
    }
  }
  
  # Combine all files into one dataframe
  homes <- bind_rows(homes_list)
  
  # Check if homes dataframe is empty
  if (nrow(homes) == 0) {
    warning(paste("No data loaded for province:", province_code))
    return(NULL)
  }
  
  # Proceed with processing as in your per-province scripts
  
  # Step 1: Remove the current incorrect header
  # Temporarily assign generic column names to facilitate manipulation
  colnames(homes) <- paste0("V", 1:ncol(homes))
  
  # Step 2: Promote the first row as the new header
  # Extract the first row to use as column names
  new_header <- homes[1, ]
  
  # Apply these as the column names
  colnames(homes) <- as.character(unlist(new_header))
  
  # Step 3: Remove the first row from the data frame
  homes <- homes[-1, ]
  
  # Loop through each column to fix any missing or empty names
  for (i in 1:ncol(homes)) {
    # Check if the column name is NA or an empty string
    if (is.na(colnames(homes)[i]) || colnames(homes)[i] == "") {
      # Rename the column with a placeholder name
      colnames(homes)[i] <- paste0("Unnamed_Column_", i)
    }
  }
  
  # Print the updated column names to confirm changes
  print(colnames(homes))
  
  # Selecting only the specified columns
  homes <- homes %>%
    select(
      EGHSPACEENERGY,
      ERSSPACECOOLENERGY,
      FURSSEFF,
      HOUSEID,
      FLOORAREA,
      AIRCOP,
      YEARBUILT,
      PROVINCE,
      TYPEOFHOUSE,
      FURNACEFUEL,
      `Postal Code`
    )
  
  # Mutate the data types and perform calculations
  homes <- homes %>%
    mutate(
      EGHSPACEENERGY = as.numeric(gsub(",", "", EGHSPACEENERGY)) / 1000,
      ERSSPACECOOLENERGY = as.numeric(gsub(",", "", ERSSPACECOOLENERGY)) / 1000,
      FURSSEFF = as.numeric(gsub(",", "", FURSSEFF)) / 100,
      HOUSEID = as.integer(gsub(",", "", HOUSEID)),
      FLOORAREA = as.numeric(gsub(",", "", FLOORAREA)),
      AIRCOP = as.numeric(gsub(",", "", AIRCOP)),
      YEARBUILT = as.integer(gsub(",", "", YEARBUILT))
    )
  
  # Return the processed dataframe
  return(homes)
}