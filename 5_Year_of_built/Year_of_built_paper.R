# Year of built period assigning for homes

homes <- homes %>%
  mutate(period = case_when(
    YEAR_of_BUILT < 1946 ~ "Before 1946",
    YEAR_of_BUILT >= 1946 & YEAR_of_BUILT <= 1960 ~ "1946–1960",
    YEAR_of_BUILT >= 1961 & YEAR_of_BUILT <= 1977 ~ "1961–1977",
    YEAR_of_BUILT >= 1978 & YEAR_of_BUILT <= 1983 ~ "1978–1983",
    YEAR_of_BUILT >= 1984 & YEAR_of_BUILT <= 1995 ~ "1984–1995",
    YEAR_of_BUILT >= 1996 & YEAR_of_BUILT <= 2000 ~ "1996–2000",
    YEAR_of_BUILT >= 2001 & YEAR_of_BUILT <= 2005 ~ "2001–2005",
    YEAR_of_BUILT >= 2006 & YEAR_of_BUILT <= 2010 ~ "2006–2010",
    YEAR_of_BUILT >= 2011 & YEAR_of_BUILT <= 2015 ~ "2011–2015",
    YEAR_of_BUILT >= 2016 & YEAR_of_BUILT <= 2020 ~ "2016–2020",
    TRUE ~ "Unknown" # This is a fallback in case any years don't match the above ranges.
  ))
# Define the desired period categories
desired_periods <- c("Before 1946", "1946–1960", "1961–1977", "1978–1983", "1984–1995", 
                     "1996–2000", "2001–2005", "2006–2010", "2011–2015", "2016–2020")

# Filter the homes dataset to include only rows with the desired periods
homes <- homes %>%
  filter(period %in% desired_periods)

homes$period <- factor(homes$period, levels = c("Before 1946", "1946–1960", "1961–1977", "1978–1983", "1984–1995", 
                                                "1996–2000", "2001–2005", "2006–2010", "2011–2015", "2016–2020"))

percentage_share <- homes %>%
  count(period) %>%
  mutate(percentage = n / sum(n) * 100)

# Print the percentage share of each period
print(percentage_share)

# Extracting province, existing furnace, and house type from the first row
first_row <- homes %>% slice(1)
prov <- first_row$PROVINCE[1]
exist_furnace <- gsub(" ", "_", first_row$exist_furnace[1])
house_type <- gsub(" ", "_", first_row$house_type[1])        

# Assuming hp_backup is already defined in your environment hp_backup <- "gas" or "electricity" based on your console output

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
file_name_with_path <- paste0(dir_path, "YEAR_of_BUILT_", prov, "_", house_type, "_", exist_furnace, "_", hp_backup, ".xlsx")

# Save the percentage_share dataframe to an Excel file in the specified directory
write_xlsx(percentage_share, file_name_with_path)

