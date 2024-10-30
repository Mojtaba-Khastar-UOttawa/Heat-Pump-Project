######## library call ----
library(tidyverse)
library(readxl)
library(plotly)
library(tidyr)
library(dplyr)
library(sf)
library(lubridate)
library(gridExtra)
library(pacman)
library(scales)
library(data.table)
library(ggplot2)
library(readr)
library(writexl)

######## 1: Call EGH data and clean it (*** Province ***) and Select house type and heating system type (*** House type ***, *** Furnace type ***) ----

# Define the list of provinces
province_codes <- c("ON", "QC", "BC", "MB", "SK", "NS", "PE", "NF", "NB", "AB")

# Define the list of house types 
house_types <- c("Single Detached", "Single Attached")

# Define the list of furnace fuel types 
furnace_fuels <- c("Natural Gas", "Electricity", "Oil")

# Define the list of backup options for each furnace type
backup_options <- list(
  "Natural Gas" = c("gas", "electricity"),
  "Oil" = c("electricity"),
  "Electricity" = c("electricity")
)

# Loop over each province
for (province in province_codes) {
  
  # Reinitialize 'homes' before loading new data
  homes <- tibble()
  
  # Construct the path to the data loading script for the current province
  data_loading_script <- paste0("Scripts_Poster_1/Load_homes/", province, "/load_EGH_homes_data_", province, ".R")
  
  # Check if the data loading script exists
  if (file.exists(data_loading_script)) {
    # Load data for the current province
    source(data_loading_script)
  } else {
    # If the script doesn't exist, skip to the next province
    next
  }
  
  # Preprocess the data to categorize specific types of homes
  homes <- homes %>%
    mutate(house_type = case_when(
      TYPEOFHOUSE %in% c("Attached Duplex", "Attached Triplex", "Row house, end unit", "Double/Semi-detached", "Row house, middle unit") ~ "Single Attached",
      TYPEOFHOUSE %in% c("Detached Duplex", "Detached Triplex", "Single Detached") ~ "Single Detached",
      TRUE ~ TYPEOFHOUSE  # Retain original type for other types of homes
    ))
  
  # Save the preprocessed 'homes' for reinitialization
  homes_for_reinitialization <- homes
  
  # Loop over each house type
  for (current_house_type in house_types) {
    
    # Loop over each furnace fuel type
    for (furnace_fuel in furnace_fuels) {
      
      # Reinitialize 'homes' from 'homes_for_reinitialization' to ensure that code starts fresh for each iteration:
      homes <- homes_for_reinitialization
      
      # Filter the data based on the current combination
      homes_filtered <- homes %>%
        filter(PROVINCE == province,
               house_type == current_house_type,
               FURNACEFUEL == furnace_fuel) %>%
        mutate(fsa = `Postal Code`,
               exist_furnace = `FURNACEFUEL`,
               house_type = `house_type`) %>%
        dplyr::select(fsa, PROVINCE, energy_heating_gj = EGHSPACEENERGY,
                      furnace_efficiency = FURSSEFF, energy_cooling_gj = ERSSPACECOOLENERGY,
                      ac_cop = AIRCOP, YEAR_of_BUILT = YEARBUILT, floor_area = FLOORAREA,
                      exist_furnace, house_type) %>%
        mutate(t_hat = 16,
               t_hat_c = 22,
               gross_output = energy_heating_gj / floor_area,
               id = row_number())
      
      # Check if there is data for the current combination
      if (nrow(homes_filtered) == 0) {
        # If no data, skip to the next iteration
        next
      }
      
      # Assign the filtered data to 'homes' for consistency in subsequent scripts
      homes <- homes_filtered
      
      # Get the backup options for the current furnace type
      current_backup_options <- backup_options[[furnace_fuel]]
      
      # Loop over backup options
      for (hp_backup in current_backup_options) {
        
        # Reinitialize variables of last loop
        source("Scripts_Poster_1/Load_homes/reinitialize_variables.R")
        
        ######## 2: Heat Pumps Data and cost calculation ----
        
        source("Scripts_Poster_1/2_load_available_hps/load_available_hps.R")
        
        ######## 3: Backup furnace cost calculation (*** furnace life left ***) ----
        
        source("Scripts_Poster_1/3_furnace_cost_calc/furnace_cost_calc.R")
        
        ######## 4: Load weather  ----
        # includes load weather, energy price, carbon charge,heating and cooling degree hours, average heating and cooling degree days
        source("Scripts_Poster_1/4_load_weather/load_weather.R")
        #source("Scripts_Poster_1/4_load_weather/load_weather_AB.R")
        ######## 5: Year of built ----
        # Assigning Year of built period 
        source("Scripts_Poster_1/5_Year_of_built/Year_of_built_paper.R")
        
        ######## 6: clustering ----
        
        source("Scripts_Poster_1/6_clustering/clustering.R")
        # source("Scripts_Poster_1/6_clustering/modified_clustering.R")
        
        ######## 7: Cooling Operation ----
        
        source("Scripts_Poster_1/7_homes_cooling/homes_cooling.R")
        
        ######## 8: Heating Operation (*** heat pump backup ***) ----
        
        source("Scripts_Poster_1/8_homes_heating/homes_heating.R")
        
        ######## 9: Calculate scale factor ----
        ## Inside 9, line 80 and 200 should be changed for NF since it appears as NL in NRCan
        source("Scripts_Poster_1/9_scale_factor/scale_factor.R")
        
        ###
        
        source("Scripts_Poster_1/9_scale_factor/scale_up_results.R")
        
        ######## 10: Plot marginal abatement cost ----
        
        source("Scripts_Poster_1/10_marginal_abatement_cost/marginal_abatement_cost.R")
        
        ###
        
        source("Scripts_Poster_1/10_marginal_abatement_cost/scaled_marginal_abatement_cost.R")
        
        ###
        
        source("Scripts_Poster_1/10_marginal_abatement_cost/positive_savings.R")
        
        ######## 11: Plot_usage ----
        
        furnace_type <- representative_homes$exist_furnace[1]
        
        if (furnace_type == "Natural Gas" && hp_backup == "gas") {
          
          source("Scripts_Poster_1/11_Plot_usage/Plot_Gas_usage.R")
          
          source("Scripts_Poster_1/11_Plot_usage/scaled_Plot_Gas_usage.R")
          
        } else if (furnace_type == "Electricity") {
          
          source("Scripts_Poster_1/11_Plot_usage/Plot_Elec_usage.R")
          
          source("Scripts_Poster_1/11_Plot_usage/scaled_Plot_Elec_usage.R")
          
        } 
        
        ######## 12: Hourly Electricity usage (*** heat pump backup ***) ----
        
        source("Scripts_Poster_1/12_Hourly_Electricity_usage/homes_heating_hourly.R")
        
        source("Scripts_Poster_1/12_Hourly_Electricity_usage/plot_elec_use_hourly_optimal_hp_model.R")
        
        source("Scripts_Poster_1/12_Hourly_Electricity_usage/plot_resistance_elec_use_hourly.R")
        
        source("Scripts_Poster_1/12_Hourly_Electricity_usage/homes_cooling_hourly.R")
        
        source("Scripts_Poster_1/12_Hourly_Electricity_usage/plot_elec_use_hourly_optimal_hp_model_cooling.R")
        
        source("Scripts_Poster_1/12_Hourly_Electricity_usage/incremental_hourly_electricity_load.R")
        
        ######## 13: Electricity Load Data and curve ----
        
        source("Scripts_Poster_1/13_combined_elec_load_plot_c&h/combined_elec_load_plot_c&h.R")
        
        # The below portion of the code plotted electricity change diagram for Ontario, because the data set only exists for Ontario's hourly electricity consumption, in kWh (2020), for residential and small general service (<50kW) consumers aggregated by forward sortation area.
        
        # source("Scripts_Poster_1/13_combined_elec_load_plot_c&h/combined_elec_load_plot_c&h_ON2020.R")
        
        ######## 14: Cost Comparison and MAC Various ----
        
        source("Scripts_Poster_1/14_cost_comparison/cost_comparison.R")
        
        # Plot marginal abatement cost (Private and Social MAC)
        source("Scripts_Poster_1/14_cost_comparison/social_and_private_marginal_abatement_cost.R")
        
        # Annual cost saving after HP adoption ($) 
        source("Scripts_Poster_1/14_cost_comparison/annual_cost_saving.R")
        
        ######## 15: Gather results ----
        
        source("Scripts_Poster_1/15_Gather_results/Gather_results.R")
        
        
        
      }
    }
  }
}




#####################
#country Level
#####################

source("Scripts_Poster_1/16_incremental_hourly_electricity_load/paper/incremental_hourly_electricity_load.R")
source("Scripts_Poster_1/16_incremental_hourly_electricity_load/paper/incremental_hourly_electricity_load_with_positive_saving.R")

source("Scripts_Poster_1/16_incremental_hourly_electricity_load/paper/incremental_hourly_electricity_load_highest_load_plot.R")
source("Scripts_Poster_1/16_incremental_hourly_electricity_load/paper/incremental_hourly_electricity_load_highest_load_plot_with_positive_saving.R")

###

source("Scripts_Poster_1/18_summary_results_all/paper/summary_results_all_with_NG_paper.R")
source("Scripts_Poster_1/18_summary_results_all/paper/summary_results_all_no_NG_paper.R")

source("Scripts_Poster_1/18_summary_results_all/paper/aggregated_summary_results_all_with_NG_paper.R")
source("Scripts_Poster_1/18_summary_results_all/paper/aggregated_summary_results_all_no_NG_paper.R")

source("Scripts_Poster_1/18_summary_results_all/paper/country_aggregated_summary_results_all_with_NG_paper.R")
source("Scripts_Poster_1/18_summary_results_all/paper/country_aggregated_summary_results_all_no_NG_paper.R")




