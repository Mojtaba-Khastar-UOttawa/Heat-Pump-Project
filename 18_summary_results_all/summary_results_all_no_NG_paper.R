library(dplyr)
library(readr)
library(purrr)

# Define the provinces, folder patterns, and scenarios ("ON", "BC", "MB", "AB", "NB", "NF", "NS", "PE", "QC", "SK")
provinces <- c("ON", "BC", "MB", "AB", "NB", "NF", "NS", "PE", "QC", "SK")
folder_patterns <- c(
  "Single_Attached_Electricity_electricity", 
  "Single_Attached_Natural_Gas_electricity", 
  "Single_Attached_Oil_electricity", 
  "Single_Detached_Electricity_electricity", 
  "Single_Detached_Natural_Gas_electricity", 
  "Single_Detached_Oil_electricity"
)
scenarios <- c("base", 
               "Rebate5K", 
               "Rebate5K+lowinterest",
               "Rebate10K", 
               "Rebate10K+lowinterest",
               "Rebate5K+ctax95", 
               "Rebate10K+ctax95", 
               "Rebate5K+ctax170", 
               "Rebate10K+ctax170")

# Function to calculate positive savings percentage, scale factor sum, mitigated GHG sum, and scaled savings sum
calculate_metrics <- function(data) {
  scenario_metrics <- tibble(scenario = character(), 
                             percentage_positive_savings = numeric(),
                             sum_scale_factor_positive_sum = numeric(),
                             total_scale_factor_sum = numeric(),
                             mitigated_ghg_sum = numeric(),
                             scaled_savings_sum = numeric(),
                             scaled_furnace_oil_use_sum = numeric(),
                             scaled_resistance_elec_use_sum = numeric(),
                             scaled_furnace_gas_use_sum = numeric(),
                             scaled_ac_elec_use_sum = numeric(),
                             scaled_hp_gas_backup_sum = numeric(),
                             scaled_hp_elec_use_sum = numeric(),
                             scaled_oldfurnace_gas_use_sum  = numeric(),
                             scaled_oldfurnace_oil_use_sum  = numeric(),
                             scaled_oldac_elec_use_sum      = numeric(),
                             
                             sum_2_5_ton = numeric(),
                             sum_3_ton   = numeric(),
                             sum_3_5_ton = numeric(),
                             
                             percent_2_5_ton = numeric(),
                             percent_3_ton   = numeric(),
                             percent_3_5_ton = numeric(),
                             mitigated_ghg_wold_sum = numeric())
  
  savings_column_exists <- "saving" %in% names(data)
  furnace_oil_use_column_exists <- "furnace_oil_use" %in% names(data)
  resistance_elec_use_column_exists <- "resistance_elec_use" %in% names(data)
  furnace_gas_use_column_exists <- "furnace_gas_use" %in% names(data)
  ac_elec_use_column_exists <- "ac_elec_use" %in% names(data)
  hp_gas_backup_column_exists <- "hp_gas_backup" %in% names(data)
  hp_elec_use_column_exists <- "hp_elec_use" %in% names(data)
  oldfurnace_gas_use_exists        <- "oldfurnace_gas_use"  %in% names(data)
  oldac_elec_use_exists            <- "oldac_elec_use"      %in% names(data)
  oldfurnace_oil_use_exists        <- "oldfurnace_oil_use"  %in% names(data)
  ghg_wold_column_exists <- "ghg_wold" %in% names(data)
  
  
  for(scen in scenarios) {
    sum_scale_factor_positive <- sum(data %>%
                                       filter(saving > 0, scenario == scen, is.finite(scale_factor)) %>%
                                       pull(scale_factor), na.rm = TRUE)
    
    total_scale_factor <- sum(data %>%
                                filter(scenario == scen) %>%
                                pull(scale_factor))
    
    mitigated_ghg <- sum(data %>%
                           filter(scenario == scen) %>%
                           mutate(ghg_scaled = ifelse(is.finite(ghg * scale_factor), ghg * scale_factor, 0)) %>%
                           pull(ghg_scaled), na.rm = TRUE)
    
    mitigated_ghg_wold <- if (ghg_wold_column_exists) {
      sum(
        data %>%
          filter(scenario == scen) %>%
          mutate(ghg_wold_scaled = ifelse(is.finite(ghg_wold * scale_factor), ghg_wold * scale_factor, 0)) %>%
          pull(ghg_wold_scaled),
        na.rm = TRUE
      )
    } else {
      NA
    }
    
    scaled_savings <- if(savings_column_exists) {
      sum(data %>%
            filter(scenario == scen) %>%
            mutate(savings_scaled = ifelse(is.finite(saving * scale_factor), saving * scale_factor, 0)) %>%
            pull(savings_scaled), na.rm = TRUE)
      
    } else {
      NA
    }
    
    scaled_furnace_oil_use <- if(furnace_oil_use_column_exists) {
      sum(data %>%
            filter(scenario == scen) %>%
            mutate(furnace_oil_use_scaled = ifelse(is.finite(furnace_oil_use * scale_factor), furnace_oil_use * scale_factor, 0)) %>%
            pull(furnace_oil_use_scaled), na.rm = TRUE)
      
    } else {
      NA
    }
    
    scaled_resistance_elec_use <- if(resistance_elec_use_column_exists) {
      sum(data %>%
            filter(scenario == scen) %>%
            mutate(resistance_elec_use_scaled = ifelse(is.finite(resistance_elec_use * scale_factor), resistance_elec_use * scale_factor, 0)) %>%
            pull(resistance_elec_use_scaled), na.rm = TRUE)
      
    } else {
      NA
    }
    
    scaled_furnace_gas_use <- if(furnace_gas_use_column_exists) {
      sum(data %>%
            filter(scenario == scen) %>%
            mutate(furnace_gas_use_scaled = ifelse(is.finite(furnace_gas_use * scale_factor), furnace_gas_use * scale_factor, 0)) %>%
            pull(furnace_gas_use_scaled), na.rm = TRUE)
      
    } else {
      NA
    }
    
    scaled_ac_elec_use <- if(ac_elec_use_column_exists) {
      sum(data %>%
            filter(scenario == scen) %>%
            mutate(ac_elec_use_scaled = ifelse(is.finite(ac_elec_use * scale_factor), ac_elec_use * scale_factor, 0)) %>%
            pull(ac_elec_use_scaled), na.rm = TRUE)
      
    } else {
      NA
    }
    
    scaled_hp_gas_backup <- if(hp_gas_backup_column_exists) {
      sum(data %>%
            filter(scenario == scen) %>%
            mutate(hp_gas_backup_scaled = ifelse(is.finite(hp_gas_backup * scale_factor), hp_gas_backup * scale_factor, 0)) %>%
            pull(hp_gas_backup_scaled), na.rm = TRUE)
      
    } else {
      NA
    }
    
    scaled_hp_elec_use <- if(hp_elec_use_column_exists) {
      sum(data %>%
            filter(scenario == scen) %>%
            mutate(hp_elec_use_scaled = ifelse(is.finite(hp_elec_use * scale_factor), hp_elec_use * scale_factor, 0)) %>%
            pull(hp_elec_use_scaled), na.rm = TRUE)
      
    } else {
      NA
    }
    
    scaled_oldfurnace_gas_use <- if (oldfurnace_gas_use_exists) {
      sum(
        data %>%
          filter(scenario == scen) %>%
          mutate(oldfurnace_gas_use_scaled = ifelse(is.finite(oldfurnace_gas_use * scale_factor),
                                                    oldfurnace_gas_use * scale_factor, 0)) %>%
          pull(oldfurnace_gas_use_scaled),
        na.rm = TRUE
      )
    } else {
      NA
    }
    
    scaled_oldac_elec_use <- if (oldac_elec_use_exists) {
      sum(
        data %>%
          filter(scenario == scen) %>%
          mutate(oldac_elec_use_scaled = ifelse(is.finite(oldac_elec_use * scale_factor),
                                                oldac_elec_use * scale_factor, 0)) %>%
          pull(oldac_elec_use_scaled),
        na.rm = TRUE
      )
    } else {
      NA
    }
    
    scaled_oldfurnace_oil_use <- if (oldfurnace_oil_use_exists) {
      sum(
        data %>%
          filter(scenario == scen) %>%
          mutate(oldfurnace_oil_use_scaled = ifelse(is.finite(oldfurnace_oil_use * scale_factor),
                                                    oldfurnace_oil_use * scale_factor, 0)) %>%
          pull(oldfurnace_oil_use_scaled),
        na.rm = TRUE
      )
    } else {
      NA
    }
    
    # Percentage of each HP model (2.5 ton, 3 ton, 3.5 ton)
    # We'll calculate sum of scale_factor for each model, scenario = scen
    # Then compute ratio over total_scale_factor * 100
    # If total_scale_factor == 0, set 0 to avoid division by zero.
    sum_2_5_ton <- sum(
      data %>%
        filter(scenario == scen, model == "NRCan 2.5 ton") %>%
        pull(scale_factor),
      na.rm = TRUE
    )
    sum_3_ton <- sum(
      data %>%
        filter(scenario == scen, model == "NRCan 3 ton") %>%
        pull(scale_factor),
      na.rm = TRUE
    )
    sum_3_5_ton <- sum(
      data %>%
        filter(scenario == scen, model == "NRCan 3.5 ton") %>%
        pull(scale_factor),
      na.rm = TRUE
    )
    
    if (total_scale_factor > 0) {
      percent_2_5_ton <- (sum_2_5_ton / total_scale_factor) * 100
      percent_3_ton   <- (sum_3_ton   / total_scale_factor) * 100
      percent_3_5_ton <- (sum_3_5_ton / total_scale_factor) * 100
    } else {
      percent_2_5_ton <- 0
      percent_3_ton   <- 0
      percent_3_5_ton <- 0
    }
    
    percentage_positive <- if (total_scale_factor > 0) {
      (sum_scale_factor_positive / total_scale_factor) * 100
    } else {
      0
    }
    
    scenario_metrics <- scenario_metrics %>% 
      add_row(scenario = scen, 
              percentage_positive_savings = percentage_positive, 
              sum_scale_factor_positive_sum = sum_scale_factor_positive,
              total_scale_factor_sum = total_scale_factor,
              mitigated_ghg_sum = mitigated_ghg,
              scaled_savings_sum = scaled_savings,
              scaled_furnace_oil_use_sum = scaled_furnace_oil_use,
              scaled_resistance_elec_use_sum = scaled_resistance_elec_use,
              scaled_furnace_gas_use_sum = scaled_furnace_gas_use,
              scaled_ac_elec_use_sum = scaled_ac_elec_use,
              scaled_hp_gas_backup_sum = scaled_hp_gas_backup,
              scaled_hp_elec_use_sum = scaled_hp_elec_use,
              scaled_oldfurnace_gas_use_sum = scaled_oldfurnace_gas_use,
              scaled_oldfurnace_oil_use_sum = scaled_oldfurnace_oil_use,
              scaled_oldac_elec_use_sum     = scaled_oldac_elec_use,
              
              sum_2_5_ton = sum_2_5_ton,
              sum_3_ton   = sum_3_ton,
              sum_3_5_ton = sum_3_5_ton,
              
              percent_2_5_ton = percent_2_5_ton,
              percent_3_ton   = percent_3_ton,
              percent_3_5_ton = percent_3_5_ton,
              mitigated_ghg_wold_sum = mitigated_ghg_wold)
  }
  return(scenario_metrics)
}

# Initialize an empty list to store the results
results_list <- list()

# Loop through each province and folder pattern
for (prov in provinces) {
  for (pattern in folder_patterns) {
    folder_name <- paste0(prov, "_", pattern)
    file_path <- paste0("Results_paper/Results_Costmood/", folder_name, "/mac_various_", folder_name, ".rds")
    
    if (file.exists(file_path)) {
      data <- readRDS(file_path)
      result <- calculate_metrics(data)
      result$province <- prov
      result$type <- pattern
      results_list[[length(results_list) + 1]] <- result
    }
  }
}

# Combine all results into one tibble
results <- bind_rows(results_list)

# Print the final tibble
print(results)

# Save the results as a CSV file
write.csv(results, "Results_paper/Results_Costmood/summary_results_all_no_NG.csv", row.names = FALSE)
