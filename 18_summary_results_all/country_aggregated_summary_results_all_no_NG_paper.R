library(dplyr)
library(readr)

# Read the results file
results <- read_csv("Results_paper/Results_Costmood/summary_results_all_no_NG.csv")  # Replace with the actual path to your CSV file

# Define the provinces and scenarios
provinces <- c("ON", "BC", "MB", "AB", "NB", "NF", "NS", "PE", "QC", "SK")
scenarios <- c("base", 
               "Rebate5K", 
               "Rebate5K+lowinterest",
               "Rebate10K", 
               "Rebate10K+lowinterest",
               "Rebate5K+ctax95", 
               "Rebate10K+ctax95", 
               "Rebate5K+ctax170", 
               "Rebate10K+ctax170")

# Initialize an empty list to store aggregated results
aggregated_results_list <- list()

# Loop through each scenario
for (scen in scenarios) {
  filtered_data <- results %>%
    filter(scenario == scen) %>%
    summarize(
      sum_scale_factor_positive_sum = sum(coalesce(sum_scale_factor_positive_sum, 0), na.rm = TRUE),
      total_scale_factor_sum = sum(coalesce(total_scale_factor_sum, 0), na.rm = TRUE),
      mitigated_ghg_sum = sum(coalesce(mitigated_ghg_sum, 0), na.rm = TRUE),
      scaled_savings_sum = sum(coalesce(scaled_savings_sum, 0), na.rm = TRUE),
      scaled_furnace_oil_use_sum = sum(coalesce(scaled_furnace_oil_use_sum, 0), na.rm = TRUE),
      scaled_resistance_elec_use_sum = sum(coalesce(scaled_resistance_elec_use_sum, 0), na.rm = TRUE),
      scaled_furnace_gas_use_sum = sum(coalesce(scaled_furnace_gas_use_sum, 0), na.rm = TRUE),
      scaled_ac_elec_use_sum = sum(coalesce(scaled_ac_elec_use_sum, 0), na.rm = TRUE),
      scaled_hp_gas_backup_sum = sum(coalesce(scaled_hp_gas_backup_sum, 0), na.rm = TRUE),
      scaled_hp_elec_use_sum = sum(coalesce(scaled_hp_elec_use_sum, 0), na.rm = TRUE),
      
      scaled_oldfurnace_gas_use_sum = sum(coalesce(scaled_oldfurnace_gas_use_sum, 0), na.rm = TRUE),
      scaled_oldfurnace_oil_use_sum = sum(coalesce(scaled_oldfurnace_oil_use_sum, 0), na.rm = TRUE),
      scaled_oldac_elec_use_sum     = sum(coalesce(scaled_oldac_elec_use_sum, 0), na.rm = TRUE),
      mitigated_ghg_wold_sum       = sum(coalesce(mitigated_ghg_wold_sum, 0), na.rm = TRUE),
      
      sum_2_5_ton = sum(coalesce(sum_2_5_ton, 0), na.rm = TRUE),
      sum_3_ton   = sum(coalesce(sum_3_ton, 0), na.rm = TRUE),
      sum_3_5_ton = sum(coalesce(sum_3_5_ton, 0), na.rm = TRUE)
    ) %>%
    mutate(
      house_percentage_positive_savings = ifelse(total_scale_factor_sum > 0, (sum_scale_factor_positive_sum / total_scale_factor_sum) * 100, 0),
      
      aggregated_percent_2_5_ton = ifelse(
        total_scale_factor_sum > 0,
        (sum_2_5_ton / total_scale_factor_sum) * 100,
        0
      ),
      aggregated_percent_3_ton = ifelse(
        total_scale_factor_sum > 0,
        (sum_3_ton / total_scale_factor_sum) * 100,
        0
      ),
      aggregated_percent_3_5_ton = ifelse(
        total_scale_factor_sum > 0,
        (sum_3_5_ton / total_scale_factor_sum) * 100,
        0
      )
    )
  filtered_data$scenario <- scen
  aggregated_results_list[[length(aggregated_results_list) + 1]] <- filtered_data
}

# Combine all aggregated results into one tibble
aggregated_results <- bind_rows(aggregated_results_list)

# Print the final aggregated tibble
print(aggregated_results)

# Optionally, save the aggregated results as a CSV file
write.csv(aggregated_results, "Results_paper/Results_Costmood/country_aggregated_summary_results_all_no_NG.csv", row.names = FALSE)
