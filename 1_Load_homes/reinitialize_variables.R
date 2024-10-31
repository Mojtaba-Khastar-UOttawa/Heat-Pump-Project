# Assign 'homes_filtered' to 'homes' for consistent use in subsequent scripts
homes <- homes_filtered

# Reinitialize 'available_hps' and related variables of section 2
available_hps <- tibble()
hpl <- tibble()
hpn <- tibble()
hp_equip_cost_data <- tibble()
hp_equip_cost <- tibble()

# Reinitialize variables from "load_weather" section 4
weather <- NULL
hourly_temperature <- NULL
fsa_prov_mapping <- NULL
energy_price <- NULL
weather_sum_hdh <- NULL
weather_sum_cdh <- NULL
daily_avg_temp <- NULL
daily_hdd <- NULL
daily_cdd <- NULL
fsa_hdd <- NULL
fsa_cdd <- NULL

# Reinitialize variables from "Year of Built" section 5
desired_periods <- NULL
percentage_share <- NULL

# Reinitialize variables from "clustering" section 6
cluster_homes <- NULL
cluster_data <- NULL
results <- NULL
unique_periods <- NULL
current_cluster_id <- NULL
period_data <- NULL
clustering_output <- NULL
clustered_period_data <- NULL
expected_periods <- NULL
missing_periods <- NULL
representative_homes <- tibble()

# Reinitialize variables from "homes_cooling" section 7
homes_cooling <- tibble()

# Reinitialize variables for Section 8 Variables
homes_heating <- tibble()
id_fsa_prov_mapping <- NULL

# Reinitialize variables for Section 9 Variables
all_heat_type_data <- NULL
scale_factor_heat_type <- NULL
final_heat_type_data <- NULL
final_vintage_data <- NULL
vintage_values_all_periods <- NULL
scale_factor_final <- NULL

# Reinitialize variables for Section 10 Variables
plot_marginal_abatement_cost <- NULL
plot_scaled_marginal_abatement_cost <- NULL

# Reinitialize variables for Section 11 Variables
summarized_gas_usage_data <- NULL
summarized_scaled_gas_usage_data <- NULL
summarized_elec_usage_data <- NULL
summarized_scaled_elec_usage_data <- NULL

# Reinitialize variables for Section 12 Variables
homes_hourly <- tibble()
homes_heating_hourly <- tibble()
homes_cooling_hourly <- tibble()
optimal_plot_data <- NULL
optimal_plot_data_c <- NULL
incremental_hourly_electricity_load <- NULL

# Reinitialize variables for Section 13 Variables
elec_load <- NULL
aggregated_data <- NULL

# Reinitialize variables for Section 14 Variables
mac_various <- tibble()
plot_marginal_abatement_cost_various_scenarios <- NULL
Annual_Saving_Box_plot <- NULL
