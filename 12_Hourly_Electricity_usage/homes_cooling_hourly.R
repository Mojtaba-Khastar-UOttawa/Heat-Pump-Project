# Hourly Electricity usage of heat pump during year for cooling purposes

cycling_cop_penalty = 0.15 # If the energy output is below minimum, unit cycles, reducing efficiency. Assume 15% (document where this assumption came from.)

energy_calc_hourly_c <- function(homes_data_hourly, hourly_temperature, hp_data, cp=cycling_cop_penalty) {
  homes_data_hourly %>%
    expand_grid(
      tibble(temperature = hourly_temperature)
    ) %>%
    mutate(cd = cdh(t_hat_c, temperature),
           energy_demand_c = cd * slope_c,
           temperature = round(temperature,0)) %>%
    left_join(hp_data) %>%
    mutate(hp_output = case_when(
      # If the heat pump cannot satisfy the entire load for the house, the heat pump shuts down, and the backup is activated.
      energy_demand_c > hc_high ~ 0,
      energy_demand_c <= hc_high ~ energy_demand_c
    ),
    hp_cop = case_when(
      energy_demand_c >= hc_low & energy_demand_c <= hc_high ~ cop_low + (cop_high - cop_low) / (hc_high - hc_low) * (hc_high - energy_demand_c),
      # Any efficiency penalty for cycling would go in the line below
      energy_demand_c < hc_low ~ cop_low * (1 - cp)
    ),
    hp_elec_use_c_hourly = energy_demand_c / hp_cop,
    ac_elec_use_hourly = energy_demand_c / ac_cop)
}

homes_hourly_c <- representative_homes 
#homes_hourly <- head(homes_hourly, 100)
# Step 1: Get unique FSAs
unique_fsas_hourly_c <- unique(homes_hourly_c$fsa)

# Step 2: Initialize list
homes_cooling_hourly <- tibble() 

# Step 3: Loop over each FSA
for (f in unique_fsas_hourly_c) {
  homes_fsa_hourly_c <- homes_hourly_c %>% filter(fsa == f) 
  weather_fsa_hourly <- weather %>% filter(fsa == f)
  
  # Step 4: Call energy_calc for each FSA
  homes_cooling_fsa_hourly <- energy_calc_hourly_c(homes_data_hourly = homes_fsa_hourly_c, hourly_temperature = weather_fsa_hourly$temperature, hp_data = hpn, cp=cycling_cop_penalty) 
  
  # Step 5: Append to list
  homes_cooling_hourly <- bind_rows(homes_cooling_hourly, homes_cooling_fsa_hourly)
}

#
# Adding hour column to homes_cooling_hourly

homes_cooling_hourly <- homes_cooling_hourly %>% 
  group_by(id) %>%
  # Filter out groups that don't have 26280 rows
  filter(n() == 26280) %>%
  # Add the hour column for the remaining groups
  mutate(hour = rep(1:8760, each=3)) %>%
  ungroup()

homes_cooling_hourly <- homes_cooling_hourly %>% left_join(select(homes_heating, id, model, saving, ghg, dollar_per_tonne, value, scale_factor,total_cluster_size_for_period, scale_factor_final), by = "id") 
homes_cooling_hourly <- homes_cooling_hourly %>%
  rename(optimal_model = model.y)   
