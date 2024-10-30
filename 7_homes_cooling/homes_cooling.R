
# Electricity usage of heat pump during year for cooling purposes

cycling_cop_penalty = 0.15 # If the energy output is below minimum, unit cycles, reducing efficiency. Assume 15% (document where this assumption came from.)

energy_calc_c <- function(homes_data, hourly_temperature, hp_data, cp=cycling_cop_penalty) {
  homes_data %>%
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
    hp_elec_use_c = energy_demand_c / hp_cop,
    ac_elec_use = energy_demand_c / ac_cop) %>%
    group_by(id,model) %>%
    summarise(hp_elec_use_c = sum(hp_elec_use_c, na.rm=T),
              ac_elec_use = sum(ac_elec_use, na.rm=T)) 
}

homes_c <- representative_homes 
#homes_hourly <- head(homes_hourly, 100)
# Step 1: Get unique FSAs
unique_fsas_c <- unique(homes_c$fsa)

# Step 2: Initialize list
homes_cooling <- tibble() 

# Step 3: Loop over each FSA
for (f in unique_fsas_c) {
  homes_fsa_c <- homes_c %>% filter(fsa == f) 
  weather_fsa <- weather %>% filter(fsa == f)
  
  # Step 4: Call energy_calc for each FSA
  homes_cooling_fsa <- energy_calc_c(homes_data = homes_fsa_c, hourly_temperature = weather_fsa$temperature, hp_data = hpn, cp=cycling_cop_penalty) 
  
  # Step 5: Append to list
  homes_cooling <- bind_rows(homes_cooling, homes_cooling_fsa)
}