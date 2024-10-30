#This next bit of code makes hourly HP energy use calculations"energy_calc_hourly" and home_heating_hourly calculation
## Already choose the electricity for hp_backup when the existing furnace is Oil or electric
## When the existing furnace is NG then should be chosen between two scenarios, gas or electricity for hp_backup
## When the existing furnace is NG then should be chosen between two moods, GHG or cost

# Assumed cycling COP penalty
cycling_cop_penalty <- 0.15 # If the energy output is below minimum, unit cycles, reducing efficiency. Assume 15% (document where this assumption came from.)

# Check the first row of representative_homes
furnace_type <- representative_homes$exist_furnace[1]

if (furnace_type == "Natural Gas") {
  ## should be chosen between two scenarios, gas or electricity for hp_backup
  ## should be chosen between two moods, GHG or cost
  

  energy_calc_hourly <- function(homes_data_hourly, hourly_temperature, hp_data, hp_backup, cp=cycling_cop_penalty, mood="Cost") {
    if (! mood %in% c("Cost", "GHG")) stop("Choose a real mood")
    homes_data_hourly %>%
      expand_grid(
        tibble(temperature = hourly_temperature)
      ) %>%
      mutate(hd = hdh(t_hat, temperature),
             energy_demand = hd * slope,
             temperature = round(temperature,0)) %>%
      left_join(hp_data) %>%
      mutate(hp_output = case_when(
        # If the heat pump cannot satisfy the entire load for the house, the heat pump shuts down, and the backup is activated.
        energy_demand > hc_high ~ 0,
        energy_demand <= hc_high ~ energy_demand
      ),
      backup_output = energy_demand - hp_output,
      hp_cop = case_when(
        energy_demand >= hc_low & energy_demand <= hc_high ~ cop_low + (cop_high - cop_low) / (hc_high - hc_low) * (hc_high - energy_demand),
        # Any efficiency penalty for cycling would go in the line below
        energy_demand < hc_low ~ cop_low * (1 - cp)
      ),
      hp_elec_use_hourly =
        case_when(
          hc_high >= energy_demand & mood == "GHG" & hp_backup == "gas" ~ energy_demand / hp_cop,
          hc_high >= energy_demand & mood == "Cost" & hp_backup == "gas" & (ng_price+ng_emit*ctax/1000) / furnace_efficiency > ((elec_price / 100 / 0.0036) + (elec_emit*ctax/1000)) / hp_cop ~ energy_demand / hp_cop,
          hc_high >= energy_demand & mood == "Cost" & hp_backup == "gas" & (ng_price+ng_emit*ctax/1000) / furnace_efficiency < ((elec_price / 100 / 0.0036) + (elec_emit*ctax/1000)) / hp_cop ~ 0,
          hc_high >= energy_demand & hp_backup == "electricity" ~ energy_demand / hp_cop,
          energy_demand > hc_high & hp_backup == "gas" ~ 0,
          energy_demand > hc_high & hp_backup == "electricity" ~ energy_demand
        ))
  }
  homes_hourly <- representative_homes 
  #homes_hourly <- head(homes_hourly, 100) 
  # Step 1: Get unique FSAs
  unique_fsas_hourly <- unique(homes_hourly$fsa)
  
  # Step 2: Initialize list
  homes_heating_hourly <- tibble() 
  
  # Step 3: Loop over each FSA
  for (f in unique_fsas_hourly) {
    homes_fsa_hourly <- homes_hourly %>% filter(fsa == f) 
    weather_fsa_hourly <- weather %>% filter(fsa == f)
    
    # Step 4: Call energy_calc for each FSA
    homes_heating_fsa_hourly <- energy_calc_hourly(homes_data_hourly = homes_fsa_hourly, hourly_temperature = weather_fsa_hourly$temperature, hp_data = hpn, hp_backup = hp_backup, cp=cycling_cop_penalty, mood="Cost") 
    
    # Step 5: Append to list
    homes_heating_hourly <- bind_rows(homes_heating_hourly, homes_heating_fsa_hourly)
  }
  
} else if (furnace_type == "Oil") {
  energy_calc_hourly <- function(homes_data_hourly, hourly_temperature, hp_data, hp_backup, cp=cycling_cop_penalty) {
    homes_data_hourly %>%
      expand_grid(
        tibble(temperature = hourly_temperature)
      ) %>%
      mutate(hd = hdh(t_hat, temperature),
             energy_demand = hd * slope,
             temperature = round(temperature,0)) %>%
      left_join(hp_data) %>%
      mutate(hp_output = case_when(
        # If the heat pump cannot satisfy the entire load for the house, the heat pump shuts down, and the backup is activated.
        energy_demand > hc_high ~ 0,
        energy_demand <= hc_high ~ energy_demand
      ),
      backup_output = energy_demand - hp_output,
      hp_cop = case_when(
        energy_demand >= hc_low & energy_demand <= hc_high ~ cop_low + (cop_high - cop_low) / (hc_high - hc_low) * (hc_high - energy_demand),
        # Any efficiency penalty for cycling would go in the line below
        energy_demand < hc_low ~ cop_low * (1 - cp)
      ),
      hp_elec_use_hourly =
        case_when(
          hc_high >= energy_demand ~ (energy_demand / hp_cop),
          energy_demand > hc_high ~ energy_demand
        ))
  }
  homes_hourly <- representative_homes 
  #homes_hourly <- head(homes_hourly, 100) 
  # Step 1: Get unique FSAs
  unique_fsas_hourly <- unique(homes_hourly$fsa)
  
  # Step 2: Initialize list
  homes_heating_hourly <- tibble() 
  
  # Step 3: Loop over each FSA
  for (f in unique_fsas_hourly) {
    homes_fsa_hourly <- homes_hourly %>% filter(fsa == f) 
    weather_fsa_hourly <- weather %>% filter(fsa == f)
    
    # Step 4: Call energy_calc for each FSA
    homes_heating_fsa_hourly <- energy_calc_hourly(homes_data_hourly = homes_fsa_hourly, hourly_temperature = weather_fsa_hourly$temperature, hp_data = hpn, cp=cycling_cop_penalty) 
    
    # Step 5: Append to list
    homes_heating_hourly <- bind_rows(homes_heating_hourly, homes_heating_fsa_hourly)
  }
  
} else if (furnace_type == "Electricity") {
  energy_calc_hourly <- function(homes_data_hourly, hourly_temperature, hp_data, hp_backup, cp=cycling_cop_penalty) {
    homes_data_hourly %>%
      expand_grid(
        tibble(temperature = hourly_temperature)
      ) %>%
      mutate(hd = hdh(t_hat, temperature),
             energy_demand = hd * slope,
             temperature = round(temperature,0)) %>%
      left_join(hp_data) %>%
      mutate(hp_output = case_when(
        # If the heat pump cannot satisfy the entire load for the house, the heat pump shuts down, and the backup is activated.
        energy_demand > hc_high ~ 0,
        energy_demand <= hc_high ~ energy_demand
      ),
      backup_output = energy_demand - hp_output,
      hp_cop = case_when(
        energy_demand >= hc_low & energy_demand <= hc_high ~ cop_low + (cop_high - cop_low) / (hc_high - hc_low) * (hc_high - energy_demand),
        # Any efficiency penalty for cycling would go in the line below
        energy_demand < hc_low ~ cop_low * (1 - cp)
      ),
      hp_elec_use_hourly =
        case_when(
          hc_high >= energy_demand ~ (energy_demand / hp_cop),
          energy_demand > hc_high ~ energy_demand
        ), resistance_elec_use_hourly = energy_demand)
  }
  homes_hourly <- representative_homes 
  #homes_hourly <- head(homes_hourly, 100) 
  # Step 1: Get unique FSAs
  unique_fsas_hourly <- unique(homes_hourly$fsa)
  
  # Step 2: Initialize list
  homes_heating_hourly <- tibble() 
  
  # Step 3: Loop over each FSA
  for (f in unique_fsas_hourly) {
    homes_fsa_hourly <- homes_hourly %>% filter(fsa == f) 
    weather_fsa_hourly <- weather %>% filter(fsa == f)
    
    # Step 4: Call energy_calc for each FSA
    homes_heating_fsa_hourly <- energy_calc_hourly(homes_data_hourly = homes_fsa_hourly, hourly_temperature = weather_fsa_hourly$temperature, hp_data = hpn, cp=cycling_cop_penalty) 
    
    # Step 5: Append to list
    homes_heating_hourly <- bind_rows(homes_heating_hourly, homes_heating_fsa_hourly)
  }
  
} else {
  stop("Unknown furnace type in the first row")
}

homes_heating_hourly <- homes_heating_hourly %>%
  group_by(id) %>%
  # Filter out groups that don't have 26280 rows
  filter(n() == 26280) %>%
  # Add the hour column for the remaining groups
  mutate(hour = rep(1:8760, each=3)) %>%
  ungroup()

homes_heating_hourly <- homes_heating_hourly %>% left_join(select(homes_heating, id, model, saving, ghg, dollar_per_tonne, value, scale_factor ,total_cluster_size_for_period, scale_factor_final), by = "id")
homes_heating_hourly <- homes_heating_hourly %>%
  rename(optimal_model = model.y)
