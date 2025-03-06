

# This next bit of code makes HP energy use calculations"energy_calc" and home_heating calculation
## Already choose the electricity for hp_backup when the existing furnace is Oil or elctric
## energy_calc_EFN (EFN is exist furnace natural gas, EFO is exist furnace is oil, EFE is exist furnace electric)

# Assumed cycling COP penalty
cycling_cop_penalty <- 0.15 # If the energy output is below minimum, unit cycles, reducing efficiency. Assume 15% (document where this assumption came from.)

# Check the first row of representative_homes
furnace_type <- representative_homes$exist_furnace[1]

if (furnace_type == "Natural Gas") {
  ## should be chosen between two scenarios, gas or electricity for hp_backup
  ## should be chosen between two moods, GHG or cost
  ## home_heating should be arranged on hp_cost_saving or hp_ghg_saving
  
  hp_backup = "gas" # "gas" or "electricity"  ------------
  energy_calc_EFN <- function(homes_data, hourly_temperature, hp_data, hp_backup, cp=cycling_cop_penalty, mood="Cost") {
    if (! mood %in% c("Cost", "GHG")) stop("Choose a real mood")
    homes_data %>%
      expand_grid(
        tibble(temperature = hourly_temperature)
      ) %>%
      mutate(hd = hdh(t_hat, temperature),
             energy_demand = hd * slope,
             temperature = round(temperature,0)) %>%
      left_join(hp_data) %>%
      # Define new furnace efficiency for Natural Gas
      mutate(newfurnace_efficiency = 0.98) %>%
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
      hp_elec_use_h =
        case_when(
          hc_high >= energy_demand & mood == "GHG" & hp_backup == "gas" ~ (energy_demand / hp_cop),
          hc_high >= energy_demand & mood == "Cost" & hp_backup == "gas" & (ng_price+ng_emit*ctax/1000) / newfurnace_efficiency > ((elec_price / 100 / 0.0036) + (elec_emit*ctax/1000)) / hp_cop ~ (energy_demand / hp_cop),
          hc_high >= energy_demand & mood == "Cost" & hp_backup == "gas" & (ng_price+ng_emit*ctax/1000) / newfurnace_efficiency < ((elec_price / 100 / 0.0036) + (elec_emit*ctax/1000)) / hp_cop ~ 0,
          hc_high >= energy_demand & hp_backup == "electricity" ~ (energy_demand / hp_cop),
          energy_demand > hc_high & hp_backup == "gas" ~ 0,
          energy_demand > hc_high & hp_backup == "electricity" ~ energy_demand
        ),
      hp_gas_use = 
        case_when(
          hc_high >= energy_demand & mood == "GHG" & hp_backup == "gas" ~ 0,
          hc_high >= energy_demand & mood == "Cost" & hp_backup == "gas" & (ng_price+ng_emit*ctax/1000) / newfurnace_efficiency > ((elec_price / 100 / 0.0036) + (elec_emit*ctax/1000)) / hp_cop ~ 0,
          hc_high >= energy_demand & mood == "Cost" & hp_backup == "gas" & (ng_price+ng_emit*ctax/1000) / newfurnace_efficiency < ((elec_price / 100 / 0.0036) + (elec_emit*ctax/1000)) / hp_cop ~ (energy_demand / newfurnace_efficiency),
          hc_high >= energy_demand & hp_backup == "electricity" ~ 0,
          energy_demand > hc_high & hp_backup == "gas" ~ (energy_demand / newfurnace_efficiency),
          energy_demand > hc_high & hp_backup == "electricity" ~ 0
        ),
      furnace_gas_use = (energy_demand / newfurnace_efficiency),
      oldfurnace_gas_use = (energy_demand / furnace_efficiency) 
      ) %>%
      group_by(id,model) %>%
      summarise(hp_elec_use_h = sum(hp_elec_use_h, na.rm=T),
                hp_gas_backup = sum(hp_gas_use, na.rm=T),
                furnace_gas_use = sum(furnace_gas_use, na.rm=T),
                oldfurnace_gas_use = sum(oldfurnace_gas_use, na.rm=T)) 
  } 
  
  # Step 1: Get unique FSAs
  unique_fsas <- unique(representative_homes$fsa)
  
  # Step 2: Initialize list
  homes_heating <- tibble()
  
  # Step 3: Loop over each FSA
  for (f in unique_fsas) {
    homes_fsa <- representative_homes %>% filter(fsa == f)
    weather_fsa <- weather %>% filter(fsa == f)
    
    # Step 4: Call energy_calc for each FSA using 'hp_backup' from the loop
    homes_heating_fsa <- energy_calc_EFN(homes_data = homes_fsa, hourly_temperature = weather_fsa$temperature, hp_data = hpn, hp_backup = "gas", cp=cycling_cop_penalty, mood="Cost") 
    
    # Step 5: Append to list
    homes_heating <- bind_rows(homes_heating, homes_heating_fsa)
  }
  
  homes_heating <- homes_heating %>%
    left_join(homes_cooling[, c("id", "model", "hp_elec_use_c", "ac_elec_use", "oldac_elec_use")], by = c("id", "model"))
  homes_heating <- homes_heating %>%
    mutate(hp_elec_use  = hp_elec_use_h + hp_elec_use_c)
  
  # Step 6: Continue with the remaining code as it is
  
  hp_backup_vector <- rep(hp_backup, nrow(homes_heating))
  
  homes_heating <- homes_heating %>%
    mutate(hp_backup = hp_backup_vector)
  
  homes_heating <- homes_heating %>%
    inner_join(representative_homes) %>%
    inner_join(hp_equip_cost_calc(hp_equip_cost_data=hp_equip_cost_data,rebate=5000,ir=0.07,irr=0,life=15)) %>%
    
    mutate(
      
      furnace_cost = furnace_gas_use * (ng_price+ng_carbon_charge),
      ac_cost = ac_elec_use * ((elec_price / 100 / 0.0036) + elec_carbon_charge) ,
      hp_cost = hp_gas_backup * (ng_price+ng_carbon_charge) + hp_elec_use * ((elec_price / 100 / 0.0036) + elec_carbon_charge),
      oldfurnace_cost = oldfurnace_gas_use * (ng_price+ng_carbon_charge),
      oldac_cost = oldac_elec_use * ((elec_price / 100 / 0.0036) + elec_carbon_charge),
      hp_cost_saving = if_else(
        hp_backup == "electricity",
        ac_cost + furnace_cost + annualized_furnace_cost + annualized_ac_cost + (12 * ng_monthly_fixed_cost) - hp_cost - annualized_hp_cost,
        ac_cost + furnace_cost + annualized_ac_cost - hp_cost - annualized_hp_cost
      ),
      hp_ghg = hp_gas_backup * ng_emit / 1000 + hp_elec_use * elec_emit / 1000,
      furnace_ghg = furnace_gas_use * ng_emit / 1000,
      ac_ghg = ac_elec_use * elec_emit / 1000,
      hp_ghg_saving = ac_ghg + furnace_ghg - hp_ghg,
      oldfurnace_ghg = oldfurnace_gas_use * ng_emit / 1000,
      oldac_ghg = oldac_elec_use * elec_emit / 1000,
      hp_ghg_saving_wold = oldac_ghg + oldfurnace_ghg - hp_ghg
      
    ) %>%
    group_by(id) %>%
    arrange(id, desc(hp_cost_saving)) %>%
    summarise(
      saving = first(hp_cost_saving),
      model = first(model),
      ghg = first(hp_ghg_saving),
      ghg_wold = first(hp_ghg_saving_wold),
      furnace_cost = first(furnace_cost),
      ac_cost = first(ac_cost),
      hp_cost = first(hp_cost),
      hp_ghg = first(hp_ghg),
      furnace_ghg = first(furnace_ghg),
      ac_ghg = first(ac_ghg),
      furnace_gas_use = first(furnace_gas_use),
      ac_elec_use = first(ac_elec_use), 
      hp_gas_backup = first(hp_gas_backup),
      hp_backup = first(hp_backup),
      annualized_furnace_cost = first(annualized_furnace_cost),
      annualized_ac_cost = first(annualized_ac_cost),
      hp_elec_use = first(hp_elec_use),
      
      # New variables summarised
      oldfurnace_gas_use = first(oldfurnace_gas_use),
      oldfurnace_cost = first(oldfurnace_cost),
      oldfurnace_ghg = first(oldfurnace_ghg),
      oldac_elec_use = first(oldac_elec_use),
      oldac_cost = first(oldac_cost),
      oldac_ghg = first(oldac_ghg)) %>%
    mutate(dollar_per_tonne = -saving/ghg)
  
  # Extract a unique mapping of id to fsa and prov
  id_fsa_prov_mapping <- representative_homes %>%
    select(id, fsa, prov, period, cluster_size) %>%
    distinct()
  
  # Add the fsa and prov columns to the home_heating tibble using left_join
  homes_heating <- homes_heating %>%
    left_join(id_fsa_prov_mapping, by = "id")
  
} else if (furnace_type == "Oil") {
  energy_calc_EFO <- function(homes_data, hourly_temperature, hp_data, cp=cycling_cop_penalty) {
    homes_data %>%
      expand_grid(
        tibble(temperature = hourly_temperature)
      ) %>%
      mutate(hd = hdh(t_hat, temperature),
             energy_demand = hd * slope,
             temperature = round(temperature,0)) %>%
      left_join(hp_data) %>%
      # Define new furnace efficiency for Oil
      mutate(newfurnace_efficiency = 0.90) %>%
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
      hp_elec_use_h =
        case_when(
          hc_high >= energy_demand ~ (energy_demand / hp_cop),
          energy_demand > hc_high ~ energy_demand
        ),
      furnace_oil_use = (energy_demand / newfurnace_efficiency),
      # old furnace oil use
      oldfurnace_oil_use = (energy_demand / furnace_efficiency)
      ) %>%
      group_by(id,model) %>%
      summarise(hp_elec_use_h = sum(hp_elec_use_h, na.rm=T),
                furnace_oil_use = sum(furnace_oil_use, na.rm=T),
                oldfurnace_oil_use = sum(oldfurnace_oil_use, na.rm=T)
      ) 
  } 
  
  # Step 1: Get unique FSAs
  unique_fsas <- unique(representative_homes$fsa)
  
  # Step 2: Initialize list
  homes_heating <- tibble()
  
  # Step 3: Loop over each FSA
  for (f in unique_fsas) {
    homes_fsa <- representative_homes %>% filter(fsa == f)
    weather_fsa <- weather %>% filter(fsa == f)
    
    # Step 4: Call energy_calc for each FSA
    homes_heating_fsa <- energy_calc_EFO(homes_data = homes_fsa, hourly_temperature = weather_fsa$temperature, hp_data = hpn, cp=cycling_cop_penalty) 
    
    # Step 5: Append to list
    homes_heating <- bind_rows(homes_heating, homes_heating_fsa)
  }
  
  homes_heating <- homes_heating %>%
    left_join(homes_cooling[, c("id", "model", "hp_elec_use_c", "ac_elec_use", "oldac_elec_use")], by = c("id", "model"))
  homes_heating <- homes_heating %>%
    mutate(hp_elec_use  = hp_elec_use_h + hp_elec_use_c)
  
  # Step 6: Continue with the remaining code as it is  
  homes_heating <- homes_heating %>%
    inner_join(representative_homes) %>%
    inner_join(hp_equip_cost_calc(hp_equip_cost_data=hp_equip_cost_data,rebate=5000,ir=0.07,irr=0,life=15)) %>%
    
    mutate(
      
      oil_furnace_cost = furnace_oil_use * (oil_price+oil_carbon_charge),
      ac_cost = ac_elec_use * ((elec_price / 100 / 0.0036) + elec_carbon_charge),
      hp_cost = hp_elec_use * ((elec_price / 100 / 0.0036) + elec_carbon_charge),
      oldfurnace_cost = oldfurnace_oil_use * (oil_price+oil_carbon_charge),
      oldac_cost = oldac_elec_use * ((elec_price / 100 / 0.0036) + elec_carbon_charge),
      hp_cost_saving = ac_cost + oil_furnace_cost + annualized_furnace_cost + annualized_ac_cost - hp_cost - annualized_hp_cost,
      hp_ghg = hp_elec_use * elec_emit / 1000,
      oil_furnace_ghg = furnace_oil_use * oil_emit / 1000,
      ac_ghg = ac_elec_use * elec_emit / 1000,
      hp_ghg_saving = ac_ghg + oil_furnace_ghg - hp_ghg,
      
      oldfurnace_ghg = oldfurnace_oil_use * oil_emit / 1000,
      oldac_ghg = oldac_elec_use * elec_emit / 1000,
      hp_ghg_saving_wold = oldac_ghg + oldfurnace_ghg - hp_ghg
      
    ) %>%
    group_by(id) %>%
    arrange(id, desc(hp_cost_saving)) %>%
    summarise(saving = first(hp_cost_saving),
              model = first(model),
              ghg = first(hp_ghg_saving),
              ghg_wold = first(hp_ghg_saving_wold),
              oil_furnace_cost = first(oil_furnace_cost),
              ac_cost = first(ac_cost),
              hp_cost = first(hp_cost),
              hp_ghg = first(hp_ghg),
              oil_furnace_ghg = first(oil_furnace_ghg),
              ac_ghg = first(ac_ghg),
              furnace_oil_use = first(furnace_oil_use),
              ac_elec_use = first(ac_elec_use),
              annualized_furnace_cost = first(annualized_furnace_cost),
              annualized_ac_cost = first(annualized_ac_cost),
              hp_elec_use = first(hp_elec_use),
              
              # New variables summarised
              oldfurnace_oil_use = first(oldfurnace_oil_use),
              oldfurnace_cost = first(oldfurnace_cost),
              oldac_elec_use = first(oldac_elec_use),
              oldac_cost = first(oldac_cost),
              oldfurnace_ghg = first(oldfurnace_ghg),
              oldac_ghg = first(oldac_ghg)
    ) %>%
    mutate(dollar_per_tonne = -saving/ghg)
  
  # Extract a unique mapping of id to fsa and prov
  id_fsa_prov_mapping <- representative_homes %>%
    select(id, fsa, prov, period, cluster_size) %>%
    distinct()
  
  # Add the fsa and prov columns to the home_heating tibble using left_join
  homes_heating <- homes_heating %>%
    left_join(id_fsa_prov_mapping, by = "id")
  
} else if (furnace_type == "Electricity") {
  energy_calc_EFE <- function(homes_data, hourly_temperature, hp_data, cp=cycling_cop_penalty) {
    homes_data %>%
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
      hp_elec_use_h =
        case_when(
          hc_high >= energy_demand ~ (energy_demand / hp_cop),
          energy_demand > hc_high ~ energy_demand
        ),
      resistance_elec_use = energy_demand) %>%
      group_by(id,model) %>%
      summarise(hp_elec_use_h = sum(hp_elec_use_h, na.rm=T),
                resistance_elec_use = sum(resistance_elec_use, na.rm=T)) 
  } 
  
  # Step 1: Get unique FSAs
  unique_fsas <- unique(representative_homes$fsa)
  
  # Step 2: Initialize list
  homes_heating <- tibble()
  
  # Step 3: Loop over each FSA
  for (f in unique_fsas) {
    homes_fsa <- representative_homes %>% filter(fsa == f)
    weather_fsa <- weather %>% filter(fsa == f)
    
    # Step 4: Call energy_calc for each FSA
    homes_heating_fsa <- energy_calc_EFE(homes_data = homes_fsa, hourly_temperature = weather_fsa$temperature, hp_data = hpn, cp=cycling_cop_penalty) 
    
    # Step 5: Append to list
    homes_heating <- bind_rows(homes_heating, homes_heating_fsa)
  }
  
  homes_heating <- homes_heating %>%
    left_join(homes_cooling[, c("id", "model", "hp_elec_use_c", "ac_elec_use", "oldac_elec_use")], by = c("id", "model"))
  homes_heating <- homes_heating %>%
    mutate(hp_elec_use  = hp_elec_use_h + hp_elec_use_c)
  
  # Step 6: Continue with the remaining code as it is  
  homes_heating <- homes_heating %>%
    inner_join(representative_homes) %>%
    inner_join(hp_equip_cost_calc(hp_equip_cost_data=hp_equip_cost_data,rebate=5000,ir=0.07,irr=0,life=15)) %>%
    
    mutate(
      annualized_furnace_cost = 0,
      resistance_cost = resistance_elec_use * ((elec_price / 100 / 0.0036) + elec_carbon_charge),
      ac_cost = ac_elec_use * ((elec_price / 100 / 0.0036) + elec_carbon_charge),
      hp_cost = hp_elec_use * ((elec_price / 100 / 0.0036) + elec_carbon_charge),
      oldac_cost = oldac_elec_use * ((elec_price / 100 / 0.0036) + elec_carbon_charge),
      oldac_ghg = oldac_elec_use * elec_emit / 1000,
      hp_cost_saving = ac_cost + resistance_cost + annualized_ac_cost - hp_cost - annualized_hp_cost,
      hp_ghg = hp_elec_use * elec_emit / 1000,
      resistance_ghg = resistance_elec_use * elec_emit / 1000,
      ac_ghg = ac_elec_use * elec_emit / 1000,
      hp_ghg_saving = ac_ghg + resistance_ghg - hp_ghg,
      hp_ghg_saving_wold = oldac_ghg + resistance_ghg - hp_ghg
    ) %>%
    group_by(id) %>%
    arrange(id, desc(hp_cost_saving)) %>%
    summarise(saving = first(hp_cost_saving),
              model = first(model),
              ghg = first(hp_ghg_saving),
              ghg_wold = first(hp_ghg_saving_wold),
              resistance_cost = first(resistance_cost),
              ac_cost = first(ac_cost),
              hp_cost = first(hp_cost),
              hp_ghg = first(hp_ghg),
              resistance_ghg = first(resistance_ghg),
              ac_ghg = first(ac_ghg),
              ac_elec_use = first(ac_elec_use),
              resistance_elec_use = first(resistance_elec_use),
              annualized_furnace_cost = first(annualized_furnace_cost),
              annualized_ac_cost = first(annualized_ac_cost),
              hp_elec_use = first(hp_elec_use),
              oldac_elec_use = first(oldac_elec_use),
              oldac_cost = first(oldac_cost),
              oldac_ghg = first(oldac_ghg)
    ) %>%
    mutate(dollar_per_tonne = -saving/ghg)
  
  # Extract a unique mapping of id to fsa and prov
  id_fsa_prov_mapping <- representative_homes %>%
    select(id, fsa, prov, period, cluster_size) %>%
    distinct()
  
  # Add the fsa and prov columns to the home_heating tibble using left_join
  homes_heating <- homes_heating %>%
    left_join(id_fsa_prov_mapping, by = "id")
  
} else {
  stop("Unknown furnace type in the first row")
}