### Scenarios:
### Equipment: 
##### (1) Heat pump with resistance heating backup; (2) Heat pump with gas furnace backup
### Timing:
##### Replacement for furnace (Random life left year)
### Operation:
##### (1) Operate to keep costs low; (3) Operate to keep GHG low (not now)

cost_comparison <- function(homes_data, hourly_temperature, hp_data, hp_equip_cost_data, rebate, ir, irr, ctax, life=15, hp_backup, cp=cycling_cop_penalty, mood) {
  
  unique_fsas_c <- unique(homes_data$fsa)
  homes_cooling <- tibble() 
  
  for (f in unique_fsas_c) {
    homes_fsa_c <- homes_data %>% filter(fsa == f)
    weather_fsa <- weather %>% filter(fsa == f)
    
    homes_cooling_fsa <- energy_calc_c(homes_fsa_c, hourly_temperature, hp_data, cp) 
    
    homes_cooling <- bind_rows(homes_cooling, homes_cooling_fsa)
  }
  
  furnace_type <- homes_data$exist_furnace[1]
  
  if (furnace_type == "Natural Gas") {
    unique_fsas <- unique(homes_data$fsa)
    homes_heating <- tibble()
    
    for (f in unique_fsas) {
      homes_fsa <- homes_data %>% filter(fsa == f)
      weather_fsa <- weather %>% filter(fsa == f)
      
      homes_heating_fsa <- energy_calc_EFN(homes_fsa, hourly_temperature, hp_data, hp_backup, cp, mood) 
      
      homes_heating <- bind_rows(homes_heating, homes_heating_fsa)
    }
    
    homes_heating <- homes_heating %>%
      left_join(homes_cooling[, c("id", "model", "hp_elec_use_c", "ac_elec_use", "oldac_elec_use")], by = c("id", "model"))
    homes_heating <- homes_heating %>%
      mutate(hp_elec_use  = hp_elec_use_h + hp_elec_use_c)
    
    homes_heating <- homes_heating %>%
      inner_join(homes_data) %>%
      inner_join(hp_equip_cost_calc(hp_equip_cost_data=hp_equip_cost_data,rebate=rebate,ir=ir,irr=irr,life=life)) %>%
      mutate(
        
        furnace_cost = furnace_gas_use * (ng_price+ng_emit*ctax/1000),
        ac_cost = ac_elec_use * ((elec_price / 100 / 0.0036) + (elec_emit*ctax/1000)),
        hp_cost = hp_gas_backup * (ng_price+ng_emit*ctax/1000) + hp_elec_use * ((elec_price / 100 / 0.0036) + (elec_emit*ctax/1000)),
        oldfurnace_cost = oldfurnace_gas_use * (ng_price+ng_emit*ctax/1000),
        oldac_cost = oldac_elec_use * ((elec_price / 100 / 0.0036) + (elec_emit*ctax/1000)),
        hp_cost_saving = case_when(
          hp_backup == "electricity" ~ ac_cost + furnace_cost + annualized_furnace_cost + annualized_ac_cost + (12 * ng_monthly_fixed_cost) - hp_cost - annualized_hp_cost,
          TRUE ~ ac_cost + furnace_cost + annualized_ac_cost - hp_cost - annualized_hp_cost
        ),
        hp_ghg = hp_gas_backup * ng_emit / 1000 + hp_elec_use * elec_emit / 1000,
        furnace_ghg = furnace_gas_use * ng_emit / 1000,
        ac_ghg = ac_elec_use * elec_emit / 1000,
        hp_ghg_saving = ac_ghg + furnace_ghg - hp_ghg,
        oldfurnace_ghg = oldfurnace_gas_use * ng_emit / 1000,
        oldac_ghg = oldac_elec_use * elec_emit / 1000,
        hp_ghg_saving_wold = oldac_ghg + oldfurnace_ghg - hp_ghg) %>%
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
        hp_backup = first(hp_backup),
        annualized_furnace_cost = first(annualized_furnace_cost),
        hp_gas_backup = first(hp_gas_backup), 
        hp_elec_use = first(hp_elec_use),
        annualized_ac_cost = first(annualized_ac_cost),
        oldfurnace_gas_use = first(oldfurnace_gas_use),
        oldfurnace_cost = first(oldfurnace_cost),
        oldfurnace_ghg = first(oldfurnace_ghg),
        oldac_elec_use = first(oldac_elec_use),
        oldac_cost = first(oldac_cost),
        oldac_ghg = first(oldac_ghg)) %>%
      mutate(dollar_per_tonne = -saving/ghg)
    
    id_fsa_prov_mapping_2 <- homes_data %>%
      select(id, fsa, prov, period, cluster_size) %>%
      distinct()
    
    homes_heating <- homes_heating %>%
      left_join(id_fsa_prov_mapping_2, by = "id")
    
    homes_heating <- homes_heating %>%
      # Group by period and compute the total cluster_size for each period
      group_by(period) %>%
      mutate(total_cluster_size_for_period = sum(cluster_size, na.rm = TRUE)) %>%
      ungroup() %>%
      # Join with vintage_values_all_periods to get the value for each period
      left_join(vintage_values_all_periods, by = c("period" = "period")) %>%
      # Compute the scale_factor
      mutate(scale_factor = cluster_size * value / total_cluster_size_for_period) %>%
      # Compute the scale_factor_final
      mutate(scale_factor_final = scale_factor)
    
  } else if (furnace_type == "Oil") {
    unique_fsas <- unique(homes_data$fsa)
    homes_heating <- tibble()
    
    for (f in unique_fsas) {
      homes_fsa <- homes_data %>% filter(fsa == f)
      weather_fsa <- weather %>% filter(fsa == f)
      
      homes_heating_fsa <- energy_calc_EFO(homes_fsa, hourly_temperature, hp_data, cp) 
      
      homes_heating <- bind_rows(homes_heating, homes_heating_fsa)
    }
    
    homes_heating <- homes_heating %>%
      left_join(homes_cooling[, c("id", "model", "hp_elec_use_c", "ac_elec_use", "oldac_elec_use")], by = c("id", "model"))
    homes_heating <- homes_heating %>%
      mutate(hp_elec_use  = hp_elec_use_h + hp_elec_use_c)
    
    homes_heating <- homes_heating %>%
      inner_join(homes_data) %>%
      inner_join(hp_equip_cost_calc(hp_equip_cost_data=hp_equip_cost_data,rebate=rebate,ir=ir,irr=irr,life=life)) %>%
      mutate(
        
        oil_furnace_cost = furnace_oil_use * (oil_price+oil_emit*ctax/1000),
        ac_cost = ac_elec_use * ((elec_price / 100 / 0.0036) + (elec_emit*ctax/1000)),
        hp_cost = hp_elec_use * ((elec_price / 100 / 0.0036) + (elec_emit*ctax/1000)),
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
                hp_backup = first(hp_backup),
                annualized_furnace_cost = first(annualized_furnace_cost),
                annualized_ac_cost = first(annualized_ac_cost),
                hp_elec_use = first(hp_elec_use),
                oldfurnace_oil_use = first(oldfurnace_oil_use),
                oldfurnace_cost = first(oldfurnace_cost),
                oldac_elec_use = first(oldac_elec_use),
                oldac_cost = first(oldac_cost),
                oldfurnace_ghg = first(oldfurnace_ghg),
                oldac_ghg = first(oldac_ghg)) %>%
      mutate(dollar_per_tonne = -saving/ghg)
    
    id_fsa_prov_mapping_2 <- homes_data %>%
      select(id, fsa, prov, period, cluster_size) %>%
      distinct()
    
    homes_heating <- homes_heating %>%
      left_join(id_fsa_prov_mapping_2, by = "id")
    
    homes_heating <- homes_heating %>%
      # Group by period and compute the total cluster_size for each period
      group_by(period) %>%
      mutate(total_cluster_size_for_period = sum(cluster_size, na.rm = TRUE)) %>%
      ungroup() %>%
      # Join with vintage_values_all_periods to get the value for each period
      left_join(vintage_values_all_periods, by = c("period" = "period")) %>%
      # Compute the scale_factor
      mutate(scale_factor = cluster_size * value / total_cluster_size_for_period) %>%
      # Compute the scale_factor_final
      mutate(scale_factor_final = scale_factor)
    
  } else if (furnace_type == "Electricity") {
    unique_fsas <- unique(homes_data$fsa)
    homes_heating <- tibble()
    
    for (f in unique_fsas) {
      homes_fsa <- homes_data %>% filter(fsa == f)
      weather_fsa <- weather %>% filter(fsa == f)
      
      homes_heating_fsa <- energy_calc_EFE(homes_fsa, hourly_temperature, hp_data, cp) 
      
      homes_heating <- bind_rows(homes_heating, homes_heating_fsa)
    }
    
    homes_heating <- homes_heating %>%
      left_join(homes_cooling[, c("id", "model", "hp_elec_use_c", "ac_elec_use", "oldac_elec_use")], by = c("id", "model"))
    homes_heating <- homes_heating %>%
      mutate(hp_elec_use  = hp_elec_use_h + hp_elec_use_c)
    
    homes_heating <- homes_heating %>%
      inner_join(homes_data) %>%
      inner_join(hp_equip_cost_calc(hp_equip_cost_data=hp_equip_cost_data,rebate=rebate,ir=ir,irr=irr,life=life)) %>%
      mutate(
        annualized_furnace_cost = 0,
        resistance_cost = resistance_elec_use * ((elec_price / 100 / 0.0036) + (elec_emit*ctax/1000)),
        ac_cost = ac_elec_use * ((elec_price / 100 / 0.0036) + (elec_emit*ctax/1000)),
        hp_cost = hp_elec_use * ((elec_price / 100 / 0.0036) + (elec_emit*ctax/1000)),
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
                oldac_ghg = first(oldac_ghg)) %>%
      mutate(dollar_per_tonne = -saving/ghg)
    
    id_fsa_prov_mapping_2 <- homes_data %>%
      select(id, fsa, prov, period, cluster_size) %>%
      distinct()
    
    homes_heating <- homes_heating %>%
      left_join(id_fsa_prov_mapping_2, by = "id")
    
    homes_heating <- homes_heating %>%
      # Group by period and compute the total cluster_size for each period
      group_by(period) %>%
      mutate(total_cluster_size_for_period = sum(cluster_size, na.rm = TRUE)) %>%
      ungroup() %>%
      # Join with vintage_values_all_periods to get the value for each period
      left_join(vintage_values_all_periods, by = c("period" = "period")) %>%
      # Compute the scale_factor
      mutate(scale_factor = cluster_size * value / total_cluster_size_for_period) %>%
      # Compute the scale_factor_final
      mutate(scale_factor_final = scale_factor)
    
  } else {
    stop("Unknown furnace type in the first row")
  }
  return(homes_heating)
}

###
# Make a series of plots

mac_various <- 
  bind_rows(
    cost_comparison(homes_data=representative_homes, hourly_temperature = weather_fsa$temperature,hp_data=hpn, hp_equip_cost_data=hp_equip_cost_data, rebate=0, ir=0.07, irr=0, ctax=0, life=15, hp_backup="electricity", cp=cycling_cop_penalty, mood="Cost") %>%
      bind_cols(tibble(scenario = "base")),
    cost_comparison(homes_data=representative_homes, hourly_temperature = weather_fsa$temperature,hp_data=hpn, hp_equip_cost_data=hp_equip_cost_data, rebate=5000, ir=0.07, irr=0, ctax=0, life=15, hp_backup="electricity", cp=cycling_cop_penalty, mood="Cost") %>%
      bind_cols(tibble(scenario = "Rebate5K")),
    cost_comparison(homes_data=representative_homes, hourly_temperature = weather_fsa$temperature,hp_data=hpn, hp_equip_cost_data=hp_equip_cost_data, rebate=5000, ir=0.07, irr=0.06, ctax=0, life=15, hp_backup="electricity", cp=cycling_cop_penalty, mood="Cost") %>%
      bind_cols(tibble(scenario = "Rebate5K+lowinterest")),
    cost_comparison(homes_data=representative_homes, hourly_temperature = weather_fsa$temperature,hp_data=hpn, hp_equip_cost_data=hp_equip_cost_data, rebate=10000, ir=0.07, irr=0, ctax=0, life=15, hp_backup="electricity", cp=cycling_cop_penalty, mood="Cost") %>%
      bind_cols(tibble(scenario = "Rebate10K")),
    cost_comparison(homes_data=representative_homes, hourly_temperature = weather_fsa$temperature,hp_data=hpn, hp_equip_cost_data=hp_equip_cost_data, rebate=10000, ir=0.07, irr=0.06, ctax=0, life=15, hp_backup="electricity", cp=cycling_cop_penalty, mood="Cost") %>%
      bind_cols(tibble(scenario = "Rebate10K+lowinterest")),
    cost_comparison(homes_data=representative_homes, hourly_temperature = weather_fsa$temperature,hp_data=hpn, hp_equip_cost_data=hp_equip_cost_data, rebate=5000, ir=0.07, irr=0, ctax=95, life=15, hp_backup="electricity", cp=cycling_cop_penalty, mood="Cost") %>%
      bind_cols(tibble(scenario = "Rebate5K+ctax95")),
    cost_comparison(homes_data=representative_homes, hourly_temperature = weather_fsa$temperature,hp_data=hpn, hp_equip_cost_data=hp_equip_cost_data, rebate=10000, ir=0.07, irr=0, ctax=95, life=15, hp_backup="electricity", cp=cycling_cop_penalty, mood="Cost") %>%
      bind_cols(tibble(scenario = "Rebate10K+ctax95")),
                cost_comparison(homes_data=representative_homes, hourly_temperature = weather_fsa$temperature,hp_data=hpn, hp_equip_cost_data=hp_equip_cost_data, rebate=5000, ir=0.07, irr=0, ctax=170, life=15, hp_backup="electricity", cp=cycling_cop_penalty, mood="Cost") %>%
                  bind_cols(tibble(scenario = "Rebate5K+ctax170")),
                cost_comparison(homes_data=representative_homes, hourly_temperature = weather_fsa$temperature,hp_data=hpn, hp_equip_cost_data=hp_equip_cost_data, rebate=10000, ir=0.07, irr=0, ctax=170, life=15, hp_backup="electricity", cp=cycling_cop_penalty, mood="Cost") %>%
                  bind_cols(tibble(scenario = "Rebate10K+ctax170"))
  )
