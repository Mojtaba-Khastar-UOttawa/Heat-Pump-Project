
## curve elec usage when exist furnace is Electric 

furnace_type <- representative_homes$exist_furnace[1]

if (furnace_type == "Electricity") {
  plot_resistance_elec_data <- homes_heating_hourly %>%
    group_by(hour) %>% 
    summarise(resistance_elec_use_hourly = ((sum(resistance_elec_use_hourly * scale_factor_final, na.rm=T))/3)*0.2777777778) #There are three hour for each hour 1:8760 so  /3
  
  plot_resistance_elec_use_hourly <- ggplot(plot_resistance_elec_data, aes(x = hour, y=resistance_elec_use_hourly)) + geom_smooth()
  print(plot_resistance_elec_use_hourly)
} 


