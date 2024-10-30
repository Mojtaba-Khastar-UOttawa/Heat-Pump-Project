## Although my electricity data is related to 2015, I used the data of housing stuck for 2020, so if I don't find the elec data for 2020, I should change the scale_factor_final and use the 2015 housing stock (https://oee.nrcan.gc.ca/corporate/statistics/neud/dpa/showTable.cfm?type=CP&sector=res&juris=on&rn=15&year=2020&page=4). The following code section calculates the electricity usage of heat pumps for all specific house types on specified provinance. We scaled the diagram to reflect a realistic representation.

optimal_plot_data_c <- homes_cooling_hourly %>%
  filter(model.x == optimal_model) %>%
  group_by(hour) %>%
  mutate(adjusted_elec_use_c_with_positive_saving = ifelse(saving >= 0, hp_elec_use_c_hourly * scale_factor_final, 0)) %>%
  summarise(
    hp_elec_use_c_hourly = sum(hp_elec_use_c_hourly * scale_factor_final, na.rm = TRUE) * 0.2777777778,  # Convert GJ to MW
    hp_elec_use_c_hourly_with_positive_saving = sum(adjusted_elec_use_c_with_positive_saving, na.rm = TRUE) * 0.2777777778
  )

# Plot the data
plot_elec_use_hourly_optimal_hp_model_cooling <- ggplot(optimal_plot_data_c, aes(x = hour, y = hp_elec_use_c_hourly)) + 
  geom_smooth() +
  labs(title = "Hourly Electricity Use of Optimal Heat Pump Model for cooling", 
       x = "Hour", 
       y = "Electricity Use (MW)", 
       color = "Optimal Model")

print(plot_elec_use_hourly_optimal_hp_model_cooling)
#ggsave("figures_tables/ON-SD-Gas-Bgas/plot_elec_use_hourly_optimal_hp_model_cooling.png", width = 10, height = 10)