## After scale up
# Introduce scaled up columns for both resistance_elec_use and hp_elec_use based on scale_factor_final
homes_heating <- homes_heating %>%
  mutate(scaled_ac_elec_use = ac_elec_use * scale_factor_final,
         scaled_resistance_elec_use = resistance_elec_use * scale_factor_final,
         scaled_hp_elec_use = hp_elec_use * scale_factor_final)

# 1-2 Elec usage -
# Select the required columns from homes_heating & Summarize the data

summarized_scaled_elec_usage_data <- homes_heating %>%
  summarise(scaled_ac_elec_use_total = sum(scaled_ac_elec_use, na.rm = TRUE) / 1e6,
            scaled_resistance_elec_use_total = sum(scaled_resistance_elec_use, na.rm = TRUE) / 1e6,
            scaled_hp_elec_use_total = sum(scaled_hp_elec_use, na.rm = TRUE) / 1e6) %>%
  mutate(
    scaled_combined_resistance_ac_use = scaled_resistance_elec_use_total + scaled_ac_elec_use_total
  )

# Reshape the data to long format
summarized_scaled_elec_usage_data_long <- pivot_longer(summarized_scaled_elec_usage_data, 
                                                       cols = c(scaled_combined_resistance_ac_use, scaled_hp_elec_use_total), 
                                                       names_to = "Variable", 
                                                       values_to = "Value")

# Create the barplot
Plot_scaled_Elec_usage <- ggplot(summarized_scaled_elec_usage_data_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_col(width = 0.4) +
  scale_fill_manual(values = c("steelblue", "darkorange"),
                    labels = c("Total Elec Usage on Heat Pump", "Total Elec Usage on Resistance + AC"),
                    guide = guide_legend(reverse = TRUE)) +
  labs(x = "Variable", y = "Elec Usage (PJ)") +
  theme_light()


print(Plot_scaled_Elec_usage)
#ggsave("figures_tables/ON-SD-Gas-Bgas/Plot_scaled_Elec_usage.png", width = 10, height = 10)