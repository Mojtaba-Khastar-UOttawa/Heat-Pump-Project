### Elec usage # This part will work if the existing furnace be Electric 

# Before scale up
# 1-2 Elec usage -
# Select the required columns from homes_heating & Summarize the data

summarized_elec_usage_data <- homes_heating %>%
  summarise(ac_elec_use_total = sum(ac_elec_use, na.rm = TRUE),
            resistance_elec_use_total = sum(resistance_elec_use, na.rm = TRUE),
            hp_elec_use_total = sum(hp_elec_use, na.rm = TRUE)) %>%
  mutate(
    combined_resistance_ac_use = resistance_elec_use_total + ac_elec_use_total
  )

# Reshape the data to long format
summarized_elec_usage_data_long <- pivot_longer(summarized_elec_usage_data, 
                                                cols = c(combined_resistance_ac_use, hp_elec_use_total), 
                                                names_to = "Variable", 
                                                values_to = "Value")

# Create the barplot
Plot_Elec_usage <- ggplot(summarized_elec_usage_data_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_col(width = 0.4) +
  scale_fill_manual(values = c("steelblue", "darkorange"),
                    labels = c("Total Elec Usage on Resistance + AC", "Total Elec Usage on Heat Pump")) +
  labs(x = "Variable", y = "Elec Usage (GJ)") +
  theme_light()


print(Plot_Elec_usage)