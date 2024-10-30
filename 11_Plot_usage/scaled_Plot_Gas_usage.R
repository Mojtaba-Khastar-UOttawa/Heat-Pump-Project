## scaled up curve 
# Introduce scaled up columns for both furnace_gas_use and hp_gas_backup based on scale_factor_final
homes_heating <- homes_heating %>%
  mutate(scaled_furnace_gas_use = furnace_gas_use * scale_factor_final,
         scaled_hp_gas_backup = hp_gas_backup * scale_factor_final)

# Select the required columns from homes_heating & Summarize the data
summarized_scaled_gas_usage_data <- homes_heating %>%
  summarise(scaled_furnace_gas_use_total = sum(scaled_furnace_gas_use, na.rm = TRUE) / 1e6,
            scaled_hp_gas_backup_total = sum(scaled_hp_gas_backup, na.rm = TRUE) / 1e6)

# Reshape the summarized data to long format
summarized_scaled_gas_usage_data_long <- pivot_longer(summarized_scaled_gas_usage_data, 
                                                      cols = c(scaled_furnace_gas_use_total, scaled_hp_gas_backup_total), 
                                                      names_to = "Variable", 
                                                      values_to = "Value")

# Create the barplot
Plot_scaled_Gas_usage <- ggplot(summarized_scaled_gas_usage_data_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_col(width = 0.4) +
  scale_fill_manual(values = c("steelblue", "darkorange"),
                    labels = c("Total Furnace Gas Usage", "Total Gas Usage as Backup for Heat Pump"),
                    guide = guide_legend(reverse = TRUE)) +
  labs(x = "Variable", y = "Gas Usage (PJ)") +
  theme_light()


print(Plot_scaled_Gas_usage)
#ggsave("figures_tables/ON-SD-Gas-Bgas/Plot_scaled_Gas_usage.png", width = 10, height = 10)
