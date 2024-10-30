### Gas usage - The following diagram displays the gas consumption of users who employ a gas furnace for home heating versus those who use a heat pump with gas furnace backup

# Select the required columns from homes_heating & Summarize the data
summarized_gas_usage_data <- homes_heating %>%
  summarise(furnace_gas_use_total = sum(furnace_gas_use, na.rm = TRUE),
            hp_gas_backup_total = sum(hp_gas_backup, na.rm = TRUE))

# Reshape the summarized data to long format
summarized_gas_usage_data_long <- pivot_longer(summarized_gas_usage_data, 
                                               cols = c(furnace_gas_use_total, hp_gas_backup_total), 
                                               names_to = "Variable", 
                                               values_to = "Value")

# Create the barplot
Plot_Gas_usage <- ggplot(summarized_gas_usage_data_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_col(width = 0.4) +
  scale_fill_manual(values = c("steelblue", "darkorange"),
                    labels = c("Total Furnace Gas Usage", "Total Heat Pump Gas Backup"),
                    guide = guide_legend(reverse = TRUE)) +
  labs(x = "Variable", y = "Gas Usage (GJ)") +
  theme_light()


print(Plot_Gas_usage)