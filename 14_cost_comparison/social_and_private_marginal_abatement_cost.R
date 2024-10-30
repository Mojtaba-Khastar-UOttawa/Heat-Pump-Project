# Define the desired order of scenarios
scenario_order <- c("base", "ctax65", "ctax170", "ctax170_+rebate", "ctax170_+rebate+lowinterest", "ctax170_+rebate+lowinterest+elecbackup")

# Set the factor levels for the scenario column in the data
mac_various$scenario <- factor(mac_various$scenario, levels = scenario_order)

# Define color palette for the scenarios
color_palette <- c("base" = "#1f77b4", 
                   "ctax65" = "#ff7f0e", 
                   "ctax170" = "#2ca02c", 
                   "ctax170_+rebate" = "#d62728", 
                   "ctax170_+rebate+lowinterest" = "#9467bd", 
                   "ctax170_+rebate+lowinterest+elecbackup" = "#8c564b")

# Calculate cumulative GHG reductions
mac_various <- mac_various %>%
  arrange(scenario, dollar_per_tonne) %>%
  group_by(scenario) %>%
  mutate(cumghg = cumsum(ghg)) %>%
  ungroup()

# Create the plot
plot_marginal_abatement_cost_various_scenarios <- ggplot(mac_various, 
                                                         aes(x=cumghg, y=dollar_per_tonne, color=scenario)) + 
  geom_step(size=1.2) +  # Increase the size to make lines more visible
  scale_color_manual(values = color_palette) +  # Define manual color scale for scenarios
  labs(x="Cumulative greenhouse gas reduction (t/yr)",
       y="Marginal abatement cost ($/t)",
       color="Scenario") +  # Add 'Scenario' label to the legend
  theme_light() +
  theme(legend.position = "bottom",  # Move legend to bottom
        plot.title = element_text(hjust = 0.5),  # Center-align the plot title
        legend.box.background = element_rect(colour = "grey80"),  # Add a background to the legend
        legend.key.size = unit(1, 'lines'))  # Increase the size of the legend keys

# Add title, subtitle, and caption if needed
plot_marginal_abatement_cost_various_scenarios <- plot_marginal_abatement_cost_various_scenarios +
  ggtitle("Marginal Abatement Cost Curve") +
  theme(plot.title = element_text(size=20))

# Print the plot
print(plot_marginal_abatement_cost_various_scenarios)

# Save the plot with the specified dimensions
ggsave("figures_tables/ON-SD-Gas-Bgas/plot_marginal_abatement_cost_various_scenarios.png", 
       plot = plot_marginal_abatement_cost_various_scenarios, 
       width = 10, height = 10)

# Introduce scaled up columns for both dollar_per_tonne and ghg based on scale_factor_final
mac_various <- mac_various %>%
  mutate(scaled_ghg = ghg * scale_factor_final)

# Calculate cumulative GHG reductions after scaling
mac_various <- mac_various %>%
  arrange(scenario, dollar_per_tonne) %>%
  group_by(scenario) %>%
  mutate(cumghg = cumsum(scaled_ghg)) %>%
  ungroup()

# Create the plot
plot_scaled_marginal_abatement_cost_various_scenarios <- ggplot(mac_various, 
                                                                aes(x=cumghg, y=dollar_per_tonne, color=scenario)) +  
  geom_step(size=1.2) +  # Increase the size for better visibility
  scale_color_manual(values = color_palette) +  # Use predefined color palette
  theme_light() +
  labs(x="Cumulative greenhouse gas reduction (t/yr)",
       y="Marginal abatement cost ($/t)",
       color="Scenario") +  # Add 'Scenario' label to the legend
  scale_x_continuous(labels=comma_format(big.mark = ",")) +  # Format the x-axis labels
  scale_y_continuous(labels=dollar_format()) +  # Format the y-axis labels
  theme(legend.position = "bottom",  # Move legend to bottom
        plot.title = element_text(hjust = 0.5),  # Center-align the plot title
        legend.box.background = element_rect(colour = "grey80"),  # Add a background to the legend
        legend.key.size = unit(1, 'lines'))  # Increase the size of the legend keys

# Add title, subtitle, and caption if needed
plot_scaled_marginal_abatement_cost_various_scenarios <- plot_scaled_marginal_abatement_cost_various_scenarios +
  ggtitle("Scaled Marginal Abatement Cost Curve") +
  theme(plot.title = element_text(size=20))

# Print the plot
print(plot_scaled_marginal_abatement_cost_various_scenarios)

# Save the plot with the specified dimensions
ggsave("figures_tables/ON-SD-Gas-Bgas/plot_scaled_marginal_abatement_cost_various_scenarios1.png", 
       plot = plot_scaled_marginal_abatement_cost_various_scenarios, 
       width = 10, height = 10)