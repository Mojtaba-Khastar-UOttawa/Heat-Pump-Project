# social_and_private_marginal_abatement_cost_paper.R

# Define the desired order of new scenarios
scenario_order <- c("base", 
                    "Rebate5K", 
                    "Rebate5K+lowinterest",
                    "Rebate10K", 
                    "Rebate10K+lowinterest",
                    "Rebate5K+ctax95", 
                    "Rebate10K+ctax95", 
                    "Rebate5K+ctax170", 
                    "Rebate10K+ctax170")

# Set the factor levels for the scenario column in the data
mac_various$scenario <- factor(mac_various$scenario, levels = scenario_order)

# Define color palette for the scenarios
color_palette <- c("base"                = "#1f77b4",  # Blue
                   "Rebate5K"            = "#ff7f0e",  # Orange
                   "Rebate5K+lowinterest"= "#2ca02c",  # Green
                   "Rebate10K"           = "#9467bd",  # Purple
                   "Rebate10K+lowinterest" = "#8c564b",# Brown
                   "Rebate5K+ctax95"     = "#d62728",  # Red
                   "Rebate10K+ctax95"    = "#7f7f7f",  # Gray
                   "Rebate5K+ctax170"    = "#e377c2",  # Pink
                   "Rebate10K+ctax170"   = "#bcbd22"   # Olive
                   )

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
#ggsave("figures_tables/ON-SD-Gas-Bgas/plot_marginal_abatement_cost_various_scenarios.png", plot = plot_marginal_abatement_cost_various_scenarios, width = 10, height = 10)

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

# Extracting variables from the first row of homes
first_row <- homes %>% slice(1)
prov <- first_row$PROVINCE[1]
exist_furnace <- gsub(" ", "_", first_row$exist_furnace[1])
house_type <- gsub(" ", "_", first_row$house_type[1])

# Print out the variables for debugging
print(prov)
print(house_type)
print(exist_furnace)
print(hp_backup)

# Define the directory path within the main directory
dir_path <- paste0("Results_paper/Results_Costmood/", prov, "_", house_type, "_", exist_furnace, "_", hp_backup, "/")
print(dir_path)

# Check if the directory exists and create it if it doesn't
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

# Define a file name for the plot
fig_name <- paste0("scaled_marginal_abatement_cost_various_scenarios_", prov, "_", house_type, "_", exist_furnace, "_", hp_backup, ".png")

print(fig_name)
# Save the plot to the defined directory
ggsave(filename = paste0(dir_path, fig_name),
       plot = plot_scaled_marginal_abatement_cost_various_scenarios,
       device = "png",    # <-- added explicitly
       width = 10, height = 6, dpi = 300)

