
# Annual cost saving after HP adoption ($) 


furnace_type <- representative_homes$exist_furnace[1]

if (furnace_type == "Natural Gas" && hp_backup == "gas") {
  
  #when the exist furnace is gas or oil
  # Define the desired order of scenarios
  scenario_order <- c("base", "ctax65", "ctax170", "ctax170_+rebate", "ctax170_+rebate+lowinterest", "ctax170_+rebate+lowinterest+elecbackup")
  
  # Reshape the data to long format
  barplot_data_long <- mac_various %>% 
    pivot_longer(cols = c(saving), names_to = "Variable", values_to = "Value")
  
  # Set the factor levels for the scenario column in the reshaped data
  barplot_data_long$scenario <- factor(barplot_data_long$scenario, levels = scenario_order)
  
  # Modify 'Variable' values
  barplot_data_long$Variable <- ifelse(barplot_data_long$Variable == "saving", "Annual_Saving", barplot_data_long$Variable)
  
  # Make the box plot
  Annual_Saving_Box_plot <- ggplot(barplot_data_long, aes(x=scenario, y=Value, fill=Variable)) +
    geom_boxplot() +
    theme_light() +
    labs(x="Scenario", y="Annual cost saving after HP adoption ($)",
         fill="Variable",
         title="The Cost Savings from Implementing HP in Various Scenarios") +
    scale_fill_brewer(palette="Pastel1") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(Annual_Saving_Box_plot)
  ggsave("figures_tables/ON-SD-Gas-Bgas/Annual_Saving_Box_plot.png", width = 10, height = 10)
  
  
} else if (furnace_type == "Electricity") {
  
  
  ## Annual cost saving after HP adoption ($) when the exist furnace is electric
  # Define the desired scenarios to be plotted
  selected_scenarios <- c("ctax170", "ctax170_+rebate", "ctax170_+rebate+lowinterest")
  
  # Reshape the data to long format
  barplot_data_long <- mac_various %>% 
    pivot_longer(cols = c(saving), names_to = "Variable", values_to = "Value") %>%
    filter(scenario %in% selected_scenarios) # Filter to keep only the selected scenarios
  
  # Modify 'Variable' values
  barplot_data_long$Variable <- ifelse(barplot_data_long$Variable == "saving", "Annual_Saving", barplot_data_long$Variable)
  
  # Set the factor levels for the scenario column in the reshaped data
  # Note that the scenario_order is now reduced to the selected scenarios
  barplot_data_long$scenario <- factor(barplot_data_long$scenario, levels = selected_scenarios)
  
  # Make the box plot
  Annual_Saving_Box_plot <- ggplot(barplot_data_long, aes(x=scenario, y=Value, fill=Variable)) +
    geom_boxplot() +
    theme_light() +
    labs(x="Scenario", y="Annual cost saving after HP adoption ($)",
         fill="Variable",
         title="The Cost Savings from Implementing HP in Various Scenarios") +
    scale_fill_brewer(palette="Pastel1") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(Annual_Saving_Box_plot)
  ggsave("figures_tables/ON-SD-Gas-Bgas/Annual_Saving_Box_plot.png", width = 10, height = 10)
  
} 