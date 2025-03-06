# Load required libraries
library(dplyr)
library(readr)
library(openxlsx)
library(ggplot2)
library(scales)

# With_NG
# Define the list of provinces 
provinces <- c("ON", "BC", "MB", "NB", "NF", "NS", "AB", "PE", "QC", "SK")

# Define the desired order of scenarios
scenario_order <- c("base", 
                    "Rebate5K", 
                    "Rebate5K+lowinterest",
                    "Rebate10K", 
                    "Rebate10K+lowinterest",
                    "Rebate5K+ctax95", 
                    "Rebate10K+ctax95", 
                    "Rebate5K+ctax170", 
                    "Rebate10K+ctax170")

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

# Initialize an empty list to store the total_mac_various tibble for each province
grand_total_list <- list()

# Loop over each province
for (province in provinces) {
  # Initialize an empty list to store tibbles for the current province
  tibble_list <- list()
  
  # Construct file paths
  file_names <- c("Single_Detached_Natural_Gas_gas",
                  "Single_Detached_Electricity_electricity",
                  "Single_Detached_Oil_electricity",
                  "Single_Attached_Natural_Gas_gas",
                  "Single_Attached_Electricity_electricity",
                  "Single_Attached_Oil_electricity")
  
  for (file_name in file_names) {
    file_path <- paste0("Results_paper/Results_Costmood/", province, "_", file_name, "/mac_various_", province, "_", file_name, ".rds")
    
    # Read the file if it exists
    if (file.exists(file_path)) {
      tibble_list[[file_name]] <- readRDS(file_path)
    }
  }
  
  # Skip to the next province if no files are found
  if (length(tibble_list) == 0) next
  
  # Combine all tibbles in the list into one for the current province
  total_mac_various <- bind_rows(tibble_list)
  
  # Filter out rows with Inf in either dollar_per_tonne or scaled_ghg
  total_mac_various <- total_mac_various %>%
    filter(!is.infinite(dollar_per_tonne) & !is.infinite(scaled_ghg))
  
  # Add the combined tibble to the grand_total_list
  grand_total_list[[province]] <- total_mac_various
}

# Combine all provincial total_mac_various tibbles into one grand total tibble
grand_total_mac_various <- bind_rows(grand_total_list)

# Filter the grand_total_mac_various to include only the specified scenarios
grand_total_mac_various <- grand_total_mac_various %>%
  filter(scenario %in% scenario_order) %>%
  filter(dollar_per_tonne >= -500 & dollar_per_tonne <= 500)  # Filter out costs above $500

# Set the factor levels for the scenario column in the grand total_mac_various data
grand_total_mac_various$scenario <- factor(grand_total_mac_various$scenario, levels = scenario_order)

# Calculate cumulative GHG reductions after scaling for the grand total
grand_total_mac_various <- grand_total_mac_various %>%
  arrange(scenario, dollar_per_tonne) %>%
  group_by(scenario) %>%
  mutate(cumghg_gt = cumsum(scaled_ghg)) %>%
  ungroup()

# Create the plot for the grand total
plot_scaled_marginal_abatement_cost_various_scenarios <- ggplot(grand_total_mac_various, 
                                                                aes(x=cumghg_gt, y=dollar_per_tonne, color=scenario)) +  
  geom_step(size=1.2) +  # Increase the size for better visibility
  scale_color_manual(values = color_palette) +  # Use predefined color palette
  theme_light() +
  labs(x="Cumulative greenhouse gas reduction (t/yr)",
       y="Marginal abatement cost ($/t)",
       color="Scenario",
       title="Scaled Marginal Abatement Cost Curve for Canada") +
  scale_x_continuous(labels=comma_format(big.mark = ",")) +  # Format the x-axis labels
  scale_y_continuous(labels=dollar_format(), limits=c(-500, 500)) +  # Format the y-axis labels
  theme(
    legend.position = "bottom",  # Move legend to bottom
    plot.title = element_text(hjust = 0.4, size=20),  # Center-align and size the plot title
    axis.text.x = element_text(face = "bold", size = rel(0.6)),  # Bold and larger x axis labels
    axis.text.y = element_text(face = "bold", size = rel(0.6)),  # Bold and larger y axis labels
    axis.title.x = element_text(face = "bold",size = rel(0.6)),  # Bold and larger x axis title
    axis.title.y = element_text(face = "bold",size = rel(0.6)),  # Bold and larger y axis title
    legend.text = element_text(face = "bold",size = rel(0.4)),  # Reduce the size of the legend text
    legend.title = element_text(face = "bold",size = rel(0.4)),  # Reduce the size of the legend title
    legend.box.background = element_rect(colour = "grey80"),  # Add a background to the legend
    legend.key.size = unit(0.3, 'lines')  # Increase the size of the legend keys
  )  # Increase the size of the legend keys

# Optionally, you can add a more detailed title, subtitle, or caption
plot_scaled_marginal_abatement_cost_various_scenarios <- plot_scaled_marginal_abatement_cost_various_scenarios +
  ggtitle("Scaled Marginal Abatement Cost Curve for Canada") +
  theme(plot.title = element_text(size=8))

# Print the grand total plot
print(plot_scaled_marginal_abatement_cost_various_scenarios)

# Save the grand total plot with the specified dimensions
ggsave("Results_paper/Results_Costmood/grand_total_plot_scaled_marginal_abatement_cost_various_scenarios_with_NG.png", 
       plot = plot_scaled_marginal_abatement_cost_various_scenarios, 
       width = 4.71, height = 3.09, units = "in", dpi = 300)

###############################
################################
#################################
rm(list = ls())
gc()

# Load required libraries
library(dplyr)
library(readr)
library(openxlsx)
library(ggplot2)
library(scales)

# No_NG
# Define the list of provinces 
provinces <- c("ON", "BC", "MB", "NB", "NF", "NS", "AB", "PE", "QC", "SK")

# Define the desired order of scenarios
scenario_order <- c("base", 
                    "Rebate5K", 
                    "Rebate5K+lowinterest",
                    "Rebate10K", 
                    "Rebate10K+lowinterest",
                    "Rebate5K+ctax95", 
                    "Rebate10K+ctax95", 
                    "Rebate5K+ctax170", 
                    "Rebate10K+ctax170")

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

# Initialize an empty list to store the total_mac_various tibble for each province
grand_total_list <- list()

# Loop over each province
for (province in provinces) {
  # Initialize an empty list to store tibbles for the current province
  tibble_list <- list()
  
  # Construct file paths
  file_names <- c("Single_Detached_Natural_Gas_electricity",
                  "Single_Detached_Electricity_electricity",
                  "Single_Detached_Oil_electricity",
                  "Single_Attached_Natural_Gas_electricity",
                  "Single_Attached_Electricity_electricity",
                  "Single_Attached_Oil_electricity")
  
  for (file_name in file_names) {
    file_path <- paste0("Results_paper/Results_Costmood/", province, "_", file_name, "/mac_various_", province, "_", file_name, ".rds")
    
    # Read the file if it exists
    if (file.exists(file_path)) {
      tibble_list[[file_name]] <- readRDS(file_path)
    }
  }
  
  # Skip to the next province if no files are found
  if (length(tibble_list) == 0) next
  
  # Combine all tibbles in the list into one for the current province
  total_mac_various <- bind_rows(tibble_list)
  
  # Filter out rows with Inf in either dollar_per_tonne or scaled_ghg
  total_mac_various <- total_mac_various %>%
    filter(!is.infinite(dollar_per_tonne) & !is.infinite(scaled_ghg))
  
  # Add the combined tibble to the grand_total_list
  grand_total_list[[province]] <- total_mac_various
}

# Combine all provincial total_mac_various tibbles into one grand total tibble
grand_total_mac_various <- bind_rows(grand_total_list)

# Filter the grand_total_mac_various to include only the specified scenarios
grand_total_mac_various <- grand_total_mac_various %>%
  filter(scenario %in% scenario_order) %>%
  filter(dollar_per_tonne >= -500 & dollar_per_tonne <= 500)  # Filter out costs above $500

# Set the factor levels for the scenario column in the grand total_mac_various data
grand_total_mac_various$scenario <- factor(grand_total_mac_various$scenario, levels = scenario_order)

# Calculate cumulative GHG reductions after scaling for the grand total
grand_total_mac_various <- grand_total_mac_various %>%
  arrange(scenario, dollar_per_tonne) %>%
  group_by(scenario) %>%
  mutate(cumghg_gt = cumsum(scaled_ghg)) %>%
  ungroup()

# Create the plot for the grand total
plot_scaled_marginal_abatement_cost_various_scenarios <- ggplot(grand_total_mac_various, 
                                                                aes(x=cumghg_gt, y=dollar_per_tonne, color=scenario)) +  
  geom_step(size=1.2) +  # Increase the size for better visibility
  scale_color_manual(values = color_palette) +  # Use predefined color palette
  theme_light() +
  labs(x="Cumulative greenhouse gas reduction (t/yr)",
       y="Marginal abatement cost ($/t)",
       color="Scenario",
       title="Scaled Marginal Abatement Cost Curve for Canada") +
  scale_x_continuous(labels=comma_format(big.mark = ",")) +  # Format the x-axis labels
  scale_y_continuous(labels=dollar_format(), limits=c(-500, 500)) +  # Format the y-axis labels
  theme(
    legend.position = "bottom",  # Move legend to bottom
    plot.title = element_text(hjust = 0.4, size=20),  # Center-align and size the plot title
    axis.text.x = element_text(face = "bold", size = rel(0.6)),  # Bold and larger x axis labels
    axis.text.y = element_text(face = "bold", size = rel(0.6)),  # Bold and larger y axis labels
    axis.title.x = element_text(face = "bold",size = rel(0.6)),  # Bold and larger x axis title
    axis.title.y = element_text(face = "bold",size = rel(0.6)),  # Bold and larger y axis title
    legend.text = element_text(face = "bold",size = rel(0.4)),  # Reduce the size of the legend text
    legend.title = element_text(face = "bold",size = rel(0.4)),  # Reduce the size of the legend title
    legend.box.background = element_rect(colour = "grey80"),  # Add a background to the legend
    legend.key.size = unit(0.3, 'lines')  # Increase the size of the legend keys
  )  # Increase the size of the legend keys

# Optionally, you can add a more detailed title, subtitle, or caption
plot_scaled_marginal_abatement_cost_various_scenarios <- plot_scaled_marginal_abatement_cost_various_scenarios +
  ggtitle("Scaled Marginal Abatement Cost Curve for Canada") +
  theme(plot.title = element_text(size=8))

# Print the grand total plot
print(plot_scaled_marginal_abatement_cost_various_scenarios)

# Save the grand total plot with the specified dimensions
ggsave("Results_paper/Results_Costmood/grand_total_plot_scaled_marginal_abatement_cost_various_scenarios_no_NG.png", 
       plot = plot_scaled_marginal_abatement_cost_various_scenarios, 
       width = 4.71, height = 3.09, units = "in", dpi = 300)


rm(list = ls())
gc()


