# 10: Plot marginal abatement cost


# Create the marginal abatement cost plot
plot_marginal_abatement_cost <- ggplot(homes_heating %>% 
                                         arrange(dollar_per_tonne) %>% 
                                         mutate(cumghg = cumsum(ghg)), 
                                       aes(x = cumghg, y = dollar_per_tonne)) + 
  geom_step(colour = "blue") + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  theme_light() +
  labs(x = "Cumulative greenhouse gas reduction (t/yr)",
       y = "Marginal abatement cost ($/t)") +
  scale_y_continuous(labels = scales::dollar_format())

# Print the plot for visual confirmation
print(plot_marginal_abatement_cost)

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
fig_name <- paste0("marginal_abatement_cost_", prov, "_", house_type, "_", exist_furnace, "_", hp_backup, ".png")

# Save the plot to the defined directory
ggsave(filename = paste0(dir_path, fig_name),
       plot = plot_marginal_abatement_cost,
       width = 10, height = 6, dpi = 300)