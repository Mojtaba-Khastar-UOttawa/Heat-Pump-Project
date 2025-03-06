positive_savings <- homes_heating %>%
  filter(saving > 0)
percentage_houses_positive_savings <- nrow(positive_savings)/nrow(homes_heating) * 100
cat("Percentage of houses with positive cost savings:", percentage_houses_positive_savings, "%\n")

positive_ghg <- homes_heating %>%
  filter(ghg > 0)
percentage_houses_positive_ghg <- nrow(positive_ghg)/nrow(homes_heating) * 100
cat("Percentage of houses with positive GHG savings:", percentage_houses_positive_ghg, "%\n")