
# For homes where exist_furnace is not "Natural Gas", furnace_life_left will be set to NA_real_, and the annualized_furnace_cost,  will return 0, indicating no cost.

## should be chosen between two scenarios, zero life left or random life left of the furnace life
# We try two different scenarios for the remaining life of the furnaces for each house.
## 1. All houses have zero-year life left for furnaces and need new ones. 
## 2. Make this parameter different for every house in the data by drawing a random value for each home (from 1-15 years).

# for reproducible results, When using random number generators in R, setting a seed allows us to obtain the same set of random numbers each time the code is run. 

furnace_life_scenario <- "random_years" # or "zero_years"
set.seed(123) 
homes <- homes %>%
  mutate(
    furnace_life_left = case_when(
      exist_furnace == "Natural Gas" & furnace_life_scenario == "zero_years" ~ 0,
      exist_furnace == "Natural Gas" & furnace_life_scenario == "random_years" ~ round(runif(n=nrow(homes), min=1, max=15), 0),
      TRUE ~ NA_real_  # Set to NA if the existing furnace is not Natural Gas
    )
  )

upfront_furnace_cost = 5000

# Annualized cost over a 15-year horizon
furnace_equip_cost_calc <- function(furnace_life_left, upfront_furnace_cost, ir, life) {
  # Capital recovery factor
  crf_fu <- ir * (1 + ir) ^ life / ((1 + ir) ^ life - 1)
  
  # uses the purrr package's map2_dbl function to apply a function to each pair of values from furnace_life_left and ir.
  annualized_furnace_cost <- purrr::map2_dbl(furnace_life_left, ir, function(furnace_life_left, ir) {
    if (is.na(furnace_life_left)) {
      return(0)  # Return 0 if furnace is not Natural Gas
    } else {
      # Calculate cost for Natural Gas furnaces
      return(sum(
        # 0 cost for years when existing furnace is still available
        c(rep(0,furnace_life_left),
          # annualized cost for years when new furnace is required
          rep((upfront_furnace_cost*crf_fu),life - furnace_life_left)
          # discounted back to present (assume expenditure at beginning of year)
        ) / (1+ir)^(0:(life - 1)))/life)
    }
  })
  return(data.frame(annualized_furnace_cost = annualized_furnace_cost))
}

homes <- homes %>% 
  cbind(furnace_equip_cost_calc(furnace_life_left=homes$furnace_life_left, upfront_furnace_cost = 5000, ir = 0.07, life = 15))


plot_furnace_efficiency <- ggplot(homes, aes(x=furnace_efficiency)) +
  geom_histogram()
print(plot_furnace_efficiency)
ggsave("figures_tables/ON-SD-Gas-Bgas/plot_furnace_efficiency.png", width = 10, height = 10)