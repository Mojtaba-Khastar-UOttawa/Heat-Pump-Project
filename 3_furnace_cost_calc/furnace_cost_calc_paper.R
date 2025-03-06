
# For homes where exist_furnace is not "Natural Gas", furnace_life_left will be set to NA_real_, and the annualized_furnace_cost,  will return 0, indicating no cost.

## should be chosen between two scenarios, zero life left or random life left of the furnace life
# We try two different scenarios for the remaining life of the furnaces for each house.
## 1. All houses have zero-year life left for furnaces and need new ones. 
## 2. Make this parameter different for every house in the data by drawing a random value for each home (from 1-15 years).

# for reproducible results, When using random number generators in R, setting a seed allows us to obtain the same set of random numbers each time the code is run. 



furnace_life_scenario <- "zero_years" # or "random_years"
set.seed(123)

# 1. Determine furnace life left based on scenario and furnace type
homes <- homes %>%
  mutate(
    furnace_life_left = case_when(
      exist_furnace %in% c("Natural Gas", "Oil") & furnace_life_scenario == "zero_years" ~ 0,
      exist_furnace %in% c("Natural Gas", "Oil") & furnace_life_scenario == "random_years" ~ round(runif(n=nrow(homes), min=1, max=15), 0),
      TRUE ~ NA_real_  
    )
  )

# 2. Define a function to calculate annualized furnace cost for each home
furnace_equip_cost_calc <- function(furnace_life_left, furnace_type, ir = 0.07, life = 15) {
  
  # Determine upfront furnace cost based on type
  upfront_furnace_cost <- ifelse(furnace_type == "Natural Gas", 5000,
                                 ifelse(furnace_type == "Oil", 4000, NA))
  
  # Capital recovery factor (CRF) - same for all furnaces
  crf_fu <- ir * (1 + ir)^life / ((1 + ir)^life - 1)
  
  # If furnace type is neither Natural Gas nor Oil, no cost:
  if (is.na(upfront_furnace_cost)) {
    return(0)
  }
  
  # If furnace_life_left is NA, no cost:
  if (is.na(furnace_life_left)) {
    return(0)
  }
  
  # Calculate the annualized cost:
  # - For years furnace_life_left: cost = 0 (existing furnace still runs)
  # - For remaining years: cost = upfront_furnace_cost * CRF_fu, discounted
  discounted_costs <- c(
    rep(0, furnace_life_left),
    rep((upfront_furnace_cost * crf_fu), life - furnace_life_left)
  ) / (1 + ir)^(0:(life - 1))
  
  annualized_cost <- sum(discounted_costs) / life
  return(annualized_cost)
}

# 3. If AC is present (non-NA and not zero), we do a similar calculation for AC
# We can define a similar function for AC:
ac_equip_cost_calc <- function(ac_cop, ir = 0.07, life = 15) {
  
  # If ac_cop is NA or 0, no AC cost:
  if (is.na(ac_cop) || ac_cop == 0) {
    return(0)
  }
  
  # Assume upfront AC cost = 4000
  upfront_ac_cost <- 4000
  
  # Capital recovery factor for AC:
  crf_ac <- ir * (1 + ir)^life / ((1 + ir)^life - 1)
  
  # AC replacement is assumed immediate since no scenario given:
  # all years incur AC cost
  discounted_ac_costs <- rep((upfront_ac_cost * crf_ac), life) / (1 + ir)^(0:(life - 1))
  
  annualized_ac_cost <- sum(discounted_ac_costs) / life
  return(annualized_ac_cost)
}

# 4. Apply the calculations to the homes dataset
homes <- homes %>%
  rowwise() %>%
  mutate(
    annualized_furnace_cost = furnace_equip_cost_calc(furnace_life_left, exist_furnace, ir = 0.07, life = 15),
    annualized_ac_cost = ac_equip_cost_calc(ac_cop, ir = 0.07, life = 15)
  ) %>%
  ungroup()


