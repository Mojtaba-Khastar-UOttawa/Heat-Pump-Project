##
weather <- read_csv("figures_tables/complete_weather_all_fsa.csv") 

hourly_temperature = weather %>% pull (temperature)

# Extract a unique mapping of fsa to prov
fsa_prov_mapping <- weather %>%
  select(fsa, prov) %>%
  distinct()

# Add the prov column to homes dataset using left_join
homes <- homes %>%
  left_join(fsa_prov_mapping, by = "fsa")

##
#gets NG, oil, and elec prices based on Prov and also get ctax
energy_price <- read_excel("data/energy_price.xlsx")
##CE
#energy_price <- read_excel("data/energy_price_CE.xlsx")

homes <- homes %>%
  left_join(energy_price, by = "PROVINCE")

# ng_emit in kg per GJ, if it's NA then put <- 67.8 
# elec_emit in kg per GJ, if it's NA then put <- 30.5
# oil_emit in kg per GJ, if it's NA then put <- 75.3   
# ng_price in $/Gj, if it's NA then put <- 15 
# oil_price in $/Gj, if it's NA then put <- 30 
# elec_price in c/kWh, if it's NA then put <- 14 

ctax <- 170 # in $/t

homes <- homes %>%
  mutate(ng_carbon_charge = ng_emit * ctax / 1000,
         oil_carbon_charge = oil_emit * ctax/1000,
         elec_carbon_charge = elec_emit * ctax/1000)

##

hdh <- function(t_hat, temperature) {
  case_when(
    temperature > t_hat ~ 0,
    temperature <= t_hat ~ t_hat - temperature
  )
}

t_hat <- 16

# Pre-compute the sum of HDH for each fsa in the weather data
weather_sum_hdh <- weather %>%
  group_by(fsa) %>%
  summarize(sum_hdh = sum(hdh(t_hat, temperature), na.rm = TRUE))

# Join this with the homes data frame
homes <- homes %>%
  left_join(weather_sum_hdh, by = "fsa")

# Calculate the slope
homes <- homes %>%
  mutate(slope = energy_heating_gj / sum_hdh)

##
cdh <- function(t_hat_c, temperature) {
  case_when(
    temperature <= t_hat_c ~ 0,
    temperature > t_hat_c ~ temperature - t_hat_c
  )
}

t_hat_c <- 22 # Assuming 22 is your cooling threshold temperature

# Pre-compute the sum of CDH for each fsa in the weather data
weather_sum_cdh <- weather %>%
  group_by(fsa) %>%
  summarize(sum_cdh = sum(cdh(t_hat_c, temperature), na.rm = TRUE))

# Join this with the homes data frame
homes <- homes %>%
  left_join(weather_sum_cdh, by = "fsa")

# Calculate the cooling line slope
homes <- homes %>%
  mutate(slope_c = energy_cooling_gj / sum_cdh)

##

# Convert ymdh to date and time
weather <- weather %>%
  mutate(date = as.Date(as.character(ymdh %/% 100), format = "%Y%m%d"),
         hour = ymdh %% 100)

# Calculate daily average temperature
daily_avg_temp <- weather %>%
  group_by(fsa, date) %>%
  summarise(daily_avg = mean(temperature, na.rm = TRUE)) %>%
  ungroup()

# Calculate Heating Degree Days for each day
daily_hdd <- daily_avg_temp %>%
  mutate(hdd = ifelse(daily_avg < 16, 16 - daily_avg, 0))

# Calculate Cooling Degree Days for each day
daily_cdd <- daily_avg_temp %>%
  mutate(cdd = ifelse(daily_avg > 22,daily_avg - 22, 0))

# Sum up HDDs for each FSA
fsa_hdd <- daily_hdd %>%
  group_by(fsa) %>%
  summarise(total_hdd = sum(hdd, na.rm = TRUE)) %>%
  ungroup()

fsa_hdd

# Sum up CDDs for each FSA
fsa_cdd <- daily_cdd %>%
  group_by(fsa) %>%
  summarise(total_cdd = sum(cdd, na.rm = TRUE)) %>%
  ungroup()

fsa_cdd
# Joining the homes data with fsa_hdd based on the fsa column
homes <- homes %>%
  left_join(fsa_hdd, by = "fsa")
homes <- homes %>%
  left_join(fsa_cdd, by = "fsa")