
# Load available heat pumps
# CANMET heat pump study
# https://emrlibrary.gov.yk.ca/ebooks/cold-climate-air-source-heat-pumps-2022.pdf
# Manufacturer data on heat pumps: https://ashp.neep.org/#!/product_list/
available_hps <- read_excel("data/nrcan_hp_dat_extended.xlsx") %>%
  # This converts to kW of output
  mutate(hc_low = rated_output * 12000 * cf_low / 3412,
         hc_high = rated_output * 12000 * cf_high / 3412) %>%
  # This converts to GJ/hour of output
  mutate(hc_low = hc_low / 277.778,
         hc_high = hc_high / 277.778)

hpl <- available_hps %>% 
  pivot_longer(
    cols=cf_low:hc_high, 
    names_pattern = "(\\w+)_(\\w+)", 
    names_to=c("variable","load"))

#This next bit of code makes a data frame that lists heat pump operation over all possible temperatures
temperature_range_heating <- -40:40

hpn <- tibble()
for (md in unique(available_hps$model)) {
  for (var in c("hc","cop")) {
    for (ld in c("low", "high")) {
      hpo <- stats::approx(
        x=hpl %>% 
          filter(model == md, 
                 variable == var, 
                 load == ld) %>% 
          select(temperature) %>% 
          pull(),
        y=hpl %>% 
          filter(model == md, 
                 variable == var, 
                 load == ld) %>% 
          select(value) %>% 
          pull(), 
        xout=temperature_range_heating) %>%
        as_tibble()
      
      hpo <- hpo %>% 
        rename(temperature=x, 
               value=y) %>% 
        bind_cols(tibble(model=md, variable=var, load=ld))
      
      hpn <- bind_rows(hpn,hpo)
    }
  }
}

hpn <- hpn %>%
  pivot_wider(id_cols = c(model,temperature),
              names_from = c(variable,load),
              values_from = value) 

# Costs -- assumptions!
# Lifetime in the initial code was supposed 10 years which here changed to 15 years
life <- 15
ir <- 0.07
irr <- 0.06 # Interest rate reduction (low interest loan)
rebate = 6250

# This next bit of code makes tibble for hp_cost and do calculation hp_cost

hp_equip_cost_data <- tibble(
  model = c("NRCan 2.5 ton",
            "NRCan 3 ton",
            "NRCan 3.5 ton"),
  rated_output = c(2.5,3,3.5),
  # Includes warranty + labour + resistance backup
  installation_cost = 5000,
  equipment_cost = 6000 + 1000 * rated_output,
) 

hp_equip_cost_calc <- function(hp_equip_cost_data, rebate, ir, irr, life) {
  # Interest rate for heat pump
  irhp <- ir - irr
  # Capital recover factor
  crf_hp <- irhp * (1 + irhp) ^ life / ((1 + irhp) ^ life - 1)
  # Calculations
  hp_equip_cost = hp_equip_cost_data %>%
    mutate(
      total_cost = installation_cost + equipment_cost - rebate,
      annualized_hp_cost = crf_hp * total_cost
    ) %>%
    select(model, annualized_hp_cost)
  return(hp_equip_cost)
}


plot_available_hps <- ggplot(hpl, aes(x=temperature, y=value, linetype=load, colour=model, group=interaction(model,load))) +
  geom_line(linewidth=1.2) +
  facet_wrap(~variable, scales="free",ncol=1) +
  theme_bw()
print(plot_available_hps)
ggsave("figures_tables/ON-SD-Gas-Bgas/plot_available_hps.png", width = 10, height = 10)