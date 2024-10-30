# Introduce scaled up columns for both dollar_per_tonne and ghg based on scale_factor_final
homes_heating <- homes_heating %>%
  mutate(scaled_ghg = ghg * scale_factor_final)


plot_scaled_marginal_abatement_cost <- ggplot(homes_heating %>% 
                                                arrange(dollar_per_tonne) %>% 
                                                mutate(cumghg = cumsum(scaled_ghg)), 
                                              aes(x=cumghg, y=dollar_per_tonne)) + 
  geom_step(colour="blue") + 
  geom_hline(yintercept = 0, linetype="dotted") + 
  theme_light() +
  labs(x="Cumulative greenhouse gas reduction (t/yr)",
       y="Marginal abatement cost ($/t)") +
  scale_x_continuous(labels=comma_format(big.mark = ",")) +
  scale_y_continuous(labels=dollar_format())

print(plot_scaled_marginal_abatement_cost)
ggsave("figures_tables/ON-SD-Gas-Bgas/plot_scaled_marginal_abatement_cost.png", width = 10, height = 10)