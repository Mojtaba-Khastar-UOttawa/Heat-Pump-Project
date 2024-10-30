plot_marginal_abatement_cost <- ggplot(homes_heating %>% 
                                         arrange(dollar_per_tonne) %>% 
                                         mutate(cumghg = cumsum(ghg)), 
                                       aes(x=cumghg, y=dollar_per_tonne)) + 
  geom_step(colour="blue") + 
  geom_hline(yintercept = 0,linetype="dotted") + 
  theme_light() +
  labs(x="Cumulative greenhouse gas reduction (t/yr)",
       y="Marginal abatement cost ($/t)") +
  scale_y_continuous(labels=scales::dollar_format())
print(plot_marginal_abatement_cost)
ggsave("figures_tables/ON-SD-Gas-Bgas/plot_marginal_abatement_cost.png", width = 10, height = 10)