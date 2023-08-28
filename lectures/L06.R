# H2 Data Science
# UG308490
# L06 - poisson 



library(tidyverse); theme_set(theme_classic()) 





# poisson shape -----------------------------------------------------------

pois_df <- expand_grid(lambda=c(0.5, 2, 5, 10),
                       x=0:20) |>
  mutate(pr=dpois(x, lambda))
ggplot(pois_df, aes(x, pr, fill=factor(lambda))) + 
  geom_bar(stat="identity", position="dodge", colour="grey30") + 
  scale_fill_brewer(expression(lambda), type="qual", palette=2) +
  labs(x="Number of events", y="Probability") +
  theme(legend.position=c(0.8, 0.6))
ggsave("figs/L06_pois_shape.png", width=6, height=3)




# worms -------------------------------------------------------------------

# handy function: tidyr::uncount()

tibble(n_worms=0:5,
       obs_freq=c(35, 28, 15, 10, 7, 5)) |>
  uncount(obs_freq) |>
  summarise(tot=sum(n_worms),
            n_obs=n(),
            mean=mean(n_worms), 
            variance=var(n_worms))

worm_df <- tibble(n_worms=0:5,
                  obs_freq=c(35, 28, 15, 10, 7, 5)) |>
  mutate(p_x=dpois(n_worms, 1.41),
         cumul_p=cumsum(p_x),
         expected_freq=p_x*100)

worm_df |>
  select(n_worms, obs_freq, expected_freq) |>
  pivot_longer(2:3, names_to="source", values_to="Count") |>
  mutate(source=factor(source, 
                       levels=c("expected_freq", "obs_freq"),
                       labels=c("Predicted", "Observed"))) |>
  ggplot(aes(n_worms, Count, fill=source)) +
  geom_bar(stat="identity", position="dodge", colour="grey30") +
  scale_fill_viridis_d(end=0.8) +
  scale_x_continuous("Worms per quadrat", breaks=0:5) +
  theme(legend.position=c(0.8, 0.7), 
        legend.title=element_blank())
ggsave("figs/L06_pois_obs_exp.png", width=4, height=3)





# dispersion --------------------------------------------------------------

disp_df <- tibble(Random=c(2,3,0,1,1,0,2,2),
                  Dispersed=c(1,2,1,1,2,1,1,2),
                  Clumped=c(0,0,3,0,0,4,2,2))
disp_df |>
  pivot_longer(everything(), names_to="Type", values_to="Counts") |>
  ggplot(aes(Counts)) + 
  geom_hline(yintercept=0) +
  geom_bar(fill="dodgerblue2", colour="grey30") + 
  facet_wrap(~Type, ncol=1) + 
  xlab("")
ggsave("figs/L06_pois_dispersion.png", width=3, height=6)
  
  