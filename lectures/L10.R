# H2 Data Science
# UG308490
# L10 - confIntervals 



library(tidyverse); theme_set(theme_classic())




# R tip: ------------------------------------------------------------------






# 95% CIs -----------------------------------------------------------------

mu <- 0
sigma <- 1
N <- 4
norm_t_df <- bind_rows(
  data.frame(value=seq(-3, 3, by=0.01)) |>
    mutate(distribution="Normal",
           density=dnorm(value, mu, sigma),
           region=if_else(between(value, qnorm(0.025), qnorm(0.975)), 
                          "Middle 95%", "Extreme 5%")),
  data.frame(value=seq(-3, 3, by=0.01)) |>
    mutate(distribution="t(df=3)",
           density=dt(value, N-1),
           region=if_else(between(value, qt(0.025, N-1), qt(0.975, N-1)), 
                          "Middle 95%", "Extreme 5%"))
)

p <- ggplot(norm_t_df) +
  geom_ribbon(aes(value, ymin=0, ymax=density, fill=region), colour="grey30") +
  scale_fill_manual(values=c("red3", "grey")) + 
  facet_grid(distribution~.) + 
  theme(legend.position=c(0.1, 0.925))
ggsave("figs/L10_norm_v_t.png", p, width=4, height=5, dpi=300)


norm_t_df <- bind_rows(
  data.frame(value=seq(-6, 6, by=0.01)) |>
    mutate(distribution="Normal",
           density=dnorm(value, mu, sigma),
           region=if_else(between(value, qnorm(0.025), qnorm(0.975)), 
                          "Middle 95%", "Extreme 5%")),
  data.frame(value=seq(-6, 6, by=0.01)) |>
    mutate(distribution="t(df=2)",
           density=dt(value, 2),
           region=if_else(between(value, qt(0.025, 2), qt(0.975, 2)), 
                          "Middle 95%", "Extreme 5%")),
  data.frame(value=seq(-6, 6, by=0.01)) |>
    mutate(distribution="t(df=10)",
           density=dt(value, 10),
           region=if_else(between(value, qt(0.025, 10), qt(0.975, 10)), 
                          "Middle 95%", "Extreme 5%"))
) |>
  mutate(low=value < 0)
p <- ggplot(norm_t_df) +
  geom_ribbon(data=norm_t_df |> filter(low & region=="Extreme 5%"),
              aes(value, ymin=0, ymax=density, fill=distribution), 
              colour="grey30", alpha=0.5) +
  geom_ribbon(data=norm_t_df |> filter(!low & region=="Extreme 5%"),
              aes(value, ymin=0, ymax=density, fill=distribution), 
              colour="grey30", alpha=0.5) +
  geom_line(aes(value, density, colour=distribution), linewidth=0.8) + 
  scale_colour_viridis_d(end=0.9) + 
  scale_fill_viridis_d(end=0.9) + 
  theme(legend.position=c(0.15, 0.75)) +
  scale_x_continuous(breaks=-6:6)
ggsave("figs/L10_norm_v_t_2.png", p, width=6, height=4, dpi=300)




# example values ----------------------------------------------------------

qnorm(0.975, mean=0, sd=1)
qt(0.975, df=100)
qt(0.975, df=2)


# Example 1 - Fisheries inspector -----------------------------------------

T_stat <- (35.8 - 36.6) / (1.2/sqrt(30))
T_stat
2 * pt(T_stat, 29)

ex1_df <- data.frame(value=seq(-4, 4, by=0.01)) |>
  mutate(density=dt(value, 29),
         region=if_else(between(value, qt(0.025, 29), qt(0.975, 29)), 
                        "Middle 95%", "Extreme 5%"))

p <- ggplot(ex1_df) +
  geom_ribbon(aes(value, ymin=0, ymax=density, fill=region), colour="grey30") + 
  geom_vline(xintercept=T_stat, linewidth=1) + 
  scale_fill_manual(values=c("red3", "grey"), guide="none") +
  scale_x_continuous(breaks=-4:4)
ggsave("figs/L10_ex1.png", p, width=5, height=4, dpi=300)



# Example 2 - River runoff  -----------------------------------------

T_stat <- (15.11 - 15.00) / (0.3/sqrt(20))
T_stat
pt(T_stat, 19, lower.tail=F)

ex2_df <- data.frame(value=seq(-4, 4, by=0.01)) |>
  mutate(density=dt(value, 19),
         region=if_else(value < qt(0.95, 19), 
                        "Lower 95%", "Extreme 5%"))

p <- ggplot(ex2_df) +
  geom_ribbon(aes(value, ymin=0, ymax=density, fill=region), colour="grey30") + 
  geom_vline(xintercept=T_stat, linewidth=1) + 
  scale_fill_manual(values=c("red3", "grey"), guide="none") +
  scale_x_continuous(breaks=-4:4)
ggsave("figs/L10_ex2.png", p, width=5, height=4, dpi=300)
