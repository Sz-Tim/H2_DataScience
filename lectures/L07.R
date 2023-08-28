# H2 Data Science
# UG308490
# L07 - normal 


library(tidyverse); theme_set(theme_classic())



# normal ------------------------------------------------------------------

norm_df <- tibble(x=seq(-4, 4, by=0.01)) |>
  mutate(density=dnorm(x))
ggplot(norm_df, aes(x, density)) + 
  geom_line()
ggsave("figs/L07_norm_examp.png", width=5, height=3)


arrows <- list(mn=tibble(x0=0, x1=0, y0=0, y1=dnorm(0), label="mu"),
               sd=tibble(x0=0, x1=1, y0=dnorm(1), y1=dnorm(1), label="sigma"))
ggplot(norm_df) + 
  geom_line(aes(x, density)) + 
  geom_segment(data=arrows[[1]], aes(x=x0, xend=x1, y=y0, yend=y1),
               colour="cadetblue") +
  geom_segment(data=arrows[[2]], aes(x=x0, xend=x1, y=y0, yend=y1), 
               colour="cadetblue") +
  geom_text(data=arrows[[1]], aes(x=x0, y=y0, label=label),
            nudge_x=0.2, parse=TRUE, size=5) +
  geom_text(data=arrows[[2]], aes(x=(x0+x1)/2, y=y0, label=label),
            nudge_y=0.02, parse=TRUE, size=5)
ggsave("figs/L07_norm_examp2.png", width=4, height=2.5)



norm_obs <- tibble(x=rnorm(20), density=0.05)
ggplot(norm_df) + 
  geom_line(aes(x, density)) + 
  geom_segment(data=arrows[[1]], aes(x=x0, xend=x1, y=y0, yend=y1),
               colour="cadetblue") +
  geom_segment(data=arrows[[2]], aes(x=x0, xend=x1, y=y0, yend=y1), 
               colour="cadetblue") +
  geom_text(data=arrows[[1]], aes(x=x0, y=y0, label=label),
            nudge_x=0.2, parse=TRUE, size=5) +
  geom_text(data=arrows[[2]], aes(x=(x0+x1)/2, y=y0, label=label),
            nudge_y=0.02, parse=TRUE, size=5) + 
  geom_point(data=norm_obs, aes(x, density), shape=1, size=2)
ggsave("figs/L07_norm_examp3.png", width=4, height=2.5)
