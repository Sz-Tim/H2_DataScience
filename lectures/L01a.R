# H2 Data Science
# UG308490
# L01a - dataViz 

library(tidyverse); theme_set(theme_classic())

worm_df <- tibble(worm_id=1:9,
                  n_setei=c(2,1,1,3,2,3,1,2,2))
p <- ggplot(worm_df, aes(n_setei)) + 
  geom_bar() + 
  labs(x="Number of setei on segment 5")
ggsave("figs/L01a_bar.png", p, width=4, height=3, dpi=300, units="in")




fish_df <- tibble(fish_id=1:30,
                  length=c(rnorm(20, 25, 4), runif(10, 20, 45)))
p <- ggplot(fish_df, aes(length)) + 
  geom_histogram(binwidth=5, colour="grey30", fill="cadetblue") + 
  labs(x="Fish length (cm)", title="Lengths of 30 fish with bins of 5 cm width", 
       subtitle="Loch Linnhe (2023-04-12)")
ggsave("figs/L01a_hist.png", p, width=4, height=3, dpi=300, units="in")

