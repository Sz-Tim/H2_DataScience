# H2 Data Science
# UG308490
# L01 - introduction 



library(tidyverse); theme_set(theme_classic())

# example continuous predictor
N <- 40
b <- runif(2, -2, 2)
sigma <- runif(1, 0.2, 1)
reg_df <- tibble(x=runif(N, -2, 2),
                 y=rnorm(N, b[1] + x*b[2], sigma))
reg.gg <- ggplot(reg_df, aes(x, y)) + 
  stat_smooth(method="lm", formula=y~x, colour="cadetblue", linewidth=0.7) + 
  geom_point(shape=1) + 
  labs(x="Predictor", y="Response")
ggsave("figs/L01_regression_examp.png", reg.gg, width=3, height=3)


# example categorical predictor
N <- 60
g <- 4
b <- runif(g, -2, 2)
sigma <- runif(1, 0.5, 1)
aov_df <- tibble(x=rep(1:g, each=N),
                 x_fac=factor(x, levels=1:g, labels=letters[1:g]),
                 y=rnorm(N*g, b[x], sigma))

aov.gg <- ggplot(aov_df, aes(x_fac, y, colour=x_fac)) + 
  geom_boxplot(colour="grey30", outlier.colour=NA) + 
  geom_jitter(shape=1, width=0.2) + 
  scale_colour_brewer("Treatment", type="qual", palette="Paired") +
  labs(x="Predictor", y="Response")
ggsave("figs/L01_anova_examp.png", aov.gg, width=4, height=3)
