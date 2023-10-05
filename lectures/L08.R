# H2 Data Science
# UG308490
# L08 - stdNormal 


library(tidyverse); theme_set(theme_classic())





# R tip: Piping -----------------------------------------------------------

# Takes the output from one function and feeds it to the next function
# Really shines with functions from the package dplyr, which loads with tidyverse

# Example: What is mean Sepal.Length in the iris dataset to 3 sig figs?

## With nested functions:
signif(mean(iris$Sepal.Length), 3)

## With pipes:
iris$Sepal.Length |>
  mean() |>
  signif(3)



# Example: Create a copy of iris and add columns for: 
#   Sepal.Area
#   Petal.Area
#   Petal.Area as a proportion of Sepal.Area

## Base R:
iris_df <- iris
iris_df$Sepal.Area <- iris_df$Sepal.Length * iris_df$Sepal.Width
iris_df$Petal.Area <- iris_df$Petal.Length * iris_df$Petal.Width
iris_df$Petal_prop <- iris_df$Petal.Area / iris_df$Sepal.Area
head(iris_df)


## With dplyr: 
library(tidyverse)  # or library(dplyr)
iris_df <- iris |>
  mutate(Sepal.Area = Sepal.Length * Sepal.Width,
         Petal.Area = Petal.Length * Petal.Width,
         Petal_prop = Petal.Area / Sepal.Area)
head(iris_df)


# example pops ------------------------------------------------------------

examp_df <- tibble(C=rnorm(30000, 100, 5),
                   D=rnorm(30000, 145, 15)) |>
  pivot_longer(1:2, names_to="Population", values_to="value")
ggplot(examp_df, aes(value, fill=Population)) + 
  geom_histogram(binwidth=2, colour="grey30", linewidth=0.1) + 
  scale_fill_manual(values=c("olivedrab", "steelblue")) +
  facet_wrap(~Population, ncol=1) + 
  theme(legend.position="none")
ggsave("figs/L08_examp_pops_1.png", width=4, height=6)
ggplot(examp_df, aes(value, fill=Population)) + 
  geom_histogram(binwidth=2, colour="grey30", linewidth=0.1) + 
  scale_fill_manual(values=c("olivedrab", "steelblue")) +
  facet_wrap(~Population, ncol=1, scales="free") + 
  theme(legend.position="none")
ggsave("figs/L08_examp_pops_2.png", width=4, height=6)





# example 1 ---------------------------------------------------------------

mu <- 37.6
sigma <- 1.2
y <- 38
pnorm(y, mu, sigma)
pnorm(38.0, mean=37.6, sd=1.2)

ex1_curves <- tibble(len=seq(mu-3*sigma, mu+3*sigma, length.out=1e3),
                     density=dnorm(len, mu, sigma),
                     shade=len < 38)
ggplot(ex1_curves, aes(len, ymin=0, ymax=density, fill=shade)) + 
  geom_ribbon(colour="grey30") + 
  scale_fill_manual(values=c("white", "red3"), guide="none") +
  labs(x="Herring length (cm)", y="density")
ggsave("figs/L08_examp1_1.png", width=5, height=3)



# example 2 ---------------------------------------------------------------

mu <- 37.6
sigma <- 1.2
y <- 37
1 - pnorm(37.0, mean=37.6, sd=1.2)
pnorm(37.0, mean=37.6, sd=1.2, lower.tail=FALSE)

ex2_curves <- tibble(len=seq(mu-3*sigma, mu+3*sigma, length.out=1e3),
                     density=dnorm(len, mu, sigma),
                     shade=len > 37)
ggplot(ex2_curves, aes(len, ymin=0, ymax=density, fill=shade)) + 
  geom_ribbon(colour="grey30") + 
  scale_fill_manual(values=c("white", "red3"), guide="none") +
  labs(x="Herring length (cm)", y="density")
ggsave("figs/L08_examp2_1.png", width=5, height=3)




# example 3 ---------------------------------------------------------------

mu <- 37.6
sigma <- 1.2
pnorm(38.2, mean=37.6, sd=1.2) - pnorm(37.2, mean=37.6, sd=1.2)

ex3_curves <- tibble(len=seq(mu-3*sigma, mu+3*sigma, length.out=1e3),
                     density=dnorm(len, mu, sigma),
                     shade=len > 37.2 & len < 38.2)
ggplot(ex3_curves, aes(len, ymin=0, ymax=density, fill=shade)) + 
  geom_ribbon(colour="grey30") + 
  scale_fill_manual(values=c("white", "red3"), guide="none") +
  labs(x="Herring length (cm)", y="density")
ggsave("figs/L08_examp3_1.png", width=5, height=3)





# example 4 ---------------------------------------------------------------

mu <- 37.6
sigma <- 1.2
qnorm(0.025, mean=37.6, sd=1.2)
qnorm(0.975, mean=37.6, sd=1.2)

ex4_curves <- tibble(len=seq(mu-3*sigma, mu+3*sigma, length.out=1e3),
                     density=dnorm(len, mu, sigma),
                     shade=len > qnorm(0.025, mean=37.6, sd=1.2) & 
                       len < qnorm(0.975, mean=37.6, sd=1.2))
ggplot(ex4_curves, aes(len, ymin=0, ymax=density, fill=shade)) + 
  geom_ribbon(colour="grey30") + 
  scale_fill_manual(values=c("white", "red3"), guide="none") +
  labs(x="Herring length (cm)", y="density")
ggsave("figs/L08_examp4_1.png", width=5, height=3)





# tables ------------------------------------------------------------------

table_curves <- tibble(z=seq(-3, 3, length.out=1e3),
                       density=dnorm(z),
                       shade=z > 0 & z < 0.33)
ggplot(table_curves, aes(z, ymin=0, ymax=density, fill=shade)) + 
  geom_ribbon(colour="grey30") + 
  scale_fill_manual(values=c("white", "red3"), guide="none") +
  labs(x="Herring length (cm)", y="density")
ggsave("figs/L08_table_curve.png", width=5, height=3)



# z intuition -------------------------------------------------------------

y <- 38
mu <- 37.6
sigma <- 1.2

y_rng <- tibble(y=seq(mu-3*sigma, mu+3*sigma, length.out=1e3),
                z=(y-mu)/sigma) 
mu_rng <- tibble(mu=seq(mu-3*sigma, mu+3*sigma, length.out=1e3),
                 z=(y-mu)/sigma)
sig_rng <- tibble(sigma=seq(0.2, 2.4, length.out=1e3),
                  z=(y-mu)/sigma)

ggplot(y_rng, aes(y, z)) + geom_line() +
  labs(x=expression(y[i]), y=expression(z[i]))
ggsave("figs/L08_intuition_y.png", width=2, height=2)
ggplot(mu_rng, aes(mu, z)) + geom_line() +
  labs(x=expression(mu), y=expression(z[i]))
ggsave("figs/L08_intuition_mu.png", width=2, height=2)
ggplot(sig_rng, aes(sigma, z)) + geom_line() +
  labs(x=expression(sigma), y=expression(z[i]))
ggsave("figs/L08_intuition_sigma.png", width=2, height=2)




# center and scale --------------------------------------------------------

mu <- 5
sigma <- 2.3

z_df <- tibble(original=seq(mu-4*sigma, mu+4*sigma, length.out=1e3),
               density=dnorm(original, mu, sigma),
               centered=original - mu,
               z=(original-mu)/sigma) |>
  pivot_longer(-density, names_to="Curve", values_to="x")
ggplot(z_df, aes(x, density, colour=Curve)) + 
  geom_line(linewidth=1) + 
  scale_colour_brewer(type="qual", palette=3)





# PROBLEM SOLUTIONS -------------------------------------------------------

# In a feasibility study looking at wave energy, satellite telemetry showed
# that, at production site 1, the mean wave height was 63.5 cm and the standard
# deviation was 15.8 cm
# a) If we know that a wave must be 45cm high to have enough energy to drive a
# turbine. Determine what proportion of waves will be useful for power
# generation in this location.

pnorm(45, mean=63.5, sd=15.8, lower.tail=FALSE)
# 0.879 

# b) Waves of 75 cm and greater swamp the turbine and reduce efficiency.
# Determine the proportion of waves that will fall into this category.

pnorm(75, mean=63.5, sd=15.8, lower.tail=FALSE)
# 0.233

# c) Optimal waves have a height of 55-65 cm. How many waves in 2000 fall into
# this range.
(pnorm(65, 63.5, 15.8) - pnorm(55, 63.5, 15.8)) * 2000

# d) Determine the range of heights that includes the middle 90% of the heights 
# observed?

qnorm(c(0.05, 0.95), 63.5, 15.8) # exclude 5% on either side to get middle 90%
# 37.5 - 89.5

#  Look at http://investing.calsci.com/statistics.html (and the following page).
#  Work out how unusual your height is (at least compared to the US male
#  population) (mean = 5’9”, sdev=3”).

# convert to inches: 5'9" = 5*12 + 9 = 69
# I'm actually about 5'9", so I am apparently an average American male...
pnorm(69, mean=69, sd=3) # adjust which tail depending on your height


