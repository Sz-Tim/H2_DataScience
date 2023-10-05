# H2 Data Science
# UG308490
# L09 - CLT 



library(tidyverse); theme_set(theme_classic())




# R tips: logical ---------------------------------------------------------

a <- rpois(10, 3)
b <- rpois(10, 1)

a
a > 1
sum(a > 1)
mean(a > 1)
sum(! (a > 1) )

a < 2 | a > 4
sum(a < 2 | a > 4)

# Do you want coffee or tea?
# 'inclusive or': T = 'coffee', 'tea', 'both';  F = 'none'
# 'exclusive or': T = 'coffee', 'tea';  F = 'both', 'none'
(a > 1) | (b > 1)  # TRUE if either or both are TRUE
xor(a > 1, b > 1)  # TRUE if exactly one is TRUE

a %in% c(1, 3)  # same as: (a == 1) | (a ==3)

any(a > 2)
any(b > 2)

all(a > 2)

c <- c(NA, rpois(9, 2))

is.na(c)
any(is.na(c))
!is.na(c)
c_noNA <- c[!is.na(c)]



# salmon in cage 3 --------------------------------------------------------

set.seed(1)
# This is the entire population
salmon_df <- data.frame(id=1:324801,
                        weight=rnorm(324801, 2500, 100))

# Step 1: Measure all individuals and take the mean
mean(salmon_df$weight)  # mu
sd(salmon_df$weight)  # sigma
range(salmon_df$weight)

# Step 2: Take 5 samples, each of 10 fish, and take the mean of each
for(i in 1:5) {
  fish_sample <- sample(salmon_df$weight, size=10)
  cat("Sample mean", i, "=", round(mean(fish_sample)), "g\n")
}

# Step 3:  :( 

# Step 4-5: See if there's a pattern in *how* the means are wrong
number_of_samples <- 10000
fish_means <- data.frame(sample_id=1:number_of_samples,
                         mean_n10=numeric(number_of_samples),
                         mean_n100=numeric(number_of_samples))
for(i in 1:nrow(fish_means)) {
  fish_means$mean_n10[i] <- mean(sample(salmon_df$weight, size=10))
  fish_means$mean_n100[i] <- mean(sample(salmon_df$weight, size=100))
}

mean(fish_means$mean_n10)
sd(fish_means$mean_n10)
range(fish_means$mean_n10)

mean(fish_means$mean_n100)
sd(fish_means$mean_n100)
range(fish_means$mean_n100)

p <- salmon_df |>
  ggplot() +
  geom_histogram(aes(weight), colour="grey30", fill="salmon", binwidth=10) +
  xlim(2000,3000) +
  labs(x="Weights (g) of 324,801 salmon in cage 3")
ggsave("figs/L09_salmon_raw_data.png", p, width=5, height=3, dpi=300)

p <- fish_means |>
  ggplot() +
  geom_histogram(aes(mean_n10), colour="grey30", fill="steelblue", binwidth=10) +
  xlim(2000,3000) +
  labs(x="Mean weight (g) of sample with N=10")
ggsave("figs/L09_salmon_sample_means_n10.png", p, width=5, height=3, dpi=300)

p <- fish_means |>
  ggplot() +
  geom_histogram(aes(mean_n100), colour="grey30", fill="steelblue", binwidth=10) +
  xlim(2000,3000) +
  labs(x="Mean weight (g) of sample with N=100")
ggsave("figs/L09_salmon_sample_means_n100.png", p, width=5, height=3, dpi=300)





# Example 1 ---------------------------------------------------------------

# y ~ Normal(100, 7)  with samples of n=25
y_df <- data.frame(value=seq(75, 125, length.out=1e3)) |>
  mutate(prob_value=dnorm(value, mean=100, sd=7))

ybar_df <- data.frame(value=seq(75, 125, length.out=1e3)) |>
  mutate(prob_value=dnorm(value, mean=100, sd=7/sqrt(25)))

ybar_z_df <- data.frame(value=seq(102, 105, length.out=1e2), 
                        ymin=0) |>
  mutate(ymax=dnorm(value, mean=100, sd=7/sqrt(25)))

p <- ggplot(y_df, aes(value, prob_value)) + geom_line() + 
  labs(x="Value", y="Probability density", title="Original variable y")
ggsave("figs/L09_examp1_1_ogData.png", p, width=3, height=2.5, dpi=300)

p <- ggplot(ybar_df, aes(value, prob_value)) + geom_line() + 
  labs(x="Value", y="Probability density", title=expression('Sample means'~bar(y)))
ggsave("figs/L09_examp1_2_ybars.png", p, width=3, height=2.5, dpi=300)

p <- ggplot(ybar_df, aes(value, prob_value)) + 
  geom_ribbon(data=ybar_z_df, aes(ymin=ymin, ymax=ymax), y=NA, fill="red", colour=NA) +
  geom_line() + 
  xlim(95, 105) +
  labs(x="Value", y="Probability density", title=expression('Sample means'~bar(y)))
ggsave("figs/L09_examp1_3_ybars_z.png", p, width=3, height=2.5, dpi=300)

1 - pnorm(102, 100, 7/sqrt(25))





# Example 2 ---------------------------------------------------------------

meas_error <- data.frame(value=seq(-9, 9, length.out=1e3)) |>
  mutate(prob_value=dnorm(value, mean=0, sd=3))
mean_meas <- data.frame(value=seq(-9, 9, length.out=1e3)) |>
  mutate(prob_value=dnorm(value, mean=0, sd=3/sqrt(5)))
mean_meas_z <- data.frame(value=seq(-9, 9, length.out=1e3), 
                          ymin=0) |>
  mutate(ymax=dnorm(value, mean=0, sd=3/sqrt(5))) |>
  mutate(ymax=if_else(between(value, -1, 1), 0, ymax))

p <- ggplot(meas_error, aes(value, prob_value)) + geom_line() + 
  labs(x="Measurement error", y="Probability density", title="Individual measurements")
ggsave("figs/L09_examp2_1_ogData.png", p, width=3, height=2.5, dpi=300)

p <- ggplot(mean_meas, aes(value, prob_value)) + geom_line() + 
  labs(x="Measurement error", y="Probability density", title="Means of 5 measurements")
ggsave("figs/L09_examp2_2_ybars.png", p, width=3, height=2.5, dpi=300)

p <- ggplot(mean_meas, aes(value, prob_value)) + 
  geom_ribbon(data=mean_meas_z, aes(ymin=ymin, ymax=ymax), y=NA, fill="red", colour=NA) +
  geom_line() +
  # ylim(-6, 6)
  labs(x="Value", y="Probability density", title="Means of 5 measurements")
ggsave("figs/L09_examp2_3_ybars_z.png", p, width=3, height=2.5, dpi=300)

2 * pnorm(-1, 0, 3/sqrt(5))

pnorm(-1, 0, 3/sqrt(5)) +
  pnorm(1, 0, 3/sqrt(5), lower.tail=FALSE)





# Example 3 ---------------------------------------------------------------

# y ~ Normal(36.6, 6.4)  with samples of n=4
y_df <- data.frame(value=seq(17.4, 55.8, length.out=1e3)) |>
  mutate(prob_value=dnorm(value, mean=36.6, sd=6.4))

ybar_df <- data.frame(value=seq(17.4, 55.8, length.out=1e3)) |>
  mutate(prob_value=dnorm(value, mean=36.6, sd=6.4/sqrt(4)))

ybar_z_df <- data.frame(value=seq(17.4, 30, length.out=1e2), 
                        ymin=0) |>
  mutate(ymax=dnorm(value, mean=36.6, sd=6.4/sqrt(4)))

p <- ggplot(y_df, aes(value, prob_value)) + geom_line() + 
  labs(x="Value", y="Probability density", title="Cod length")
ggsave("figs/L09_examp3_1_ogData.png", p, width=3, height=2.5, dpi=300)

p <- ggplot(ybar_df, aes(value, prob_value)) + geom_line() + 
  labs(x="Value", y="Probability density", title="Mean cod length (n=4)")
ggsave("figs/L09_examp3_2_ybars.png", p, width=3, height=2.5, dpi=300)

p <- ggplot(ybar_df, aes(value, prob_value)) + 
  geom_ribbon(data=ybar_z_df, aes(ymin=ymin, ymax=ymax), y=NA, fill="red", colour=NA) +
  geom_line() + 
  xlim(17.4, 55.8) +
  labs(x="Value", y="Probability density", title="Mean cod length (n=4)")
ggsave("figs/L09_examp3_3_ybars_z.png", p, width=3, height=2.5, dpi=300)

# proportion of *fish* < 30cm
pnorm(30, 36.6, 6.4)

# proportion of sample means < 30cm
pnorm(30, 36.6, 6.4/sqrt(4))






# Quantiles ---------------------------------------------------------------

z_df <- data.frame(z=seq(-4, 4, length.out=1e3),
                   ymin=0) |>
  mutate(prob=dnorm(z))
q_10 <- z_df |> 
  filter(z < qnorm(0.1))
q_50 <- z_df |> 
  filter(z < qnorm(0.5))
q_95 <- z_df |> 
  filter(z < qnorm(0.95))

p <- ggplot(z_df, aes(z, prob)) + 
  geom_line() + 
  labs(x="Value", y="Probability density")
ggsave("figs/L09_quantiles_1.png", p, width=3, height=2.5, dpi=300)

p <- ggplot(z_df, aes(z, prob)) + 
  geom_ribbon(data=q_10, aes(ymin=ymin, ymax=prob), fill="red", colour=NA) +
  geom_vline(xintercept=qnorm(0.1), linewidth=1) +
  geom_line() + 
  labs(x="Value", y="Probability density")
ggsave("figs/L09_quantiles_2_q10.png", p, width=3, height=2.5, dpi=300)

p <- ggplot(z_df, aes(z, prob)) + 
  geom_ribbon(data=q_50, aes(ymin=ymin, ymax=prob), fill="red", colour=NA) +
  geom_vline(xintercept=qnorm(0.5), linewidth=1) +
  geom_line() + 
  labs(x="Value", y="Probability density")
ggsave("figs/L09_quantiles_3_q50.png", p, width=3, height=2.5, dpi=300)

p <- ggplot(z_df, aes(z, prob)) + 
  geom_ribbon(data=q_95, aes(ymin=ymin, ymax=prob), fill="red", colour=NA) +
  geom_vline(xintercept=qnorm(0.95), linewidth=1) +
  geom_line() + 
  labs(x="Value", y="Probability density")
ggsave("figs/L09_quantiles_4_q95.png", p, width=3, height=2.5, dpi=300)

p <- ggplot(z_df, aes(z, prob)) + 
  geom_line() + 
  geom_vline(xintercept=qnorm(c(0.25, 0.5, 0.75)), linewidth=1) + 
  labs(x="Value", y="Probability density", title="Quartiles")
ggsave("figs/L09_quartiles.png", p, width=3, height=2.5, dpi=300)

p <- ggplot(z_df, aes(z, prob)) + 
  geom_line() + 
  geom_vline(xintercept=qnorm(c(0.2, 0.4, 0.6, 0.8)), linewidth=1) + 
  labs(x="Value", y="Probability density", title="Quintiles")
ggsave("figs/L09_quintiles.png", p, width=3, height=2.5, dpi=300)

p <- ggplot(z_df, aes(z, prob)) + 
  geom_line() + 
  geom_vline(xintercept=qnorm(seq(0.1, 0.9, by=0.1)), linewidth=1) + 
  labs(x="Value", y="Probability density", title="Deciles")
ggsave("figs/L09_dectiles.png", p, width=3, height=2.5, dpi=300)




# Example 4 ---------------------------------------------------------------

# y ~ Normal(36.6, 6.4)  with samples of n=4
y_df <- data.frame(value=seq(17.4, 55.8, length.out=1e3)) |>
  mutate(prob_value=dnorm(value, mean=36.6, sd=6.4))

ybar_df <- data.frame(value=seq(17.4, 55.8, length.out=1e3)) |>
  mutate(prob_value=dnorm(value, mean=36.6, sd=6.4/sqrt(4)))

ybar_z_df <- data.frame(value=seq(17.4, 55.8, length.out=1e3), 
                        ymin=0) |>
  mutate(ymax=dnorm(value, mean=36.6, sd=6.4/sqrt(4))) |>
  mutate(ymax=if_else(value > qnorm(0.025, 36.6, 3.2) & value < qnorm(0.975, 36.6, 3.2), ymax, 0))

p <- ggplot(y_df, aes(value, prob_value)) + geom_line() + 
  labs(x="Value", y="Probability density", title="Cod length")
ggsave("figs/L09_examp4_1_ogData.png", p, width=3, height=2.5, dpi=300)

p <- ggplot(ybar_df, aes(value, prob_value)) + geom_line() + 
  labs(x="Value", y="Probability density", title="Mean cod length (n=4)")
ggsave("figs/L09_examp4_2_ybars.png", p, width=3, height=2.5, dpi=300)

p <- ggplot(ybar_df, aes(value, prob_value)) + 
  geom_ribbon(data=ybar_z_df, aes(ymin=ymin, ymax=ymax), y=NA, fill="red", colour=NA) +
  geom_line() + 
  xlim(17.4, 55.8) +
  labs(x="Value", y="Probability density", title="Mean cod length (n=4)")
ggsave("figs/L09_examp4_3_ybars_z.png", p, width=3, height=2.5, dpi=300)

qnorm(0.025, 36.6, 3.2)
qnorm(0.975, 36.6, 3.2)






# PROBLEM SOLUTIONS -------------------------------------------------------

# The maternity unit at Oban hospital delivers 65 babies per year.

# Given that there are 5.2 million UK sufferers of asthma (population=60
# million) determine the probability of having asthma.
p_asthma <- 5.2/60

# What is an appropriate model we could use to predict the numbers of babies
# with asthma here?

# binomial seems suitable:

#   - what is the event?
# A birth -- with or without asthma

#   - how many trials are there?
# 65, assuming a single typical year

#   - what is p, what is q. 
p <- p_asthma # 0.0867
q <- 1 - p # 0.913

# What is the probability of >0 cases of paediatric asthma at Oban hospital (on
# an annual basis).

1 - dbinom(0, 65, p)

# What could be used as an alternative model and why?

# Poisson is a possibility, given n and p

#   using the alternative model, determine the probability of >0 cases of asthma
#   (on an annual basis)

1 - dpois(0, 65*p)

# At another hospital there are 110 births per year (usually).  Attempt to
# answer the question using this initial (binomial) model
1 - dbinom(0, 110, p)

# what logistics problems are you faced with

# In R, no problems at all. Calculating by hand gets difficult with a large n. I
# think this question will be altered for next year since I strongly encourage
# working in R as a general rule.
