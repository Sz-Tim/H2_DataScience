# H2 Data Science
# UG308490
# L05 - binomial 


library(tidyverse); theme_set(theme_classic())





# example 1 ---------------------------------------------------------------

p_heads <- 0.5
n_flips <- 10

flip_set <- rbinom(n=n_flips, size=1, prob=p_heads)
flip_set
sum(flip_set)
sum(flip_set)/length(flip_set)

rbinom(n=1, size=n_flips, prob=p_heads)

n_students <- 30
flip_set_df <- tibble(student=1:n_students) |>
  mutate(n_heads=rbinom(n=n_students, size=n_flips, prob=p_heads))

ggplot(flip_set_df, aes(n_heads)) + 
  geom_bar() + 
  labs(x=paste("Number of heads out of", n_flips, "flips"), 
       y="Number of students") + 
  scale_x_continuous(limits=c(0, n_flips), breaks=0:n_flips)
ggsave("figs/L05_coinflip_bar.png", width=3.5, height=3, dpi=300)


tibble(n_heads=0:n_flips) |>
  mutate(prob=dbinom(x=n_heads, size=n_flips, prob=p_heads)) |>
  ggplot(aes(n_heads, prob)) + 
  geom_bar(stat="identity") + 
  labs(x=paste("Number of heads out of", n_flips, "flips"), 
       y="Probability") + 
  scale_x_continuous(limits=c(0, n_flips), breaks=0:n_flips)
ggsave("figs/L05_coinflip_theoretical.png", width=3.5, height=3, dpi=300)





# flip break --------------------------------------------------------------

obs_df <- tibble(
  Simulated=c(),
  Observed=c()
) |>
  mutate(student=row_number())

summary(obs_df)

obs_df |>
  pivot_longer(1:2, names_to="Source", values_to="n_heads") |>
  ggplot(aes(n_heads)) +
  geom_bar() + 
  facet_wrap(~Source) +
  labs(x="Number of heads out of 10 flips", 
       y="Frequency") 

obs_df |>
  pivot_longer(1:2, names_to="Source", values_to="n_heads") |>
  mutate(p_hat=n_heads/10) |>
  ggplot(aes(p_hat)) +
  geom_bar() + 
  facet_wrap(~Source) +
  labs(x="Proportion heads", 
       y="Frequency") 





# cards -------------------------------------------------------------------

factorial(52)/(factorial(5)*factorial(47))





# SE ----------------------------------------------------------------------

se_df <- expand_grid(p_hat=seq(0, 1, by=0.01),
                     k=c(5, 10, 30, 50, 100)) |>
  mutate(SE=sqrt((p_hat*(1-p_hat))/(k-1)))

ggplot(se_df, aes(p_hat, SE, colour=factor(k))) + 
  geom_line(linewidth=0.9) +
  scale_colour_viridis_d(expression(italic(k)), end=0.9) +
  labs(x=expression(hat(p)), y=expression('SE'[hat(p)]))
ggsave("figs/L05_SE.png", width=5.5, height=3, dpi=300)





# CI ----------------------------------------------------------------------

ci_proportion <- function(X, k, prob=0.95) {
  p_ <- (X+2) / (k+4)
  z <- abs(qnorm((1-prob)/2))
  ci <- list(lo=p_ - z * sqrt( (p_*(1-p_)) / (k+4) ),
             hi=p_ + z * sqrt( (p_*(1-p_)) / (k+4) ))
  return(ci)
}

ci_proportion(5, 10)
ci_proportion(50, 100)

true_p <- 0.35
k <- 20

ci_df <- tibble(dataset_id=1:100) |>
  mutate(x=rbinom(n=n(), size=k, prob=true_p),
         p_hat=x/k) |>
  rowwise() |>
  mutate(ci_lo=ci_proportion(x, k)$lo,
         ci_hi=ci_proportion(x, k)$hi) |>
  ungroup() |>
  mutate(true_in_CIs=ci_lo <= true_p & ci_hi >= true_p)

sum(ci_df$true_in_CIs == TRUE) / nrow(ci_df)

ggplot(ci_df, aes(dataset_id, p_hat, 
                  ymin=ci_lo, ymax=ci_hi, colour=true_in_CIs)) + 
  geom_point() + 
  geom_linerange() + 
  geom_hline(yintercept=true_p) + 
  scale_colour_manual(values=c("red2", "steelblue2")) +
  ylim(0,1) +
  theme(legend.position="bottom")
ggsave("figs/L05_CIs.png", width=7, height=3.5, dpi=300)





# birthday problem --------------------------------------------------------

bday_prob <- function(k) {
  1 - factorial(k) * choose(365, k) / 365^k
}

tibble(k=1:50) |>
  mutate(prob=bday_prob(k)) |>
  ggplot(aes(k, prob)) + 
  geom_line() +
  ylim(0, 1) + 
  labs(x="Number of people (k)", 
       y="P(at least one shared birthday)")

ggsave("figs/L05_bday.png", width=5.5, height=3, dpi=300)





# sample size -------------------------------------------------------------

kp_df <- expand_grid(k=c(10, 100),
                     p=c(0.1, 0.3, 0.5)) |>
  rowwise() |>
  mutate(x=list(0:k)) |>
  unnest(x) |>
  mutate(p_hat=x/k,
         prob=dbinom(x, k, p),
         xmin=p_hat-1/k/2,
         xmax=p_hat+1/k/2,
         k=paste("k:", k),
         p=paste("p:", p)) 

ggplot(kp_df, aes(xmin=xmin, xmax=xmax, ymin=0, ymax=prob)) + 
  geom_rect(colour="grey30", fill="steelblue", linewidth=0.1) + 
  facet_grid(k~p, scales="free_y") +
  scale_x_continuous(breaks=c(0, 0.5, 1)) + 
  labs(x=expression(hat('p')), y="density")
ggsave("figs/L05_p_sampsize.png", width=5, height=3, dpi=300)
