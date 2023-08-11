# H2 Data Science
# UG308490
# L03 - Descriptive stats 

library(tidyverse); theme_set(theme_classic())




# income distribution -----------------------------------------------------

income_df <- tibble(income_lb=c(12500, 15000, 20000, 30000, 40000, 50000, 70000, 
                                100000, 150000, 200000, 300000, 500000, 1000000),
                    n_indiv=c(3050, 6320, 9260, 5200, 3100, 2510, 1170, 
                              570, 194, 135, 77, 39, 19)) |>
  mutate(income_lb=income_lb/1000,
         income_ub=lead(income_lb)) |>
  mutate(income_rng=if_else(is.na(income_ub), 
                            paste0(income_lb, "+"),
                            paste0(income_lb, " - ", income_ub))) |>
  mutate(income_rng=factor(income_rng, levels=unique(income_rng)))
p <- ggplot(income_df, aes(income_rng, n_indiv)) + 
  geom_bar(stat="identity", colour="grey30", fill="steelblue2") + 
  labs(x="Pre-tax income (1,000s GBP)", y="Number of individuals (1,000s)",
       title="UK Household Income 2020-2021")
ggsave("figs/L03_income-1.png", p, width=9, height=5, dpi=300)

p <- income_df |>
  mutate(income_ub=if_else(is.na(income_ub), 2000, income_ub)) |>
  ggplot(aes(xmin=income_lb, xmax=income_ub, ymin=0, ymax=n_indiv)) + 
  geom_rect(colour="grey30", fill="steelblue2") + 
  labs(x="Pre-tax income (1,000s GBP)", y="Number of individuals (1,000s)",
       title="UK Household Income 2020-2021")
ggsave("figs/L03_income-2.png", p, width=9, height=5, dpi=300)




# central tendency --------------------------------------------------------

# Median
y1 <- c(4, 3, 5, 1, 3, 2, 4)
sort(y1)
length(y1)
median(y1)
sort(y1)[(length(y1)+1)/2]

y2 <- c(4, 3, 5, 1, 3, 2 ,4, 5)
sort(y2)
length(y2)
median(y2)
sort(y2)[length(y2)/2]

med_df <- tibble(id=1:31,
                 y3=rnorm(31)) |>
  mutate(rank=min_rank(y3))
p <- ggplot(med_df, aes(id, y3)) + 
  geom_point()
ggsave("figs/L03_median_1.png", p, width=3, height=3, dpi=300)
p <- ggplot(med_df, aes(rank, y3)) + 
  geom_point() +
  geom_hline(yintercept=median(med_df$y3), linetype=2) +
  geom_vline(xintercept=(nrow(med_df)+1)/2, linetype=2)
ggsave("figs/L03_median_2.png", p, width=3, height=3, dpi=300)



# Mode
mu <- c(10, 18)
N <- 100
mode_df <- tibble(sizeCat=sample(1:2, N, replace=T, prob=c(0.5,0.5)),
                  x=rnorm(N, mu[sizeCat], 2))
p <- ggplot(mode_df, aes(x)) + 
  geom_histogram(bins=15, fill="cadetblue4", colour="grey30") + 
  labs(x="Weight (kg)")
ggsave("figs/L03_multimodal-1.png", p, width=4, height=3, dpi=300)
p <- ggplot(mode_df, aes(x, y=1)) + 
  geom_boxplot(fill="cadetblue4", colour="grey30") + 
  labs(x="Weight (kg)") + 
  ylim(0,2) + 
  theme(axis.title.y=element_blank(), 
        axis.text.y=element_blank())
ggsave("figs/L03_multimodal-2.png", p, width=4, height=3, dpi=300)




# dispersion --------------------------------------------------------------

# IQR
iqr_df <- tibble(x=rnorm(100))
iqr_sum <- iqr_df |>
  summarise(Q1=quantile(x, type=6, prob=0.25),
            med=median(x),
            Q3=quantile(x, type=6, prob=0.75),
            IQR=IQR(x, type=6)) |>
  mutate(y=-0.01)
p <- ggplot(iqr_df) + 
  geom_density(aes(x)) + 
  geom_rug(aes(x), sides="b", alpha=0.3) +
  geom_point(data=iqr_sum, aes(med,y), size=2) + 
  geom_errorbarh(data=iqr_sum, aes(xmin=Q1, xmax=Q3, y=y), linewidth=1, height=0.02) + 
  geom_text(data=iqr_sum, aes(Q1, y), label="Q1", nudge_y=0.025) +
  geom_text(data=iqr_sum, aes(Q3, y), label="Q3", nudge_y=0.025) +
  labs(x="Observations", y="Density")
ggsave("figs/L03_IQR.png", p, width=6, height=2.5, dpi=300)




# L3 example data ---------------------------------------------------------

N <- 100
L3_df <- tibble(id=1:N,
                y1=rnorm(N, 34, 2),
                y2=rpois(N, 3),
                y3=rbinom(N, 5, 0.2),
                y4=rlnorm(N, log(34), log(2))) |>
  pivot_longer(starts_with("y"), names_to="Variable", values_to="Value")

L3_cenTen <- L3_df |>
  group_by(Variable) |>
  reframe(tibble(Value_summary=c(mean(Value), median(Value)),
                 metric=c("mean", "median")))

L3_disp <- L3_df |>
  group_by(Variable) |>
  reframe(tibble(Value_summary=c(diff(range(Value)), IQR(Value, type=6), sd(Value)),
                 metric=c("Range", "IQR", "sd"))) |>
  left_join(L3_cenTen |> 
              pivot_wider(names_from="metric", values_from="Value_summary")) |>
  left_join(L3_df |> group_by(Variable) |> 
              summarise(min=min(Value), max=max(Value),
                        q1=quantile(Value, probs=0.25, type=6),
                        q3=quantile(Value, probs=0.75, type=6))) |>
  mutate(val_min=case_when(metric=="Range" ~ min,
                           metric=="IQR" ~ q1,
                           metric=="sd" ~ mean - Value_summary),
         val_max=case_when(metric=="Range" ~ max,
                           metric=="IQR" ~ q3,
                           metric=="sd" ~ mean + Value_summary),
         y=case_when(metric=="Range" ~ -1,
                     metric=="IQR" ~ -2,
                     metric=="sd" ~ -3))

p <- L3_df |>
  filter(Variable=="y1") |>
  ggplot(aes(id, Value)) + 
  geom_rug(sides="l", alpha=0.25) +
  geom_point()
ggsave("figs/L03_y_01_data.png", p, width=4, height=4, dpi=300)
p <- L3_df |>
  filter(Variable=="y1") |>
  ggplot(aes(id, Value)) + 
  geom_rug(sides="l", alpha=0.25) +
  geom_point() +
  geom_hline(data=L3_cenTen |> filter(Variable=="y1"),
             aes(colour=metric, yintercept=Value_summary)) +
  scale_colour_manual(values=c("red", "red4")) 
ggsave("figs/L03_y_02_central.png", p, width=5, height=4, dpi=300)

p <- L3_df |>
  ggplot(aes(id, Value)) + 
  geom_rug(sides="l", alpha=0.25) +
  geom_point() +
  geom_hline(data=L3_cenTen,
             aes(colour=metric, yintercept=Value_summary), linewidth=1) +
  scale_colour_manual(values=c("red", "red4")) +
  facet_wrap(~Variable, scales="free_y")
ggsave("figs/L03_y_03_all-y-pts.png", p, width=8, height=6, dpi=300)

p <- L3_df |>
  ggplot(aes(Value)) + 
  geom_histogram(bins=15, colour="grey30", linewidth=0.2, fill="steelblue3") +
  geom_vline(data=L3_sum,
             aes(colour=metric, xintercept=Value_summary), linewidth=1) +
  scale_colour_manual(values=c("red", "red4")) +
  facet_wrap(~Variable, scales="free_x")
ggsave("figs/L03_y_04_all-y-hist.png", p, width=8, height=6, dpi=300)

p <- L3_df |>
  filter(Variable=="y1") |>
  mutate(mean=mean(Value)) |>
  ggplot(aes(id)) +
  geom_point(aes(y=Value)) + 
  geom_hline(aes(yintercept=first(mean)), colour="red4", linewidth=1) +
  geom_linerange(aes(ymin=Value, ymax=mean, group=id), colour="cadetblue")
ggsave("figs/L03_y_05_sumOfSquares.png", p, width=5, height=4, dpi=300)

L3_df |>
  filter(Variable=="y1") |>
  mutate(mean=mean(Value),
         diff=Value-mean) 

L3_df |>
  filter(Variable=="y1") |>
  mutate(mean=mean(Value),
         diff=Value-mean) |>
  summarise(tot_diff=sum(diff))

L3_df |>
  filter(Variable=="y1") |>
  mutate(mean=mean(Value),
         diff=Value-mean) |>
  summarise(tot_diff=sum(diff),
            tot_abs_diff=sum(abs(diff)),
            tot_sq_diff=sum(diff^2))

p <- L3_df |>
  ggplot() + 
  geom_histogram(aes(Value), bins=15, colour="grey30", linewidth=0.2, fill="steelblue3") +
  geom_errorbarh(data=L3_disp, height=3, linewidth=1.5, 
                aes(colour=metric, y=y*2, xmin=val_min, xmax=val_max)) +
  scale_colour_brewer(type="qual", palette="Paired") +
  facet_wrap(~Variable, scales="free_x") + 
  ylab("Count")
ggsave("figs/L03_y_06_all-y-disp.png", p, width=8, height=6, dpi=300)


SS <- function(x) {
  sum((x-mean(x))^2)
}

L3_df |>
  filter(Variable=="y1") |>
  reframe(tibble(variance=c(var(Value),
                            SS(Value)/n(),
                            SS(Value)/(n()-1)),
                 std_dev=c(sd(Value),
                           sqrt(SS(Value)/n()),
                           sqrt(SS(Value)/(n()-1))),
                 type=c("R", "population", "sample")))





# crowd size --------------------------------------------------------------

crowd_df <- tibble(date=ymd("2023-05-01") + (0:25)*7) |>
  mutate(crowd_size=rnorm(n(), 500, sqrt(5000)))

crowd_df |> print(n=4)

crowd_df |>
  summarise(mean=mean(crowd_size), variance=var(crowd_size))

p <- ggplot(crowd_df, aes(crowd_size)) + 
  geom_histogram(bins=10, colour="grey30", fill="cadetblue") 
ggsave("figs/L03_crowd_size.png", p, width=5, height=3, dpi=300)




# L3 dispersion -----------------------------------------------------------

disp_df <- tibble(id=1:N,
                  y1=rnorm(N, 34, 1)) |>
  mutate(y2=c(y1[-c(1:10)], rnorm(10, 34, 5)),
         y3=c(y1[-c(1:10)], rnorm(10, 34, 10))) |>
  pivot_longer(starts_with("y"), names_to="Variable", values_to="Value")

disp_cenTen <- disp_df |>
  group_by(Variable) |>
  reframe(tibble(Value_summary=c(mean(Value), median(Value)),
                 metric=c("mean", "median")))

disp_disp <- disp_df |>
  group_by(Variable) |>
  reframe(tibble(Value_summary=c(diff(range(Value)), IQR(Value, type=6), sd(Value)),
                 metric=c("Range", "IQR", "sd"))) |>
  left_join(disp_cenTen |> 
              pivot_wider(names_from="metric", values_from="Value_summary")) |>
  left_join(disp_df |> group_by(Variable) |> 
              summarise(min=min(Value), max=max(Value),
                        q1=quantile(Value, probs=0.25, type=6),
                        q3=quantile(Value, probs=0.75, type=6))) |>
  mutate(val_min=case_when(metric=="Range" ~ min,
                           metric=="IQR" ~ q1,
                           metric=="sd" ~ mean - Value_summary),
         val_max=case_when(metric=="Range" ~ max,
                           metric=="IQR" ~ q3,
                           metric=="sd" ~ mean + Value_summary),
         y=case_when(metric=="Range" ~ -1,
                     metric=="IQR" ~ -2,
                     metric=="sd" ~ -3))

p <- disp_df |>
  ggplot() + 
  geom_histogram(aes(Value), bins=20, colour="grey30", linewidth=0.2, fill="steelblue3") +
  geom_errorbarh(data=disp_disp, height=5, linewidth=1.5, 
                 aes(colour=metric, y=y*4, xmin=val_min, xmax=val_max)) +
  scale_colour_brewer(type="qual", palette="Paired") +
  facet_wrap(~Variable, scales="free_y", ncol=1) + 
  ylab("Count")
ggsave("figs/L03_y_06_disp_increase.png", p, width=5, height=6, dpi=300)







# Population v sample -----------------------------------------------------

# Population measurements: All salmon in Cage 3
salmon_df <- tibble(id=1:324801) |>
  mutate(weight=rnorm(n(), 2500, 100))

ggplot(salmon_df) +
  geom_histogram(aes(weight), 
                 colour="grey30",
                 fill="salmon") +
  labs(x="Weight (g)")

# Scenario 1: Population mean
salmon_mu <- mean(salmon_df$weight)

ggplot(salmon_df) +
  geom_histogram(aes(weight), 
                 colour="grey30",
                 fill="salmon") +
  geom_vline(xintercept=salmon_mu, 
             linewidth=1) +
  annotate("text", x=2750, y=35000, parse=T,
           label=paste0('mu ==', 
                        round(salmon_mu))) +
  labs(x="Weight (g)")

# Scenario 2: Sample mean of 100 fish
salmon_samp_df <- salmon_df |>
  sample_n(100)
salmon_ybar <- mean(salmon_samp_df$weight)

ggplot(salmon_df) +
  geom_histogram(aes(weight), 
                 colour="grey30",
                 fill="salmon") +
  geom_vline(xintercept=salmon_ybar, 
             linewidth=1, linetype=2) +
  annotate("text", x=2750, y=32500, parse=T,
           label=paste0('bar(y) ==', 
                        round(salmon_ybar))) +
  labs(x="Weight (g)")



p <- salmon_df |>
  ggplot() +
  geom_histogram(aes(weight), colour="grey30", fill="salmon") +
  labs(x="Weight (g)")
ggsave("figs/L03_salmon_1.png", p, width=5, height=3, dpi=300)

p <- ggplot(salmon_df) +
  geom_histogram(aes(weight),  colour="grey30", fill="salmon") +
  geom_vline(xintercept=salmon_mu, linewidth=1) +
  annotate("text", x=2850, y=35000, parse=T, size=6,
           label=paste0('mu ==', round(salmon_mu))) +
  labs(x="Weight (g)", y="count")
ggsave("figs/L03_salmon_2.png", p, width=5, height=3, dpi=300)

p <- ggplot(salmon_df) +
  geom_histogram(aes(weight), colour="grey30", fill="salmon") +
  geom_vline(xintercept=salmon_mu, linewidth=1) +
  geom_vline(xintercept=salmon_ybar, linewidth=1, linetype=2) +
  annotate("text", x=2850, y=35000, parse=T, size=6,
           label=paste0('mu ==', round(salmon_mu))) +
  annotate("text", x=2850, y=30000, parse=T, size=6,
           label=paste0('bar(y) ==', round(salmon_ybar))) +
  labs(x="Weight (g)", y="count")
ggsave("figs/L03_salmon_3.png", p, width=5, height=3, dpi=300)





# Code to iterate ---------------------------------------------------------

N <- 100

L3_df <- tibble(id=1:N,
                y1=rnorm(N, 34, 2),
                y2=rpois(N, 3),
                y3=rbinom(N, 5, 0.2),
                y4=rlnorm(N, log(34), log(2))) |>
  pivot_longer(starts_with("y"), names_to="Variable", values_to="Value")

L3_cenTen <- L3_df |>
  group_by(Variable) |>
  reframe(tibble(Value_summary=c(mean(Value), median(Value)),
                 metric=c("mean", "median")))

L3_df |>
  ggplot() + 
  geom_histogram(aes(x=Value), bins=15, colour="grey30", fill="steelblue3") +
  geom_vline(data=L3_cenTen,
             aes(colour=metric, xintercept=Value_summary), linewidth=1) +
  scale_colour_manual(values=c("red", "red4")) +
  facet_wrap(~Variable, scales="free_x")

