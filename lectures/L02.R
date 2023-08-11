# H2 Data Science
# UG308490
# L02 - variableTypes 


library(tidyverse); theme_set(theme_classic())


tibble(Individual=1:4,
       Length=signif(rnorm(4, 2.2, 1)),
       Reproductive=if_else(rbinom(4,1,0.5)>0.5, "Yes", "No"))


obs_df <- data.frame(obs_id=1:20,
           species=rep(1:4, 5),
           height=rnorm(20, 4, 2)) 
obs_df |>
  glimpse()
obs_df |>
  mutate(species=factor(species)) |>
  glimpse()


tibble(student_id=runif(30, 10000000, 99999999),
       height=c(rnorm(15, 1.65, 0.09), rnorm(15, 1.75, 0.1)),
       num_teeth=sample(28:32, 30, replace=T, prob=c(0.8, rep(0.05, 4))),
       hair_colour=sample(c("Black", "Brown", "Red", "Blond"), 30, replace=T),
       left_handed=as.logical(rbinom(30, 1, prob=0.2)))








# visualizations ----------------------------------------------------------

# continuous
N <- 100
cont_df <- tibble(x=rnorm(N),
                  var="x")
p <- ggplot(cont_df) + 
  geom_point(aes(x, var), shape=1) + 
  ggtitle("Points") +
  labs(y="")
ggsave("figs/L02_viz_cont_1.png", p, width=3, height=3, dpi=300, units="in")

p <- ggplot(cont_df) + 
  geom_histogram(aes(x), bins=15) + 
  ggtitle("Histogram")
ggsave("figs/L02_viz_cont_2.png", p, width=3, height=3, dpi=300, units="in")
  
p <- ggplot(cont_df) + 
  geom_density(aes(x)) + 
  ggtitle("Density")
ggsave("figs/L02_viz_cont_3.png", p, width=3, height=3, dpi=300, units="in")

p <- ggplot(cont_df) + 
  geom_boxplot(aes(x, var)) + 
  ggtitle("Boxplot") +
  labs(y="")
ggsave("figs/L02_viz_cont_4.png", p, width=3, height=3, dpi=300, units="in")


# discrete
N <- 100
disc_df <- tibble(x=rpois(N, 2),
                  var="x")
p <- ggplot(disc_df, aes(x)) + 
  geom_bar() + 
  ggtitle("Bar plot")
ggsave("figs/L02_viz_disc_1.png", p, width=3, height=3, dpi=300, units="in")


# ordinal
N <- 100
lvls <- c("Superabundant", "Abundant", "Common", "Frequent", "Ocassional", "Rare")
probs <- runif(length(lvls))
probs <- probs/sum(probs)
ord_df <- tibble(x=factor(sample(lvls, N, prob=probs, replace=T), 
                          levels=lvls, ordered=T),
                 var="x") 
p <- ggplot(ord_df, aes(x)) +
  geom_bar() +
  scale_x_discrete(labels=c("S", "A", "C", "F", "O", "R")) +
  ggtitle("Bar plot")
ggsave("figs/L02_viz_ord_1.png", p, width=3, height=3, dpi=300, units="in")

p <- ggplot(ord_df, aes(var, fill=x)) +
  geom_bar(position="fill", colour="grey30", linewidth=0.2) +
  scale_fill_viridis_d(direction=-1) +
  ggtitle("Bar plot") + 
  labs(x="", y="Proportion")
ggsave("figs/L02_viz_ord_2.png", p, width=3, height=3, dpi=300, units="in")


# categorical
N <- 100
lvls <- paste("Species", LETTERS[1:4])
probs <- runif(length(lvls))
probs <- probs/sum(probs)
cat_df <- tibble(x=factor(sample(lvls, N, prob=probs, replace=T), 
                          levels=lvls, ordered=F),
                 var="x") 

p <- ggplot(cat_df, aes(x)) +
  geom_bar() +
  scale_x_discrete(labels=LETTERS[1:4]) +
  ggtitle("Bar plot")
ggsave("figs/L02_viz_cat_1.png", p, width=3, height=3, dpi=300, units="in")

p <- ggplot(cat_df, aes(var, fill=x)) +
  geom_bar(position="fill", colour="grey30", linewidth=0.2) +
  scale_fill_brewer(type="qual", palette=2) +
  ggtitle("Bar plot") + 
  labs(x="", y="Proportion")
ggsave("figs/L02_viz_cat_2.png", p, width=3, height=3, dpi=300, units="in")




# accuracy and precision --------------------------------------------------


bias <- c(0, 2.5) 
prec <- c(0.2, 1.2)
N <- 10

obs_df <- expand_grid(Accuracy=factor(paste(c("High", "Low"), "Accuracy")),
                      Precision=factor(paste(c("High", "Low"), "Precision")),
                      obsid=1:N) |>
  mutate(truth_x=5,
         truth_y=5,
         obs_x=rnorm(4*N, truth_x+bias[Accuracy], prec[Precision]),
         obs_y=rnorm(4*N, truth_y+bias[Accuracy], prec[Precision]),
         type=paste(Accuracy, Precision, sep="\n"))

p <- ggplot(obs_df, aes(obs_x, obs_y)) + 
  annotate("point", 5, 5, colour="red", size=2) + 
  annotate("point", 5, 5, colour="red", size=10, shape=1) + 
  annotate("point", 5, 5, colour="red", size=25, shape=1) + 
  annotate("point", 5, 5, colour="red", size=50, shape=1) + 
  geom_point() + 
  facet_grid(Accuracy~Precision) + 
  xlim(0,10) + ylim(0,10) +
  labs(x="Variable A", y="Variable B")
ggsave("figs/L02_prec_acc_bullseye.png", p, width=4, height=4, dpi=300, units="in")

N <- 1000

obs_df <- expand_grid(Accuracy=factor(paste(c("High", "Low"), "Accuracy")),
                      Precision=factor(paste(c("High", "Low"), "Precision")),
                      obsid=1:N) |>
  mutate(truth_x=5,
         truth_y=5,
         obs_x=rnorm(4*N, truth_x+bias[Accuracy], prec[Precision]),
         obs_y=rnorm(4*N, truth_y+bias[Accuracy], prec[Precision]),
         type=paste(Accuracy, Precision, sep="\n"))

p <- ggplot(obs_df, aes(obs_x)) + 
  geom_vline(xintercept=5, colour="red", linewidth=1.5) + 
  geom_density(aes(colour=type), linewidth=1) + 
  xlim(0, 10) + 
  scale_colour_brewer(type="div", palette=1) + 
  labs(x="Variable A") + 
  theme(legend.title=element_blank())
ggsave("figs/L02_prec_acc_density.png", p, width=4.5, height=3, dpi=300, units="in")
