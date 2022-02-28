#### Packages ####
library(tidyverse)

#### Data simulation ####
set.seed(20220217)
n_sims <- 1000
sex <- round(runif(n_sims, min = 0, max = 1))
sex <- factor(sex, levels = 0:1, labels = c("Female", "Male"))
height <- runif(n_sims, min = 152.4, max = 230.0) # cm
age <- as.integer(runif(n_sims, min = 18, max = 140))
weight <- runif(n_sims, min = 20, max = 250) # kg
creat <- runif(n_sims, min = 10, max = 350) # micromol/Litre

sim_popn <- tibble(id = 1:n_sims, sex, height, age, weight, creat)

sim_popn %>%
  write_csv("data/sim_popn.csv")
