#### Packages ####
library(tidyverse)

#### Data simulation ####
n_sims <- 10
sex <- round(runif(n_sims, min = 0, max = 1))
sex <- factor(sex, levels = 0:1, labels = c("Female", "Male"))
age <- runif(n_sims, min = 18, max = 140)
height <- runif(n_sims, min = 152.4, max = 230.0)
weight <- runif(n_sims, min = 20, max = 250)
creat <- runif(n_sims, min = 10, max = 350)

sim_popn <- tibble(id = 1:n_sims, sex, age, height, weight, creat)
