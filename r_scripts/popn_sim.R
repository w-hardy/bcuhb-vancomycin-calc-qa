#### Packages ####
library(tidyverse)
library(readxl)
library(writexl)

#### Data simulation ####
set.seed(20220217)
n_sims <- 100000
sex <- round(runif(n_sims, min = 0, max = 1))
sex <- factor(sex, levels = 0:1, labels = c("Female", "Male"))
height <- runif(n_sims, min = 152.4, max = 230.0) # cm
age <- as.integer(runif(n_sims, min = 18, max = 140))
weight <- runif(n_sims, min = 20, max = 250) # kg
creat <- runif(n_sims, min = 10, max = 350) # micromol/Litre
datetime <-
  sample(seq.POSIXt(ISOdate(2020,1,1), ISOdate(2030,1,1), by="min"),
         n_sims, replace = TRUE) %>%
  as.character() %>%
  str_remove_all(" GMT")

sim_popn <- tibble(id = 1:n_sims, sex, height, age, weight, creat, datetime)

#### Write date to file ####
sim_popn %>%
  write_csv("data/sim_popn.csv")

sim_popn %>%
  write_xlsx("data/sim_popn.xlsx")
