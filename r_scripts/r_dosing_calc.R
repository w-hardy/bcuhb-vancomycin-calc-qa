#### Packages ####
library(tidyverse)

#### Data ####
sim_popn <- read_csv("data/sim_popn.csv")

#### Dosing ####
sim_popn %>%
  mutate(ideal_weight =
           # Pai2000 - which versions should I be using? People under 5' (152.4cm)?
           # 2.54 to convert cm to inches
           case_when(sex == "Male" ~ 50 + 2.3 * (height / 2.54 - 60),
                     sex == "Female" ~ 45.5 + 2.3 * (height / 2.54 - 60)),
         dosing_weight = if_else(weight < ideal_weight, weight,
                                 ideal_weight + (0.4 * (weight - ideal_weight))),
         # What is the correct formula and conversion for eGFR?
         # Cockcroft1976 0.0113 to convert from micromol/Litre to mg/100ml?
         egfr = (140 - age) * weight / 72 * 0.0113 * creat,
         egfr = case_when(sex == "Male" ~ egfr,
                          sex == "Female" ~ 0.85 * egfr),
         # Dosing information take from Thomson et al. (2009) - https://academic.oup.com/jac/article/63/5/1050/715674
         inital_dose_min = weight * 15,
         initial_dose_max = weight * 20,
         maintainance_dose_min = case_when(between(egfr, 20, 49) ~ 15 * weight),
         maintainance_dose_max = case_when(between(egfr, 20, 49) ~ 20 * weight)
         )