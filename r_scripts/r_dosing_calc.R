#### Packages ####
library(tidyverse)
library(lubridate)
library(hms)

#### Data ####
sim_popn <- read_csv("data/sim_popn.csv")

#### Dosing ####
(r_results <-
  sim_popn %>%
     mutate(load_dose = case_when(weight < 60 ~ "1000mg",
                                  weight >=60 & weight <=90 ~ "1500mg",
                                  weight >90 ~ "2000mg"),
            wght_gen_cnst = case_when(sex == "Female" ~ 45.5,
                                      sex == "Male" ~ 50.0),
            ibw = wght_gen_cnst + .91*(height-152.4),
            ddw = ibw + (0.4*(weight - ibw)),
            calc_weight = if_else(weight > 1.2*ibw, true = ddw, false = weight),
            ren_gen_cnst = case_when(sex == "Female" ~ 1.04,
                                     sex == "Male" ~ 1.23),
            crcl = ((140-age)*ren_gen_cnst*calc_weight)/creat,
            # Unclear which dose should be given for some pts (e.g., crcl == 89.5)
            dose1 = case_when(crcl > 110 ~ "1500mg",
                              crcl >= 90 & crcl <= 110 ~ "1250mg",
                              crcl >= 75 & crcl < 90 ~ "1000mg",
                              crcl >= 55 & crcl < 75 ~ "750mg",
                              crcl >= 40 & crcl < 55 ~ "500mg",
                              crcl >= 30 & crcl < 40 ~ "750mg",
                              crcl >= 20 & crcl < 30 ~ "500mg",
                              crcl < 20 ~ "500mg"),
            dose2 = dose1,
            dose3 = dose2,
            dose4 = dose3,
            dose_interval = case_when(crcl > 110 ~ hours(12),
                                      crcl >= 90 & crcl <= 110 ~ hours(12),
                                      crcl >= 75 & crcl < 90 ~ hours(12),
                                      crcl >= 55 & crcl < 75 ~ hours(12),
                                      crcl >= 40 & crcl < 55 ~ hours(12),
                                      crcl >= 30 & crcl < 40 ~ hours(24),
                                      crcl >= 20 & crcl < 30 ~ hours(24),
                                      crcl < 20 ~ hours(48)),
            dose1_time = datetime + dose_interval,
            dose2_time = dose1_time + dose_interval,
            dose3_time = dose2_time + dose_interval,
            dose4_time = dose3_time + dose_interval,
            # Assuming that level before 3rd dose should show at dose3
            pre_level = case_when(crcl >= 40 ~ 3,
                                  crcl < 40 ~ 2),
            # With no modification, when would the pre-dose level be scheduled?
            pre_level_time =
              case_when(pre_level == 3 ~ as_hms(dose3_time),
                        pre_level == 2 ~ as_hms(dose2_time)),
            # What are working hours? Assumed 0600-1800
            pre_level_work_time =
              if_else(pre_level_time > hms(59, 59, 5) &
                        pre_level_time < hms(00,00,18), TRUE, FALSE),
            # For levels 1-5, on the dosing ladder the pre-dose level should
             # ideally be done in working hours. Given the default is to take it
             # at the 3rd dose, if this is outside working hours then it should
             # be moved to the 4th dose.
            # For levels 6-8, the gap between doses is 24 or 48 hours. Unless
             # this dose time is moved within working hours, and therefore,
             # away from the protocol, the pre-dose level will always be outside
             # work hours, therefore should remain at 2
            pre_level_before =
              ifelse(dose_interval == period(12, "hours") & pre_level_work_time == FALSE,
                     pre_level + 1, pre_level)
     ))


### Check for missing values ###
r_results %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  rowSums()


#### Save results ####
r_results %>%
  write_csv("data/r_results.csv")

