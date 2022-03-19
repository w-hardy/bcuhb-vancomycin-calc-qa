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
            calc_weight = if_else(weight > 1.2*ibw, true = ibw, false = weight),
            ren_gen_cnst = case_when(sex == "Female" ~ 1.04,
                                     sex == "Male" ~ 1.23),
            crcl = ((140-age)*ren_gen_cnst*calc_weight)/creat,
            # Unclear which dose should be given for some pts (e.g., crcl == 89.5)
            dose1 = case_when(crcl > 110 ~ "1500mg",
                              crcl >= 90 & crcl <= 110 ~ "1250mg",
                              crcl >= 75 & crcl < 90 ~ "1250mg",
                              crcl >= 55 & crcl < 75 ~ "1250mg",
                              crcl >= 40 & crcl < 55 ~ "1250mg",
                              crcl >= 30 & crcl < 40 ~ "1250mg",
                              crcl >= 20 & crcl < 30 ~ "1250mg",
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
            dose4_time = dose2_time + dose_interval,
            # Assuming that level before 3rd dose should show at dose2
            pre_level = case_when(crcl >= 40 ~ 2,
                                  crcl < 40 ~ 3),
            pre_level_time =
              case_when(pre_level == 2 ~ as_hms(dose2_time),
                        pre_level == 3 ~ as_hms(dose3_time)),
            # What are working hours? Assumed 0700-1800
            pre_level_work_time =
              if_else(pre_level_time > hms(59, 59, 6) &
                        pre_level_time < hms(00,00,18), TRUE, FALSE),
            # If the level is needed before the xth dose and the xth dose is outside work hours, then bring it forward by 1
            pre_level_before =
              ifelse(pre_level_work_time == TRUE, pre_level, pre_level - 1)
     ))


### Check for missing values ###
r_results %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  rowSums()


#### Save results ####
r_results %>%
  write_csv("data/r_results.csv")

