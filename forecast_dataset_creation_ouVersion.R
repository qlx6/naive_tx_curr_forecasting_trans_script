
# -------------------------------------- #
# - Create Dataset for TX_CUR Forecast - #
# -------------------------------------- #
# -------------------------------------- #

library(tidyverse)
library(readxl)

#setwd("C:/Users/qlx6/Downloads/PSNU_IM_FY19-22_20211217_v2_1")
#ous <- read_tsv(file = "MER_Structured_Datasets_PSNU_IM_FY19-22_20211217_v2_1.txt")

ou <- read_tsv(file.choose()) %>% 
  filter(indicator %in% c("TX_CURR",
                          "HTS_TST_POS",
                          "PREP_CURR",
                          "VMMC_CIRC"))

names(ou)


tx_curr <- ou %>% 
  select(operatingunit, snu1, indicator, sex, standardizeddisaggregate, trendsfine, 
         trendscoarse, fundingagency, community, facility, fiscal_year, 
         qtr1, qtr2, qtr3, qtr4, targets) %>% 
  filter(indicator %in% c("TX_CURR") & 
           standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus",
                                           "Age/Sex/HIVStatus",
                                           "KeyPop/HIVStatus",
                                           "Total Numerator")) %>% 
  dplyr::rename(ou=operatingunit, Q1=qtr1, Q2=qtr2, Q3=qtr3, Q4=qtr4, tar=targets) 

# ---------------------------------------------------------------------------------------------

hts_tst_pos <- ou %>% 
  select(operatingunit, snu1, indicator, sex, standardizeddisaggregate, trendsfine, 
         trendscoarse, fundingagency, community, facility, fiscal_year, 
         qtr1, qtr2, qtr3, qtr4, targets) %>% 
  filter(indicator %in% c("HTS_TST_POS") & 
           standardizeddisaggregate %in% c("KeyPop/Result",
                                           "Modality/Age/Sex/Result",
                                           "Total Numerator")) %>% 
  dplyr::rename(ou=operatingunit, Q1=qtr1, Q2=qtr2, Q3=qtr3, Q4=qtr4, tar=targets) 


prep_curr <- ou %>% 
  select(operatingunit, snu1, indicator, sex, standardizeddisaggregate, trendsfine, 
         trendscoarse, fundingagency, community, facility, fiscal_year, 
         qtr1, qtr2, qtr3, qtr4, targets) %>% 
  filter(indicator %in% c("PREP_CURR") & 
           standardizeddisaggregate %in% c("Age/Sex",
                                           "KeyPop",
                                           "ThreeMonthTestResult",
                                           "Total Numerator")) %>% 
  dplyr::rename(ou=operatingunit, Q1=qtr1, Q2=qtr2, Q3=qtr3, Q4=qtr4, tar=targets) 


vmmc_circ <- ou %>% 
  select(operatingunit, snu1, indicator, sex, standardizeddisaggregate, trendsfine, 
         trendscoarse, fundingagency, community, facility, fiscal_year, 
         qtr1, qtr2, qtr3, qtr4, targets) %>% 
  filter(indicator %in% c("VMMC_CIRC") & 
           standardizeddisaggregate %in% c("Age/Sex",
                                           "Age/Sex/HIVStatus",
                                           "TechFollowUp/Sex",
                                           "TechFollowUp>14days/Sex",
                                           "Technique/Sex",
                                           "Total Numerator")) %>% 
  dplyr::rename(ou=operatingunit, Q1=qtr1, Q2=qtr2, Q3=qtr3, Q4=qtr4, tar=targets) 

# ---------------------------------------------------------------------------------------------
ous2 <- bind_rows(tx_curr, hts_tst_pos, prep_curr, vimmc_circ)
# ---------------------------------------------------------------------------------------------


ous2 <- pivot_longer(ous1, Q1:tar,
                     names_to = "period",
                     values_to = "TX_CURR_r") %>% 
  na.omit(TX_CURR_r)

ous3 <- ous2 %>% 
  select(ou, psnu, indicator, sex, standardizeddisaggregate, trendsfine, 
         trendscoarse, fundingagency, fiscal_year, period, TX_CURR_r) %>% 
  unite("quarter",
        c("fiscal_year", "period"),
        sep = "_",
        remove = FALSE)

library(plyr)

ous3$trendsfine <- revalue(ous3$trendsfine, 
                           c("43834" = "1-4",
                             "44200" = "1-4",
                             "44205" = "1-9",
                             "1-4" = "0-4",
                             "0 - 4" = "0-4",
                             "43960" = "5-9",
                             "44325" = "5-9",
                             "44118" = "10-14",
                             "44483" = "10-14",
                             "15 - 19" = "15-19",
                             "20 - 24" = "20-24",
                             "25 - 29" = "25-29",
                             "30 - 34" = "30-34",
                             "35 - 39" = "35-39",
                             "40 - 44" = "40-44",
                             "45 - 49" = "45-49",
                             "50 - 54" = "50-54",
                             "55 - 59" = "55-59",
                             "60 - 64" = "60-64",
                             "65 - 69" = "65-69",
                             "70 - 74" = "70-74",
                             "75 - 79" = "75-79"))

#a <- c("2019_Q1", "2019_Q2", "2019_Q3", "2019_Q4", "2019_tar")

#ous3 <- ous3 %>% 
#  dplyr::filter(quarter != a)

ous3$trendsfine <- as.character(ous3$trendsfine)

write_csv(ous3, file = "C:/Users/qlx6/OneDrive - CDC/general dynamics - icpi/clusters.teams.workgroups/a_innovation/mer_forecasting/ous_1.csv")
