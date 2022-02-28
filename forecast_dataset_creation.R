
# -------------------------------------- #
# - Create Dataset for TX_CUR Forecast - #
# -------------------------------------- #
# -------------------------------------- #

library(tidyverse)
library(readxl)

setwd("C:/Users/qlx6/Downloads/PSNU_IM_FY19-22_20211217_v2_1")
ous <- read_tsv(file = "MER_Structured_Datasets_PSNU_IM_FY19-22_20211217_v2_1.txt")

names(ous)

ous1 <- ous %>% 
  select(operatingunit, psnu, indicator, sex, standardizeddisaggregate, trendsfine, 
         trendscoarse, fundingagency, fiscal_year, qtr1, qtr2, qtr3, qtr4, targets) %>% 
  filter(indicator %in% c("TX_CURR",
                          "TX_NEW",
                          "TX_ML",
                          "TX_RTT") & 
           standardizeddisaggregate %in%  c("Age/Sex/HIVStatus",
                                            "Age/Sex/ARTNoContactReason/HIVStatus",
                                            "ARTNoContactReasonIIT/HIVStatus")) %>% 
  dplyr::rename(ou=operatingunit, Q1=qtr1, Q2=qtr2, Q3=qtr3, Q4=qtr4, tar=targets) 



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
