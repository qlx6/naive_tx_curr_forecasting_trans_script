# qlx6@cdc.gov
# ------------------------------------------------------- #
# - Create Dataset for TX_CUR Naive Forecast ------------ #
# - Description: Script to transform the the MSD PSNU*IM  #
#              MSD to structure for naive indicator ----- #
#              forecast --------------------------------- #
# ------------------------------------------------------- #
# ------------------------------------------------------- #

library(tidyverse)
library(readxl)

#setwd("C:/Users/qlx6/Downloads/PSNU_IM_FY19-22_20211217_v2_1")
#ous <- read_tsv(file = "MER_Structured_Datasets_PSNU_IM_FY19-22_20211217_v2_1.txt")

ou <- read_tsv(file.choose()) %>% 
  filter(indicator %in% c("TX_CURR",
                          "HTS_TST_POS",
                          "PrEP_CURR",
                          "VMMC_CIRC",
                          "PrEP_CT"))

names(ou)


tx_curr <- ou %>% 
  select(operatingunit, snu1, indicator, sex, standardizeddisaggregate, trendsfine, 
         trendscoarse, primepartner, fundingagency, mech_name, mech_code, community, 
         facility, modality, fiscal_year, qtr1, qtr2, qtr3, qtr4, targets) %>% 
  filter(indicator %in% c("TX_CURR") & 
           standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus",
                                           "Age/Sex/HIVStatus",
                                           "KeyPop/HIVStatus",
                                           "Total Numerator")) %>% 
  dplyr::rename(ou=operatingunit, Q1=qtr1, Q2=qtr2, Q3=qtr3, Q4=qtr4, tar=targets) 

# ---------------------------------------------------------------------------------------------

hts_tst_pos <- ou %>% 
  select(operatingunit, snu1, indicator, sex, standardizeddisaggregate, trendsfine, 
         trendscoarse, primepartner, fundingagency, mech_name, mech_code, community, 
         facility, modality, fiscal_year, qtr1, qtr2, qtr3, qtr4, targets) %>% 
  filter(indicator %in% c("HTS_TST_POS") & 
           standardizeddisaggregate %in% c("KeyPop/Result",
                                           "Modality/Age/Sex/Result",
                                           "Total Numerator")) %>% 
  dplyr::rename(ou=operatingunit, Q1=qtr1, Q2=qtr2, Q3=qtr3, Q4=qtr4, tar=targets) 


prep_curr <- ou %>% 
  select(operatingunit, snu1, indicator, sex, standardizeddisaggregate, trendsfine, 
         trendscoarse, primepartner, fundingagency, mech_name, mech_code, community, 
         facility, modality, fiscal_year, qtr1, qtr2, qtr3, qtr4, targets) %>% 
  filter(indicator %in% c("PrEP_CURR") & 
           standardizeddisaggregate %in% c("Age/Sex",
                                           "KeyPop",
                                           "ThreeMonthTestResult",
                                           "Total Numerator")) %>% 
  dplyr::rename(ou=operatingunit, Q1=qtr1, Q2=qtr2, Q3=qtr3, Q4=qtr4, tar=targets) 


prep_ct <- ou %>% 
  select(operatingunit, snu1, indicator, sex, standardizeddisaggregate, trendsfine, 
         trendscoarse, primepartner, fundingagency, mech_name, mech_code, community, 
         facility, modality, fiscal_year, qtr1, qtr2, qtr3, qtr4, targets) %>% 
  filter(indicator %in% c("PrEP_CT") & 
           standardizeddisaggregate %in% c("Age/Sex",
                                           "KeyPop",
                                           "Sex/PregnantBreastfeeding",
                                           "TestResult",
                                           "Total Numerator")) %>% 
  dplyr::rename(ou=operatingunit, Q1=qtr1, Q2=qtr2, Q3=qtr3, Q4=qtr4, tar=targets)

vmmc_circ <- ou %>% 
  select(operatingunit, snu1, indicator, sex, standardizeddisaggregate, trendsfine, 
         trendscoarse, primepartner, fundingagency, mech_name, mech_code, community, 
         facility, modality, fiscal_year, qtr1, qtr2, qtr3, qtr4, targets) %>% 
  filter(indicator %in% c("VMMC_CIRC") & 
           standardizeddisaggregate %in% c("Age/Sex",
                                           "Age/Sex/HIVStatus",
                                           "TechFollowUp/Sex",
                                           "TechFollowUp>14days/Sex",
                                           "Technique/Sex",
                                           "Total Numerator")) %>% 
  dplyr::rename(ou=operatingunit, Q1=qtr1, Q2=qtr2, Q3=qtr3, Q4=qtr4, tar=targets) 

# ---------------------------------------------------------------------------------------------
ous2 <- bind_rows(tx_curr, hts_tst_pos, prep_curr, prep_ct, vmmc_circ)
# ---------------------------------------------------------------------------------------------


ous2 <- pivot_longer(ous2, c("Q1", "Q2", "Q3", "Q4", "tar"),
                      names_to = "period",
                      values_to = "value") %>%     
  filter(!is.na(value))



ous3 <- ous2 %>% 
  select(ou, snu1, indicator, sex, standardizeddisaggregate, trendsfine, 
         trendscoarse, primepartner, fundingagency, mech_name, mech_code, community, 
         facility, modality, fiscal_year, period, value) %>% 
  unite("quarter",
        c("fiscal_year", "period"),
        sep = "_",
        remove = FALSE)

ous3$trendsfine <- as.character(ous3$trendsfine)

write_csv(ous3, file = "C:/Users/qlx6/OneDrive - CDC/general dynamics - icpi/clusters.teams.workgroups/a_innovation/mer_forecasting/all_mar_03a.csv")
