# qlx6@cdc.gov
# ------------------------------------------------------- #
# - Create Dataset for TX_CUR Naive Forecast ------------ #
# - Description: Script to transform the the MSD SITE*IM  #
#              MSD to structure for naive indicator ----- #
#              forecast --------------------------------- #
# ------------------------------------------------------- #
# ------------------------------------------------------- #

library(tidyverse)
library(readxl)

oua <- read_tsv(file.choose()) %>% 
  filter(indicator %in% c("TX_CURR",
                          "HTS_TST_POS",
                          "PrEP_CURR",
                          "VMMC_CIRC"))

names(oua)


tx_curra <- oua %>% 
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

hts_tst_posa <- oua %>% 
  select(operatingunit, snu1, indicator, sex, standardizeddisaggregate, trendsfine, 
         trendscoarse, fundingagency, community, facility, fiscal_year, 
         qtr1, qtr2, qtr3, qtr4, targets) %>% 
  filter(indicator %in% c("HTS_TST_POS") & 
           standardizeddisaggregate %in% c("KeyPop/Result",
                                           "Modality/Age/Sex/Result",
                                           "Total Numerator")) %>% 
  dplyr::rename(ou=operatingunit, Q1=qtr1, Q2=qtr2, Q3=qtr3, Q4=qtr4, tar=targets) 


prep_curra <- oua %>% 
  select(operatingunit, snu1, indicator, sex, standardizeddisaggregate, trendsfine, 
         trendscoarse, fundingagency, community, facility, fiscal_year, 
         qtr1, qtr2, qtr3, qtr4, targets) %>% 
  filter(indicator %in% c("PrEP_CURR") & 
           standardizeddisaggregate %in% c("Age/Sex",
                                           "KeyPop",
                                           "ThreeMonthTestResult",
                                           "Total Numerator")) %>% 
  dplyr::rename(ou=operatingunit, Q1=qtr1, Q2=qtr2, Q3=qtr3, Q4=qtr4, tar=targets) 


prep_cta <- oua %>% 
  select(operatingunit, snu1, indicator, sex, standardizeddisaggregate, trendsfine, 
         trendscoarse, fundingagency, community, facility, fiscal_year, 
         qtr1, qtr2, qtr3, qtr4, targets) %>% 
  filter(indicator %in% c("PrEP_CT") & 
           standardizeddisaggregate %in% c("Age/Sex",
                                           "KeyPop",
                                           "Sex/PregnantBreastfeeding",
                                           "TestResult",
                                           "Total Numerator")) %>% 
  dplyr::rename(ou=operatingunit, Q1=qtr1, Q2=qtr2, Q3=qtr3, Q4=qtr4, tar=targets)

vmmc_circa <- oua %>% 
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
ous2a <- bind_rows(tx_curra, hts_tst_posa, prep_curra, vmmc_circa)
# ---------------------------------------------------------------------------------------------


ous2a <- pivot_longer(ous2a, Q1:tar,
                     names_to = "period",
                     values_to = "value") %>% 
  na.omit(indicator)



ous3a <- ous2a %>% 
  select(ou, snu1, indicator, sex, standardizeddisaggregate, trendsfine, 
         trendscoarse, fundingagency, community, facility,
         fiscal_year, period, value) %>% 
  unite("quarter",
        c("fiscal_year", "period"),
        sep = "_",
        remove = FALSE)

ous3a$trendsfine <- as.character(ous3a$trendsfine)

write_csv(all, file = "C:/Users/qlx6/OneDrive - CDC/general dynamics - icpi/clusters.teams.workgroups/a_innovation/mer_forecasting/all_feb_28a.csv")


all <- bind_rows(ous3, ous3a)
