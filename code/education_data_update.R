library(tidyverse)
library(tidycensus)
library(stringr)
setwd("/Users/sunnyshao/Documents/quickstats_datapull")

year_setting <- 2019
codebook <- load_variables(year =year_setting, dataset = "acs5", cache = TRUE)

##################
#AAPI ALONE
##################

#getting the label ready-------
label_aapi <- codebook %>% 
  filter(str_detect(name, "C15002D") | str_detect(name, "C15002E")) %>% 
  rename(variable = name) %>% 
  mutate(label = case_when(
    # variable == c("C15002D_001", "C15002E_001") ~"Pop Age 25+",
    variable %in% c("C15002D_003", "C15002D_008") ~"Less than HS",
    variable %in% c("C15002E_003", "C15002E_008") ~"Less than HS",
    variable %in% c("C15002D_004", "C15002D_009") ~"HS or GED",
    variable %in% c("C15002E_004", "C15002E_009") ~"HS or GED",
    variable %in% c("C15002D_005", "C15002D_010") ~"Some College or AA",
    variable %in% c("C15002E_005", "C15002E_010") ~"Some College or AA",
    variable %in% c("C15002D_006", "C15002D_011") ~"BA or higher",
    variable %in% c("C15002E_006", "C15002E_011") ~"BA or higher")) %>%  
  filter(is.na(label) == F) %>% 
  mutate(group = case_when(
    str_detect(variable, "C15002D") ~"Asian American Alone",
    str_detect(variable, "C15002E") ~"NHPI Alone")) %>% 
  select(variable, label, group)


#data cleaning race function--------
data_clean <- function(table_name, summary_var, geo, geo_label, data_year){
  data <- get_acs(table = table_name, 
                   year = data_year,
                   geography = geo,
                   summary_var = summary_var)
  
  final_dta <- data %>% 
    #turn NA moe value to zero
    mutate(moe = case_when(
      is.na(moe) == T ~0,
      TRUE ~moe)) %>% 
    left_join(label_aapi) %>% 
    filter(is.na(label) == F) %>% 
    group_by(GEOID, label) %>% 
    mutate(estimate = sum(estimate),
           moe = sum(moe)) %>% 
    ungroup() %>% 
    select(-variable) %>% 
    unique() %>% 
    mutate(pct = case_when(
      summary_est > 0 ~round((estimate/summary_est), digits = 3),
      TRUE ~NA_real_),
      topic = "edu",
      geography = geo_label) %>% 
    #set estimate unreliable if moe is larger than 50% of itself
    mutate(reliable = case_when(
      moe <= 0.5*estimate ~"YES",
      TRUE ~"NO")) %>% 
    select(GEOID, NAME, topic, group, geography, label, estimate, pct, summary_est, reliable)
  
  return(final_dta)
}
#final tables --------
aa_us <- data_clean(table_name = "C15002D", summary_var = "C15002D_001", geo = "us", geo_label = "national", data_year = year_setting)
aa_st <- data_clean(table_name = "C15002D", summary_var = "C15002D_001", geo = "state", geo_label = "state", data_year = year_setting)
aa_ct <- data_clean(table_name = "C15002D", summary_var = "C15002D_001", geo = "county", geo_label = "county", data_year = year_setting)
aa_cd <- data_clean(table_name = "C15002D", summary_var = "C15002D_001", geo = "congressional district", geo_label = "district", data_year = year_setting)

pi_us <- data_clean(table_name = "C15002E", summary_var = "C15002E_001", geo = "us", geo_label = "national", data_year = year_setting)
pi_st <- data_clean(table_name = "C15002E", summary_var = "C15002E_001", geo = "state", geo_label = "state", data_year = year_setting)
pi_ct <- data_clean(table_name = "C15002E", summary_var = "C15002E_001", geo = "county", geo_label = "county", data_year = year_setting)
pi_cd <- data_clean(table_name = "C15002E", summary_var = "C15002E_001", geo = "congressional district", geo_label = "district", data_year = year_setting)

final <- rbind(aa_us, aa_st, aa_ct, aa_cd, pi_us, pi_st, pi_ct,  pi_cd)
rm(label_aapi, aa_us, aa_st, aa_ct, aa_cd, pi_us, pi_st, pi_ct,  pi_cd)

final <- final %>%
  filter(!str_detect(NAME, "Puerto Rico")) %>% 
  filter(!str_detect(NAME, "PR Metro Area")) %>%
  mutate(NAME = case_when(
    str_detect(NAME, "Ana County, New Mexico") ~"Dona Ana County, New Mexico",
    TRUE ~NAME)) %>%
  mutate(estimate_reliable = case_when(
    reliable == "YES" ~estimate,
    TRUE ~NA_real_)) %>% 
  mutate(pct_reliable = case_when(
    reliable == "YES" ~pct,
    TRUE ~NA_real_))

write_csv(final, "acs_database/education_dta.csv", na = "")

