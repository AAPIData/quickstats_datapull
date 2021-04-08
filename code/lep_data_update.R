library(tidyverse)
library(tidycensus)
library(stringr)

###change the year number when updating
year_setting <- 2019

#getting the master codebook
codebook <- load_variables(year =year_setting, dataset = "acs5", cache = TRUE)

##################
#AAPI ALONE
##################

#getting the label ready-------
label_aapi <- codebook %>% 
  filter(str_detect(name, "B16005D") | str_detect(name, "B16005E")) %>% 
  rename(variable = name) %>% 
  mutate(label = case_when(
    variable %in% c("B16005D_004", "B16005D_009", "B16005E_004", "B16005E_009") ~"Speak another language",
    variable %in% c("B16005D_006", "B16005D_011", "B16005E_006", "B16005E_011") ~"LEP",
    TRUE ~NA_character_)) %>%  
  filter(is.na(label) == F) %>% 
  mutate(group = case_when(
    str_detect(variable, "B16005D") ~"Asian American Alone",
    str_detect(variable, "B16005E") ~"NHPI Alone")) %>% 
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
      topic = "LEP",
      geography = geo_label) %>% 
    #set estimate unreliable if moe is larger than 50% of itself
    mutate(reliable = case_when(
      moe <= 0.5*estimate ~"YES",
      TRUE ~"NO")) %>% 
    select(GEOID, NAME, topic, group, geography, label, estimate, pct, summary_est, reliable)
  
  return(final_dta)
}
#final tables --------
aa_us <- data_clean(table_name = "B16005D", summary_var = "B16005D_001", geo = "us", geo_label = "national", data_year = year_setting)
aa_st <- data_clean(table_name = "B16005D", summary_var = "B16005D_001", geo = "state", geo_label = "state", data_year = year_setting)
aa_ct <- data_clean(table_name = "B16005D", summary_var = "B16005D_001", geo = "county", geo_label = "county", data_year = year_setting)
aa_cd <- data_clean(table_name = "B16005D", summary_var = "B16005D_001", geo = "congressional district", geo_label = "district", data_year = year_setting)

pi_us <- data_clean(table_name = "B16005E", summary_var = "B16005E_001", geo = "us", geo_label = "national", data_year = year_setting)
pi_st <- data_clean(table_name = "B16005E", summary_var = "B16005E_001", geo = "state", geo_label = "state", data_year = year_setting)
pi_ct <- data_clean(table_name = "B16005E", summary_var = "B16005E_001", geo = "county", geo_label = "county", data_year = year_setting)
pi_cd <- data_clean(table_name = "B16005E", summary_var = "B16005E_001", geo = "congressional district", geo_label = "district", data_year = year_setting)

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


write_csv(final, "acs_database/lep_dta.csv", na = "")

