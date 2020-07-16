library(tidyverse)
library(tidycensus)
library(stringr)

year_setting <- 2018
codebook <- load_variables(year =year_setting, dataset = "acs5", cache = TRUE)

##################
#AAPI ALONE
##################

#getting the label ready-------
label_aapi <- codebook %>% 
  filter(str_detect(name, "C15002D") | str_detect(name, "C15002E")) %>% 
  rename(variable = name) %>% 
  mutate(label = case_when(
    variable == c("C15002D_001", "C15002E_001") ~"Pop Age 25+",
    variable %in% c("C15002D_003", "C15002D_008") ~"%Less than HS",
    variable %in% c("C15002E_003", "C15002E_008") ~"%Less than HS",
    variable %in% c("C15002D_004", "C15002D_009") ~"%HS or GED",
    variable %in% c("C15002E_004", "C15002E_009") ~"%HS or GED",
    variable %in% c("C15002D_005", "C15002D_010") ~"%Some College or AA",
    variable %in% c("C15002E_005", "C15002E_010") ~"%Some College or AA",
    variable %in% c("C15002D_006", "C15002D_011") ~"%BA or higher",
    variable %in% c("C15002E_006", "C15002E_011") ~"%BA or higher")) %>%  
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
    #set estimate unreliable if moe is larger than 50% of itself
    mutate(reliable = case_when(
      moe <= 0.5*estimate ~"YES",
      TRUE ~"NO")) %>% 
    mutate(final_est = case_when(
      label == "Pop Age 25+" ~estimate,
      TRUE ~estimate / summary_est),
      topic = "edu",
      geography = geo_label) %>% 
    select(GEOID, NAME, topic, group, geography, label, final_est, reliable)
  
  return(final_dta)
}
#final tables --------
aa_us <- data_clean(table_name = "C15002D", summary_var = "C15002D_001", geo = "us", geo_label = "national", data_year = year_setting)
aa_st <- data_clean(table_name = "C15002D", summary_var = "C15002D_001", geo = "state", geo_label = "state", data_year = year_setting)
aa_ct <- data_clean(table_name = "C15002D", summary_var = "C15002D_001", geo = "county", geo_label = "county", data_year = year_setting)
dta_cd <- data_clean(table_name = "C15002D", summary_var = "C15002D_001", geo = "congressional district", geo_label = "district", data_year = year_setting)


final_race <- rbind(dta_race_us, dta_race_st, dta_race_ct,  dta_race_cd, dta_race_metro)
rm(label_race, dta_race_us, dta_race_st, dta_race_ct,  dta_race_cd, dta_race_metro)






data <- get_acs(table = "C15002D", 
                geography = "us", #for state use "state", for congressional district use "district"
                year = 2018, 
                survey = "acs5",
                summary_var = "C15002D_001") #the default is "acs5"

national_data <- data %>%
  mutate(label = case_when(
    variable %in% c("C15002D_003", "C15002D_008") ~"Less than HS",
    variable %in% c("C15002D_004", "C15002D_009") ~"HS or GED",
    variable %in% c("C15002D_005", "C15002D_010") ~"Some College or AA",
    variable %in% c("C15002D_006", "C15002D_011") ~"BA or higher",
    TRUE ~NA_character_)) %>% 
  filter(is.na(label) == F) %>% 
  select(-variable) %>% 
  group_by(GEOID, label) %>% 
  mutate(estimate = sum(estimate),
         moe = sum(moe)) %>% 
  ungroup() %>% 
  unique() %>% 
  mutate(ci_detect = )



#aa alone
#nhpi alone
#aa combo
#nhpi combo
#total pop

data <- get_acs(variables = c("B02015_001", "B02016_001",
                              "B02018_001", "B02019_001"),
                geography = "state",
                year = 2018,
                summary_var = "B01003_001")

















