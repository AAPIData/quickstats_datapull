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
  filter(str_detect(name, "C27001D") | str_detect(name, "C27001E")) %>% 
  rename(variable = name) %>% 
  mutate(label = case_when(
    variable == "C27001D_001" ~"Asian Alone Pop",
    variable == "C27001E_001" ~"NHPI Alone Pop",
    variable %in% c("C27001D_003", "C27001D_006","C27001D_009",
                    "C27001E_003", "C27001E_006","C27001E_009") ~"with health insurance",
    TRUE ~NA_character_)) %>%  
  filter(is.na(label) == F) %>% 
  mutate(group = case_when(
    str_detect(variable, "C27001D") ~"Asian American Alone",
    str_detect(variable, "C27001E") ~"NHPI Alone")) %>% 
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
    mutate(pct = estimate/summary_est,
      topic = "insurance",
      geography = geo_label) %>% 
    select(GEOID, NAME, topic, group, geography, label, estimate, pct, summary_est, reliable)
  
  return(final_dta)
}
#final tables --------
aa_us <- data_clean(table_name = "C27001D", summary_var = "C27001D_001", geo = "us", geo_label = "national", data_year = year_setting)
aa_st <- data_clean(table_name = "C27001D", summary_var = "C27001D_001", geo = "state", geo_label = "state", data_year = year_setting)
aa_ct <- data_clean(table_name = "C27001D", summary_var = "C27001D_001", geo = "county", geo_label = "county", data_year = year_setting)
aa_cd <- data_clean(table_name = "C27001D", summary_var = "C27001D_001", geo = "congressional district", geo_label = "district", data_year = year_setting)

pi_us <- data_clean(table_name = "C27001E", summary_var = "C27001E_001", geo = "us", geo_label = "national", data_year = year_setting)
pi_st <- data_clean(table_name = "C27001E", summary_var = "C27001E_001", geo = "state", geo_label = "state", data_year = year_setting)
pi_ct <- data_clean(table_name = "C27001E", summary_var = "C27001E_001", geo = "county", geo_label = "county", data_year = year_setting)
pi_cd <- data_clean(table_name = "C27001E", summary_var = "C27001E_001", geo = "congressional district", geo_label = "district", data_year = year_setting)

final <- rbind(aa_us, aa_st, aa_ct, aa_cd, pi_us, pi_st, pi_ct,  pi_cd)
rm(label_aapi, aa_us, aa_st, aa_ct, aa_cd, pi_us, pi_st, pi_ct,  pi_cd)

write_csv(final, "acs_database/education_dta.csv", na = "")

