library(tidyverse)
library(tidycensus)
library(stringr)

year_setting <- 2018
codebook <- load_variables(year =year_setting, dataset = "acs1", cache = TRUE)

##################
#race
##################

#getting the label ready-------
label_race <- codebook %>% 
  filter(name %in% c("B03001_001", "B02015_001", "B02016_001", "B02018_001", "B02019_001")) %>% 
  mutate(label = case_when(
    name == "B03001_001" ~"Total Population",
    name == "B02015_001" ~"Asian American Alone",
    name == "B02016_001" ~"Asian American Alone or in Combo",
    name == "B02018_001" ~"NHPI Alone",
    name == "B02019_001" ~"NHPI Alone or in Combo")) %>% 
  rename(variable = name) %>% 
  select(variable, label)

#data cleaning race function--------
data_clean_race <- function(geo, geo_label, data_year, survey_type){
  
  data1 <- get_acs(variables = c("B03001_001", "B02015_001", "B02018_001"), 
                   year = data_year,
                   geography = geo,
                   survey = survey_type,
                   summary_var = "B03001_001") %>% 
    mutate(group = "AAPI Alone")
  
  data2 <- get_acs(variables = c("B03001_001", "B02016_001", "B02019_001"), 
                  year = data_year,
                  geography = geo,
                  survey = survey_type,
                  summary_var = "B03001_001") %>% 
    mutate(group = "AAPI Alone or in Combo")
  
  data <- rbind(data1, data2)

  final_dta <- data %>% 
    #turn NA moe value to zero
    mutate(moe = case_when(
      is.na(moe) == T ~0,
      TRUE ~moe)) %>% 
    left_join(label_race) %>% 
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
    mutate(pct = estimate / summary_est,
           topic = "population",
           geography = geo_label) %>% 
    select(GEOID, NAME, topic, group, geography, label, estimate, pct, reliable)
  
  return(final_dta)
}
#final tables --------
dta_race_us <- data_clean_race(geo = "us", geo_label = "national", data_year = year_setting, survey_type = "acs1")
dta_race_st <- data_clean_race(geo = "state", geo_label = "state", data_year = year_setting, survey_type = "acs5")
dta_race_ct <- data_clean_race(geo = "county", geo_label = "county", data_year = year_setting, survey_type = "acs5")
dta_race_cd <- data_clean_race(geo = "congressional district", geo_label = "district", data_year = year_setting, survey_type = "acs5")
dta_race_metro <- data_clean_race(geo = "metropolitan statistical area/micropolitan statistical area", geo_label = "metro", data_year = year_setting, survey_type = "acs5") %>% 
  filter(str_detect(NAME,"Metro Area"))

final_race <- rbind(dta_race_us, dta_race_st, dta_race_ct,  dta_race_cd, dta_race_metro)
rm(label_race, dta_race_us, dta_race_st, dta_race_ct,  dta_race_cd, dta_race_metro)





##################
#Asian Am
##################

#label for the data tables--------
label_detailed_aa_alone <- codebook %>% 
  filter(str_detect(name, "B02015")) %>% 
  mutate(label = str_remove(label, "Estimate!!Total!!")) %>% 
  rename(variable = name) %>% 
  select(variable, label)

label_detailed_aa_combo <- codebook %>% 
  filter(str_detect(name, "B02018")) %>% 
  mutate(label = str_remove(label, "Estimate!!Total Groups Tallied!!")) %>% 
  rename(variable = name) %>% 
  select(variable, label)

other_aa_alone_list <- c("B02015_023", "B02015_024", "B02015_025")
other_aa_combo_list <- c("B02018_023", "B02018_024")


#data_cleaning function for AA-------
data_clean_aa <- function(table_name, summary_name, geo, geo_label, data_year, label_df, merge_list, group, survey_type){
  
  data <- get_acs(table = table_name, 
                  year = data_year,
                  geography = geo,
                  survey = survey_type,
                  summary_var = summary_name)
  
  final_dta <- data %>% 
    #merge in label
    left_join(label_df) %>% 
    #turn NA moe value to zero
    mutate(moe = case_when(
      is.na(moe) == T ~0,
      TRUE ~moe)) %>% 
    #merge other categories into one
    mutate(label = case_when(
      variable %in% merge_list ~"Other Asian",
      variable == summary_name ~"Asian American",
      TRUE ~label)) %>% 
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
    mutate(pct = estimate / summary_est,
           topic = "population",
           geography = geo_label,
           group = group) %>% 
    select(GEOID, NAME, topic, group, geography, label, estimate, pct, reliable)
  
  return(final_dta)
}


#detailed AA alone ---------------
detailed_aa_alone_us <- data_clean_aa(table_name = "B02015", summary_name = "B02015_001", geo = "us", geo_label = "national", data_year = year_setting,
                                   label_df = label_detailed_aa_alone, merge_list = other_aa_alone_list, group = "Detailed AAPI Alone", survey_type = "acs1")

detailed_aa_alone_st <- data_clean_aa(table_name = "B02015", summary_name = "B02015_001", geo = "state", geo_label = "state", data_year = year_setting,
                                      label_df = label_detailed_aa_alone, merge_list = other_aa_alone_list, group = "Detailed AAPI Alone", survey_type = "acs5")

detailed_aa_alone_ct <- data_clean_aa(table_name = "B02015", summary_name = "B02015_001", geo = "county", geo_label = "county", data_year = year_setting,
                                      label_df = label_detailed_aa_alone, merge_list = other_aa_alone_list, group = "Detailed AAPI Alone", survey_type = "acs5")

detailed_aa_alone_cd <- data_clean_aa(table_name = "B02015", summary_name = "B02015_001", geo = "congressional district", geo_label = "district", data_year = year_setting,
                                      label_df = label_detailed_aa_alone, merge_list = other_aa_alone_list, group = "Detailed AAPI Alone", survey_type = "acs5")

detailed_aa_alone_metro <- data_clean_aa(table_name = "B02015", summary_name = "B02015_001", geo = "metropolitan statistical area/micropolitan statistical area", geo_label = "metro", data_year = year_setting,
                                      label_df = label_detailed_aa_alone, merge_list = other_aa_alone_list,group = "Detailed AAPI Alone", survey_type = "acs5") %>% 
  filter(str_detect(NAME, "Metro Area"))

final_detailed_aa_alone <- rbind(detailed_aa_alone_us, detailed_aa_alone_st, detailed_aa_alone_ct,
                                 detailed_aa_alone_cd, detailed_aa_alone_metro)
rm(detailed_aa_alone_us, detailed_aa_alone_st, detailed_aa_alone_ct,
   detailed_aa_alone_cd, detailed_aa_alone_metro, label_detailed_aa_alone, other_aa_alone_list)

#detailed AA combo ---------------
detailed_aa_combo_us <- data_clean_aa(table_name = "B02018", summary_name = "B02018_001", geo = "us", geo_label = "national", data_year = year_setting,
                                      label_df = label_detailed_aa_combo, merge_list = other_aa_combo_list, group = "Detailed AAPI Combo", survey_type = "acs1")

detailed_aa_combo_st <- data_clean_aa(table_name = "B02018", summary_name = "B02018_001", geo = "state", geo_label = "state", data_year = year_setting,
                                      label_df = label_detailed_aa_combo, merge_list = other_aa_combo_list, group = "Detailed AAPI Combo", survey_type = "acs5")

detailed_aa_combo_ct <- data_clean_aa(table_name = "B02018", summary_name = "B02018_001", geo = "county", geo_label = "county", data_year = year_setting,
                                      label_df = label_detailed_aa_combo, merge_list = other_aa_combo_list, group = "Detailed AAPI Combo", survey_type = "acs5")

detailed_aa_combo_cd <- data_clean_aa(table_name = "B02018", summary_name = "B02018_001", geo = "congressional district", geo_label = "district", data_year = year_setting,
                                      label_df = label_detailed_aa_combo, merge_list = other_aa_combo_list, group = "Detailed AAPI Combo", survey_type = "acs5")

detailed_aa_combo_metro <- data_clean_aa(table_name = "B02018", summary_name = "B02018_001", geo = "metropolitan statistical area/micropolitan statistical area", geo_label = "metro", data_year = year_setting,
                                      label_df = label_detailed_aa_combo, merge_list = other_aa_combo_list, group = "Detailed AAPI Combo", survey_type = "acs5") %>% 
  filter(str_detect(NAME, "Metro Area"))

final_detailed_aa_combo <- rbind(detailed_aa_combo_us, detailed_aa_combo_st, detailed_aa_combo_ct,
                                 detailed_aa_combo_cd, detailed_aa_combo_metro)
rm(detailed_aa_combo_us, detailed_aa_combo_st, detailed_aa_combo_ct,
   detailed_aa_combo_cd, detailed_aa_combo_metro, label_detailed_aa_combo, other_aa_combo_list)




##################
#NHPI
##################

#label for the data tables--------
label_detailed_pi_alone <- codebook %>% 
  filter(str_detect(name, "B02016")) %>% 
  mutate(label = str_remove(label, ".*\\!!")) %>% 
  rename(variable = name) %>% 
  select(variable, label)

nhpi_alone1 <- label_detailed_pi_alone %>% 
  mutate(label = case_when(
    variable %in% c("B02016_002", "B02016_003", "B02016_004","B02016_005") ~"Polynesian",
    variable %in% c("B02016_006", "B02016_007", "B02016_008") ~"Micronesian",
    variable %in% c("B02016_009", "B02016_010") ~"Melanesian",
    TRUE ~NA_character_)) %>% 
  filter(is.na(label) == F)

nhpi_alone2 <- label_detailed_pi_alone %>% 
  mutate(label = case_when(
    variable == "B02016_001" ~"NHPI",
    variable == "B02016_005" ~NA_character_,
    variable == "B02016_008" ~NA_character_,
    variable == "B02016_010" ~NA_character_,
    variable %in% c("B02016_011", "B02016_012") ~NA_character_,
    TRUE ~label)) %>% 
  filter(is.na(label) == F)


label_detailed_pi_combo <- codebook %>% 
  filter(str_detect(name, "B02019")) %>% 
  mutate(label = str_remove(label, ".*\\!!")) %>% 
  rename(variable = name) %>% 
  select(variable, label)

nhpi_combo1 <- label_detailed_pi_combo %>% 
  mutate(label = case_when(
    variable %in% c("B02019_002", "B02019_003", "B02019_004","B02019_005") ~"Polynesian",
    variable %in% c("B02019_006", "B02019_007", "B02019_008") ~"Micronesian",
    variable %in% c("B02019_009", "B02019_010") ~"Melanesian",
    TRUE ~NA_character_)) %>% 
  filter(is.na(label) == F)

nhpi_combo2 <- label_detailed_pi_combo %>% 
  mutate(label = case_when(
    variable == "B02019_001" ~"NHPI",
    variable == "B02019_005" ~NA_character_,
    variable == "B02019_008" ~NA_character_,
    variable == "B02019_010" ~NA_character_,
    variable == "B02019_011" ~NA_character_,
    TRUE ~label)) %>% 
  filter(is.na(label) == F)


#nhpi data cleaning function ------------
data_clean_pi<- function(table_name, summary_name, geo, geo_label, 
                         data_year, label_df1, label_df2, group, survey_type){
  
  data <- get_acs(table = table_name, 
                  year = data_year,
                  geography = geo,
                  survey = survey_type,
                  summary_var = summary_name)
  
  dta1 <- data %>% 
    #merge in label
    left_join(label_df1) %>% 
    filter(is.na(label) == F) %>% 
    #turn NA moe value to zero
    mutate(moe = case_when(
      is.na(moe) == T ~0,
      TRUE ~moe)) %>% 
    group_by(GEOID, label) %>% 
    mutate(estimate = sum(estimate),
           moe = sum(moe)) %>% 
    ungroup() %>% 
    select(-variable) %>% 
    unique()
  
  dta2 <- data %>% 
    #merge in label
    left_join(label_df2) %>% 
    filter(is.na(label) == F) %>% 
    #turn NA moe value to zero
    mutate(moe = case_when(
      is.na(moe) == T ~0,
      TRUE ~moe)) %>% 
    group_by(GEOID, label) %>% 
    mutate(estimate = sum(estimate),
           moe = sum(moe)) %>% 
    ungroup() %>% 
    select(-variable) %>% 
    unique()
  
  final_dta <- rbind(dta1, dta2) %>% 
    #set estimate unreliable if moe is larger than 50% of itself
    mutate(reliable = case_when(
      moe <= 0.5*estimate ~"YES",
      TRUE ~"NO")) %>% 
    mutate(pct = estimate / summary_est,
           topic = "population",
           geography = geo_label,
           group = group) %>% 
    select(GEOID, NAME, topic, group, geography, label, estimate, pct, reliable)
  
  return(final_dta)
}



#detailed NHPI alone ---------------
detailed_pi_alone_us <- data_clean_pi(table_name = "B02016", summary_name = "B02016_001", geo = "us", geo_label = "national", data_year = year_setting,
                                      label_df1 = nhpi_alone1, label_df2 = nhpi_alone2, group = "Detailed AAPI Alone", survey_type = "acs1")

detailed_pi_alone_st <- data_clean_pi(table_name = "B02016", summary_name = "B02016_001", geo = "state", geo_label = "state", data_year = year_setting,
                                      label_df1 = nhpi_alone1, label_df2 = nhpi_alone2, group = "Detailed AAPI Alone", survey_type = "acs5")

detailed_pi_alone_ct <- data_clean_pi(table_name = "B02016", summary_name = "B02016_001", geo = "county", geo_label = "county", data_year = year_setting,
                                      label_df1 = nhpi_alone1, label_df2 = nhpi_alone2, group = "Detailed AAPI Alone", survey_type = "acs5")

detailed_pi_alone_cd <- data_clean_pi(table_name = "B02016", summary_name = "B02016_001", geo = "congressional district", geo_label = "district", data_year = year_setting,
                                      label_df1 = nhpi_alone1, label_df2 = nhpi_alone2, group = "Detailed AAPI Alone", survey_type = "acs5")

detailed_pi_alone_metro <- data_clean_pi(table_name = "B02016", summary_name = "B02016_001", geo = "metropolitan statistical area/micropolitan statistical area", geo_label = "metro", data_year = year_setting,
                                      label_df1 = nhpi_alone1, label_df2 = nhpi_alone2, group = "Detailed AAPI Alone", survey_type = "acs5") %>% 
  filter(str_detect(NAME, "Metro Area"))

final_detailed_pi_alone <- rbind(detailed_pi_alone_us, detailed_pi_alone_st, detailed_pi_alone_ct,
                                 detailed_pi_alone_cd, detailed_pi_alone_metro)
rm(detailed_pi_alone_us, detailed_pi_alone_st, detailed_pi_alone_ct,
   detailed_pi_alone_cd, detailed_pi_alone_metro, label_detailed_pi_alone, nhpi_alone1, nhpi_alone2)


#detailed NHPI combo ---------------
detailed_pi_combo_us <- data_clean_pi(table_name = "B02019", summary_name = "B02019_001", geo = "us", geo_label = "national", data_year = year_setting,
                                      label_df1 = nhpi_combo1, label_df2 = nhpi_combo2, group = "Detailed AAPI Alone", survey_type = "acs1")

detailed_pi_combo_st <- data_clean_pi(table_name = "B02019", summary_name = "B02019_001", geo = "state", geo_label = "state", data_year = year_setting,
                                      label_df1 = nhpi_combo1, label_df2 = nhpi_combo2, group = "Detailed AAPI Alone", survey_type = "acs5")

detailed_pi_combo_ct <- data_clean_pi(table_name = "B02019", summary_name = "B02019_001", geo = "county", geo_label = "county", data_year = year_setting,
                                      label_df1 = nhpi_combo1, label_df2 = nhpi_combo2, group = "Detailed AAPI Alone", survey_type = "acs5")

detailed_pi_combo_cd <- data_clean_pi(table_name = "B02019", summary_name = "B02019_001", geo = "congressional district", geo_label = "district", data_year = year_setting,
                                      label_df1 = nhpi_combo1, label_df2 = nhpi_combo2, group = "Detailed AAPI Alone", survey_type = "acs5")

detailed_pi_combo_metro <- data_clean_pi(table_name = "B02019", summary_name = "B02019_001", geo = "metropolitan statistical area/micropolitan statistical area", geo_label = "metro", data_year = year_setting,
                                         label_df1 = nhpi_combo1, label_df2 = nhpi_combo2, group = "Detailed AAPI Alone", survey_type = "acs5") %>% 
  filter(str_detect(NAME, "Metro Area"))

final_detailed_pi_combo <- rbind(detailed_pi_combo_us, detailed_pi_combo_st, detailed_pi_combo_ct,
                                 detailed_pi_combo_cd, detailed_pi_combo_metro)
rm(detailed_pi_combo_us, detailed_pi_combo_st, detailed_pi_combo_ct,
   detailed_pi_combo_cd, detailed_pi_combo_metro, label_detailed_pi_combo, nhpi_combo1, nhpi_combo2)



##################
#final merge
##################

final <- rbind(final_race, final_detailed_aa_alone, final_detailed_aa_combo, final_detailed_pi_alone, final_detailed_pi_combo)
rm(final_race, final_detailed_aa_alone, final_detailed_aa_combo, final_detailed_pi_alone, final_detailed_pi_combo)

write_csv(final, "acs_database/population_dta.csv", na = "")

###temp check-----------
# data <- get_acs(variables =  c("B03001_001", "B02015_001", "B02016_001", "B02018_001", "B02019_001"), 
#                 year = 2018,
#                 geography = "us",
#                 survey = "acs5",
#                 summary_var = "B02016_001")
#   

