library(tidyverse)
library(readxl)
library(stringr)
library(tidycensus)
setwd("/Users/sunnyshao/Documents/quickstats_datapull/")

#national-level detailed ----------------
us_detailed_pop <- get_acs(variables = c("B02015_002", "B02015_007", "B02015_012", "B02015_022", "B02015_018"), geography = "us", year = 2018, survey = "acs1") %>% 
  rename(summary_est = estimate) %>% 
  mutate(group = case_when(
    variable == "B02015_002" ~"Indian", 
    variable == "B02015_007" ~"Chinese", 
    variable == "B02015_012" ~"Korean", 
    variable == "B02015_022" ~"Vietnamese", 
    variable == "B02015_018" ~"Pakistani")) %>% 
  select(NAME, group, summary_est)

data <- read_excel("undocumented_raw/Occ_723202095011.xlsx", range = "A19:B41", col_names = c("country","pop"))

final_us_detailed <- data %>% 
  mutate(group = case_when(
    country == "India" ~"Indian",
    country == "China" ~"Chinese",
    country == "South Korea" ~"Korean",
    country == "Vietnam" ~"Vietnamese",
    country == "Pakistan" ~"Pakistani",
    TRUE ~NA_character_)) %>% 
  filter(is.na(group) == F) %>% 
  left_join(us_detailed_pop) %>% 
  mutate(geography = "national",
         topic = "undoc", 
         label = paste("Undocumented",group, sep = " "),
         group = "Detailed Asian Am Alone",
         estimate = pop,
         pct = round(estimate / summary_est, digits = 3),
         reliable = "YES",
         estimate_reliable = estimate,
         pct_reliable = pct) %>% 
  select(geography, NAME, group, topic, label, estimate, pct, summary_est, reliable, estimate_reliable, pct_reliable)
  

#national-level aa alone ----------------

us_pop <- get_acs(variables = "B02001_005", geography = "us", year = 2018, survey = "acs1") %>% 
  rename(summary_est = estimate) %>% 
  select(NAME, summary_est)

data <- read_excel("undocumented_raw/Occ_723202095011.xlsx", range = "D13", col_names = c("pop")) %>% 
  mutate(group = "Asian American Alone",
         NAME = "United States")

final_us_aa <- data %>%
  left_join(us_pop) %>% 
  rename(estimate = pop) %>% 
  mutate(geography = "national", 
         topic = "undoc", 
         label = "Undocumented", 
         pct = round(estimate / summary_est, digits = 3), 
         reliable = "YES", 
         estimate_reliable = estimate, 
         pct_reliable = pct) %>% 
select(geography, NAME, group, topic, label, estimate, pct, summary_est, reliable, estimate_reliable, pct_reliable)


#state-level AA ALONE ----------------
data1 <- read_excel("undocumented_raw/Occ_723202095011.xlsx", range = "D13", col_names = c("pop")) %>% mutate(group = "Asian American Alone")
data2 <- read_excel("undocumented_raw/Occ_723202095011.xlsx", range = "A3", col_names = c("state")) %>% mutate(group = "Asian American Alone")
data <- data1 %>%
  left_join(data2)
data_previous <- data %>% filter(group == "nhpi")
rm(data1, data2, data)

dict <- read_excel("undocumented_raw/list.xlsx")


i <- 1
for(i in 1:51) {
  print(i)
  if(i <= 51){
    path <- paste("undocumented_raw/", toString(dict[i,1]), sep = "")
    
    data1 <- read_excel(path, range = "D13", col_names = c("pop")) %>% mutate(group = "Asian American Alone")
    data2 <- read_excel(path, range = "A3", col_names = c("state")) %>% mutate(group = "Asian American Alone")
    data <- data1 %>% left_join(data2)
    data <- rbind(data, data_previous)
    data_previous <- data
  }else{
    data_final <- data_previous
    return(data_final)
    
  }
  
  i <- i + 1
  
}
rm(data1, data2, data_previous)

# data[13,1]

state_pop <- get_acs(variables = "B02001_005", geography = "state", year = 2018, survey = "acs1") %>% 
  rename(summary_est = estimate) %>% 
  select(NAME, summary_est)


final_state_aa <- data %>% 
  mutate(NAME = stringr::str_remove(state, "Results for: "),
         geography = "state",
         topic = "undoc",
         label = "Undocumented",
         estimate = as.numeric(pop),
         reliable = "YES",
         estimate_reliable = estimate) %>% 
  left_join(state_pop) %>% 
  mutate(pct = case_when(
    is.na(estimate) == F ~round(estimate/summary_est, digits = 3),
    TRUE ~NA_real_),
         pct_reliable = pct) %>% 
  select(geography, NAME, group, topic, label, estimate, pct, summary_est, reliable, estimate_reliable, pct_reliable)


#state-level detailed ----------------
data1 <- read_excel("undocumented_raw/Occ_723202095011.xlsx", range = "A19:B41", 
                   col_names = c("country","pop")) %>% mutate(group = "Detailed Asian Am Alone")
data2 <- read_excel("undocumented_raw/Occ_723202095011.xlsx", range = "A3", col_names = c("state")) %>% mutate(group = "Detailed Asian Am Alone")
data <- data1 %>% left_join(data2)
data_previous <- data %>% filter(group == "nhpi")
rm(data1, data2, data)

dict <- read_excel("undocumented_raw/list.xlsx")
i <- 1

for(i in 1:51) {
  print(i)
  if(i <= 51){
    path <- paste("undocumented_raw/", toString(dict[i,1]), sep = "")
    
    data1 <- read_excel(path, range = "A21:B44", col_names = c("country","pop")) %>% mutate(group = "Detailed Asian Am Alone")
    data2 <- read_excel(path, range = "A3", col_names = c("state")) %>% mutate(group = "Detailed Asian Am Alone")
    data <- data1 %>% left_join(data2)
    data <- rbind(data, data_previous)
    data_previous <- data
  }else{
    data_final <- data_previous
    return(data_final)
    
  }
  
  i <- i + 1
  
}
rm(data1, data2, data_previous)


state_detailed_pop <- get_acs(variables = c("B02015_002", "B02015_007", "B02015_012", "B02015_022", "B02015_018"), geography = "state", year = 2018, survey = "acs1") %>% 
  rename(summary_est = estimate,
         summary_moe = moe) %>% 
  mutate(group = case_when(
    variable == "B02015_002" ~"Indian", 
    variable == "B02015_007" ~"Chinese", 
    variable == "B02015_012" ~"Korean", 
    variable == "B02015_022" ~"Vietnamese", 
    variable == "B02015_018" ~"Pakistani")) %>% 
  select(NAME, group, summary_est, summary_moe)


final_state_detailed <- data %>% 
  mutate(group = case_when(
    country == "India" ~"Indian",
    country == "China" ~"Chinese",
    country == "South Korea" ~"Korean",
    country == "Vietnam" ~"Vietnamese",
    country == "Pakistan" ~"Pakistani",
    TRUE ~NA_character_)) %>% 
  filter(is.na(group) == F) %>% 
  mutate(NAME = str_remove(state, "Results for: ")) %>% 
  left_join(state_detailed_pop) %>% 
  mutate(geography = "state",
         topic = "undoc", 
         label = paste("Undocumented",group, sep = " "),
         group = "Detailed Asian Am Alone",
         estimate = as.numeric(pop),
         reliable = case_when(
           is.na(estimate) == T ~"NO",
           is.na(summary_est) == T ~"NO",
           summary_moe > 0.5 * summary_est ~"NO",
           TRUE ~"YES"),
         pct = case_when(
           summary_est > 0 ~round(estimate / summary_est, digits = 3),
           TRUE ~NA_real_),
         estimate_reliable = case_when(
           reliable == "YES" ~estimate,
           TRUE ~NA_real_),
         pct_reliable = case_when(
           reliable == "YES" ~pct,
           TRUE ~NA_real_)) %>% 
  select(geography, NAME, group, topic, label, estimate, pct, summary_est, reliable, estimate_reliable, pct_reliable)

final <- rbind(final_state_detailed, final_state_aa, final_us_detailed, final_us_aa)

write_csv(final, "acs_database/undocumented.csv", na = "")
