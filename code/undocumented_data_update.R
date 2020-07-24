library(tidyverse)
library(readxl)
library(stringr)
library(tidycensus)
setwd("/Users/sunnyshao/Documents/quickstats_datapull/")



data1 <- read_excel("undocumented_raw/Occ_723202084837.xlsx", range = "D13", col_names = c("pop_18")) %>% mutate(race = "Asian American")
data2 <- read_excel("undocumented_raw/Occ_723202084837.xlsx", range = "A3", col_names = c("state")) %>% mutate(race = "Asian American")
data <- data1 %>% 
  left_join(data2)
data_previous <- data %>% filter(race == "nhpi")
rm(data1, data2, data)

dict <- read_excel("undocumented_raw/list.xlsx")


i <- 1
for(i in 1:51) {
  print(i)
  if(i <= 51){
    path <- paste("undocumented_raw/", toString(dict[i,1]), sep = "")
    
    data1 <- read_excel(path, range = "D13", col_names = c("pop_18")) %>% mutate(group = "Asian American Alone")
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


final <- data %>% 
  mutate(NAME = stringr::str_remove(state, "Results for: "),
         geography = "state",
         topic = "undoc",
         label = "Undocumented",
         estimate = as.numeric(pop_18),
         reliable = "YES",
         estimate_reliable = estimate) %>% 
  left_join(state_pop) %>% 
  mutate(pct = case_when(
    is.na(estimate) == F ~round(estimate/summary_est, digits = 3),
    TRUE ~NA_real_),
         pct_reliable = pct) %>% 
  select(geography, NAME, group, topic, label, estimate, pct, summary_est, reliable, estimate_reliable, pct_reliable)

write_csv(final, "acs_database/undocumented.csv", na = "")


