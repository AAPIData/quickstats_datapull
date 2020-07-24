############
#attention!!!
#CVAP data comes from the census special tabulation: 
#https://www.census.gov/programs-surveys/decennial-census/about/voting-rights/cvap.html
#download the latest data, unzip and replace files (nation.csv, state.csv, county.csv and cd.csv) inside cvap_raw folder

library(tidyverse)
library(stringr)

data_us <- read_csv("cvap_raw/Nation.csv") %>% mutate(geography = "national")
data_st <- read_csv("cvap_raw/State.csv") %>% mutate(geography = "state")
data_ct <- read_csv("cvap_raw/County.csv") %>% mutate(geography = "county")
data_cd <- read_csv("cvap_raw/CD.csv") %>% mutate(geography = "district")

data <- rbind(data_us, data_st, data_ct, data_cd)


final_dta <- data %>%
  filter(lnnumber %in% c("4", "6")) %>% 
  rename(NAME = geoname) %>% 
  mutate(group = case_when(
    lntitle == "Asian Alone" ~"Asian American Alone",
    lntitle == "Native Hawaiian or Other Pacific Islander Alone" ~"NHPI Alone")) %>% 
  mutate(estimate = tot_est,
         pct = case_when(
           tot_est > 0 ~round(cvap_est/tot_est, digits = 3),
           TRUE ~NA_real_)) %>% 
  mutate(reliable = case_when(
    cvap_moe <= 0.5 * cvap_est ~"YES",
    TRUE ~"NO")) %>% 
  mutate(estimate_reliable = case_when(
    reliable == "YES" ~estimate,
    TRUE ~NA_real_),
         pct_reliable = case_when(
    reliable == "YES" ~pct,
    TRUE ~NA_real_)) %>% 
  mutate(topic = "cvap",
         label = "CVAP") %>% 
  filter(!str_detect(NAME, "Puerto Rico")) %>% 
select(NAME, topic, group, geography, label, estimate, pct, reliable, estimate_reliable, pct_reliable)

write_csv(final_dta, "acs_database/cvap_dta.csv", na = "")
