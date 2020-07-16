############
#attention!!!
#CVAP data comes from the census special tabulation: 
#https://www.census.gov/programs-surveys/decennial-census/about/voting-rights/cvap.html
#download the latest data, unzip and replace files (nation.csv, state.csv, county.csv and cd.csv) inside cvap_raw folder

library(tidyverse)
library(stringr)

data <- read_csv("cvap_raw/Nation.csv")

data_clean <- function(){
  final_dta <- data %>%
    filter(lnnumber %in% c("4", "6"))
}
