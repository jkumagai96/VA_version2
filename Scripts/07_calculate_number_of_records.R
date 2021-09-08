# Joy Kumagai (joy.kumagai@senckenberg.de)
# Date: September 8th 2021
# Calculate number of records 
# Values Assessment 

#### Load Packages ####
library(tidyverse)
library(readxl)

#### Load Data ####
data <- read_excel("Data/IPBES_VA_Uptake_Corpus_06May20_GEONames_TS_16June20.xlsx", sheet = 1)

# How many records 
nrow(data)
    # 79,040
  
# How many reocrds in the corpus for 2010 or later
data %>%  
  filter(PY >= 2010) %>% 
  nrow()
    # 56,650
data %>%  
  filter(PY > 2010) %>% 
  nrow()
    # 53,310

# How many records in corpus for 2010 or later and have at least one country in Name 1
data %>% 
  filter(PY >= 2010) %>% 
  drop_na(CountryName_TI_AB_DE_ID) %>% 
  nrow()
    # 35,705

data %>% 
  filter(PY > 2010) %>% 
  drop_na(CountryName_TI_AB_DE_ID) %>% 
  nrow()
    # 33,606

# How many records in corpus for 2010 or later and have at least one country in Name 2
data %>% 
  filter(PY >= 2010) %>% 
  drop_na(CountryName_CI_FU_FX) %>% 
  nrow()
    # 56,562    

data %>% 
  filter(PY > 2010) %>% 
  drop_na(CountryName_CI_FU_FX) %>% 
  nrow()
    # 53,233   

# How many records in the coprus for 2010 or later and have no country identified at all
data %>% 
  filter(PY >= 2010) %>% 
  filter_at(vars(starts_with("CountryName")), all_vars(is.na(.))) %>% 
  nrow()
    # 45  

data %>% 
  filter(PY > 2010) %>% 
  filter_at(vars(starts_with("CountryName")), all_vars(is.na(.))) %>% 
  nrow()
    # 41  

  