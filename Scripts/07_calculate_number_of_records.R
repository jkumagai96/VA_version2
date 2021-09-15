# Joy Kumagai (joy.kumagai@senckenberg.de)
# Date: September 8th 2021
# Calculate number of records 
# Values Assessment 

#### Load Packages ####
library(tidyverse)
library(readxl)

#### Load Data ####
data <- read_excel("Data/IPBES_VA_Uptake_Corpus_06May20_GEONames_TS_16June20.xlsx", sheet = 1)

#### Analysis ####
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


# How many records per region for names 1?

data %>% 
  mutate(x = strsplit(as.character(Region_TI_AB_DE_ID), ", ")) %>% 
  unnest(x) %>% 
  count(x, sort = TRUE)

# How many records per region including 2010 or after for names 1?
data %>% 
  filter(PY >= 2010) %>% 
  mutate(x = strsplit(as.character(Region_TI_AB_DE_ID), ", ")) %>% 
  unnest(x) %>% 
  count(x, sort = TRUE)

# How may records per sub region for names 1?
data %>% 
  mutate(x = strsplit(as.character(Subregion_TI_AB_DE_ID), ", ")) %>% 
  unnest(x) %>% 
  count(x, sort = TRUE)

# How many records per sub region for names 1 >= 2010?
data %>% 
  filter(PY >= 2010) %>% 
  mutate(x = strsplit(as.character(Subregion_TI_AB_DE_ID), ", ")) %>% 
  unnest(x) %>% 
  count(x, sort = TRUE)

# How many records per region for names 2?

data %>% 
  mutate(x = strsplit(as.character(Region_CI_FU_FX), ", ")) %>% 
  unnest(x) %>% 
  count(x, sort = TRUE)

# How many records per region including 2010 or after for names 2?
data %>% 
  filter(PY >= 2010) %>% 
  mutate(x = strsplit(as.character(Region_CI_FU_FX), ", ")) %>% 
  unnest(x) %>% 
  count(x, sort = TRUE)

# How may records per sub region for names 2?
data %>% 
  mutate(x = strsplit(as.character(Subregion_CI_FU_FX), ", ")) %>% 
  unnest(x) %>% 
  count(x, sort = TRUE)

# How many records per sub region for names 2 >= 2010?
data %>% 
  filter(PY >= 2010) %>% 
  mutate(x = strsplit(as.character(Subregion_CI_FU_FX), ", ")) %>% 
  unnest(x) %>% 
  count(x, sort = TRUE)
