# Joy Kumagai (joy.kumagai@senckenberg.de)
# Date: September 2nd 2021
# Join together the data and create final harmonized datasets to be analyzed 
# Values Assessment 

# Code is structured so that we load and clean all variables, then for each dataset: calculate the Names columns, 
# join the variables, and export 

#### Load Packages ####
library(tidyverse)
library(readxl)
library(FAOSTAT)

#### Declare Functions ####
countFunction <- function(data, x) {
  data %>% 
    mutate(x = strsplit(as.character(x), ", ")) %>% 
    unnest(x) %>% 
    count(x, sort = TRUE) %>% 
    drop_na() %>% 
    mutate(n_log = log(n))
}

#### Load Data ####
corpus <- read_excel("Data/IPBES_VA_Uptake_Corpus_06May20_GEONames_TS_16June20.xlsx", sheet = 1)
uptake <- read_excel("Data/Uptake_results_sample_characteristics.xlsx")
HDI <- read.csv("Data/Human development index (HDI).csv", header = F)
GDP <- read.csv("Data/GDP.csv", sep = "\t")
GLO <- read.csv("data/learning-outcomes-vs-gdp-per-capita.csv")
POP <- read.csv("Data/population.csv")
CPI <- read_excel("Data/2020_CPI_FullDataSet.xlsx", 
                  skip = 2,
                  sheet = "CPI Timeseries 2012 - 2020") %>% 
  select(Country, ISO3, starts_with("CPI score"))  

countrycodes <- read.csv("Data/iso_codes_2021_03_24.csv")

#### Clean Data ####
# GLO
GLO <- GLO %>% 
  select(Entity, Code, Year, Average.harmonised.learning.outcome.score..Altinok..Angrist..and.Patrinos..2018...) %>% 
  rename("Learning_outcomes_2015" = Average.harmonised.learning.outcome.score..Altinok..Angrist..and.Patrinos..2018...) %>% 
  filter(Code != "", 
         Learning_outcomes_2015 != "",
         Year == "2015") %>% # 2010 has more data 
  select(Code, Learning_outcomes_2015) %>% 
  rename("ISO_Alpha_3" = Code)

# GDP
gdp_2019 <- translateCountryCode(data = GDP, from = "FAOST_CODE", to = "ISO3_CODE", "Area.Code") # Add's the ISO Code to the data
gdp_2019[207, 2]  <- "SSD"
gdp_2019 <- gdp_2019 %>% 
  select(ISO3_CODE, Value) %>% # Unit is US Dollars Millions 
  drop_na()
colnames(gdp_2019) <- c("ISO_Alpha_3", "GDP_2019")

# CPI
CPI <- CPI %>% 
  select(ISO3, `CPI score 2020`) %>% 
  rename("ISO_Alpha_3" = ISO3, "CPI_2020" = `CPI score 2020`)

# HDI
colnames(HDI) <- HDI[2,]
HDI <- HDI[-c(1,2),]
HDI <- HDI %>% 
  select(Country, "2019") %>% 
  mutate(Country = str_trim(Country, side = "left")) 
HDI <- HDI[-(190:206),] # remove non-county specific data

# Add ISO_3 names
colnames(countrycodes) <- c("Country", "ISO_Alpha_3")
HDI <- left_join(x = HDI, y = countrycodes, by = "Country") 
nas <- HDI[rowSums(is.na(HDI)) > 0,] # separate country's that were not joined
HDI <- HDI %>% drop_na()
nas[1,3] <- "BHS"
nas[2,3] <- "CAF"
nas[3,3] <- "COM"
nas[4,3] <- "COG" 
nas[5,3] <- "COD"
nas[6,3] <- "CIV"
nas[7,3] <- "DOM"
nas[8,3] <- "SWZ"
nas[9,3] <- "GMB"
nas[10,3] <- "HKG"
nas[11,3] <- "KOR"
nas[12,3] <- "LAO"
nas[13,3] <- "MHL"
nas[14,3] <- "MDA"
nas[15,3] <- "NLD"
nas[16,3] <- "NER"
nas[17,3] <- "PHL"
nas[18,3] <- "RUS"
nas[19,3] <- "SDN"
nas[20,3] <- "SYR"
nas[21,3] <- "TZA"
nas[22,3] <- "ARE"
nas[23,3] <- "GBR"
nas[24,3] <- "USA"

HDI <- rbind(HDI, nas) # join back the countries that were not originally joined
HDI <- HDI[,c(2,3)] %>% arrange(ISO_Alpha_3) # removes country column 
colnames(HDI) <- c("hdi_2018", "ISO_Alpha_3")

# POP
pop_2018 <- translateCountryCode(data = POP, from = "FAOST_CODE", to = "ISO3_CODE", "Area.Code") %>% # Add's the ISO Code to the data
  mutate(ISO3_CODE = replace(ISO3_CODE, Area=="South Sudan", "SSD")) %>% 
  select(ISO3_CODE, Value) %>% # Unit is 1000 persons (2018) 
  drop_na()

colnames(pop_2018) <- c("ISO_Alpha_3", "Pop_2018") 

variables <- full_join(HDI, GLO, by = "ISO_Alpha_3")
variables <- full_join(variables, gdp_2019, by = "ISO_Alpha_3")
variables <- full_join(variables, CPI, by = "ISO_Alpha_3")
variables <- full_join(variables, pop_2018, by = "ISO_Alpha_3")

write.csv(variables, "Outputs/variables_compiled.csv", row.names = F)
#### Full Dataset ####
# Names1 
n1 <- corpus %>% 
  countFunction(x = corpus$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n, "Names1_log" = n_log)

# Names2
n2 <- corpus %>% 
  countFunction(x = corpus$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names2" = n, "Names2_log" = n_log)

# Join datasets together
df <- full_join(n1, n2, by = "ISO_Alpha_3")
df <- full_join(df, HDI, by = "ISO_Alpha_3")
df <- full_join(df, GLO, by = "ISO_Alpha_3")
df <- full_join(df, gdp_2019, by = "ISO_Alpha_3")
df <- full_join(df, CPI, by = "ISO_Alpha_3")
df <- full_join(df, pop_2018, by = "ISO_Alpha_3")

# Export 
# dir.create("Outputs/Corpus")
write.csv(df, "Outputs/Corpus/harmonized_data.csv", row.names = F)

#### Uptake - Q6 -> Yes ####
# Names1 
uptakeQ6 <- uptake %>% 
  filter(Q6informative == "YES")

n1 <- uptakeQ6 %>% 
  countFunction(x = uptakeQ6$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n, "Names1_log" = n_log)

# Names2
n2 <- uptakeQ6 %>% 
  countFunction(x = uptakeQ6$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names2" = n, "Names2_log" = n_log)

# Join datasets together
df <- full_join(n1, n2, by = "ISO_Alpha_3")
df <- full_join(df, HDI, by = "ISO_Alpha_3")
df <- full_join(df, GLO, by = "ISO_Alpha_3")
df <- full_join(df, gdp_2018, by = "ISO_Alpha_3")
df <- full_join(df, CPI, by = "ISO_Alpha_3")
df <- full_join(df, pop_2018, by = "ISO_Alpha_3")

# Export 
write.csv(df, "Outputs/Uptake_Q6/harmonized_data_Q6.csv", row.names = F)


#### Uptake - Q8 -> Yes ####
# Names1 
uptakeQ8 <- uptake %>% 
  filter(Q8decisive == "YES")

n1 <- uptakeQ8 %>% 
  countFunction(x = uptakeQ8$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n, "Names1_log" = n_log)

# Names2
n2 <- uptakeQ8 %>% 
  countFunction(x = uptakeQ8$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names2" = n, "Names2_log" = n_log)

# Join datasets together
df <- full_join(n1, n2, by = "ISO_Alpha_3")
df <- full_join(df, HDI, by = "ISO_Alpha_3")
df <- full_join(df, GLO, by = "ISO_Alpha_3")
df <- full_join(df, gdp_2018, by = "ISO_Alpha_3")
df <- full_join(df, CPI, by = "ISO_Alpha_3")
df <- full_join(df, pop_2018, by = "ISO_Alpha_3")

# Export 
write.csv(df, "Outputs/Uptake_Q8/harmonized_data_Q8.csv", row.names = F)


#### Uptake - Q13 -> Yes ####
# Names1 
uptakeQ13 <- uptake %>% 
  filter(Q13technical == "YES")

n1 <- uptakeQ13 %>% 
  countFunction(x = uptakeQ13$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n, "Names1_log" = n_log)

# Names2
n2 <- uptakeQ13 %>% 
  countFunction(x = uptakeQ13$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names2" = n, "Names2_log" = n_log)

# Join datasets together
df <- full_join(n1, n2, by = "ISO_Alpha_3")
df <- full_join(df, HDI, by = "ISO_Alpha_3")
df <- full_join(df, GLO, by = "ISO_Alpha_3")
df <- full_join(df, gdp_2018, by = "ISO_Alpha_3")
df <- full_join(df, CPI, by = "ISO_Alpha_3")
df <- full_join(df, pop_2018, by = "ISO_Alpha_3")

# Export 
write.csv(df, "Outputs/Uptake_Q13/harmonized_data_13.csv", row.names = F)

#### Uptake - Q6, 8, or 13 -> Yes ####
# Names1 
uptake_6_8_13 <- uptake %>% 
  filter(Q6informative == "YES" |
           Q8decisive == "YES" |
           Q13technical == "YES")

n1 <- uptake_6_8_13 %>% 
  countFunction(x = uptake_6_8_13$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n, "Names1_log" = n_log)

# Names2
n2 <- uptake_6_8_13 %>% 
  countFunction(x = uptake_6_8_13$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names2" = n, "Names2_log" = n_log)

# Join datasets together
df <- full_join(n1, n2, by = "ISO_Alpha_3")
df <- full_join(df, HDI, by = "ISO_Alpha_3")
df <- full_join(df, GLO, by = "ISO_Alpha_3")
df <- full_join(df, gdp_2018, by = "ISO_Alpha_3")
df <- full_join(df, CPI, by = "ISO_Alpha_3")
df <- full_join(df, pop_2018, by = "ISO_Alpha_3")

# Export 
write.csv(df, "Outputs/Uptake_Q6_8_13/harmonized_data_Q6_8_13.csv", row.names = F)


#### Uptake - Q2 Valuation ####
# Names1 
uptake_2 <- uptake %>% 
  filter(Q2ESvaluation == "Valuation of ES/NCP")

n1 <- uptake_2 %>% 
  countFunction(x = uptake_2$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n, "Names1_log" = n_log)

# Names2
n2 <- uptake_2 %>% 
  countFunction(x = uptake_2$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names2" = n, "Names2_log" = n_log)

# Join datasets together
df <- full_join(n1, n2, by = "ISO_Alpha_3")
df <- full_join(df, HDI, by = "ISO_Alpha_3")
df <- full_join(df, GLO, by = "ISO_Alpha_3")
df <- full_join(df, gdp_2018, by = "ISO_Alpha_3")
df <- full_join(df, CPI, by = "ISO_Alpha_3")
df <- full_join(df, pop_2018, by = "ISO_Alpha_3")

# Export 
write.csv(df, "Outputs/Uptake_Q2ESvaluation/harmonized_data_Q2.csv", row.names = F)

#### Uptake - Q4 ####
# Names1 
uptake_4 <- uptake %>% 
  filter(Q4uptake == "Cursory reference to uptake" |
           Q4uptake == "Documented uptake")

n1 <- uptake_4 %>% 
  countFunction(x = uptake_4$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n, "Names1_log" = n_log)

# Names2
n2 <- uptake_4 %>% 
  countFunction(x = uptake_4$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names2" = n, "Names2_log" = n_log)

# Join datasets together
df <- full_join(n1, n2, by = "ISO_Alpha_3")
df <- full_join(df, HDI, by = "ISO_Alpha_3")
df <- full_join(df, GLO, by = "ISO_Alpha_3")
df <- full_join(df, gdp_2018, by = "ISO_Alpha_3")
df <- full_join(df, CPI, by = "ISO_Alpha_3")
df <- full_join(df, pop_2018, by = "ISO_Alpha_3")

# Export 
write.csv(df, "Outputs/Uptake_Q4/harmonized_data_Q4.csv", row.names = F)

#### Uptake - Q8 AND 13 -> Yes ####
# Names1 
uptake_8_13 <- uptake %>% 
  filter(Q8decisive == "YES",
         Q13technical == "YES")

n1 <- uptake_8_13 %>% 
  countFunction(x = uptake_8_13$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n, "Names1_log" = n_log)

# Names2
n2 <- uptake_8_13 %>% 
  countFunction(x = uptake_8_13$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names2" = n, "Names2_log" = n_log)

# Join datasets together
df <- full_join(n1, n2, by = "ISO_Alpha_3")
df <- full_join(df, HDI, by = "ISO_Alpha_3")
df <- full_join(df, GLO, by = "ISO_Alpha_3")
df <- full_join(df, gdp_2018, by = "ISO_Alpha_3")
df <- full_join(df, CPI, by = "ISO_Alpha_3")
df <- full_join(df, pop_2018, by = "ISO_Alpha_3")

# Export 
write.csv(df, "Outputs/Uptake_Q8_13/harmonized_data_Q8_13.csv", row.names = F)


#### Uptake - Q8 OR Q13 -> Yes ####
# Names1 
uptake_8_13_OR <- uptake %>% 
  filter(Q8decisive == "YES" |
         Q13technical == "YES")

n1 <- uptake_8_13_OR %>% 
  countFunction(x = uptake_8_13_OR$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n, "Names1_log" = n_log)

# Names2
n2 <- uptake_8_13_OR %>% 
  countFunction(x = uptake_8_13_OR$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names2" = n, "Names2_log" = n_log)

# Join datasets together
df <- full_join(n1, n2, by = "ISO_Alpha_3")
df <- full_join(df, HDI, by = "ISO_Alpha_3")
df <- full_join(df, GLO, by = "ISO_Alpha_3")
df <- full_join(df, gdp_2018, by = "ISO_Alpha_3")
df <- full_join(df, CPI, by = "ISO_Alpha_3")
df <- full_join(df, pop_2018, by = "ISO_Alpha_3")

# Export 
write.csv(df, "Outputs/Uptake_Q8_13_OR/harmonized_data_Q8_13_OR.csv", row.names = F)
