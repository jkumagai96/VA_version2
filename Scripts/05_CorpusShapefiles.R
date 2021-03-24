# Joy Kumagai (joy.kumagai@senckenberg.de)
# Date: Dec 2nd 2020
# Create four shapefiles with the CORPUS dataset, indicators, and variables for before 1990, 1990-1999, 2000 - 2009, 2010 to now
# Values Assessment 

#### Load Packages ####
library(tidyverse)
library(readxl)
library(FAOSTAT)
library(sf)

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
HDI <- read.csv("Data/Human development index.csv", header = F)
GDP <- read.csv("Data/GDP.csv")
GLO <- read.csv("data/learning-outcomes-vs-gdp-per-capita.csv")
POP <- read.csv("Data/population.csv")
CPI <- read_excel("Data/2018_CPI_FullDataSet.xlsx", 
                  skip = 2,
                  sheet = "CPI Timeseries 2012 - 2018") %>% 
  select(Country, ISO3, starts_with("CPI score"))  

countrycodes <- read.csv("Data/iso_codes_2020_06_15.csv")
indicators <- read.csv("Outputs/Indicators_compiled.csv")
column_names <- read_excel("Data/names_of_columns.xlsx")
country_poly <- read_sf("Data/UNEP_CountryShapefile/Country_Polygon.shp")
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
gdp_2018 <- translateCountryCode(data = GDP, from = "FAOST_CODE", to = "ISO3_CODE", "Area.Code") # Add's the ISO Code to the data
gdp_2018[207, 2]  <- "SSD"
gdp_2018 <- gdp_2018 %>% 
  select(ISO3_CODE, Value) %>% # Unit is US Dollars Millions 
  drop_na()
colnames(gdp_2018) <- c("ISO_Alpha_3", "GDP_2018")

# CPI
CPI <- CPI %>% 
  select(ISO3, `CPI score 2018`) %>% 
  rename("ISO_Alpha_3" = ISO3, "CPI_2018" = `CPI score 2018`)

# HDI
colnames(HDI) <- HDI[2,]
HDI <- HDI[-c(1,2),]
HDI <- HDI %>% 
  select(Country, "2018") 
HDI <- HDI[-(190:212),] # remove non-county specific data

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
nas[6,3] <- "DOM"
nas[7,3] <- "SWZ"
nas[8,3] <- "GMB"
nas[9,3] <- "HKG"
nas[10,3] <- "KOR"
nas[11,3] <- "LAO"
nas[12,3] <- "MHL"
nas[13,3] <- "MDA"
nas[14,3] <- "NLD"
nas[15,3] <- "NER"
nas[16,3] <- "PHL"
nas[17,3] <- "RUS"
nas[18,3] <- "SDN"
nas[19,3] <- "SYR"
nas[20,3] <- "TZA"
nas[21,3] <- "ARE"
nas[22,3] <- "GBR"
nas[23,3] <- "USA"

HDI <- rbind(HDI, nas) # join back the countries that were not originally joined
HDI <- HDI[,c(2,3)] # removes country column 
colnames(HDI) <- c("hdi_2018", "ISO_Alpha_3")

# POP
pop_2018 <- translateCountryCode(data = POP, from = "FAOST_CODE", to = "ISO3_CODE", "Area.Code") %>% # Add's the ISO Code to the data
  mutate(ISO3_CODE = replace(ISO3_CODE, Area=="South Sudan", "SSD")) %>% 
  select(ISO3_CODE, Value) %>% # Unit is 1000 persons (2018) 
  drop_na()

colnames(pop_2018) <- c("ISO_Alpha_3", "Pop_2018") 

#### Full Corpus ####
corpusE <- corpus

# Names1 
n1 <- corpusE %>% 
  countFunction(x = corpusE$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n, "Names1_log" = n_log)

# Names2
n2 <- corpusE %>% 
  countFunction(x = corpusE$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names2" = n, "Names2_log" = n_log)

# Join datasets together
df <- full_join(n1, n2, by = "ISO_Alpha_3")
df <- full_join(df, HDI, by = "ISO_Alpha_3")
df <- full_join(df, GLO, by = "ISO_Alpha_3")
df <- full_join(df, gdp_2018, by = "ISO_Alpha_3")
df <- full_join(df, CPI, by = "ISO_Alpha_3")
df <- full_join(df, pop_2018, by = "ISO_Alpha_3")
df <- full_join(df, indicators, by = "ISO_Alpha_3")
# Export 
write.csv(df, "Outputs/Corpus_YearsDivision/VA_Corpus__full.csv", row.names = F)


#### Corpus before 1990 ####
corpusA <- corpus %>% 
  filter(PY < "1990")

# Names1 
n1 <- corpusA %>% 
  countFunction(x = corpusA$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n, "Names1_log" = n_log)

# Names2
n2 <- corpusA %>% 
  countFunction(x = corpusA$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names2" = n, "Names2_log" = n_log)

# Join datasets together
df <- full_join(n1, n2, by = "ISO_Alpha_3")
df <- full_join(df, HDI, by = "ISO_Alpha_3")
df <- full_join(df, GLO, by = "ISO_Alpha_3")
df <- full_join(df, gdp_2018, by = "ISO_Alpha_3")
df <- full_join(df, CPI, by = "ISO_Alpha_3")
df <- full_join(df, pop_2018, by = "ISO_Alpha_3")
df <- full_join(df, indicators, by = "ISO_Alpha_3")

# Export 
write.csv(df, "Outputs/Corpus_YearsDivision/VA_corpus__before1990.csv", row.names = F)

#### Corpus 1990 - 1999 ####
corpusB <- corpus %>% 
  filter(PY >= "1990",
         PY < "2000")

# Names1 
n1 <- corpusB %>% 
  countFunction(x = corpusB$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n, "Names1_log" = n_log)

# Names2
n2 <- corpusB %>% 
  countFunction(x = corpusB$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names2" = n, "Names2_log" = n_log)

# Join datasets together
df <- full_join(n1, n2, by = "ISO_Alpha_3")
df <- full_join(df, HDI, by = "ISO_Alpha_3")
df <- full_join(df, GLO, by = "ISO_Alpha_3")
df <- full_join(df, gdp_2018, by = "ISO_Alpha_3")
df <- full_join(df, CPI, by = "ISO_Alpha_3")
df <- full_join(df, pop_2018, by = "ISO_Alpha_3")
df <- full_join(df, indicators, by = "ISO_Alpha_3")

# Export 
write.csv(df, "Outputs/Corpus_YearsDivision/VA_corpus__1990_1999.csv", row.names = F)


#### Corpus 2000 - 2009 ####
corpusC <- corpus %>% 
  filter(PY >= "2000",
         PY < "2010")

# Names1 
n1 <- corpusC %>% 
  countFunction(x = corpusC$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n, "Names1_log" = n_log)

# Names2
n2 <- corpusC %>% 
  countFunction(x = corpusC$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names2" = n, "Names2_log" = n_log)

# Join datasets together
df <- full_join(n1, n2, by = "ISO_Alpha_3")
df <- full_join(df, HDI, by = "ISO_Alpha_3")
df <- full_join(df, GLO, by = "ISO_Alpha_3")
df <- full_join(df, gdp_2018, by = "ISO_Alpha_3")
df <- full_join(df, CPI, by = "ISO_Alpha_3")
df <- full_join(df, pop_2018, by = "ISO_Alpha_3")
df <- full_join(df, indicators, by = "ISO_Alpha_3")

# Export 
write.csv(df, "Outputs/Corpus_YearsDivision/VA_corpus__2000_2009.csv", row.names = F)

#### Corpus 2010 - present ####
corpusD <- corpus %>% 
  filter(PY >= "2010")

# Names1 
n1 <- corpusD %>% 
  countFunction(x = corpusD$CountryName_TI_AB_DE_ID) %>% 
  rename("ISO_Alpha_3" = x, "Names1" = n, "Names1_log" = n_log)

# Names2
n2 <- corpusD %>% 
  countFunction(x = corpusD$CountryName_CI_FU_FX) %>% 
  rename("ISO_Alpha_3" = x, "Names2" = n, "Names2_log" = n_log)

# Join datasets together
df <- full_join(n1, n2, by = "ISO_Alpha_3")
df <- full_join(df, HDI, by = "ISO_Alpha_3")
df <- full_join(df, GLO, by = "ISO_Alpha_3")
df <- full_join(df, gdp_2018, by = "ISO_Alpha_3")
df <- full_join(df, CPI, by = "ISO_Alpha_3")
df <- full_join(df, pop_2018, by = "ISO_Alpha_3")
df <- full_join(df, indicators, by = "ISO_Alpha_3")

# Export 
write.csv(df, "Outputs/Corpus_YearsDivision/VA_corpus__2010_present.csv", row.names = F)
