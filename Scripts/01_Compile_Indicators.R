# Joy Kumagai (joy.kumagai@senckenberg.de)
# Date: AUgust 31st 2021
# Compile Indicators
# Values Assessment 

#### Load Packages / Data ####
library(tidyverse)
library(readxl)

# Load Data 
indicators <- read.csv("Data/CountryLevelData_cleaned.csv")

##### Prepare Indicator Datasets #####
indicator1 <- indicators %>% 
  filter(Indicator == "Area of forest production under FSC and PEFC certification") %>% 
  filter(Category == "FSC_area") %>% 
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3)

indicator2 <- indicators %>% 
  filter(Indicator == "Biodiversity Habitat Index") %>% 
  filter(Category == "Average") %>% 
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3)
indicator2 <- indicator2[-c(34, 185),] # removed repeated USA value - went back to the original dataset to confirm which is correct (removed hawaii)

indicator3 <- indicators %>% 
  filter(Indicator == "Biodiversity Intactness Index") %>% 
  filter(Category == "Value") %>% 
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3)

indicator4 <- indicators %>% 
  filter(Indicator == "Biocapacity per capita") %>% 
  filter(Category == "Value - Total") %>% 
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3)

indicator5 <- indicators %>% 
  filter(Indicator == "Ecological Footprint per capita") %>% 
  filter(Category == "Value - Total") %>% 
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3)

indicator6 <- indicators %>% 
  filter(Indicator == "Forest area") %>% 
  filter(Category == "Forest area (1000ha)") %>% 
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3)

indicator7 <- indicators %>% 
  filter(Indicator == "Water Footprint") %>% 
  filter(Category == "Water Footprint - Total (Mm3/y)") %>% 
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3)
indicator7 <- indicator7[-33,] # removed repeated CIV value - went back to the original dataset to confirm which is correct 

indicator8 <- indicators %>% 
  filter(Indicator == "Inland Fishery Production") %>% 
  filter(Category == "Capture") %>% # can be aquaculture or capture 
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3) 

indicator9 <- indicators %>% 
  filter(Indicator == "Region-based Marine Trophic Index") %>% 
  filter(Category == "1950") %>% # Relevant to 1950 
  filter(Year == max(Year)) %>% # latest year of data
  group_by(ISO_Alpha_3) %>% 
  summarise(Value = mean(Value)) %>%                          # Took the mean of the regions!!!! 
  mutate(Indicator = "Region-mean Marine Trophic Index",
         Category = "1950",
         Year = 2014) %>% select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3)

indicator10 <- indicators %>% 
  filter(Indicator == "Nitrogen + Phosphate Fertilizers") %>% 
  filter(Category == "N total nutrients - Consumption in nutrients") %>% # selected nitrogen not phosphate consumption 
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3) 

indicator11 <- indicators %>% 
  filter(Indicator == "Nitrogen Use Efficiency (%)") %>% 
  filter(Category == "Nitrogen Use Efficiency (%)") %>%  
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  arrange(ISO_Alpha_3) 

# Indicator 12 is binary! So it is not included 

indicator13 <- indicators %>% 
  filter(Indicator == "Percentage and total area covered by protected areas") %>% 
  filter(Category == "Terrestrial - Protected Area (%)") %>%  # TERRESTRIAL, not marine 
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3)  

# indicator14 is binary! So it is not included

indicator15 <- indicators %>% 
  filter(Indicator == "Percentage of undernourished people") %>% 
  filter(Category == "Prevalence of undernourishment (%) (3-year average)") %>%  
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3) 

indicator16 <- indicators %>% 
  filter(Indicator == "Proportion of local breeds, classified as being at risk, not-at-risk or unknown level of risk of extinction") %>% 
  filter(Category == "At Risk of Extinction") %>%  # RISK OF EXTINCTION
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3) 

indicator17 <- indicators %>% 
  filter(Indicator == "PA of Key Biodiversity Areas Coverage (%)") %>% 
  filter(Category == "Estimate") %>%  
  filter(Year == max(Year)) %>% # latest year of data\
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3) 

indicator18 <- indicators %>% 
  filter(Indicator == "Protected area management effectiveness") %>% 
  filter(Category == "PA Assessed on Management Effectiveness (%)") %>%  
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3)

indicator19 <- indicators %>% 
  filter(Indicator == "Protected Area Connectedness Index") %>% 
  filter(Category == "Protected Area Connectedness Index") %>%  
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3) 
indicator19 <- indicator19[-c(41,74,233),] # removed repeated values - went back to the original dataset to confirm which is correct (233 removes hawaii)

indicator20 <- indicators %>% 
  filter(Indicator == "Species Habitat Index") %>% 
  filter(Category == "Species Habitat Index") %>%  
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3)
indicator20 <- indicator20[-176, ] ## removed repeated values by removing hawaii for USA)

indicator21 <- indicators %>% 
  filter(Indicator == "Species Protection Index (%)") %>% 
  mutate(Category = "Species Protection Index (%)") %>%  
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3)

indicator22 <- indicators %>% 
  filter(Indicator == "Species Status Information Index") %>% 
  filter(Category == "Value") %>%  
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3)

indicator23 <- indicators %>% 
  filter(Indicator == "Total Wood Removals (roundwood, m3)") %>% 
  filter(Category == "Total") %>%  
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3)

indicator24 <- indicators %>% 
  filter(Indicator == "Trends in forest extent (tree cover)") %>% 
  filter(Category == "Percentage of Tree Cover Loss") %>%  
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3)

indicator25 <- indicators %>% 
  filter(Indicator == "Nitrogen Deposition Trends (kg N/ha/yr)") %>% 
  filter(Category == "Nitrogen Deposition Trends (kg N/ha/yr)") %>%  
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3)

indicator26 <- indicators %>% 
  filter(Indicator == "Trends in Pesticides Use") %>% 
  filter(Category == "Use of pesticides (3-year average)") %>%  
  filter(Year == max(Year)) %>% # latest year of data
  select(Indicator, Category, ISO_Alpha_3,Year, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  drop_na() %>% 
  arrange(ISO_Alpha_3)

indicator26 <- indicator26[-16,] # removed repeated china value - went back to the original dataset to confirm which is correct 

##### Join Indicator Datasets #####
indicator1$Indicator <- "I_1"
indicator2$Indicator <- "I_2"
indicator3$Indicator <- "I_3"
indicator4$Indicator <- "I_4"
indicator5$Indicator <- "I_5"
indicator6$Indicator <- "I_6"
indicator7$Indicator <- "I_7"
indicator8$Indicator <- "I_8"
indicator9$Indicator <- "I_9"
indicator10$Indicator <- "I_10"
indicator11$Indicator <- "I_11"
indicator13$Indicator <- "I_13"
indicator15$Indicator <- "I_15"
indicator16$Indicator <- "I_16"
indicator17$Indicator <- "I_17"
indicator18$Indicator <- "I_18"
indicator19$Indicator <- "I_19"
indicator20$Indicator <- "I_20"
indicator21$Indicator <- "I_21"
indicator22$Indicator <- "I_22"
indicator23$Indicator <- "I_23"
indicator24$Indicator <- "I_24"
indicator25$Indicator <- "I_25"
indicator26$Indicator <- "I_26"

datasets <- rbind(indicator1, indicator2, indicator3, indicator4, indicator5, indicator6, indicator7, 
                  indicator8, indicator9, indicator10, indicator11, indicator13,
                  indicator15, indicator16, indicator17, indicator18, indicator19, indicator20,
                  indicator21, indicator22, indicator23, indicator24, indicator25, indicator26) %>% 
  select(Indicator, ISO_Alpha_3, Value) %>% 
  pivot_wider(names_from = Indicator, values_from = Value)  %>% 
  arrange(ISO_Alpha_3)

# dir.create("Outputs/")
write.csv(datasets, "Outputs/Indicators_compiled.csv", row.names = F)
