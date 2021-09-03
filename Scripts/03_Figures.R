# Joy Kumagai (joy.kumagai@senckenberg.de)
# Date: September 3rd 2021
# Create Figures 
# Values Assessment 

#### Declare Functions ####
plotFunctionN1 <- function(data, x, y, IT) {
  p1 <- ggplot(data = data, aes(x = x, y = y)) +
    geom_point() +
    geom_point() + 
    geom_smooth(method = "lm") +
    theme_bw() +
    labs(x = "Density of Studies", y = "Value", title = IT) +
    theme(plot.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  return(p1)
}

plotFunctionN1_log <- function(data, x, y, IT) {
  p1 <- ggplot(data = data, aes(x = x, y = y)) +
    geom_point() +
    geom_point() + 
    geom_smooth(method = "lm") +
    theme_bw() +
    labs(x = "Density of Studies (log)", y = "Value", title = IT) +
    theme(plot.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  return(p1)
}

plotFunctionN2 <- function(data, x, y, IT) {
  p1 <- ggplot(data = data, aes(x = x, y = y)) +
    geom_point() +
    geom_point() + 
    geom_smooth(method = "lm") +
    theme_bw() +
    labs(x = "Density of Institutions", y = "Value", title = IT) +
    theme(plot.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  return(p1)
}

plotFunctionN2_log <- function(data, x, y, IT) {
  p1 <- ggplot(data = data, aes(x = x, y = y)) +
    geom_point() +
    geom_point() + 
    geom_smooth(method = "lm") +
    theme_bw() +
    labs(x = "Density of Institutions (log)", y = "Value", title = IT) +
    theme(plot.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  return(p1)
}

#### Load Packages ####
library(tidyverse)
library(cowplot)
library(readxl)

#### Load Data ####
setwd("~/IPBES/R/VA_version2")
column_names <- read_excel("Data/names_of_columns.xlsx")
indicators <- read.csv("Outputs/Indicators_compiled.csv")

setwd("C:/Users/jkumagai/Documents/IPBES/R/VA_version2/Outputs/Uptake_Q13_2010") # CHANGE THIS 
data <- read.csv("harmonized_data_13.csv") 

#### Individual Figures for Names 1 ####
# Figure Names 1 vs. Names 2
plot1 <- data %>% 
  ggplot(aes(x = Names1, y = Names2)) +
  geom_point() +
  labs(x = "Density of Studies", y = "Density of Institutions") +
  geom_smooth(method = "lm") +
  theme_bw()
plot1

# Figure Names1 vs. HDI
plot2 <- data %>% 
  ggplot(aes(x = Names1, y = hdi_2018)) +
  geom_point() +
  labs(x = "Density of Studies", y = "Humman Development Index (2018)") +
  geom_smooth(method = "lm") +
  theme_bw()
plot2

plot2_log <- data %>% 
  ggplot(aes(x = Names1_log, y = hdi_2018)) +
  geom_point() +
  labs(x = "Density of Studies (log)", y = "Humman Development Index (2018)") +
  geom_smooth(method = "lm") +
  theme_bw()

# Figure Names 1 vs. Learning Outcomes 
plot3 <- data %>% 
  ggplot(aes(x = Names1, y = Learning_outcomes_2015)) +
  geom_point() +
  labs(x = "Density of Studies", y = "Learning Outcomes (2015)") +
  geom_smooth(method = "lm") +
  theme_bw()
plot3

plot3_log <- data %>% 
  ggplot(aes(x = Names1_log, y = Learning_outcomes_2015)) +
  geom_point() +
  labs(x = "Density of Studies (log)", y = "Learning Outcomes (2015)") +
  geom_smooth(method = "lm") +
  theme_bw()
plot3_log

# Figure Names 1 vs. GDP
plot4 <- data %>% 
  ggplot(aes(x = Names1, y = GDP_2019)) +
  geom_point() +
  labs(x = "Density of Studies", y = "GDP (2019)") +
  geom_smooth(method = "lm") +
  theme_bw()
plot4

plot4_log <- data %>% 
  ggplot(aes(x = Names1_log, y = GDP_2019)) +
  geom_point() +
  labs(x = "Density of Studies (log)", y = "GDP (2019)") +
  geom_smooth(method = "lm") +
  theme_bw()
plot4_log

# Figure Names 1 vs. CPI 
plot5 <- data %>% 
  ggplot(aes(x = Names1, y = CPI_2020)) +
  geom_point() +
  labs(x = "Density of Studies", y = "Corruption Perception Index (2020)") +
  geom_smooth(method = "lm") +
  theme_bw()
plot5

plot5_log <- data %>% 
  ggplot(aes(x = Names1_log, y = CPI_2020)) +
  geom_point() +
  labs(x = "Density of Studies (log)", y = "Corruption Perception Index (2020)") +
  geom_smooth(method = "lm") +
  theme_bw()
plot5_log


plot11 <- data %>% 
  mutate(gdp_per_pop = GDP_2019/Pop_2018) %>% 
  ggplot(aes(x = Names1, y = gdp_per_pop)) +
  geom_point() +
  labs(x = "Density of Studies", y = "GDP per captia (2018)") +
  geom_smooth(method = "lm") +
  theme_bw()
plot11

plot11_log <- data %>% 
  mutate(gdp_per_pop = GDP_2019/Pop_2018) %>% 
  ggplot(aes(x = Names1_log, y = gdp_per_pop)) +
  geom_point() +
  labs(x = "Density of Studies (log)", y = "GDP per captia (2018)") +
  geom_smooth(method = "lm") +
  theme_bw()
plot11_log

# Export 
ggsave("Names1_Names2.png", plot1, device = "png", width = 8, height = 6, dpi = 600)
ggsave("Names1_HDI.png", plot2, device = "png", width = 8, height = 6, dpi = 600)
ggsave("Names1_HDI_log.png", plot2_log, device = "png", width = 8, height = 6, dpi = 600)
ggsave("Names1_LearningOutcomes.png", plot3, device = "png", width = 8, height = 6, dpi = 600)
ggsave("Names1_LearningOutcomes_log.png", plot3_log, device = "png", width = 8, height = 6, dpi = 600)
ggsave("Names1_GDP.png", plot4, device = "png", width = 8, height = 6, dpi = 600)
ggsave("Names1_GDP_log.png", plot4_log, device = "png", width = 8, height = 6, dpi = 600)
ggsave("Names1_CPI.png", plot5, device = "png", width = 8, height = 6, dpi = 600)
ggsave("Names1_CPI_log.png", plot5_log, device = "png", width = 8, height = 6, dpi = 600)
ggsave("Names1_GDP_per_pop.png", plot11, device = "png", width = 8, height = 6, dpi = 600)
ggsave("Names1_GDP_per_pop_log.png", plot11_log, device = "png", width = 8, height = 6, dpi = 600)
#### Individual Figures for Names 2 ####

# Figure Names2 vs. HDI
plot6 <- data %>% 
  ggplot(aes(x = Names2, y = hdi_2018)) +
  geom_point() +
  labs(x = "Density of Institutions", y = "Humman Development Index (2018)") +
  geom_smooth(method = "lm") +
  theme_bw()
plot6

# Figure Names 1 vs. Learning Outcomes 
plot7 <- data %>% 
  ggplot(aes(x = Names2, y = Learning_outcomes_2015)) +
  geom_point() +
  labs(x = "Density of Institutions", y = "Learning Outcomes (2015)") +
  geom_smooth(method = "lm") +
  theme_bw()
plot7

# Figure Names 1 vs. GDP
plot8 <- data %>% 
  ggplot(aes(x = Names2, y = GDP_2019)) +
  geom_point() +
  labs(x = "Density of Institutions", y = "GDP (2019)") +
  geom_smooth(method = "lm") +
  theme_bw()
plot8

# Figure Names 1 vs. CPI 
plot9 <- data %>% 
  ggplot(aes(x = Names2, y = CPI_2020)) +
  geom_point() +
  labs(x = "Density of Institutions", y = "Corruption Perception Index (2020)") +
  geom_smooth(method = "lm") +
  theme_bw()
plot9

plot10 <- data %>% 
  mutate(gdp_per_pop = GDP_2019/Pop_2018) %>% 
  ggplot(aes(x = Names2, y = gdp_per_pop)) +
  geom_point() +
  labs(x = "Density of Institutions", y = "GDP per captia (2018)") +
  geom_smooth(method = "lm") +
  theme_bw()
plot10

# Export 
ggsave("Names2_HDI.png", plot6, device = "png", width = 8, height = 6, dpi = 600)
ggsave("Names2_LearningOutcomes.png", plot7, device = "png", width = 8, height = 6, dpi = 600)
ggsave("Names2_GDP.png", plot8, device = "png", width = 8, height = 6, dpi = 600)
ggsave("Names2_CPI.png", plot9, device = "png", width = 8, height = 6, dpi = 600)
ggsave("Names2_GDP_per_pop.png", plot10, device = "png", width = 8, height = 6, dpi = 600)

#### Indicator Correlation Plots Names 1 ####

all_data <- data %>% 
  left_join(y = indicators, by = "ISO_Alpha_3")

titles <- column_names[,2]

# Names 1
plot1 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_1, titles[1,]) 
plot2 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_2, titles[2,])
plot3 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_3, titles[3,])
plot4 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_4, titles[4,])
plot5 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_5, titles[5,]) 
plot6 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_6, titles[6,])
plot7 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_7, titles[7,]) 
plot8 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_8, titles[8,]) 
plot9 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_9, titles[9,]) 
plot10 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_10, titles[10,])
plot11 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_11, titles[11,])
# Plot 12
plot13 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_13, titles[12,])
# Plot 14
plot15 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_15, titles[13,])
plot16 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_16, titles[14,])
plot17 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_17, titles[15,])
plot18 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_18, titles[16,])
plot19 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_19, titles[17,])
plot20 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_20, titles[18,])
plot21 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_21, titles[19,])
plot22 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_22, titles[20,])
plot23 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_23, titles[21,])
plot24 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_24, titles[22,])
plot25 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_25, titles[23,])
plot26 <- plotFunctionN1(data = all_data, x = all_data$Names1, y = all_data$I_26, titles[24,])

# 5x5 plot  (25 indicators total)
Names1plot <- plot_grid(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9,
                        plot10, plot11, plot13, plot15, plot16, plot17, plot18, 
                        plot19, plot20, plot21, plot22, plot23, plot24, plot25, plot26,
                        nrow = 5)
ggsave("Correlation25_Names1.png", Names1plot, device = "png", width = 16, height = 13, dpi = 800)


# Names 1 Log
plot1 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_1, titles[1,]) 
plot2 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_2, titles[2,])
plot3 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_3, titles[3,])
plot4 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_4, titles[4,])
plot5 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_5, titles[5,]) 
plot6 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_6, titles[6,])
plot7 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_7, titles[7,]) 
plot8 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_8, titles[8,]) 
plot9 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_9, titles[9,]) 
plot10 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_10, titles[10,])
plot11 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_11, titles[11,])
# Plot 12
plot13 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_13, titles[12,])
# Plot 14
plot15 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_15, titles[13,])
plot16 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_16, titles[14,])
plot17 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_17, titles[15,])
plot18 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_18, titles[16,])
plot19 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_19, titles[17,])
plot20 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_20, titles[18,])
plot21 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_21, titles[19,])
plot22 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_22, titles[20,])
plot23 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_23, titles[21,])
plot24 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_24, titles[22,])
plot25 <- plotFunctionN1_log(data = all_data, x = all_data$Names1_log, y = all_data$I_25, titles[23,])
plot26 <- plotFunctionN1(data = all_data, x = all_data$Names1_log, y = all_data$I_26, titles[24,])

# 5x5 plot  (25 indicators total)
Names1_logplot <- plot_grid(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9,
                        plot10, plot11, plot13, plot15, plot16, plot17, plot18, 
                        plot19, plot20, plot21, plot22, plot23, plot24, plot25, plot26,
                        nrow = 5)
ggsave("Correlation25_Names1_log.png", Names1_logplot, device = "png", width = 16, height = 13, dpi = 800)


#### Indicator Correlation Plots Names 2 ####

# Names 2
plot1 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_1, titles[1,]) 
plot2 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_2, titles[2,])
plot3 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_3, titles[3,])
plot4 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_4, titles[4,])
plot5 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_5, titles[5,]) 
plot6 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_6, titles[6,])
plot7 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_7, titles[7,]) 
plot8 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_8, titles[8,]) 
plot9 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_9, titles[9,]) 
plot10 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_10, titles[10,])
plot11 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_11, titles[11,])
# Plot 12
plot13 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_13, titles[12,])
# Plot 14
plot15 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_15, titles[13,])
plot16 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_16, titles[14,])
plot17 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_17, titles[15,])
plot18 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_18, titles[16,])
plot19 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_19, titles[17,])
plot20 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_20, titles[18,])
plot21 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_21, titles[19,])
plot22 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_22, titles[20,])
plot23 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_23, titles[21,])
plot24 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_24, titles[22,])
plot25 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_25, titles[23,])
plot26 <- plotFunctionN2(data = all_data, x = all_data$Names2, y = all_data$I_26, titles[24,])

# 5x5 plot  (25 indicators total)
Names2plot <- plot_grid(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9,
                        plot10, plot11, plot13, plot15, plot16, plot17, plot18, 
                        plot19, plot20, plot21, plot22, plot23, plot24, plot25, plot26,
                        nrow = 5)
ggsave("Correlation25_Names2.png", Names2plot, device = "png", width = 16, height = 13, dpi = 800)


# Names 2 Log
plot1 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_1, titles[1,]) 
plot2 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_2, titles[2,])
plot3 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_3, titles[3,])
plot4 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_4, titles[4,])
plot5 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_5, titles[5,]) 
plot6 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_6, titles[6,])
plot7 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_7, titles[7,]) 
plot8 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_8, titles[8,]) 
plot9 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_9, titles[9,]) 
plot10 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_10, titles[10,])
plot11 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_11, titles[11,])
# Plot 12
plot13 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_13, titles[12,])
# Plot 14
plot15 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_15, titles[13,])
plot16 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_16, titles[14,])
plot17 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_17, titles[15,])
plot18 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_18, titles[16,])
plot19 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_19, titles[17,])
plot20 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_20, titles[18,])
plot21 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_21, titles[19,])
plot22 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_22, titles[20,])
plot23 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_23, titles[21,])
plot24 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_24, titles[22,])
plot25 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_25, titles[23,])
plot26 <- plotFunctionN2_log(data = all_data, x = all_data$Names2_log, y = all_data$I_26, titles[24,])

# 5x5 plot  (25 indicators total)
Names2_logplot <- plot_grid(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9,
                            plot10, plot11, plot13, plot15, plot16, plot17, plot18, 
                            plot19, plot20, plot21, plot22, plot23, plot24, plot25, plot26,
                            nrow = 5)
ggsave("Correlation25_Names2_log.png", Names2_logplot, device = "png", width = 16, height = 13, dpi = 800)


