



#' BMP Project
#' 
#' BMP + Stormwater
#' 
#' 


setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")

library(dplyr)

flow <- read.csv("Flow.csv", header = T)
wq <- read.csv("WaterQuality.csv", header = T)


P_data <- wq %>%
  filter(Analyte %in% c("Phosphorus as P", "Phosphorus, orthophosphate as P", "Orthophosphate", 
                        "Phosphorus", "Phosphorus, orthophosphate as PO4", "Phosphorus, Particulate Organic", 
                        "Phosphorus, organic as P")) %>%
  left_join(flow, by = c("MSID", "EventID"))


TotalP <- P_data %>%
  select(c("MSDI", "EventID"))



P_data %>% count(Analyte)

