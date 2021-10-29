



#' BMP Project
#' 
#' BMP + Stormwater
#' 
#' 


setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")

library(dplyr)
library(tidyr)

flow <- read.csv("Flow.csv", header = T)
wq <- read.csv("WaterQuality.csv", header = T)
unique(wq$Analyte)

#### filter for only P analytes
P_data <- wq %>%
  filter(Analyte %in% c("Phosphorus as P", "Phosphorus, orthophosphate as P", "Orthophosphate", 
                        "Phosphorus", "Phosphorus, orthophosphate as PO4", "Phosphorus, Particulate Organic", 
                        "Phosphorus, organic as P"))
#### summarize data by analyte
P_data <- wq %>%
  filter(Analyte %in% c("Phosphorus as P", "Phosphorus, orthophosphate as P", "Orthophosphate", 
                        "Phosphorus", "Phosphorus, orthophosphate as PO4", "Phosphorus, Particulate Organic", 
                        "Phosphorus, organic as P")) %>%
  group_by(Analyte) %>%
  summarize(numAn = n())


events <- wq %>% 
  group_by(MSID, EventID) %>%
  summarize(uniq = n())







tss <- wq %>%
  filter(Analyte %in% c("Total solids", "Total Solids",
                        "Total dissolved solids", "Total Dissolved Solids",
                        "total suspended solids", "Total Suspended Solids", "Total suspended solids",
                        "Volatile Suspended Solids", "Total volatile solids", 
                        "Suspended Sediment Concentration (SSC)",
                        "Turbidity"))

carbon <- wq %>%
  filter(Analyte %in% c("Organic Carbon"))

bacteria <- wq %>%
  filter(Analyte %in% c("Fecal Coliform", "Escherichia coli", "Enterococcus" , "Fecal coliform", 
                        "Total coliform", "Total Coliform", "Fecal Streptococcus Group Bacteria" ))

other  <- wq %>%
  filter(Analyte %in% c("Temperature, water", "pH", "Hardness, carbonate", "Hardness, Ca, Mg",
                        "Conductivity",  "Alkalinity", "Specific conductance",
                        "Temperature, air", "Dissolved oxygen (DO)", "Chloride"))


P_data %>% count(Analyte)

All_wq <- wq %>%
  left_join(flow, by = c("MSID", "EventID"))


test <- All_wq %>%
  drop_na(TimeEnd)
count(test[which(test$SampleType == "EMC-Flow Weighted"),])  
  
  
count(All_wq[which(All_wq$SampleType == "EMC-Flow Weighted"),])

test <- All_wq %>%
  filter(SampleType %in% c("EMC-Flow Weighted")) %>%
  drop_na(PeakFlow_Rate)


count(is.na(All_wq$TimeEnd))

      
      
      unique(wq$SampleType)
count(wq[which(wq$SampleType == "EMC-Flow Weighted"),])
count(wq[which()])