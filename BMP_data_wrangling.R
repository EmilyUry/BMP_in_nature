



#' BMP Project
#' 
#' BMP + Stormwater
#' 
#' Last update: January 27, 2022


# setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project") ## laptop
setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/BMP_project")

library(dplyr)
library(tidyr)


### READ IN AND ORGANIZE ALL OF THE DATA



## first read in the files that identify and classify each monitoring station 
## as either 'inflow' or 'outflow' and the type of BMP

ms <- read.csv("MonitoringStation.csv", header = TRUE)  %>%
  select(c( 'SiteID','MSID', 'BMPID','MSType'))  %>%
  filter(MSType == "Inflow" | MSType == "Outflow")

bmp <- read.csv("BMPInfo.csv", header = TRUE)  %>%
  select(c('BMPID','SiteID','BMPType')) %>%
  mutate(BMPType = as.factor(BMPType))


# Join monitoring station info with bmp info
ms.bmp <- ms %>%
  left_join(bmp, by = c("SiteID","BMPID") ) ; rm(ms, bmp)



# Read in flow data ## and fix units!
flow <- read.csv("Flow.csv", header = T) %>%
  select('SiteID', 'MSID','EventID', 'DateStart', 'DateEnd',
   'TimeStart', 'TimeEnd', 'Volume_Total', 'Volume_Units',
   'PeakFlow_Rate','PeakFlow_Units') %>%
  filter(Volume_Total > 0) %>%
  mutate(Volume_Total = case_when(Volume_Units == 'L' ~ Volume_Total/1000,
                      Volume_Units == 'm3' ~ Volume_Total,           
                      Volume_Units == 'cf' ~ Volume_Total*0.0283,
                      Volume_Units == 'CF' ~ Volume_Total*0.0283,
                      Volume_Units == 'gal' ~ Volume_Total*0.00379,
                      Volume_Units == 'AF' ~ Volume_Total*1233.5,))








## some quick data checks
table(flow$Volume_Units)   ## need to convert volumes to L
# nrow(distinct(flow, SiteID))
# nrow(distinct(flow, MSID))
# nrow(distinct(flow, EventID))
# 
# nrow(unique(flow[c('SiteID', 'MSID')]))
# nrow(unique(flow[c('SiteID', 'EventID')]))
# nrow(unique(flow[c('SiteID', 'EventID')]))
# check <- na.omit(flow$TimeEnd)  ## this is the number of event records with start and stop times


## combine the identifying data with the flow data

flow.ID <- ms.bmp %>%
  left_join(flow, by = c("SiteID", "MSID")) %>%
  select(-c(Volume_Units)) ;rm(flow, ms.bmp)




## Read in Analyte data
## To simplify matters, we will do this one analyte group at a time
## First, Phosphorus


### merge Analyte naming convention
old_names <-  c("Phosphorus as P Total","Phosphorus, orthophosphate as P NS", "Phosphorus Total", "Phosphorus Dissolved", 
                "Orthophosphate Total", "Orthophosphate NS","Phosphorus, orthophosphate as P Dissolved", 
                "Phosphorus, orthophosphate as PO4 NS", "Orthophosphate Dissolved", "Phosphorus as P Dissolved",  
                "Phosphorus Total recoverable", "Phosphorus, orthophosphate as P Total", "Phosphorus, Particulate Organic NS",
                "Phosphorus as P TOTAL" )
new_names <- c("TP", "ortho-P", "TP", "TP.dis", "ortho-P", "ortho-P", "ortho-P", "ortho-P-PO4", "ns", "TP.dis", "TP", 
               "ortho-P", "ns", "TP")
key <- data.frame(old_names, new_names); rm(new_names, old_names)


Phos <- read.csv("WaterQuality.csv", header = T)  %>%
  select('SiteID','MSID','EventID','DateSample','TimeSample',
         'Analyte','Value_SubHalfDL', 'SampleFraction', 'Value_Unit',         ## might want to double check WQQualifer and SampleFraction
         'SampleType') %>%
  filter(SampleType == 'EMC-Flow Weighted') %>%
  filter(Value_SubHalfDL >= -0.001) %>%
  filter(Analyte %in% c("Phosphorus as P", "Phosphorus, orthophosphate as P", "Orthophosphate", 
                        "Phosphorus", "Phosphorus, orthophosphate as PO4", "Phosphorus, Particulate Organic", 
                        "Phosphorus, organic as P")) %>%
  mutate(Analyte_SampleType = paste(Analyte, SampleFraction)) %>%
  select(-c("SampleFraction", "Analyte")) %>%
  left_join(key, by = c("Analyte_SampleType" = "old_names")) 

table(Phos$new_names)


### change units of ortho-P as PO4 to ortho-P as P
## Phosphate = 95
## P = 31
## [ortho-P as PO4] * 31/95 = [ortho-p as P]
Phos$Value_harm <- Phos$Value_SubHalfDL
Phos$Value_harm[Phos$new_names=="ortho-P-PO4"] <- Phos$Value_harm[Phos$new_names=="ortho-P-PO4"]*(31/95)


## create new column called "Analyte_harm" --- these are the harmonized analyte names to go with the harmonized analyte values
old_names2 <- c("ns", "ortho-P", "ortho-P-PO4", "TP", "TP.dis")
Analyte_harm <-  c("ns", "ortho-P", "ortho-P" ,  "TP", "TP.dis")
key2 <- data.frame(old_names2, Analyte_harm) ; rm(old_names2, Analyte_harm)
Phos <- left_join(Phos, key2, by = c("new_names" = "old_names2"))

## remove old analyte values and names from the dataset
Phos <- Phos %>%
  select(-c("Value_SubHalfDL", "Analyte_SampleType", "SampleType", "new_names"))

## average duplicate data points
Phos <- aggregate(Value_harm ~ SiteID + MSID + EventID + DateSample + TimeSample + Value_Unit + Analyte_harm, data = Phos, FUN = mean)


### pivot wide
Phos.wide <- Phos  %>%
  pivot_wider(names_from = 'Analyte_harm', values_from = 'Value_harm' )  %>%    ##note collapse sampleTypes 
  select(-c("ns"))

head(Phos.wide)


### merge phosphorus data with flow data
P.all <- flow.ID %>%
  left_join(Phos.wide, by = c("SiteID", "MSID", "EventID"))

TP.all <- flow.ID %>%
  left_join(Phos.wide, by = c("SiteID", "MSID", "EventID")) %>%
  drop_na('TP')  # filter by a single analyte with measurements using this line


OrthoP.all <- flow.ID %>%
  left_join(Phos.wide, by = c("SiteID", "MSID", "EventID")) %>%
  drop_na('ortho-P') 




### match inflow/outflow data


TP.wide <- TP.all[-3,] %>%               # there is randomly one duplicated measurment...grrr
  select(-c("ortho-P", "TP.dis")) %>%
  pivot_wider(id_cols = c("SiteID", "BMPID", "BMPType", "EventID", "DateStart", "Value_Unit"), 
              names_from = 'MSType', values_from = c("Volume_Total", "TP"))  %>%
  mutate(C1 = as.character(TP_Outflow), C2 = as.character(TP_Inflow), C3 = as.character(Volume_Total_Inflow), 
         C4 = as.character(Volume_Total_Outflow)) %>%
  filter(!grepl('c', C1 ), !grepl('c', C2 ), !grepl('c', C3 ), !grepl('c', C4 ) ) %>%
  mutate(TP_Outflow = as.double(C1), TP_Inflow = as.double(C2), Volume_Total_Inflow = as.double(C3), 
         Volume_Total_Outflow = as.double(C4)) %>%
  select(-c(C1, C2, C3, C4))
  

summary(TP.wide)
TP.test <- na.omit(TP.wide)
names(TP.test)[8] <- "Inflow_vol_m3"
names(TP.test)[7] <- "Outflow_vol_m3"

### fix the volume issue, convert everything to m3


write.csv(TP.test, file = "TP_toy.csv")























