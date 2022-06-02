### BMP_data_wrangling_NITROGEN
#' BMP Project
#' 
#' BMP + Stormwater
#' 
#' Last update: June 2, 2022


setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project") ## laptop
#setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/BMP_project")

library(tidyverse)

### READ IN AND ORGANIZE ALL OF THE DATA



## first read in the files that identify and classify each monitoring station 
## as either 'inflow' or 'outflow' and the type of BMP

ms <- read.csv("MonitoringStation.csv", header = TRUE)  %>%
  select(c( 'SiteID','MSID', 'BMPID','MSType'))  %>%
  filter(MSType == "Inflow" | MSType == "Outflow")

bmp <- read.csv("BMPInfo.csv", header = TRUE)  %>%
  select(c('BMPID','SiteID','BMPType', 'DateInstalled')) %>%
  mutate(BMPType = as.factor(BMPType))


# Join monitoring station info with bmp info
ms.bmp <- ms %>%
  left_join(bmp, by = c("SiteID","BMPID") ) ; rm(ms, bmp)


# Read in flow data ## and fix units!
flow <- read.csv("Flow.csv", header = T) %>%
  select('SiteID', 'MSID','EventID', 'DateStart', 
         'Volume_Total', 'Volume_Units') %>%
  filter(Volume_Total > 0) %>%
  mutate(Volume_Total = case_when(Volume_Units == 'L' ~ Volume_Total/1000,
                                  Volume_Units == 'm3' ~ Volume_Total,           
                                  Volume_Units == 'cf' ~ Volume_Total*0.0283,
                                  Volume_Units == 'CF' ~ Volume_Total*0.0283,
                                  Volume_Units == 'gal' ~ Volume_Total*0.00379,
                                  Volume_Units == 'AF' ~ Volume_Total*1233.5,)) %>%
  mutate(Volume_Units = "m3") 


## combine the identifying data with the flow data
flow.ID <- ms.bmp %>%
  left_join(flow, by = c("SiteID", "MSID"))  ;rm(flow, ms.bmp)


## Read in Analyte data
## To simplify matters, we will do this one analyte group at a time
## For Nitrogen
N.key <- read.csv("N_key.csv", head = TRUE) %>%
  select(c("old_names", "new_names"))

Nit <- read.csv("WaterQuality.csv", header = T)  %>%
  select('SiteID','MSID','EventID', 'SampleMedia', #'DateSample', #'TimeSample',
         'Analyte','Value_SubHalfDL', 'SampleFraction', 'Value_Unit', 'SampleType') %>%
  filter(SampleMedia == "Surface Runoff/Flow") %>%
  filter(SampleType == 'EMC-Flow Weighted') %>%
  filter(Value_SubHalfDL >= -0.001) %>%
  filter(Analyte %in% c("Nitrogen, nitrate (NO3) as N","Nitrogen, ammonium (NH4) as N", "Organic Nitrogen","Nitrogen, nitrite (NO2) as N",
                        "Nitrogen",  "nitrogen","Nitrogen, Nitrite (NO2) + Nitrate (NO3) as N",
                        "Nitrogen, ammonia as N","Kjeldahl nitrogen", "Nitrate", "Nitrite" )) %>%
  mutate(Analyte_SampleType = paste(Analyte, SampleFraction)) %>%
  select(-c("SampleFraction", "Analyte", "SampleMedia")) %>%
  left_join(N.key, by = c("Analyte_SampleType" = "old_names")) %>%
  rename(Analyte_harm = new_names)

table(Nit$Analyte_harm)

### change units of Nitrate to Nitrate_N [come back to this]
## Nitrate = 62
## N = 14
## [Nitrate] * 14/62 = [Nitrate_N]
Nit$Value_harm <- Nit$Value_SubHalfDL


## remove old analyte values and names from the dataset
Nit <- Nit %>%
  select(-c("Value_SubHalfDL", "Analyte_SampleType", "SampleType"))






## average  field duplicates into one
Nit <- aggregate(Value_harm ~ SiteID + MSID + EventID
                    + Value_Unit + Analyte_harm, data = Nit, FUN = mean)

#rename
Nit <- Nit %>%
  filter(Analyte_harm != "ns") %>%
  filter(Analyte_harm != "Nitrate") %>%
  filter(Analyte_harm != "ON") %>%
  droplevels() %>%
  rename(Species = Analyte_harm)

table(Nit$Species)


### merge phosphorus data with flow data
N.all <- Nit %>%
  left_join(flow.ID, by = c("SiteID", "MSID", "EventID")) %>%
  select(-c("DateStart")) %>%
  mutate(Analyte_load = Value_harm*Volume_Total) %>%
  mutate(load_units = "g_per_event")

load.combine <-  aggregate(Analyte_load ~ SiteID + EventID + Species + BMPID + BMPType + MSType, FUN = sum, data = N.all)
flow.combine <-  aggregate(Volume_Total ~ SiteID + EventID + Species + BMPID + BMPType + MSType, FUN = sum, data = N.all)

data <- load.combine %>%
  full_join(flow.combine, by = c("SiteID", "EventID", "Species", "BMPID", "BMPType", "MSType")) %>%
  mutate(Analyte_concentration = Analyte_load/Volume_Total) %>% ## mg/L
  pivot_wider(id_cols = c( "SiteID", "EventID", "Species", "BMPID", "BMPType"), 
              names_from = 'MSType', 
              values_from = c(Analyte_load, Volume_Total, Analyte_concentration)) %>%
  mutate(load_units = "g/event") %>%
  mutate(Volume_units = "m3/event") %>%
  mutate(concentration_units = "mg/L")

#rm(load.combine); rm(flow.combine); rm(N.all); rm(Nit)
data <- na.omit(data)          ### remove data without a matching inflow/outflow
data <- data[,c(1,2,4,5, 3, 12, 6, 7, 13, 8,9, 14, 10, 11)]
names(data) <- c("SiteID","EventID", "BMPID", "BMPType" ,  "Species", "Units_load","Load_in" ,"Load_out",
                 "Units_vol", "Vol_in", "Vol_out", "Units_conc", "Conc_in", "Conc_out")


### reintroduce dates
dates <- flow.ID %>%
  select(c("SiteID", "BMPID", "EventID", "DateStart")) %>%
  distinct()

N_final <- data %>%
  left_join(dates, by = c("SiteID", "BMPID", "EventID"))



## write out final data
write.csv(N_final, "BMP_N_clean.csv")




