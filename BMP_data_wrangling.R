
#' BMP Project
#' 
#' BMP + Stormwater
#' 
#' Last update: May 11, 2022


# setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project") ## laptop
setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/BMP_project")

library(tidyverse)

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
  select('SiteID', 'MSID','EventID', 'DateStart',
         'Volume_Total', 'Volume_Units') %>%
  filter(Volume_Total > 0) %>%
  mutate(Volume_Total = case_when(Volume_Units == 'L' ~ Volume_Total/1000,
                      Volume_Units == 'm3' ~ Volume_Total,           
                      Volume_Units == 'cf' ~ Volume_Total*0.0283,
                      Volume_Units == 'CF' ~ Volume_Total*0.0283,
                      Volume_Units == 'gal' ~ Volume_Total*0.00379,
                      Volume_Units == 'AF' ~ Volume_Total*1233.5,)) %>%
  mutate(Volume_Units = "m3")  #%>%


## combine the identifying data with the flow data
flow.ID <- ms.bmp %>%
  left_join(flow, by = c("SiteID", "MSID")) ;rm(flow, ms.bmp)


## Read in Analyte data
## To simplify matters, we will do this one analyte group at a time
## First, Phosphorus
P.key <- read.csv("Phos_Key.csv", head = TRUE) %>%
  select(c("old_names", "new_names"))

Phos <- read.csv("WaterQuality.csv", header = T)  %>%
  select('SiteID','MSID','EventID',  'SampleMedia',  #'DateSample', #'TimeSample',
         'Analyte','Value_SubHalfDL', 'SampleFraction', 'Value_Unit',         ## might want to double check WQQualifer and SampleFraction
         'SampleType') %>%
  filter(SampleMedia == "Surface Runoff/Flow") %>%
  filter(SampleType == 'EMC-Flow Weighted') %>%
  filter(Value_SubHalfDL >= -0.001) %>%
  filter(Analyte %in% c("Phosphorus as P", "Phosphorus, orthophosphate as P", "Orthophosphate", 
                        "Phosphorus", "Phosphorus, orthophosphate as PO4")) %>%
  mutate(Analyte_SampleType = paste(Analyte, SampleFraction)) %>%
  select(-c("SampleFraction", "Analyte", "SampleMedia")) %>%
  left_join(P.key, by = c("Analyte_SampleType" = "old_names")) 

table(Phos$new_names)


### change units of ortho-P as PO4 to ortho-P as P
## Phosphate = 95
## P = 31
## [ortho-P as PO4] * 31/95 = [ortho-p as P]
Phos$Value_harm <- Phos$Value_SubHalfDL
Phos$Value_harm[Phos$new_names=="orthoPPO4"] <- Phos$Value_harm[Phos$new_names=="orthoPPO4"]*(31/95)
## create new column called "Analyte_harm" --- these are the harmonized analyte names to go with the harmonized analyte values
old_names2 <- c("ns", "orthoP", "orthoPPO4", "TP", "TP.dis")
Analyte_harm <-  c("ns", "orthoP", "orthoP" ,  "TP", "TP.dis")
key2 <- data.frame(old_names2, Analyte_harm) ; rm(old_names2, Analyte_harm)
Phos <- left_join(Phos, key2, by = c("new_names" = "old_names2"))
## remove old analyte values and names from the dataset
Phos <- Phos %>%
  select(-c("Value_SubHalfDL", "Analyte_SampleType", "SampleType", "new_names"))
rm(P.key); rm(key2);

###average field duplicates into one
Phos <- aggregate(Value_harm ~ SiteID + MSID + EventID + 
                    #DateSample + TimeSample 
                  + Value_Unit + Analyte_harm, data = Phos, FUN = mean)
#rename
Phos <- Phos %>%
  filter(Analyte_harm != "ns") %>%
  droplevels() %>%
  rename(Species = Analyte_harm)

table(Phos$Species)



### merge phosphorus data with flow data
P.all <- Phos %>%
  left_join(flow.ID, by = c("SiteID", "MSID", "EventID")) %>%
  select(-c("DateStart")) %>%
  mutate(Analyte_load = Value_harm*Volume_Total) %>%
  mutate(load_units = "g_per_event")

load.combine <-  aggregate(Analyte_load ~ SiteID + EventID + Species + BMPID + BMPType + MSType, FUN = sum, data = P.all)
flow.combine <-  aggregate(Volume_Total ~ SiteID + EventID + Species + BMPID + BMPType + MSType, FUN = sum, data = P.all)

data <- load.combine %>%
  full_join(flow.combine, by = c("SiteID", "EventID", "Species", "BMPID", "BMPType", "MSType")) %>%
  mutate(Analyte_concentration = Analyte_load/Volume_Total) %>% ## mg/L
  pivot_wider(id_cols = c( "SiteID", "EventID", "Species", "BMPID", "BMPType"), 
              names_from = 'MSType', 
              values_from = c(Analyte_load, Volume_Total, Analyte_concentration)) %>%
  mutate(load_units = "g/event") %>%
  mutate(Volume_units = "m3/event") %>%
  mutate(concentration_units = "mg/L")
rm(load.combine); rm(flow.combine); rm(P.all); rm(Phos)
data <- na.omit(data)          ### remove data without a matching inflow/outflow
data <- data[,c(1,2,4,5, 3, 12, 6, 7, 13, 8,9, 14, 10, 11)]
names(data) <- c("SiteID","EventID", "BMPID", "BMPType" ,  "Species", "Units_load","Load_in" ,"Load_out",
                 "Units_vol", "Vol_in", "Vol_out", "Units_conc", "Conc_in", "Conc_out")


### reintroduce dates
dates <- flow.ID %>%
  select(c("SiteID", "BMPID", "EventID", "DateStart")) %>%
  distinct()

P_final <- data %>%
  left_join(dates, by = c("SiteID", "BMPID", "EventID"))



## write out final data
write.csv(P_final, "BMP_P_clean.csv")























### Old workflow
## separate P data into 3 species [TP, OrthoP, TP.dis]

TP.all <- flow.ID %>%
  left_join(Phos.wide, by = c("SiteID", "MSID", "EventID")) %>%
  drop_na('TP')  # filter by a single analyte with measurements using this line


OrthoP.all <- flow.ID %>%
  left_join(Phos.wide, by = c("SiteID", "MSID", "EventID")) %>%
  drop_na('ortho-P') 
names(OrthoP.all)[14] <- "OP"  ## the hyphen causes issues later down the line


TP.dis.all <- flow.ID %>%
  left_join(Phos.wide, by = c("SiteID", "MSID", "EventID")) %>%
  drop_na('TP.dis') 



### match inflow/outflow data


### Total P

TP.wide <- TP.all[-3,] %>%              
  select(-c("ortho-P", "TP.dis")) %>%
  pivot_wider(id_cols = c("SiteID", "BMPID", "BMPType", "EventID", 
                          #"DateStart", "TimeStart", "TimeEnd",
                          "Value_Unit"), ### broke this by including Timestart and Time end
              names_from = 'MSType', values_from = c("Volume_Total", "TP"))  %>%
  mutate(C1 = as.character(TP_Inflow), C2 = as.character(TP_Outflow), C3 = as.character(Volume_Total_Inflow), 
         C4 = as.character(Volume_Total_Outflow)) %>%
  filter(!grepl('c', C1 ), !grepl('c', C2 ), !grepl('c', C3 ), !grepl('c', C4 ) ) %>%     #removes duplicate measures (21)
  mutate(TP_Inflow = as.double(C1), TP_Outflow = as.double(C2), Volume_Total_Inflow = as.double(C3), 
         Volume_Total_Outflow = as.double(C4)) %>%
  select(-c(C1, C2, C3, C4)) %>%
  relocate(TP_Outflow, .after = TP_Inflow)%>%
  relocate(Volume_Total_Outflow, .after = Volume_Total_Inflow)


TP.final <- TP.wide[complete.cases(TP.wide[,9]),] ## TP_Outflow_mg_L
TP.final <- TP.final[complete.cases(TP.final[,8]),] ## TP_Inflow_mg_L
names(TP.final)[6] <- "Inflow_vol_m3"
names(TP.final)[7] <- "Outflow_vol_m3"
TP.final$Species <- "TP"
names(TP.final)[8] <- "Inflow_mg_L"
names(TP.final)[9] <- "Outflow_mg_L"

#write.csv(TP.final, file = "BMP_SUMMARY_TP.csv")


###Ortho-P

OrthoP.wide <- OrthoP.all[-3,] %>%              
  select(-c("TP", "TP.dis")) %>%
  pivot_wider(id_cols = c("SiteID", "BMPID", "BMPType", "EventID", "Value_Unit"), 
              names_from = 'MSType', values_from = c("Volume_Total", "OP")) %>%
  mutate(C1 = as.character(OP_Outflow), C2 = as.character(OP_Inflow), C3 = as.character(Volume_Total_Inflow), 
         C4 = as.character(Volume_Total_Outflow)) %>%
  filter(!grepl('c', C1 ), !grepl('c', C2 ), !grepl('c', C3 ), !grepl('c', C4 ) ) %>%
  mutate(OP_Outflow = as.double(C1), OP_Inflow = as.double(C2), Volume_Total_Inflow = as.double(C3), 
         Volume_Total_Outflow = as.double(C4)) %>%
  select(-c(C1, C2, C3, C4)) %>%
  relocate(OP_Outflow, .after = OP_Inflow)%>%
  relocate(Volume_Total_Outflow, .after = Volume_Total_Inflow)


OrthoP.final <- OrthoP.wide[complete.cases(OrthoP.wide[,9]),] ## Outflow_mg_L
OrthoP.final <- OrthoP.final[complete.cases(OrthoP.final[,8]),] ## Inflow_mg_L
names(OrthoP.final)[6] <- "Inflow_vol_m3"
names(OrthoP.final)[7] <- "Outflow_vol_m3"
OrthoP.final$Species <- "OrthoP"
names(OrthoP.final)[8] <- "Inflow_mg_L"
names(OrthoP.final)[9] <- "Outflow_mg_L"

#write.csv(OrthoP.final, file = "BMP_SUMMARY_OrthoP.csv")



### Total P (dissolved/filtered)

TPdis.wide <- TP.dis.all[-3,] %>%              
  select(-c("TP", "ortho-P"))  %>%
  pivot_wider(id_cols = c("SiteID", "BMPID", "BMPType", "EventID", "Value_Unit"), 
              names_from = 'MSType', values_from = c("Volume_Total", "TP.dis")) %>%
  mutate(C1 = as.character(TP.dis_Outflow), C2 = as.character(TP.dis_Inflow), C3 = as.character(Volume_Total_Inflow), 
         C4 = as.character(Volume_Total_Outflow)) %>%
  filter(!grepl('c', C1 ), !grepl('c', C2 ), !grepl('c', C3 ), !grepl('c', C4 ) ) %>%
  mutate(TP.dis_Outflow = as.double(C1), TP.dis_Inflow = as.double(C2), Volume_Total_Inflow = as.double(C3), 
         Volume_Total_Outflow = as.double(C4)) %>%
  select(-c(C1, C2, C3, C4)) %>%
  relocate(TP.dis_Outflow, .after = TP.dis_Inflow)%>%
  relocate(Volume_Total_Outflow, .after = Volume_Total_Inflow)

TPdis.final <- TPdis.wide[complete.cases(TPdis.wide[,9]),] ## Outflow_mg_L
TPdis.final <- TPdis.final[complete.cases(TPdis.final[,8]),] ## Inflow_mg_L
names(TPdis.final)[6] <- "Inflow_vol_m3"
names(TPdis.final)[7] <- "Outflow_vol_m3"
TPdis.final$Species <- "TPdis"
names(TPdis.final)[8] <- "Inflow_mg_L"
names(TPdis.final)[9] <- "Outflow_mg_L"

#write.csv(TPdis.final, file = "BMP_SUMMARY_TPdis.csv")


### reintroduce dates
dates <- flow.ID %>%
  select(c("SiteID", "BMPID", "EventID", "DateStart")) %>%
  distinct()

P_final <- rbind(TP.final, OrthoP.final, TPdis.final) %>%
  left_join(dates, by = c("SiteID", "BMPID", "EventID"))

## write out final csv

write.csv(P_final, file = "BMP_SUMMARY_P_all.csv")





