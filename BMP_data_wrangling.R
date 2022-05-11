



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
  select(c('BMPID','SiteID','BMPType', 'DateInstalled')) %>%
  mutate(BMPType = as.factor(BMPType))


# Join monitoring station info with bmp info
ms.bmp <- ms %>%
  left_join(bmp, by = c("SiteID","BMPID") ) ; rm(ms, bmp)



# Read in flow data ## and fix units!
flow <- read.csv("Flow.csv", header = T) %>%
  select('SiteID', 'MSID','EventID', 'DateStart', 'DateEnd',
        # 'TimeStart', 'TimeEnd', 
         'Volume_Total', 'Volume_Units',
   'PeakFlow_Rate','PeakFlow_Units') %>%
  filter(Volume_Total > 0) %>%
  mutate(Volume_Total = case_when(Volume_Units == 'L' ~ Volume_Total/1000,
                      Volume_Units == 'm3' ~ Volume_Total,           
                      Volume_Units == 'cf' ~ Volume_Total*0.0283,
                      Volume_Units == 'CF' ~ Volume_Total*0.0283,
                      Volume_Units == 'gal' ~ Volume_Total*0.00379,
                      Volume_Units == 'AF' ~ Volume_Total*1233.5,)) #%>%
  #mutate(TimeStart = TimeStart - floor(TimeStart)) %>%
  #mutate(TimeEnd = TimeEnd - floor(TimeEnd))


## some quick data checks
##table(flow$Volume_Units)   
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



P.key <- read.csv("Phos_Key.csv", head = TRUE) %>%
  select(c("old_names", "new_names"))

Phos <- read.csv("WaterQuality.csv", header = T)  %>%
  select('SiteID','MSID','EventID','DateSample', #'TimeSample',
         'Analyte','Value_SubHalfDL', 'SampleFraction', 'Value_Unit',         ## might want to double check WQQualifer and SampleFraction
         'SampleType') %>%
  filter(SampleType == 'EMC-Flow Weighted') %>%
  filter(Value_SubHalfDL >= -0.001) %>%
  filter(Analyte %in% c("Phosphorus as P", "Phosphorus, orthophosphate as P", "Orthophosphate", 
                        "Phosphorus", "Phosphorus, orthophosphate as PO4", "Phosphorus, Particulate Organic", 
                        "Phosphorus, organic as P")) %>%
  mutate(Analyte_SampleType = paste(Analyte, SampleFraction)) %>%
  select(-c("SampleFraction", "Analyte")) %>%
  left_join(P.key, by = c("Analyte_SampleType" = "old_names")) 

# table(Phos$Analyte_SampleType)
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
Phos <- aggregate(Value_harm ~ SiteID + MSID + EventID + DateSample + 
                    #TimeSample 
                  + Value_Unit + Analyte_harm, data = Phos, FUN = mean)


### pivot wide
Phos.wide <- Phos  %>%
  pivot_wider(names_from = 'Analyte_harm', values_from = 'Value_harm' )  %>%    ##note collapse sampleTypes 
  select(-c("ns"))

head(Phos.wide)


### merge phosphorus data with flow data
P.all <- flow.ID %>%
  left_join(Phos.wide, by = c("SiteID", "MSID", "EventID")) 



## separate P data into 3 species [TP, OrthoP, TP.dis]

TP.all <- flow.ID %>%
  left_join(Phos.wide, by = c("SiteID", "MSID", "EventID")) %>%
  drop_na('TP')  # filter by a single analyte with measurements using this line


OrthoP.all <- flow.ID %>%
  left_join(Phos.wide, by = c("SiteID", "MSID", "EventID")) %>%
  drop_na('ortho-P') 
names(OrthoP.all)[15] <- "OP"  ## the hyphen causes issues later down the line


TP.dis.all <- flow.ID %>%
  left_join(Phos.wide, by = c("SiteID", "MSID", "EventID")) %>%
  drop_na('TP.dis') 



### match inflow/outflow data


### Total P

TP.wide <- TP.all[-3,] %>%              
  select(-c("ortho-P", "TP.dis")) %>%
  pivot_wider(id_cols = c("SiteID", "BMPID", "BMPType", "EventID", "DateStart", 
                          #"TimeStart", "TimeEnd",
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


TP.final <- TP.wide[complete.cases(TP.wide[,10]),] ## TP_Outflow
TP.final <- TP.final[complete.cases(TP.final[,9]),] ## TP_Inflow
names(TP.final)[7] <- "Inflow_vol_m3"
names(TP.final)[8] <- "Outflow_vol_m3"
TP.final$Species <- "TP"
names(TP.final)[9] <- "Inflow_mg_L"
names(TP.final)[10] <- "Outflow_mg_L"

#write.csv(TP.final, file = "BMP_SUMMARY_TP.csv")


###Ortho-P

OrthoP.wide <- OrthoP.all[-3,] %>%              
  select(-c("TP", "TP.dis")) %>%
  pivot_wider(id_cols = c("SiteID", "BMPID", "BMPType", "EventID", "DateStart", "Value_Unit"), 
              names_from = 'MSType', values_from = c("Volume_Total", "OP")) %>%
  mutate(C1 = as.character(OP_Outflow), C2 = as.character(OP_Inflow), C3 = as.character(Volume_Total_Inflow), 
         C4 = as.character(Volume_Total_Outflow)) %>%
  filter(!grepl('c', C1 ), !grepl('c', C2 ), !grepl('c', C3 ), !grepl('c', C4 ) ) %>%
  mutate(OP_Outflow = as.double(C1), OP_Inflow = as.double(C2), Volume_Total_Inflow = as.double(C3), 
         Volume_Total_Outflow = as.double(C4)) %>%
  select(-c(C1, C2, C3, C4)) %>%
  relocate(OP_Outflow, .after = OP_Inflow)%>%
  relocate(Volume_Total_Outflow, .after = Volume_Total_Inflow)


OrthoP.final <- na.omit(OrthoP.wide)
names(OrthoP.final)[7] <- "Inflow_vol_m3"
names(OrthoP.final)[8] <- "Outflow_vol_m3"
OrthoP.final$Species <- "OrthoP"
names(OrthoP.final)[9] <- "Inflow_mg_L"
names(OrthoP.final)[10] <- "Outflow_mg_L"

#write.csv(OrthoP.final, file = "BMP_SUMMARY_OrthoP.csv")



### Total P (dissolved/filtered)

TPdis.wide <- TP.dis.all[-3,] %>%              
  select(-c("TP", "ortho-P"))  %>%
  pivot_wider(id_cols = c("SiteID", "BMPID", "BMPType", "EventID", "DateStart", "Value_Unit"), 
              names_from = 'MSType', values_from = c("Volume_Total", "TP.dis")) %>%
  mutate(C1 = as.character(TP.dis_Outflow), C2 = as.character(TP.dis_Inflow), C3 = as.character(Volume_Total_Inflow), 
         C4 = as.character(Volume_Total_Outflow)) %>%
  filter(!grepl('c', C1 ), !grepl('c', C2 ), !grepl('c', C3 ), !grepl('c', C4 ) ) %>%
  mutate(TP.dis_Outflow = as.double(C1), TP.dis_Inflow = as.double(C2), Volume_Total_Inflow = as.double(C3), 
         Volume_Total_Outflow = as.double(C4)) %>%
  select(-c(C1, C2, C3, C4)) %>%
  relocate(TP.dis_Outflow, .after = TP.dis_Inflow)%>%
  relocate(Volume_Total_Outflow, .after = Volume_Total_Inflow)

TPdis.final <- na.omit(TPdis.wide)
names(TPdis.final)[7] <- "Inflow_vol_m3"
names(TPdis.final)[8] <- "Outflow_vol_m3"
TPdis.final$Species <- "TPdis"
names(TPdis.final)[9] <- "Inflow_mg_L"
names(TPdis.final)[10] <- "Outflow_mg_L"

#write.csv(TPdis.final, file = "BMP_SUMMARY_TPdis.csv")





P_final <- rbind(TP.final, OrthoP.final, TPdis.final)



write.csv(P_final, file = "BMP_SUMMARY_P_all.csv")





