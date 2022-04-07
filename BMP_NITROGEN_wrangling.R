


### BMP_data_wrangling_NITROGEN







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

Analyte2 <- c('Nitrogen, Nitrite (NO2) + Nitrate (NO3) as N',"Nitrogen, nitrate (NO3) as N",
              'Nitrate', 'Nitrite',
              'nitrogen', "Nitrogen", 'Nitrogen, nitrite (NO2) as N',
              "Kjeldahl nitrogen", "Nitrogen, ammonium (NH4) as N",
              'Nitrogen, ammonia as N')
newAnalyte <- c("Nitrate", "Nitrate", 'Nitrate', 'Nitrate',
                "Total Nitrogen", "Total Nitrogen", 'Nitrate',
                'TKN', 'Ammonia', 'Ammonia')
Keys <- data.frame(Analyte2,newAnalyte )






Nit <- read.csv("WaterQuality.csv", header = T)  %>%
  select('SiteID','MSID','EventID','DateSample', #'TimeSample',
         'Analyte','Value_SubHalfDL', 'SampleFraction', 'Value_Unit',         ## might want to double check WQQualifer and SampleFraction
         'SampleType') %>%
  filter(SampleType == 'EMC-Flow Weighted') %>%
  filter(Value_SubHalfDL >= -0.001) %>%
  filter(Analyte %in% c("Nitrogen, nitrate (NO3) as N","Nitrogen, ammonium (NH4) as N", 
                        "Organic Nitrogen","Nitrogen, nitrite (NO2) as N",
                        "Nitrogen",  "nitrogen","Nitrogen, Nitrite (NO2) + Nitrate (NO3) as N",
                        "Nitrogen, ammonia as N","Kjeldahl nitrogen", 
                        "Nitrate", "Nitrite" )) %>%
  mutate(Analyte_SampleType = paste(Analyte, SampleFraction)) %>%
  select(-c("SampleFraction")) %>%
  left_join(Keys, by = c("Analyte" = "Analyte2")) %>%
  select(-c("Analyte")) %>%
  rename(Analyte_harm = newAnalyte)



#table(Nit$new_names)


### change units of ortho-P as PO4 to ortho-P as P
## Phosphate = 95
## P = 31
## [ortho-P as PO4] * 31/95 = [ortho-p as P]
Nit$Value_harm <- Nit$Value_SubHalfDL


## remove old analyte values and names from the dataset
Nit <- Nit %>%
  select(-c("Value_SubHalfDL", "Analyte_SampleType", "SampleType"))

## average duplicate data points
Nit <- aggregate(Value_harm ~ SiteID + MSID + EventID + DateSample + 
                    #TimeSample 
                    + Value_Unit + Analyte_harm, data = Nit, FUN = mean)


### pivot wide
Nit.wide <- Nit  %>%
  pivot_wider(names_from = 'Analyte_harm', values_from = 'Value_harm' )    ##note collapse sampleTypes 


head(Nit.wide)


### merge phosphorus data with flow data
N.all <- flow.ID %>%
  left_join(Nit.wide, by = c("SiteID", "MSID", "EventID"))


## separate P data into 3 species [TP, OrthoP, TP.dis]

Ammonia.all <- flow.ID %>%
  left_join(Nit.wide, by = c("SiteID", "MSID", "EventID")) %>%
  drop_na('Ammonia')  # filter by a single analyte with measurements using this line


Nitrate.all <- flow.ID %>%
  left_join(Nit.wide, by = c("SiteID", "MSID", "EventID")) %>%
  drop_na('Nitrate') 


TKN.all <- flow.ID %>%
  left_join(Nit.wide, by = c("SiteID", "MSID", "EventID")) %>%
  drop_na('TKN') 


TN.all <- flow.ID %>%
  left_join(Nit.wide, by = c("SiteID", "MSID", "EventID")) %>%
  drop_na('Total Nitrogen') 

### match inflow/outflow data


### Ammonia

Ammonia.wide <- Ammonia.all[-3,] %>%              
  select(-c("TKN", "Total Nitrogen", "Nitrate")) %>%
  pivot_wider(id_cols = c("SiteID", "BMPID", "BMPType", "EventID", "DateStart", 
                          #"TimeStart", "TimeEnd",
                          "Value_Unit"), ### broke this by including Timestart and Time end
              names_from = 'MSType', values_from = c("Volume_Total", "Ammonia")) # %>%
  # mutate(C1 = as.character(TP_Outflow), C2 = as.character(TP_Inflow), C3 = as.character(Volume_Total_Inflow), 
  #        C4 = as.character(Volume_Total_Outflow)) %>%
  # filter(!grepl('c', C1 ), !grepl('c', C2 ), !grepl('c', C3 ), !grepl('c', C4 ) ) %>%     #removes duplicate measures (21)
  # mutate(TP_Outflow = as.double(C1), TP_Inflow = as.double(C2), Volume_Total_Inflow = as.double(C3), 
  #        Volume_Total_Outflow = as.double(C4)) %>%
  # select(-c(C1, C2, C3, C4))


Ammonia.final <- Ammonia.wide[complete.cases(Ammonia.wide[,10]),] ## Outflow
Ammonia.final <- Ammonia.final[complete.cases(Ammonia.final[,9]),] ## Inflow
names(Ammonia.final)[7] <- "Inflow_vol_m3"
names(Ammonia.final)[8] <- "Outflow_vol_m3"
Ammonia.final$Species <- "Ammonia"
names(Ammonia.final)[9] <- "Inflow_mg_L"
names(Ammonia.final)[10] <- "Outflow_mg_L"


write.csv(Ammonia.final, file = "BMP_SUMMARY_AMMONIA.csv")


### Nitrate

Nitrate.wide <- Nitrate.all[-3,] %>%              
  select(-c("TKN", "Total Nitrogen", "Ammonia")) %>%
  pivot_wider(id_cols = c("SiteID", "BMPID", "BMPType", "EventID", "DateStart", "Value_Unit"), 
              names_from = 'MSType', values_from = c("Volume_Total", "Nitrate")) 

Nitrate.final <- Nitrate.wide[complete.cases(Nitrate.wide[,10]),] ## Outflow
Nitrate.final <- Nitrate.final[complete.cases(Nitrate.final[,9]),] ## Inflow
names(Nitrate.final)[7] <- "Inflow_vol_m3"
names(Nitrate.final)[8] <- "Outflow_vol_m3"
Nitrate.final$Species <- "Nitrate"
names(Nitrate.final)[9] <- "Inflow_mg_L"
names(Nitrate.final)[10] <- "Outflow_mg_L"

write.csv(Nitrate.final, file = "BMP_SUMMARY_Nitrate.csv")



### TKN

TKN.wide <- TKN.all[-3,] %>%              
  select(-c("Nitrate", "Total Nitrogen", "Ammonia")) %>%
  pivot_wider(id_cols = c("SiteID", "BMPID", "BMPType", "EventID", "DateStart", "Value_Unit"), 
              names_from = 'MSType', values_from = c("Volume_Total", "TKN"))  %>%
mutate(C1 = as.character(TKN_Inflow), C2 = as.character(TKN_Outflow), C3 = as.character(Volume_Total_Inflow),
       C4 = as.character(Volume_Total_Outflow)) %>%
filter(!grepl('c', C1 ), !grepl('c', C2 ), !grepl('c', C3 ), !grepl('c', C4 ) ) %>%     #removes duplicate measures (21)
mutate(TKN_Inflow = as.double(C1), TKN_Outflow = as.double(C2), Volume_Total_Inflow = as.double(C3),
       Volume_Total_Outflow = as.double(C4)) %>%
select(-c(C1, C2, C3, C4))

TKN.final <- TKN.wide[complete.cases(TKN.wide[,10]),] ## Outflow
TKN.final <- TKN.final[complete.cases(TKN.final[,9]),] ## Inflow
names(TKN.final)[7] <- "Inflow_vol_m3"
names(TKN.final)[8] <- "Outflow_vol_m3"
TKN.final$Species <- "TKN"
names(TKN.final)[9] <- "Inflow_mg_L"
names(TKN.final)[10] <- "Outflow_mg_L"

write.csv(TKN.final, file = "BMP_SUMMARY_TKN.csv")



### TN

TN.wide <- TN.all[-3,] %>%              
  select(-c("TKN", "Nitrate", "Ammonia")) %>%
  pivot_wider(id_cols = c("SiteID", "BMPID", "BMPType", "EventID", "DateStart", "Value_Unit"), 
              names_from = 'MSType', values_from = c("Volume_Total", "Total Nitrogen")) 

TN.final <- TN.wide[complete.cases(TN.wide[,10]),] ## Outflow
TN.final <- TN.final[complete.cases(TN.final[,9]),] ## Inflow
names(TN.final)[7] <- "Inflow_vol_m3"
names(TN.final)[8] <- "Outflow_vol_m3"
TN.final$Species <- "TN"
names(TN.final)[9] <- "Inflow_mg_L"
names(TN.final)[10] <- "Outflow_mg_L"

write.csv(Nitrate.final, file = "BMP_SUMMARY_TN.csv")









