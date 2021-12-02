



#' BMP Project
#' 
#' BMP + Stormwater
#' 
#' Last update: November 4, 2021


# setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project") ## laptop
setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/BMP_project")

library(dplyr)
library(tidyr)


### READ IN AND ORGANIZE ALL OF THE DATA

ms <- read.csv("MonitoringStation.csv", header = TRUE)  %>%
  select(c( 'SiteID','MSID', 'BMPID','MSType'))  %>%
  filter(MSType == "Inflow" | MSType == "Outflow")

bmp <- read.csv("BMPInfo.csv", header = TRUE)  %>%
  select(c('BMPID','SiteID','BMPType')) %>%
  mutate(BMPType = as.factor(BMPType))


ms.bmp <- ms %>%
  left_join(bmp, by = c("SiteID","BMPID") ) # ; rm(ms, bmp)


flow <- read.csv("Flow.csv", header = T) %>%
  select('SiteID', 'MSID','EventID', 'DateStart', 'DateEnd',
   'TimeStart', 'TimeEnd', 'Volume_Total', 'Volume_Units',
   'PeakFlow_Rate','PeakFlow_Units') %>%
  filter(Volume_Total > 0)


table(flow$Volume_Units)   ## need to convert volumes to L


newd <- ms.bmp %>%
  left_join(flow, by = c("SiteID", "MSID"))


long.wq <- read.csv("WaterQuality.csv", header = T) #%>%
  select('SiteID','MSID','EventID','DateSample','TimeSample',
         'Analyte','Value_SubHalfDL', 'WQQualifier',
         'SampleType','SampleFraction') %>%
  filter(SampleType == 'EMC-Flow Weighted')




wq <- read.csv("WaterQuality.csv", header = T)  %>%
  select('SiteID','MSID','EventID','DateSample','TimeSample',
         'Analyte','Value_SubHalfDL', 'WQQualifier',
         'SampleType','SampleFraction') %>%
  filter(SampleType == 'EMC-Flow Weighted') %>%
  pivot_wider(names_from = 'Analyte', values_from = 'Value_SubHalfDL' )  ##note collapse sampleTypes



Phos <- read.csv("WaterQuality.csv", header = T)  %>%
  select('SiteID','MSID','EventID','DateSample','TimeSample',
         'Analyte','Value_SubHalfDL', 'SampleFraction', 'Value_Unit',         ## might want to double check WQQualifer and SampleFraction
         'SampleType') %>%
  filter(SampleType == 'EMC-Flow Weighted') %>%
  filter(Analyte %in% c("Phosphorus as P", "Phosphorus, orthophosphate as P", "Orthophosphate", 
                        "Phosphorus", "Phosphorus, orthophosphate as PO4", "Phosphorus, Particulate Organic", 
                        "Phosphorus, organic as P")) %>%
  mutate(Analyte_SampleType = paste(Analyte, SampleFraction)) %>%
  select(-c("SampleFraction", "Analyte")) %>%
  pivot_wider(names_from = 'Analyte_SampleType', values_from = 'Value_SubHalfDL' )  ##note collapse sampleTypes




all <- newd %>%
  left_join(wq, by = c("SiteID", "MSID", "EventID"))








  
test <- aggregate(newd$Volume_Total, by = newd[c("SiteID", "EventID", "MSType", "BMPType", "DateStart", "Volume_Units")], sum)








nrow(distinct(test, SiteID, EventID, BMPID )) ## 19752
nrow(distinct(test, SiteID, EventID, BMPID, Volume_Units )) ## 19752
  
nrow(distinct(flow, SiteID))


test <- merge(ms.bmp, flow, by = c("SiteID", "MSID"), all.y = TRUE) %>%
  filter(Volume_Total > -999)


test3 <- aggregate(test$Volume_Total, by = test[c("SiteID", "BMPID", "EventID", "MSType")], sum) %>%
  pivot_wider(names_from = MSType, values_from = x)


test2 <- aggregate(test$Volume_Total, by = test[c("SiteID", "BMPID", "EventID", "MSType",         #"DateStart", "DateEnd", "TimeStart", "TimeEnd",
                                                  "BMPType", "DateStart", "Volume_Units")], sum) %>%
        pivot_wider(names_from = MSType, values_from = x)
  
  
table(bmp$BMPType)


uniq <- unique(bmp$BMPID)
length(uniq)


flow.ms <- flow %>%
  left_join(ms, by = c("MSID")) #; rm(flow, ms)

wq <- read.csv("WaterQuality.csv", header = T)  %>%
  select('SiteID','MSID','EventID','DateSample','TimeSample',
         'Analyte','Value_SubHalfDL', 'WQQualifier',
         'SampleType','SampleFraction')

flow.ms.wq <- flow.ms %>%
  left_join(wq, by = c("MSID", "EventID"))

flow.ms.wq <- flow.ms %>%
  left_join(wq, by = c("MSID", "EventID", "SiteID")) #; rm(flow.ms, wq)

bmp <- read.csv("BMPInfo.csv", header = TRUE) %>%
  select(c('BMPID','SiteID','BMPType')) %>%
  mutate(BMPType = as.factor(BMPType))

all <- flow.ms.wq %>%
  left_join(bmp, by = c("SiteID", "BMPID")) ; rm(flow.ms.wq, bmp)





###### FILTER DATA

## filter out events with no flow
filter1 <- all[which(all$Volume_Total > -1), ] #; rm(all)

summary(filter1$EventID)

## filter so we only have flow weighted means
filter2 <- filter1[which(filter1$SampleType == "EMC-Flow Weighted"),] #; rm(filter1)

## filter MS type to just inflow and outflow
inflow <- filter2[which(filter2$MSType == "Inflow"),]
outflow <- filter2[which(filter2$MSType == "Outflow"),] %>%
  select('SiteID', 'EventID', 'Value_SubHalfDL', 'Volume_Total')
names(outflow) <- c('SiteID', 'EventID', 'outflow.Value_SubHalfDL', 'outflow.Volume_Total')

merge <- inflow %>%
  left_join(outflow, by =c('SiteID', 'EventID') )


test <- na.omit(merge, cols = c('Value_SubHalfDL', 'Volume_Total', 'outflow.Value_SubHalfDL', 'outflow.Volume_Total'))



test <- wider.df[which(wider.df$Analyte == "Nitrogen"),]














## exploring analytes

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