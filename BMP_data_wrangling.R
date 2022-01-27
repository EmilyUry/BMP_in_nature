



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

table(TP.test$BMPType)




### fix the volume issue, convert everything to m3

TP.test <- TP.test %>% 
  mutate(V5 = case_when(Volume_Units == 'L' ~ Volume_Total_Inflow/1000,
                        Volume_Units == 'cf' ~ Volume_Total_Inflow*0.0283, 
                        Volume_Units == 'CF' ~ Volume_Total_Inflow*0.0283, 
                        Volume_Units == 'gal' ~ Volume_Total_Inflow*0.00379, 
                        Volume_Units == 'AF' ~ Volume_Total_Inflow*1233.5,))


write.csv(TP.test, file = "TP_toy.csv")



























### Junk code


###a <- stringr::str_extract(Phos.wide$TP, pattern = 'c')


table(Phos$Value_Unit) 
names(Phos)
## what is the frequency of each analyte name
3074 - nrow(Phos[which(Phos$`Phosphorus as P TOTAL` == "NULL"),])

##table(Phos$SampleFraction)










## reads in all WQ, but some issues here

## long format
wq <- read.csv("WaterQuality.csv", header = T)  %>%
  select('SiteID','MSID','EventID','DateSample','TimeSample',
         'Analyte','Value_SubHalfDL', 'WQQualifier',
         'SampleType','SampleFraction', 'Value_Unit') %>%
  filter(SampleType == 'EMC-Flow Weighted')

## wide format - has ISSUES
wq <- read.csv("WaterQuality.csv", header = T)  %>%
  select('SiteID','MSID','EventID','DateSample','TimeSample',
         'Analyte','Value_SubHalfDL', 'WQQualifier',
         'SampleType','SampleFraction', 'Value_Unit') %>%
  filter(SampleType == 'EMC-Flow Weighted') %>%
  pivot_wider(names_from = 'Analyte', values_from = 'Value_SubHalfDL' )  ##note collapse sampleTypes






# 
# long.wq <- read.csv("WaterQuality.csv", header = T) #%>%
#   select('SiteID','MSID','EventID','DateSample','TimeSample',
#          'Analyte','Value_SubHalfDL', 'WQQualifier',
#          'SampleType','SampleFraction') %>%
#   filter(SampleType == 'EMC-Flow Weighted')
# 





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
  left_join(Phos, by = c("SiteID", "MSID", "EventID"))  %>%
  filter(SampleType == 'EMC-Flow Weighted')








  
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