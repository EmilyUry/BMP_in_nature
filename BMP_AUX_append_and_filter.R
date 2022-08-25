


#### Aux data munging

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/BMP_project")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")

library(tidyverse)
library(ggplot2)
library(viridis)
options(scipen=999)

Phos <- read.csv("BMP_P_clean.csv")
Nit <- read.csv("BMP_N_clean.csv")
data <- rbind(Phos, Nit) 


## calculate FLOW ATTENUATION
data$flow_atten <- (data$Vol_in-data$Vol_out) 
data$flow_atten_percent <- (data$Vol_in-data$Vol_out) / data$Vol_in *100

## calculate solute retention
data$retention <- data$Load_in - data$Load_out
data$retention_percent <- (data$Load_in - data$Load_out)/data$Load_in*100


## AGE
bmp <- read.csv("BMPInfo.csv", header = TRUE)  %>%
  select(c('BMPID','SiteID', 'DateInstalled')) %>%
  mutate(YearInstalled = stringi::stri_sub(DateInstalled, -4,)) %>%
  mutate(YearInstalled = as.integer(YearInstalled))

data <- data %>%
  left_join(bmp, by = c("SiteID","BMPID"))  %>%
  mutate(year = stringi::stri_sub(DateStart, -2 ,)) %>%
  mutate(year0 = ifelse(year < 25, 20, 19)) %>%
  mutate(EventYear = paste(year0, year, sep = "")) %>%
  mutate(EventYear = as.integer(EventYear)) %>%
  mutate(BMPAge = EventYear - YearInstalled) %>%
  select(-c('X', 'year', 'year0', 'DateInstalled', 'YearInstalled', 'EventYear'))


## Dimensions

dim <- read.csv("BMP_dimensions_all.csv", header = TRUE) %>%
  select(c("BMPID", "Area_ha", "Vol_m3", "Length_m"))  %>% #drop_na %>%
  unique()

data <- data %>% 
  left_join(dim, by = c("BMPID"))



### Location info

loc <- read.csv("BMP_GeospatialData_AI_Biomes.csv", header = TRUE) %>%
  unique()

data <- data %>%
  left_join(loc, by = "SiteID")



## Filtering


## Select BMP types of interest and merge BI and BS
our_types <- c("BI", "BR", "BS", "DB", "RP", "WB", "WC" )
select <- data %>%
  filter(BMPType %in% our_types)
select$BMPType <- factor(select$BMPType , levels=c('DB','RP','WB', "WC",  'BS',  "BI", "BR"))
new_types <- c('DB','RP','WB', "WC", "BS/BI", "BS/BI", "BR")
levels(select$BMPType) <- new_types

select$BMPType_name <- select$BMPType
BMP_names <- c("Detention basin (dry)", "Retention pond", "Wetland basin", "Wetland channel", "Grass strip/swale", "Bioretention")
levels(select$BMPType_name) <- BMP_names



## rename Species and remove TP.dis
select$Species <- factor(select$Species, levels=c("ammonia_N", "Nitrate_N", "TN", "TKN", "TP.dis", "orthoP","TP"))
Species_names <-c("NH4", "NO3", "TN", "TKN", "disTP", "PO4", "TP")
levels(select$Species) <- Species_names
select <- select %>%
  filter(Species != "disTP") %>%
  droplevels()




## write out full data, unfiltered

write.csv(select, "BMP_Clean_full.csv")

## filter for bad flows

filter <- select[which(select$flow_atten_percent != 0),]
filter <- select[which(select$flow_atten_percent > -10),]


## write out filtered data

write.csv(filter, "BMP_Clean_filtered.csv")




