


### BMP Age analysis

setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")

library(tidyverse)
library(ggplot2)
library(viridis)
options(scipen=999)

## order = BR GS DB RP WB WC
pal <- c("#d6e2c7", "#94ac8b", "#fff3c8",  "#d7c5e6", "#c1c9e5", "#8f94c8" ) 

select <- read.csv("BMP_Clean_filtered.csv", stringsAsFactors = TRUE)
### reorder factors

select$BMPType <- factor(select$BMPType , levels=c('BR','BS/BI','DB', "RP",  "WB", "WC"))
levels(select$BMPType) <- c('BR','GS','DB', "RP",  "WB", "WC")
BMP_names <- c("Bioretention","Grass strip/swale", "Detention basin (dry)", "Retention pond", "Wetland basin", "Wetland channel")
levels(select$BMPType_name) <- BMP_names
select$Species <- factor(select$Species , levels=c("TN","NH4", "NO3","TKN", "TP", "PO4"))
data <- select[which(select$Species != "TKN"),]; rm(select)




summary <- data %>% 
  select(c('BMPID', 'BMPType', 'YearInstalled')) %>%
  unique() %>%
  mutate(decade = ifelse(YearInstalled < 1970, "1960s", 
                         ifelse(YearInstalled < 1980, "1970s",
                                ifelse(YearInstalled < 1990, "1980s", 
                                       ifelse(YearInstalled < 2000, "1990s", 
                                              ifelse(YearInstalled < 2010, "2000s",
                                                     ifelse(YearInstalled < 2020, "2010s", "2020s")))))))

table(summary$BMPType, summary$YearInstalled)

d <- summary %>%
  group_by(YearInstalled, BMPType) %>%
  summarize(n = n())
d <- d[1:66,]
  
ggplot(d, aes(fill = BMPType, y = n, x = YearInstalled)) +
  geom_bar(position = "dodge", stat = "identity")

###### by decade
d <- summary %>%
  group_by(decade, BMPType) %>%
  summarize(n = n())
d <- d[1:20,]

ggplot(d, aes(fill = BMPType, y = n, x = decade)) +
  geom_bar(position = "dodge", stat = "identity")


## regional trends
summary <- data %>% 
  select(c('BMPID', 'state', 'BMPType', 'YearInstalled')) %>%
  unique()

table(summary$BMPType, summary$YearInstalled)

d <- summary %>%
  group_by(YearInstalled, BMPType) %>%
  summarize(n = n())
d <- d[1:66,]

ggplot(d, aes(fill = BMPType, y = n, x = YearInstalled)) +
  geom_bar(position = "dodge", stat = "identity")



