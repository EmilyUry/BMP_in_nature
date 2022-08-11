


### map data

### 
# Mapping BMP Type by US State

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/BMP_project")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")

library(tidyverse)
library(ggplot2)
library(viridis)
#library(usmap)
library(maps)
library(scatterpie)

options(scipen=999)

x <- read.csv("BMP_Clean_filtered.csv", stringsAsFactors = TRUE)

### reorder factors
x$BMPType <- factor(x$BMPType , levels=c('DB','RP','WB', "WC",  "BS/BI", "BR"))
x$BMPType_name <- x$BMPType
BMP_names <- c("Detention basin (dry)", "Retention pond", "Wetland basin", "Wetland channel", "Grass strip/swale", "Bioretention")
levels(x$BMPType_name) <- BMP_names
x$Species <- factor(x$Species , levels=c("NH4", "NO3","TKN", "TN" , "TP", "PO4"))

names <- c("Detention basin (dry)", "Retention pond", "Wetland basin", "Wetland channel", "Grass strip/swale", "Bioretention")



## look at how the data breaks down by region
table(x$State)

### summarize number of BMP Type in each region
data <- x %>%
  select(c("SiteID", "BMPID", "BMPType", "BMPType_name" , "State", "Lat", "Long")) %>%
  unique() %>%
  rename(state = State)

table(data$state)
table(data$BMPType)
m <- table(data$state, data$BMPType)
m <- as.data.frame.matrix(m)

unique(data$state)
## assign lat and long for each pie chart
m$Long <- c(-90, -120, -105, -75, -80, -82, -85, -76, -85, -95, -95, -77, -72, -75, -83, -123, -100, -79, -121, -90, NA)
m$Lat <- c(  33, 36, 38, 38, 29, 33, 37, 40, 45, 48, 35,34, 43, 43, 42, 44, 30, 38, 47, 46, NA)
m$Total <- m$DB + m$RP + m$WB +m$WC + m$`BS/BI` + m$BR
m

### Call in US state data and sort into custom regions
States <- map_data("state")
ggplot(States, aes (long, lat)) +
  geom_map(map = States, aes(map_id=(region)), fill = "grey97", color = "grey20") +  #US BASE MAP
  geom_point(data = data, aes(x = Long, y = Lat)) +                               ## BMPs as points
  xlim(-125, -65) +
  ylim(25,50) +
  geom_scatterpie(data = m,                                                      ## add pie charts
                  aes(Long, Lat, r = Total/8+0.5),                                   ## 'r' scales the pies # of BMPs in a region
                  cols = c("DB", "RP", "WB", "WC", "BS/BI", "BR"),
                  alpha = 0.5) +
  scale_fill_manual(labels = names,
                    values = c("DB" = "#440154",
                               "RP" = "#414487",
                               "WB" = "#2a788e",
                               "WC" = "#22a884", 
                               "BS/BI" = "#7ad151",
                               "BR" = "#fde725")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
#



### percent removal by state


data2 <- x %>%
  select(c("SiteID", "BMPID", "BMPType", "BMPType_name" , "State", "Lat", "Long", "retention_percent")) %>%
  group_by(State)%>%
  summarize(median = median(retention_percent))

data2$Long <- c(-90, -120, -105, -75, -80, -82, -85, -76, -85, -95, -95, -77, -72, -75, -83, -122, -100, -79, -121, -90, NA)
data2$Lat <- c(  33, 36, 38, 38, 29, 33, 37, 40, 45, 48, 35,34, 43, 43, 42, 44, 30, 38, 47, 46, NA)

data2[15,2] <- 0
data2[7,2] <- 0

ggplot(States, aes (long, lat)) +
  geom_map(map = States, aes(map_id=(region)), fill = "grey97", color = "grey20") +  #US BASE MAP
  geom_point(data = data, aes(x = Long, y = Lat)) +                               ## BMPs as points
  xlim(-125, -65) +
  ylim(25,50) +
  geom_scatterpie(data = data2,                                                      ## add pie charts
                  aes(Long, Lat, r = median/25),                                   ## 'r' scales the pies # of BMPs in a region
                  cols = c("col"),
                  alpha = 0.5) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
