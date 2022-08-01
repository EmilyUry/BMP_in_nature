


### map data

### 
# Clean code for working up the clean data and running some comparative analyses

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/BMP_project")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")

library(tidyverse)
library(ggplot2)
library(viridis)
options(scipen=999)

x <- read.csv("BMP_Clean_filtered.csv", stringsAsFactors = TRUE)

### reorder factors
x$BMPType <- factor(x$BMPType , levels=c('DB','RP','WB', "WC",  "BS/BI", "BR"))
x$BMPType_name <- x$BMPType
BMP_names <- c("Detention basin (dry)", "Retention pond", "Wetland basin", "Wetland channel", "Grass strip/swale", "Bioretention")
levels(x$BMPType_name) <- BMP_names
x$Species <- factor(x$Species , levels=c("NH4", "NO3","TKN", "TN" , "TP", "PO4"))


plot(x$Long, x$Lat)
levels(x$State)

x$Region <- ifelse(x$State == "NH" | x$State == "NY" | x$State == "PA", "North East", 
                   ifelse(x$State == "VA" | x$State == "WV" | x$State == "MD" |x$State == "DE" | x$State == "NC" , "Mid Atlantic",
                          ifelse(x$State == "AL" | x$State == "GA" |x$State == "FL", "South East",
                                 ifelse(x$State == "OR" | x$State == "WA", "PNW",
                                        ifelse(x$State == "CO" | x$State == "CA"  | x$State == "TX", "South West", "Mid West")))))

### or simplified
x$Region <- ifelse(x$State == "VA" | x$State == "WV" | x$State == "MD" |x$State == "DE" |
                     x$State == "NH" | x$State == "NY" | x$State == "PA", "East", 
                   ifelse(x$State == "TX" | x$State == "AL" | x$State == "GA" |
                            x$State == "NC" |x$State == "FL", "South",
                          ifelse(x$State == "OR" | x$State == "CO" | x$State == "CA" | x$State == "WA", "West",
                                 "Mid West")))

x$Region <- as.factor(x$Region)
levels(x$Region)
table(x$State)
table(x$Region)


data <- x %>%
  select(c("SiteID", "BMPID", "BMPType", "BMPType_name" , "State", "Region", "Lat", "Long")) %>%
  unique() %>%
  rename(state = State)

table(data$state)
table(data$Region)
m <- table( data$Region, data$BMPType)

m <- as.data.frame.matrix(m)
m$Long <- c(-76, -90, -70, -120, -86, -110)
m$Lat <- c(35, 41, 45, 45, 31, 35)
m$Total <- m$DB + m$RP + m$WB +m$WC + m$`BS/BI` + m$BR
m


library(usmap)
library(ggplot2)
library(maps)
library(scatterpie)


States <- map_data("state")
NE <- filter(States, region ==  "new york" | region ==
                                "vermont" | region ==  "new hampshire" | region ==
                                "massachusetts" | region ==  "rhode island" | 
                                region ==  "connecticut" | region == "maine" )
MA <- filter(States, region ==  "pennsylvania" | region ==
               "new jersey" | region ==  "delaware" | region ==
               "maryland" | region ==  "virginia" | 
               region ==  "west virginia" | region == "north carolina" )
SE <- filter(States, region ==  "south carolina" | region ==
               "georgia" | region ==  "florida" | region ==
               "alabama" | region ==  "mississippi" 
             | region ==  "louisiana" | region ==  "arkansas")
SW <- filter(States, region ==  "texas" | region ==
               "new mexico" | region ==  "arizona" | region ==
               "colorado" | region ==  "utah" | 
               region ==  "nevada" | region == "california" )
PNW <- filter(States, region ==  "washington" | region == "oregon"| 
              region ==  "idaho" | region == "montana" |
                region ==  "wyoming")


ggplot(States, aes (long, lat)) +
  geom_map(map = States, aes(map_id=(region)), fill = "grey97", color = "grey20") +
  geom_map(map = NE, aes(map_id=(region)), fill = "gray77", color = "grey20")+
  geom_map(map = MA, aes(map_id=(region)), fill = "gray55", color = "grey20")+
  geom_map(map = SE, aes(map_id=(region)), fill = "gray77", color = "grey20")+
  geom_map(map = SW, aes(map_id=(region)), fill = "gray55", color = "grey20")+
  geom_map(map = PNW, aes(map_id=(region)), fill = "gray77", color = "grey20")+
  geom_point(data = data, aes(x = Long, y = Lat)) +
  xlim(-125, -65) +
  ylim(25,50) +
  geom_scatterpie(data = m, 
                  aes(Long, Lat, r = Total/7),
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
  

names <- levels(x$BMPType_name)


table(x$State, x$Region)


