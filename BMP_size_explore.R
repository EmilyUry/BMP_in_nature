

### Exploring BMP Size



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

levels(x$BMPType)

bowls <- x[which(x$BMPType == "DB" | x$BMPType == "RP" | x$BMPType == "WB"),]
chutes <- x[which(x$BMPType == "WC" | x$BMPType == "BS/BI"),]

#### bowls - Volume 
b <- ggplot(bowls, aes(x = log(Vol_m3), y = retention_percent, color = BMPType)) +
  geom_point() +
  scale_color_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(ylim = c(-200,100)) +
  facet_wrap(.~Species, nrow = 2, scales = "free") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom" ) +
  ylab("Percent retention") +
  xlab("BMP Volume (m3)")
b
b <- ggplot(bowls, aes(x = log(Area_ha), y = retention_percent, color = BMPType)) +
  geom_point() +
  scale_color_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(ylim = c(-200,100)) +
  facet_wrap(.~Species, nrow = 2, scales = "free") +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom" ) +
  ylab("Percent retention") +
  xlab("BMP Volume (m3)")
b


plot(bowls$Vol_m3, bowls$Area_ha, log = "xy")

ggplot(bowls, aes(x = log(Area_ha), y = log(Vol_m3), color = BMPType)) +
  geom_point() +
  scale_color_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  geom_smooth(method = "lm", se = TRUE) 

ggplot(chutes, aes(x = log(Area_ha), y = log(Length_m), color = BMPType)) +
  geom_point() +
  scale_color_manual(values = c("purple", "green"), name = "BMP Type", labels = c("Wetland channel", "Grass swale")) +
  geom_smooth(method = "lm", se = TRUE) 











