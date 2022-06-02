

### BMP data exploration - RETENTION PONDS



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

hist(data$flow_atten, xlim = c(-10000, 10000), breaks = 1000)

## calculate solute retention
data$retention <- data$Load_in - data$Load_out
data$retention_percent <- (data$Load_in - data$Load_out)/data$Load_in*100


filter <- data[which(data$flow_atten_percent != 0),]
RP <- filter[which(filter$BMPType == "RP"),]

unique(RP$SiteID)

m <- as.data.frame(table(RP$Species))
ggplot(m, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete= TRUE) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  geom_text(aes(label=Freq), vjust=-0.3, color="black",
            position = position_dodge(0.9), size=5) +
  ylim(0,480)


### Inflow Concentration
ggplot(RP, aes(x = Species, y = Conc_in, fill = factor(Species))) +
  geom_boxplot(trim = TRUE) +
  scale_fill_viridis(discrete= TRUE) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")
  # add jittered points (looks terrible)
  #geom_jitter(shape=16, position=position_jitter(0.2), color = "#000000aa", cex = 0.5)

### Inflow load
ggplot(RP, aes(x = Species, y = Load_in, fill = factor(Species))) +
  geom_boxplot(trim = TRUE) +
  scale_fill_viridis(discrete= TRUE) +
  coord_cartesian(ylim = c(0,20000)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")


### retention
ggplot(RP, aes(x = Species, y = retention, fill = factor(Species))) +
  geom_boxplot(trim = TRUE) +
  scale_fill_viridis(discrete= TRUE) +
  coord_cartesian(ylim = c(-500,5000)) +
  #ylim(-100,500) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")


### retention percent
ggplot(RP, aes(x = Species, y = retention_percent, fill = factor(Species))) +
  geom_boxplot(trim = TRUE) +
  scale_fill_viridis(discrete= TRUE) +
  coord_cartesian(ylim =c(-200,100)) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")


### flow attenuation % vs solute retention %
ggplot(RP, aes(x = flow_atten_percent, y = retention_percent, color = factor(Species))) +
  geom_point() +
  facet_wrap(.~Species) +
  scale_color_viridis(discrete= TRUE) +
  ylim(-100,100) +
  xlim(-100,100) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")


### flow versus solute retention %
ggplot(RP, aes(x = log(Vol_in), y = retention_percent, color = factor(Species))) +
  geom_point() +
  facet_wrap(.~Species) +
  scale_color_viridis(discrete= TRUE) +
  ylim(-100,100) +
  #xlim(-100,100) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

### flow versus solute retention zoomed in
ggplot(RP, aes(x = log(Vol_in), y = retention, color = factor(Species))) +
  geom_point() +
  facet_wrap(.~Species) +
  scale_color_viridis(discrete= TRUE) +
  ylim(-100,5000) +
  #xlim(-100,100) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

### flow versus solute retention zoomed out
ggplot(RP, aes(x = log(Vol_in), y = retention, color = factor(Species))) +
  geom_point() +
  facet_wrap(.~Species) +
  scale_color_viridis(discrete= TRUE) +
  ylim(-30000,90000) +
  #xlim(-100,100) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")


