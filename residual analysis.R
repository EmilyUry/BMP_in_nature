

### residual analysis








setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")

library(tidyverse)
library(ggplot2)
library(cowplot)

options(scipen=999)

select <- read.csv("BMP_Clean_filtered.csv", stringsAsFactors = TRUE)


### reorder factors
select$BMPType <- factor(select$BMPType , levels=c('BR','BS/BI','DB', "RP",  "WB", "WC"))
levels(select$BMPType) <- c('BR','GS','DB', "RP",  "WB", "WC")
BMP_names <- c("Bioretention","Grass strip/swale", "Detention basin", "Retention pond", "Wetland basin", "Wetland channel")
levels(select$BMPType_name) <- BMP_names
select$Species <- factor(select$Species , levels=c("TN","NH4", "NO3","TKN", "TP", "PO4"))

select <- select[which(select$Species != "TKN"),]

data <- select[!is.na(select$flow_atten_percent),]
data <- data[which(data$retention_percent > -100),]


pal <- c("#95bf5a", "#455e22", "#baa745",  "#846887", "#748ca1", "#0c406e" ) 

data %>%
  ggplot(aes(x = (flow_atten_percent) , y = retention_percent,  color = BMPType_name))+
  geom_point() +
  scale_color_manual(values = pal)+
  theme_bw(base_size=14) +
  #geom_smooth(method = "lm", se = FALSE) +
  #ggtitle(expression(TN)) +
  xlab("Water attenuation (%)") +
  ylab("Solute retention (%)") +
  facet_wrap(~Species) +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  theme(legend.position = c(0.84, 0.24), legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

data %>%
  ggplot(aes(x = (flow_atten_percent) , y = retention_percent,  color = BMPType_name))+
  geom_point() +
  scale_color_manual(values = pal)+
  theme_bw(base_size=10) +
  #geom_smooth(method = "lm", se = FALSE) +
  #ggtitle(expression(TN)) +
  xlab("Water attenuation (%)") +
  ylab("Solute retention (%)") +
  facet_grid(Species~BMPType_name) +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  theme(legend.position = 'none', legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


## conceptual figure
data %>%   
  ggplot(aes(x = (flow_atten_percent) , y = retention_percent)) +
  geom_point(col = "white") +
  theme_bw(base_size=10) +
  xlab("Water attenuation (%)") +
  ylab("Solute retention (%)") +
  theme(legend.position = 'none', legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  annotate("rect", xmin = -10, xmax = 100, ymin = -30, ymax = 100,
           alpha = 1 ,fill = "gray80") +
  annotate("polygon", x = c(-10, 100, 100), y = c(-30, -30, 80), fill = "#ffffb3")+
  annotate("rect", xmin = -10, xmax = 100, ymin = -100, ymax = -30,
           alpha = 1 ,fill = "#ffffb3") +
  annotate("polygon", x = c(-10, -10, 80), y = c(10, 100, 100), fill = "#8dd3c7")+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  annotate("text", x = 20, y = 75, label = "Uptake", fontface = "bold") +
  annotate("text", x = 50, y = -20, label = "Leaching", fontface = "bold") +
  annotate("text", x = 50, y = 60, label = "Water attenuation = solute retention", angle = 30)
  
  
  
  
#### residual analysis

mod <- lm(retention_percent ~ flow_atten_percent, data = data)

summary <- c(summary(mod)$coefficients[,1][[2]], summary(mod)$coefficients[,1][[1]], summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(data$flow_atten_percent, data$retention_percent), nrow(data)  )
summary <- data.frame(summary)
rownames(summary) <- c("slope", "intercept", "P", "R2_adj", "Corr", "n" )
summary

data$residual <- residuals(mod)
data$predicted <- predict(mod)


data %>%
  ggplot(aes(x = (flow_atten_percent) , y = residual,  color = BMPType_name))+
  geom_point() +
  scale_color_manual(values = pal)+
  theme_bw(base_size=10) +
  #geom_smooth(method = "lm", se = FALSE) +
  #ggtitle(expression(TN)) +
  xlab("Water attenuation (%)") +
  ylab("Residuals") +
  facet_grid(Species~BMPType_name) +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  #geom_abline(slope = 1, intercept = 0) +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())



## conceptual figure
data %>%   
  ggplot(aes(x = (flow_atten_percent) , y = retention_percent)) +
  geom_point(col = "white") +
  theme_bw(base_size=10) +
  xlab("Water attenuation (%)") +
  ylab("Residuals") +
  theme(legend.position = 'none', legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  annotate("rect", xmin = -10, xmax = 100, ymin = 20, ymax = 100, fill = "#8dd3c7") +
  annotate("rect", xmin = -10, xmax = 100, ymin = -20, ymax = 20, fill = "gray80") +
  annotate("rect", xmin = -10, xmax = 100, ymin = -100, ymax = -20, fill = "#ffffb3") +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  annotate("text", x = 45, y = 50, label = "Uptake", fontface = "bold") +
  annotate("text", x = 45, y = -50, label = "Leaching", fontface = "bold") +
  annotate("text", x = 45, y = 10, label = "Water attenuation = solute retention")



########### Drivers -- continuous


########### Drivers -- categorical

# Surface area - not strongly correlated with residuals - smaller DBs leach TP and PO4
# 
# Depth - more leaching of TN in deeper retention ponds, more uptake of PO4 in deeper RP
#
# BMP Age - no strong overall pattern - older Bioretention leach more NO3, older Retention pont leach more PO4
           # but older bioretention also takes up more TN, NH4 and TP. 

# Event discharge - bioretention leach more N (all) with higher discharge but take up more TP and PO4
                  # Detention basin leach more NO3 but take up more TP and PO4
                  # retention pond leach more PO4
# Inflow load - DB more uptake with more load, samw with rP, WB for NH4, WC for TN
                # bioretention leaches more NO3, takes up more TP

# Inflow concentration - generally positive, more uptake with more coming in. Except PO4 in wetland basin


# AI -- ok, maybe keep this -- it looks kind of interesting -- more uptake in more arid climes 

# Latitude


data %>%
  ggplot(aes(x = log(Area_ha) , y = residual,  color = BMPType_name))+
  geom_point() +
  scale_color_manual(values = pal)+
  theme_bw(base_size=10) +
  geom_smooth(method = "lm", se = FALSE) +
  #ggtitle(expression(TN)) +
  xlab("Surface Area (ha)") +
  ylab("Residuals") +
  facet_grid(Species~BMPType_name, scales = "free") +
  geom_hline(yintercept = 0)+
  #geom_vline(xintercept = 0)+
  #geom_abline(slope = 1, intercept = 0) +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

data %>%
  ggplot(aes(x = (Depth_m) , y = residual,  color = BMPType_name))+
  geom_point() +
  scale_color_manual(values = pal)+
  theme_bw(base_size=10) +
  geom_smooth(method = "lm", se = FALSE) +
  #ggtitle(expression(TN)) +
  xlab("Depth (m)") +
  ylab("Residuals") +
  facet_grid(Species~BMPType_name, scales = "free") +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  #geom_abline(slope = 1, intercept = 0) +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

data %>%
  ggplot(aes(x = (BMPAge) , y = residual,  color = BMPType_name))+
  geom_point() +
  scale_color_manual(values = pal)+
  theme_bw(base_size=10) +
  geom_smooth(method = "lm", se = FALSE) +
  #ggtitle(expression(TN)) +
  xlab("Age (years)") +
  ylab("Residuals") +
  facet_grid(Species~BMPType_name, scales = "free") +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  #geom_abline(slope = 1, intercept = 0) +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


data %>%
  ggplot(aes(x = log(Vol_in) , y = residual,  color = BMPType_name))+
  geom_point() +
  scale_color_manual(values = pal)+
  theme_bw(base_size=10) +
  geom_smooth(method = "lm", se = FALSE) +
  #ggtitle(expression(TN)) +
  xlab("Event discharge (m3)") +
  ylab("Residuals") +
  facet_grid(Species~BMPType_name, scales = "free") +
  geom_hline(yintercept = 0)+
  #geom_vline(xintercept = 0)+
  #geom_abline(slope = 1, intercept = 0) +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


data %>%
  ggplot(aes(x = log(Load_in) , y = residual,  color = BMPType_name))+
  geom_point() +
  scale_color_manual(values = pal)+
  theme_bw(base_size=10) +
  geom_smooth(method = "lm", se = FALSE) +
  #ggtitle(expression(TN)) +
  xlab("Solute load (g/event)") +
  ylab("Residuals") +
  facet_grid(Species~BMPType_name, scales = "free") +
  geom_hline(yintercept = 0)+
  #geom_vline(xintercept = 0)+
  #geom_abline(slope = 1, intercept = 0) +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

data %>%
  ggplot(aes(x = log(Conc_in) , y = residual,  color = BMPType_name))+
  geom_point() +
  scale_color_manual(values = pal)+
  theme_bw(base_size=10) +
  geom_smooth(method = "lm", se = FALSE) +
  #ggtitle(expression(TN)) +
  xlab("Solute concentration (mg/L)") +
  ylab("Residuals") +
  facet_grid(Species~BMPType_name, scales = "free") +
  geom_hline(yintercept = 0)+
  #geom_vline(xintercept = 0)+
  #geom_abline(slope = 1, intercept = 0) +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


data %>%
  ggplot(aes(x = (AI) , y = residual,  color = BMPType_name))+
  geom_point() +
  scale_color_manual(values = pal)+
  theme_bw(base_size=10) +
  geom_smooth(method = "lm", se = FALSE) +
  #ggtitle(expression(TN)) +
  xlab("Aridity Index") +
  ylab("Residuals") +
  facet_grid(Species~BMPType_name, scales = "free") +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  #geom_abline(slope = 1, intercept = 0) +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

data %>%
  ggplot(aes(x = (latitude) , y = residual,  color = BMPType_name))+
  geom_point() +
  scale_color_manual(values = pal)+
  theme_bw(base_size=10) +
  geom_smooth(method = "lm", se = FALSE) +
  #ggtitle(expression(TN)) +
  xlab("Latitude") +
  ylab("Residuals") +
  facet_grid(Species~BMPType_name, scales = "free") +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  #geom_abline(slope = 1, intercept = 0) +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


##########################################

##### figure - All drivers -- All Species (BMP Type lumped together)

{
data$Area <- "Surface Area"
sig <- c(coef(summary(lm(residual~log(Area_ha), data = data[data$Species == "TN",])))[2,4] < 0.05,
         coef(summary(lm(residual~log(Area_ha), data = data[data$Species == "NH4",])))[2,4] < 0.05,
         coef(summary(lm(residual~log(Area_ha), data = data[data$Species == "NO3",])))[2,4] < 0.05,
         coef(summary(lm(residual~log(Area_ha), data = data[data$Species == "TP",])))[2,4] < 0.05,
         coef(summary(lm(residual~log(Area_ha), data = data[data$Species == "PO4",])))[2,4] < 0.05)
names(sig)<- c("TN", "NH4", "NO3", "TP", "PO4")
Area <- data %>%
  ggplot(aes(x = log(Area_ha) , y = residual))+
  geom_point() +
  theme_bw(base_size=10) +
  geom_smooth(data = data[data$Species %in% names(sig)[sig == TRUE],], method = "lm", se = TRUE) +
  xlab("Surface Area \n(ha)") +
  ylab("Residuals") +
  facet_grid(Species~Area) +
  ylim(-175,75)+
  geom_hline(yintercept = 0)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background.y = element_blank(), strip.text.y = element_blank())

data$Depth <- "Depth"
sig <- c(coef(summary(lm(residual~Depth_m, data = data[data$Species == "TN",])))[2,4] < 0.05,
         coef(summary(lm(residual~Depth_m, data = data[data$Species == "NH4",])))[2,4] < 0.05,
         coef(summary(lm(residual~Depth_m, data = data[data$Species == "NO3",])))[2,4] < 0.05,
         coef(summary(lm(residual~Depth_m, data = data[data$Species == "TP",])))[2,4] < 0.05,
         coef(summary(lm(residual~Depth_m, data = data[data$Species == "PO4",])))[2,4] < 0.05)
names(sig)<- c("TN", "NH4", "NO3", "TP", "PO4")
Depth <- data %>%
  ggplot(aes(x = (Depth_m) , y = residual))+
  geom_point() +
  theme_bw(base_size=10) +
  geom_smooth(data = data[data$Species %in% names(sig)[sig == TRUE],], method = "lm", se = TRUE) +
  xlab("Depth \n(m)") +
  facet_grid(Species~Depth) +
  ylim(-175,75)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        strip.background.y = element_blank(), strip.text.y = element_blank() )

data$Age <- "Age"
sig <- c(coef(summary(lm(residual~BMPAge, data = data[data$Species == "TN",])))[2,4] < 0.05,
         coef(summary(lm(residual~BMPAge, data = data[data$Species == "NH4",])))[2,4] < 0.05,
         coef(summary(lm(residual~BMPAge, data = data[data$Species == "NO3",])))[2,4] < 0.05,
         coef(summary(lm(residual~BMPAge, data = data[data$Species == "TP",])))[2,4] < 0.05,
         coef(summary(lm(residual~BMPAge, data = data[data$Species == "PO4",])))[2,4] < 0.05)
names(sig)<- c("TN", "NH4", "NO3", "TP", "PO4")
Age <- data %>%
  ggplot(aes(x = (BMPAge) , y = residual))+
  geom_point() +
  theme_bw(base_size=10) +
  geom_smooth(data = data[data$Species %in% names(sig)[sig == TRUE],], method = "lm", se = TRUE) +
  xlab("Age \n(years)") +
  ylab("Residuals") +
  facet_grid(Species~Age) +
  ylim(-175,75)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        strip.background.y = element_blank(), strip.text.y = element_blank())

data$Discharge <- "Event volume"
sig <- c(coef(summary(lm(residual~log(Vol_in), data = data[data$Species == "TN",])))[2,4] < 0.05,
         coef(summary(lm(residual~log(Vol_in), data = data[data$Species == "NH4",])))[2,4] < 0.05,
         coef(summary(lm(residual~log(Vol_in), data = data[data$Species == "NO3",])))[2,4] < 0.05,
         coef(summary(lm(residual~log(Vol_in), data = data[data$Species == "TP",])))[2,4] < 0.05,
         coef(summary(lm(residual~log(Vol_in), data = data[data$Species == "PO4",])))[2,4] < 0.05)
names(sig)<- c("TN", "NH4", "NO3", "TP", "PO4")
Vol <- data %>%
  ggplot(aes(x = log(Vol_in) , y = residual))+
  geom_point() +
  theme_bw(base_size=10) +
  geom_smooth(data = data[data$Species %in% names(sig)[sig == TRUE],], method = "lm", se = TRUE) +
  xlab("Event volume \n(m3)") +
  ylab("Residuals") +
  facet_grid(Species~Discharge) +
  ylim(-175,75) +
  geom_hline(yintercept = 0)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        strip.background.y = element_blank(), strip.text.y = element_blank())

data$Load <- "Solute Load"
sig <- c(coef(summary(lm(residual~log(Load_in), data = data[data$Species == "TN",])))[2,4] < 0.05,
         coef(summary(lm(residual~log(Load_in), data = data[data$Species == "NH4",])))[2,4] < 0.05,
         coef(summary(lm(residual~log(Load_in), data = data[data$Species == "NO3",])))[2,4] < 0.05,
         coef(summary(lm(residual~log(Load_in), data = data[data$Species == "TP",])))[2,4] < 0.05,
         coef(summary(lm(residual~log(Load_in), data = data[data$Species == "PO4",])))[2,4] < 0.05)
names(sig)<- c("TN", "NH4", "NO3", "TP", "PO4")
Load <- data %>%
  ggplot(aes(x = log(Load_in) , y = residual))+
  geom_point() +
  theme_bw(base_size=10) +
  geom_smooth(data = data[data$Species %in% names(sig)[sig == TRUE],], method = "lm", se = TRUE) +
  xlab("Solute load \n(g/event)") +
  ylab("Residuals") +
  facet_grid(Species~Load) +
  ylim(-175,75)+
  geom_hline(yintercept = 0)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        strip.background.y = element_blank(), strip.text.y = element_blank())

data$Conc <- "Concentration"
sig <- c(coef(summary(lm(residual~log(Conc_in), data = data[data$Species == "TN",])))[2,4] < 0.05,
         coef(summary(lm(residual~log(Conc_in), data = data[data$Species == "NH4",])))[2,4] < 0.05,
         coef(summary(lm(residual~log(Conc_in), data = data[data$Species == "NO3",])))[2,4] < 0.05,
         coef(summary(lm(residual~log(Conc_in), data = data[data$Species == "TP",])))[2,4] < 0.05,
         coef(summary(lm(residual~log(Conc_in), data = data[data$Species == "PO4",])))[2,4] < 0.05)
names(sig)<- c("TN", "NH4", "NO3", "TP", "PO4")
conc.data <- data[data$Conc_in > 0.00001,]
Conc <- conc.data %>%
  ggplot(aes(x = log(Conc_in) , y = residual))+
  geom_point() +
  theme_bw(base_size=10) +
  geom_smooth(data = data[data$Species %in% names(sig)[sig == TRUE],], method = "lm", se = TRUE) +
  xlab("Solute concentration \n(mg/L)") +
  ylab("Residuals") +
  facet_grid(Species~Conc) +
  ylim(-175,75)+
  geom_hline(yintercept = 0)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        strip.background.y = element_blank(), strip.text.y = element_blank())

data$Aridity <- "Aridity Index"
sig <- c(coef(summary(lm(residual~AI, data = data[data$Species == "TN",])))[2,4] < 0.05,
         coef(summary(lm(residual~AI, data = data[data$Species == "NH4",])))[2,4] < 0.05,
         coef(summary(lm(residual~AI, data = data[data$Species == "NO3",])))[2,4] < 0.05,
         coef(summary(lm(residual~AI, data = data[data$Species == "TP",])))[2,4] < 0.05,
         coef(summary(lm(residual~AI, data = data[data$Species == "PO4",])))[2,4] < 0.05)
names(sig)<- c("TN", "NH4", "NO3", "TP", "PO4")
AI <- data %>%
  ggplot(aes(x = (AI) , y = residual))+
  geom_point() +
  theme_bw(base_size=10) +
  geom_smooth(data = data[data$Species %in% names(sig)[sig == TRUE],], method = "lm", se = TRUE) +
  xlab("Aridity Index\n") +
  ylab("Residuals") +
  facet_grid(Species~Aridity) +
  ylim(-175,75)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        strip.background.y = element_blank(), strip.text.y = element_blank())

data$Lat <- "Latitude"
sig <- c(coef(summary(lm(residual~latitude, data = data[data$Species == "TN",])))[2,4] < 0.05,
         coef(summary(lm(residual~latitude, data = data[data$Species == "NH4",])))[2,4] < 0.05,
         coef(summary(lm(residual~latitude, data = data[data$Species == "NO3",])))[2,4] < 0.05,
         coef(summary(lm(residual~latitude, data = data[data$Species == "TP",])))[2,4] < 0.05,
         coef(summary(lm(residual~latitude, data = data[data$Species == "PO4",])))[2,4] < 0.05)
names(sig)<- c("TN", "NH4", "NO3", "TP", "PO4")
Lat.data <- data[data$latitude > 20,]
Lat <- Lat.data %>%
  ggplot(aes(x = (latitude) , y = residual))+
  geom_point() +
  theme_bw(base_size=10) +
  geom_smooth(data = data[data$Species %in% names(sig)[sig == TRUE],], method = "lm", se = TRUE) +
  xlab("Latitude\n") +
  ylab("Residuals") +
  facet_grid(Species~Lat) +
  xlim(25,50) +
  ylim(-175,75)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank())

}
plot_grid(Area, Depth, Age, Vol, Load, Conc, AI, Lat, nrow = 1, 
          rel_widths = c(1.35, 1, 1,1,1,1,1, 1.1))



#### heat map of correlation values (R2 and significance)

WB <- data[data$BMPType == "WB",]
#input <- WB
my.fun <- function(input){
  m <- lm(residual~log(Area_ha), data = input)
  p.value <- coef(summary())[2,4]
  corr <- cor(input$residual, log(input$Area_ha))
  out <- c(corr, p.value)
  out
}
sapply(split(WB, WB$Species), function(x)my.fun(x))

Species <- c("TN", "NH4", "NO3", "TP", "PO4")

df <- data[,5:6]
df$residuals <- data$residual
df$Area <- log(data$Area_ha)
df$Depth <- data$Depth_m
df$Age <- data$BMPAge
df$Vol <- log(data$Vol_in)
df$Load <- log(data$Load_in)
df$Conc <- log(data$Conc_in)
df$AI <- data$AI
df$Lat <- data$latitude

Species <- c("TN", "NH4", "NO3", "TP", "PO4")
Drivers <- c("Area", "Depth", "Age", "Vol", "Load", "Conc", "AI", "Lat")




# 
# table <- data.frame(matrix(ncol = 8, nrow = 3))
# for (j in 1:5) {
#   subset <- df[df$Species == Species[j],]
#    for (i in 1:8) {
#       m <- lm(subset$residual~subset[,i+3])
#       table[j,i] <- coef(summary(m))[2,4]
#   }
# }
# 
# WB <- df[df$BMPType == "WB",]
# table <- data.frame(matrix(ncol = 8, nrow = 3))
# for (j in 1:5) {
#   subset <- WB[WB$Species == Species[j],]
#   for (i in 1:8) {
#     m <- lm(subset$residual~subset[,i+3])
#     table[j,i] <- coef(summary(m))[2,4]
#   }
# }



cor.fun <- function(input){
  output <- data.frame(matrix(ncol = 8, nrow = 3))
  for (j in 1:5) {
    subset <- input[input$Species == Species[j],]
    for (i in 1:8) {
      if (length(unique(c(subset[,i+3], NA))) > 2) {
        m <- lm(subset$residual~subset[,i+3])
        output[j,i] <- coef(summary(m))[2,4]
      }
    }
  }
  names(output) <- Drivers
  rownames(output) <- Species
  round(output,3)
}

cor.fun(df)





BR <- df[df$BMPType == "BR",]
BR.table <- cor.fun(BR)


GS <- df[df$BMPType == "GS",]
GS.table <- cor.fun(GS)


DB <- df[df$BMPType == "DB",]
DB.table <- cor.fun(DB)


RP <- df[df$BMPType == "RP",]
RP.table <- cor.fun(RP)

WB <- df[df$BMPType == "WB",]
WB.table <- cor.fun(WB)


WC <- df[df$BMPType == "WC",]
WC.table <- cor.fun(WC)





sig <- c(coef(summary(lm(residual~log(Area_ha), data = data[data$Species == "TN",])))[2,4],
         coef(summary(lm(residual~log(Area_ha), data = data[data$Species == "NH4",])))[2,4],
         coef(summary(lm(residual~log(Area_ha), data = data[data$Species == "NO3",])))[2,4],
         coef(summary(lm(residual~log(Area_ha), data = data[data$Species == "TP",])))[2,4],
         coef(summary(lm(residual~log(Area_ha), data = data[data$Species == "PO4",])))[2,4])
names(sig)<- c("TN", "NH4", "NO3", "TP", "PO4")








### for categorical variable



# wet/dry
# planted/unplanted
# basin/channel

