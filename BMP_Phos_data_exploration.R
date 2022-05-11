

## TP data exploration

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/BMP_project")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")

library(ggplot2)
options(scipen=999)

data <- read.csv("BMP_P_clean.csv")


## calculate FLOW ATTENUATION
data$flow_atten <- (data$Vol_in-data$Vol_out) 
data$flow_atten_percent <- (data$Vol_in-data$Vol_out) / data$Vol_in *100

## calculate solute retention
data$retention <- data$Load_in - data$Load_out
data$retention_percent <- (data$Load_in - data$Load_out)/data$Load_in*100




ggplot(data, aes(x = flow_atten_percent, y = retention_percent)) +
  geom_point() +
  facet_wrap(.~Species) +
  ylim(-100,100) +
  xlim(-100,100)

data <- data[which(data$flow_atten_percent != 0),]
data.select <- data %>%
  filter(BMPType == "RP" | BMPType == "WB" | BMPType == "DB" | BMPType == "BR" |
           BMPType == "BI" | BMPType == "BS" | BMPType == "WC" | BMPType == "PC" |
         BMPType == "FS")

ggplot(data.select, aes(x = flow_atten_percent, y = retention_percent, color = Species)) +
  geom_point() +
  facet_wrap(.~BMPType) +
  ylim(-100,100) +
  xlim(-100,100)

ggplot(data.select, aes(x = flow_atten_percent, y = retention_percent)) +
  geom_point() +
  facet_wrap(Species~BMPType) +
  ylim(-100,100) +
  xlim(-100,100)


RP <- data %>%
  filter(BMPType == "RP")

plot(RP$Vol_in, RP$Vol_out,
     xlim = c(0,50000), ylim = c(0,40000))

plot(RP$Vol_in, RP$Vol_out, log = "xy")

filter <- RP[which(RP$flow_atten > -1000 & RP$flow_atten < 10000),]
filter <- RP[which(RP$flow_atten_percent > -100 & RP$flow_atten != 0),]
hist(filter$flow_atten_percent, breaks = 200)
abline(v=0)







data3 <- data[which(data$flow_atten_percent != 0 & data$flow_atten_percent > -30 & data$flow_atten_percent < 90),]

hist(data3$retention, breaks = 500,
     xlim = c(-50, 50))


### Remove rows where Inflow = Outflow

data3$Species <- as.factor(data3$Species)

plot(data3$flow_atten_percent, data3$retention_percent, 
     ylim = c(-100,100), # xlim = c(-25,1), 
     col = c('red', 'blue', 'green')[as.factor(data3$Species)], 
     pch = 16)
legend("bottomright", levels(data3$Species), pch = 16,  col = c('red', 'blue', 'green'))
abline(a=0, b = 1)


data.wide <- data %>%
  pivot_wider(id_cols = c("SiteID", "BMPID", "BMPType", "EventID", "DateStart"), 
              names_from = 'Species', values_from = c("retention_percent"))


### compare retention percent across P species
ggplot(data.wide, aes(x = TP, y = OrthoP, color = BMPType)) +
  geom_point() +
  xlim(-200,100) +
  ylim(-200,100) +
  facet_wrap(.~BMPType)





