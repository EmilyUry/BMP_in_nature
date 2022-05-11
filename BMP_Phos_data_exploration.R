

## TP data exploration

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/BMP_project")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")

library(ggplot2)
options(scipen=999)

data <- read.csv("BMP_SUMMARY_P_all.csv")


## calculate retention percent
data$flow_atten <- (data$Inflow_vol_m3-data$Outflow_vol_m3) 
data$flow_atten_percent <- (data$Inflow_vol_m3-data$Outflow_vol_m3) / data$Inflow_vol_m3 *100

data$Load_In <- data$Inflow_mg_L*data$Inflow_vol_m3
data$Load_Out <- data$Outflow_mg_L*data$Outflow_vol_m3
data$retention <- data$Load_In - data$Load_Out
data$retention_percent <- (data$Load_In - data$Load_Out)/data$Load_In*100


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





