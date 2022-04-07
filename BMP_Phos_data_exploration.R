

## TP data exploration

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/BMP_project")

library(ggplot2)


TP <- read.csv("BMP_SUMMARY_TP.csv", head = TRUE)
SRP <- read.csv("BMP_SUMMARY_OrthoP.csv", head = TRUE)
TPdis <- read.csv("BMP_SUMMARY_TPdis.csv", head = TRUE)


head(TP)
table(TP$BMPType)
do.call("rbind",lapply(unique(TP$BMPType),function(b){
  data.frame(BMPType=b,
    num_sites=length(unique(subset(TP,BMPType==b)$BMPID))
  )	
}
))



### TP vs PO4 retention

data <- rbind(TP, SRP, TPdis)

## calculate retention percent
data$retention_percent <- (data$Inflow_mg_L - data$Outflow_mg_L) / data$Inflow_mg_L *100
data$flow_atten <- (data$Inflow_vol_m3-data$Outflow_vol_m3) / data$Inflow_vol_m3

### Remove rows where Inflow = Outflow
nrow(data[which(data$flow_atten == 0),])
data2 <- data[which(data$flow_atten != 0), ]

plot(data2$flow_atten, data2$retention_percent, 
     ylim = c(-25,1), xlim = c(-25,1), 
     col = c('red', 'blue', 'green')[as.factor(data2$Species)], 
     pch = 16)
legend("bottomleft", levels(data2$Species), pch = 16,  col = c('red', 'blue', 'green'))



data.wide <- data %>%
  pivot_wider(id_cols = c("SiteID", "BMPID", "BMPType", "EventID", "DateStart"), 
              names_from = 'Species', values_from = c("retention_percent"))


### compare retention percent across P species
ggplot(data.wide, aes(x = TP, y = OrthoP, color = BMPType)) +
  geom_point() +
  xlim(-200,100) +
  ylim(-200,100) +
  facet_wrap(.~BMPType)





