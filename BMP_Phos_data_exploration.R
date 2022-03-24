

## TP data exploration

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/BMP_project")


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

BR <- TP[which(TP$BMPType == "BR"),]

table(BR$EventID)
table(BR$BMPID)



TP$retention <- (TP$TP_Inflow - TP$TP_Outflow)/TP$TP_Inflow*100  ## percent

TP$Wat<- (TP$Volume_Total_Inflow - TP$Volume_Total_Outflow)/TP$Volume_Total_Inflow*100  ## percent



plot(TP$Volume_Total_Inflow, TP$retention, log = "xy", main = "TP % Retention")



plot(TP$Wat, TP$retention, main = "TP % Retention",
     xlim = c(-2000,100), 
     ylim = c(-2000,100))

plot(TP$Wat, TP$retention, main = "TP % Retention",
     xlim = c(-100,100), 
     ylim = c(-100,100))




### TP vs PO4 retention


TP$species <- "TP"
SRP$species <- "SRP"
TPdis$species <- "TPdis"

names(TP) <- c("X", "SiteID", "BMPID", "BMPType", "EventID", "DateStart",
               "Value_Unit", "Outflow_vol_m3", "Inflow_vol_m3", 
               "Outflow", "Inflow", "species")
names(SRP) <- c("X", "SiteID", "BMPID", "BMPType", "EventID", "DateStart",
                "Value_Unit", "Outflow_vol_m3", "Inflow_vol_m3", 
                "Outflow", "Inflow", "species")
names(TPdis) <- c("X", "SiteID", "BMPID", "BMPType", "EventID", "DateStart",
                  "Value_Unit", "Outflow_vol_m3", "Inflow_vol_m3", 
                  "Outflow", "Inflow", "species")

data <- rbind(TP, SRP, TPdis)



data$retention_percent <- (data$Inflow - data$Outflow) / data$Inflow
data$flow_atten <- (data$Inflow_vol_m3-data$Outflow_vol_m3) / data$Inflow_vol_m3



data <- data[which(data$flow_atten != 0), ]

plot(data$flow_atten, data$retention_percent, 
     ylim = c(-25,1), xlim = c(-25,1), 
     col = c('red', 'blue', 'green')[as.factor(data$species)], 
     pch = 16)
legend("bottomleft", c("SRP", "TP", "TPdis"), pch = 16,  col = c('red', 'blue', 'green'))




levels(as.factor(data$species))






