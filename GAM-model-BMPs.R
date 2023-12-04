
### GAM model, BMP drivers

library(tidyverse)
library(ggplot2)
library(mgcv)
library(tidymv)
library(lme4)


setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")
select <- read.csv("BMP_Clean_filtered.csv", stringsAsFactors = TRUE)

### reorder factors
select$BMPType <- factor(select$BMPType , levels=c('BR','BS/BI','DB', "RP",  "WB", "WC"))
levels(select$BMPType) <- c('BR','GS','DB', "RP",  "WB", "WC")
select$Species <- factor(select$Species , levels=c("TN","NH4", "NO3","TKN", "TP", "PO4"))
select <- select[which(select$Species != "TKN"),]
data <- select[!is.na(select$flow_atten_percent),]

check <- data[!is.na(data$Vol_m3),]
check <- data[!is.na(data$Area_ha),]


######## exclude very low retention percent (5% of data)
data <- data[data$retention_percent > -230,]
data$latitude <- abs(data$latitude)


## GAM
## mgcv::gamm() same as nlme::lme() plus mgcv::gam()

### full model
model_full <- gam((retention_percent) ~ s(log(Conc_in)) + 
                    s(log(Vol_in)) + 
                    s(AI) + 
                    s(latitude) + 
                    s(BMPType, bs = "re") +             #### random effect
                    s(Species, bs = "re"),              #### random effect
                  data = data, method = "REML", family = gaussian(), select = TRUE )
summary(model_full)

#### NULL model -- attenuation only model
model_FA <- gam((retention_percent) ~ s(flow_atten_percent) +  
                                  s(BMPType, bs = "re") +             #### random effect
                                  s(Species, bs = "re"),              #### random effect
                          data = data, method = "REML", family = gaussian() )
summary(model_FA)


### full model plus flow attenuation
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) +  
                    s(log(Conc_in)) + 
                    s(log(Vol_in)) + 
                    s(AI) + 
                    s(latitude) + 
                    s(BMPType, bs = "re") +             #### random effect
                    s(Species, bs = "re"),              #### random effect
                  data = data, method = "REML", family = gaussian() )
summary(model_full_plus_FA)


### make figure
tiff(filename = "figures/GAM-full-model.tif", height = 4, width = 6, 
     units = "in", res = 800, compression = 'lzw')
par(mfrow = c(2,3), mar = c(4,2,1,2), oma = c(0,2,0.2,0))
plot(model_full_plus_FA, residuals = FALSE, rug = FALSE, se = TRUE,
     shade = TRUE, shade.col = "#FF000040", col = "red", lwd = 2, 
     select = 1, ylab = "", xlab = "Flow \nattenuation (%)")
plot(model_full_plus_FA, residuals = FALSE, rug = FALSE, se = TRUE,
     shade = TRUE, shade.col = "#FF00FF40", col = "#FF00FF", lwd = 2, 
     select = 2, ylab = "", xlab = "Log concentration\n(mg/L)")
plot(model_full_plus_FA, residuals = FALSE, rug = FALSE, se = TRUE,
     shade = TRUE, shade.col = "#00FF5040", col = "#00FF90", lwd = 2, 
     select = 3, ylab = "", xlab = "Log volume\n(m2)")
plot(model_full_plus_FA, residuals = FALSE, rug = FALSE, se = TRUE,
     shade = TRUE, shade.col = "#0000FF40", col = "#0000FF", lwd = 2, 
     select = 4, ylab = "", xlab = "Aridity Index\n")
plot(model_full_plus_FA, residuals = FALSE, rug = FALSE, se = TRUE,
     shade = TRUE, shade.col = "#e8ab0740", col = "#e8ab07", lwd = 2, 
     select = 5, ylab = "", xlab = "Latitude\n")
mtext("   Solute retention (%)", outer = TRUE, cex = 0.8, side = 2, line = 0.5)
dev.off()







#### Model by nutrient group


P.data <- data[which(data$Species == "TP" | data$Species == "PO4"),]
N.data <- data[which(data$Species != "TP" & data$Species != "PO4"),]

#### PHOSPHORUS
input <- P.data
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) + 
                    s(AI) + s(latitude) + 
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + 
                            s(log(Conc_in)) +  s(log(Vol_in)) + 
                            s(AI) + s(latitude) +
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
summary(model_full_plus_FA)$r.sq

###### NITROGEN
input <- N.data
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) + 
                    s(AI) + s(latitude) +  
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + 
                            s(log(Conc_in)) +  s(log(Vol_in)) + 
                            s(AI) + s(latitude) + 
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
summary(model_full_plus_FA)$r.sq






##
### DB
##

#### PHOSPHORUS
input <- P.data[which(P.data$BMPType == "DB"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) +  
                    s(AI) + s(latitude) + 
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + 
                            s(log(Conc_in)) +  s(log(Vol_in)) +  
                            s(AI) + s(latitude) +
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
summary(model_full_plus_FA)$r.sq

###### NITROGEN
input <- N.data[which(N.data$BMPType == "DB"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) +  
                          s(AI) + s(latitude) + 
                          s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + 
                                  s(log(Conc_in)) +  s(log(Vol_in)) +  
                                  s(AI) + s(latitude) +
                                  s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
summary(model_full_plus_FA)$r.sq


##
### RP
##

#### PHOSPHORUS
input <- P.data[which(P.data$BMPType == "RP"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) +  
                          s(AI) + s(latitude) + 
                          s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + 
                                  s(log(Conc_in)) +  s(log(Vol_in)) +  
                                  s(AI) + s(latitude) +
                                  s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
summary(model_full_plus_FA)$r.sq

###### NITROGEN
input <- N.data[which(N.data$BMPType == "RP"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) +  
                          s(AI) + s(latitude) + 
                          s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + 
                                  s(log(Conc_in)) +  s(log(Vol_in)) +  
                                  s(AI) + s(latitude) +
                                  s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
summary(model_full_plus_FA)$r.sq


##
### WC
##


#### PHOSPHORUS
input <- P.data[which(P.data$BMPType == "WC"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) +  
                          s(AI, k = 4) + s(latitude, k = 4) + 
                          s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + 
                                  s(log(Conc_in)) +  s(log(Vol_in)) +  
                                  s(AI, k = 4) + s(latitude, k = 4) +
                                  s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
summary(model_full_plus_FA)$r.sq

###### NITROGEN
input <- N.data[which(N.data$BMPType == "WC"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) +  
                          s(AI, k = 4) + s(latitude, k = 4) + 
                          s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + 
                                  s(log(Conc_in)) +  s(log(Vol_in)) +  
                                  s(AI, k = 4) + s(latitude, k = 4) +
                                  s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
summary(model_full_plus_FA)$r.sq



##
### WB
##

#### PHOSPHORUS
input <- P.data[which(P.data$BMPType == "WB"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) +  
                          s(AI) + s(latitude) + 
                          s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + 
                                  s(log(Conc_in)) +  s(log(Vol_in)) +  
                                  s(AI) + s(latitude) +
                                  s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
summary(model_full_plus_FA)$r.sq


###### NITROGEN
input <- N.data[which(N.data$BMPType == "WB"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) +  
                          s(AI) + s(latitude) + 
                          s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + 
                                  s(log(Conc_in)) +  s(log(Vol_in)) +  
                                  s(AI) + s(latitude) +
                                  s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
summary(model_full_plus_FA)$r.sq


##
### GS
##


#### PHOSPHORUS
input <- P.data[which(P.data$BMPType == "GS"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) +  
                          s(AI) + s(latitude) + 
                          s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + 
                                  s(log(Conc_in)) +  s(log(Vol_in)) +  
                                  s(AI) + s(latitude) +
                                  s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
summary(model_full_plus_FA)$r.sq

###### NITROGEN
input <- N.data[which(N.data$BMPType == "GS"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) +  
                          s(AI) + s(latitude) + 
                          s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + 
                                  s(log(Conc_in)) +  s(log(Vol_in)) +  
                                  s(AI) + s(latitude) +
                                  s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
summary(model_full_plus_FA)$r.sq


##
### BR
##


#### PHOSPHORUS
input <- P.data[which(P.data$BMPType == "BR"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) +  
                          s(AI) + s(latitude) + 
                          s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + 
                                  s(log(Conc_in)) +  s(log(Vol_in)) +  
                                  s(AI) + s(latitude) +
                                  s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
summary(model_full_plus_FA)$r.sq

###### NITROGEN
input <- N.data[which(N.data$BMPType == "BR"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) +  
                          s(AI) + s(latitude) + 
                          s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + 
                                  s(log(Conc_in)) +  s(log(Vol_in)) +  
                                  s(AI) + s(latitude) +
                                  s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
summary(model_full_plus_FA)$r.sq


