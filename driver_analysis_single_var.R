

### stormwater BMP performance driver analysis


### for continous variables
##### ordinary least squares regression


# Surface area
# BMP Age
# Event discharge
# Inflow concentration
# Depth
# Aridity index

### for categorical variable
##### premutation test

# wet/dry
# planted/unplanted
# basin/channel


setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")

library(tidyverse)
library(ggplot2)

options(scipen=999)

select <- read.csv("BMP_Clean_filtered.csv", stringsAsFactors = TRUE)


### reorder factors
select$BMPType <- factor(select$BMPType , levels=c('BR','BS/BI','DB', "RP",  "WB", "WC"))
levels(select$BMPType) <- c('BR','GS','DB', "RP",  "WB", "WC")
BMP_names <- c("Bioretention","Grass strip/swale", "Detention basin (dry)", "Retention pond", "Wetland basin", "Wetland channel")
levels(select$BMPType_name) <- BMP_names
select$Species <- factor(select$Species , levels=c("TN","NH4", "NO3","TKN", "TP", "PO4"))

select <- select[which(select$Species != "TKN"),]




###### BMP size (Area_ha) -- select the complete cases
data <- select[!is.na(select$Area_ha),]

### TN
Spec <- data[which(data$Species == "TN"),]

Spec %>%
  ggplot(aes(x = log(Area_ha) , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-3,2) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("TN")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
BR <- c(NA, NA, NA, nrow(input))  ### BR has no area given

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

TN <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(TN) <- c("P", "R2_adj", "Corr", "n" )


############


### NO3
Spec <- data[which(data$Species == "NO3"),]

Spec %>%
  ggplot(aes(x = log(Area_ha) , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-3,2) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("NO3")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
BR <- c(NA, NA, NA, nrow(input))

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
WC <- c(NA, NA, NA, nrow(input) ) ## only on site, so all areas the same

NO3 <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(NO3) <- c("P", "R2_adj", "Corr", "n" )


############

### NH4
Spec <- data[which(data$Species == "NH4"),]

Spec %>%
  ggplot(aes(x = log(Area_ha) , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-3,2) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("NH4")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
BR <- c(NA, NA, NA, nrow(input))

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
DB <- c(NA, NA, NA, nrow(input))

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

NH4 <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(NH4) <- c("P", "R2_adj", "Corr", "n" )


############


### TP
Spec <- data[which(data$Species == "TP"),]

Spec %>%
  ggplot(aes(x = log(Area_ha) , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-3,2) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("TP")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
BR <- c(NA, NA, NA, nrow(input))

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

TP <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(TP) <- c("P", "R2_adj", "Corr", "n" )


########

### PO4
Spec <- data[which(data$Species == "PO4"),]

Spec %>%
  ggplot(aes(x = log(Area_ha) , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-3,2) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("PO4")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
BR <- c(NA, NA, NA, nrow(input))

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
DB <- c(NA, NA, NA, nrow(input))

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Area_ha), input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ log(Area_ha), data = input)
WC <- c(NA, NA, NA, nrow(input))


PO4 <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(PO4) <- c("P", "R2_adj", "Corr", "n" )




TN
NO3
NH4
TP
PO4

#############################################################
############################################################

### BMP Age
###### BMP Age (yrs) -- select the complete cases
data <- select[!is.na(select$BMPAge),]

### TN
Spec <- data[which(data$Species == "TN"),]

Spec %>%
  ggplot(aes(x = log(Vol_in) , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-3,2) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("TN")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
BR <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

TN <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(TN) <- c("P", "R2_adj", "Corr", "n" )


############


### NO3
Spec <- data[which(data$Species == "NO3"),]

Spec %>%
  ggplot(aes(x = BMPAge , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-3,2) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("NO3")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
BR <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

NO3 <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(NO3) <- c("P", "R2_adj", "Corr", "n" )


############

### NH4
Spec <- data[which(data$Species == "NH4"),]

Spec %>%
  ggplot(aes(x = BMPAge , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-3,2) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("NH4")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
BR <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

NH4 <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(NH4) <- c("P", "R2_adj", "Corr", "n" )


############


### TP
Spec <- data[which(data$Species == "TP"),]

Spec %>%
  ggplot(aes(x = BMPAge , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-3,2) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("TP")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
BR <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

TP <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(TP) <- c("P", "R2_adj", "Corr", "n" )


########

### PO4
Spec <- data[which(data$Species == "PO4"),]

Spec %>%
  ggplot(aes(x = BMPAge , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-3,2) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("PO4")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
BR <-  c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ BMPAge, data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(input$BMPAge, input$retention_percent), nrow(input)  )


PO4 <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(PO4) <- c("P", "R2_adj", "Corr", "n" )




TN
NO3
NH4
TP
PO4



#############################################################
############################################################

### Event Discharge
###### Vol_in (m3/event) -- select the complete cases
data <- select[!is.na(select$Vol_in),]

### TN
Spec <- data[which(data$Species == "TN"),]

Spec %>%
  ggplot(aes(x = log(Vol_in) , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-3,2) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("TN")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
BR <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

TN <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(TN) <- c("P", "R2_adj", "Corr", "n" )


############


### NO3
Spec <- data[which(data$Species == "NO3"),]

Spec %>%
  ggplot(aes(x = log(Vol_in) , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-3,2) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("NO3")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
BR <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

NO3 <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(NO3) <- c("P", "R2_adj", "Corr", "n" )


############

### NH4
Spec <- data[which(data$Species == "NH4"),]

Spec %>%
  ggplot(aes(x = log(Vol_in) , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-3,2) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("NH4")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
BR <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

NH4 <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(NH4) <- c("P", "R2_adj", "Corr", "n" )


############


### TP
Spec <- data[which(data$Species == "TP"),]

Spec %>%
  ggplot(aes(x = log(Vol_in) , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-3,2) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("TP")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
BR <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

TP <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(TP) <- c("P", "R2_adj", "Corr", "n" )


########

### PO4
Spec <- data[which(data$Species == "PO4"),]

Spec %>%
  ggplot(aes(x = log(Vol_in) , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-3,2) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("PO4")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
BR <-  c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ log(Vol_in), data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Vol_in), input$retention_percent), nrow(input)  )


PO4 <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(PO4) <- c("P", "R2_adj", "Corr", "n" )




TN
NO3
NH4
TP
PO4





#############################################################
############################################################

### Inflow concentration
###### Conc_in (mg/L) -- select the complete cases
data <- select[!is.na(select$Conc_in),]

### TN
Spec <- data[which(data$Species == "TN"),]

Spec %>%
  ggplot(aes(x = log(Conc_in) , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  xlim(-2.2,3) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("TN")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
BR <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

TN <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(TN) <- c("P", "R2_adj", "Corr", "n" )


############


### NO3
Spec <- data[which(data$Species == "NO3"),]

Spec %>%
  ggplot(aes(x = log(Conc_in) , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-2.2,3) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("NO3")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
BR <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

NO3 <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(NO3) <- c("P", "R2_adj", "Corr", "n" )


############

### NH4
Spec <- data[which(data$Species == "NH4"),]

Spec %>%
  ggplot(aes(x = log(Conc_in) , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-2.2,3) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("NH4")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
BR <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

NH4 <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(NH4) <- c("P", "R2_adj", "Corr", "n" )


############


### TP
Spec <- data[which(data$Species == "TP"),]

Spec %>%
  ggplot(aes(x = log(Conc_in) , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  #xlim(-3,2) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("TP")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
BR <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

TP <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(TP) <- c("P", "R2_adj", "Corr", "n" )


########

### PO4
Spec <- data[which(data$Species == "PO4"),]

Spec %>%
  ggplot(aes(x = log(Conc_in) , y = retention_percent,  color = BMPType))+
  geom_point() +
  #scale_color_manual(values = pal)+
  theme_classic(base_size=14) +
  xlim(-7,1) +
  ylim(-100,100) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("PO4")

## Bioretention --- no area data
input <- Spec[which(Spec$BMPType == "BR"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
BR <-  c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Grass Strip/swale
input <- Spec[which(Spec$BMPType == "GS"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
GS <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Detention basin
input <- Spec[which(Spec$BMPType == "DB"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
DB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Retention pond
input <- Spec[which(Spec$BMPType == "RP"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
RP <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Wetland basin
input <- Spec[which(Spec$BMPType == "WB"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
WB <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )

## Wetland channel
input <- Spec[which(Spec$BMPType == "WC"),]
mod <- lm(retention_percent ~ log(Conc_in), data = input)
WC <- c(summary(mod)$coefficients[,4][[2]], summary(mod)$adj.r.squared, cor(log(input$Conc_in), input$retention_percent), nrow(input)  )


PO4 <- data.frame(BR, GS, DB, RP, WB, WC)
rownames(PO4) <- c("P", "R2_adj", "Corr", "n" )




TN
NO3
NH4
TP
PO4



### Depth

### Aridity index




