


### GAM model, BMP drivers




setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")

library(tidyverse)
library(ggplot2)
library(cowplot)
library(RColorBrewer)

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


######## exclude very low retention percent (5% of data)
data <- data[data$retention_percent > -230,]


## GAM


library(mgcv)
library(lme4)
## mgcv::gamm() same as nlme::lme() plus mgcv::gam()

### full model
model_full <- gam((retention_percent) ~ s(log(Conc_in)) + 
                    s(log(Vol_in)) + 
                    s(log(Load_in)) +    ##exclude cause it is not sig? 
                    s(AI) + 
                    s(latitude) + 
                    s(BMPAge) +
                    s(Area_ha) +
                    s(BMPType, bs = "re") +             #### random effect
                    s(Species, bs = "re"),              #### random effect
                  data = data, method = "REML", family = gaussian() )
summary(model_full)
AIC(model_full)
summary(model_full)$r.sq



### full model plus flow attenuation

model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) +  
                    s(log(Conc_in)) + 
                    s(log(Vol_in)) + 
                    s(log(Load_in)) + 
                    s(AI) + 
                    s(latitude) + 
                    s(BMPAge) +
                    s(Area_ha) +
                    s(BMPType, bs = "re") +             #### random effect
                    s(Species, bs = "re"),              #### random effect
                  data = data, method = "REML", family = gaussian() )
summary(model_full_plus_FA)
AIC(model_full_plus_FA)

summary(model_full_plus_FA)$r.sq







#### EACH BMP Type separate -- Solute group by N/P 
#### BMPsize and BMPage removed (degrees of freedom issue)




##
### DB
##
{
#### PHOSPHORUS
input <- P.data[which(P.data$BMPType == "DB"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                    s(AI) + s(latitude) + #s(BMPAge) + 
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
AIC(model_full)
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                            s(AI) + s(latitude) + #s(BMPAge) + 
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq

###### NITROGEN
input <- N.data[which(N.data$BMPType == "DB"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                    s(AI) + s(latitude) + #s(BMPAge) + 
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
AIC(model_full)
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) +  s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                            s(AI) + s(latitude) + #s(BMPAge) + 
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq
}

##
### RP
##
{
#### PHOSPHORUS
input <- P.data[which(P.data$BMPType == "RP"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                    s(AI) + s(latitude) + #s(BMPAge) + 
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
AIC(model_full)
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                            s(AI) + s(latitude) + #s(BMPAge) + 
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq

###### NITROGEN
input <- N.data[which(N.data$BMPType == "RP"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                    s(AI) + s(latitude) + #s(BMPAge) + 
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
AIC(model_full)
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) +  s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                            s(AI) + s(latitude) + #s(BMPAge) + 
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq
}

##
### WC
##

{
#### PHOSPHORUS
input <- P.data[which(P.data$BMPType == "WC"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                    s(AI) + s(latitude) + #s(BMPAge) + 
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
AIC(model_full)
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                            s(AI) + s(latitude) + #s(BMPAge) + 
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq

###### NITROGEN
input <- N.data[which(N.data$BMPType == "WC"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                    s(AI) + s(latitude) + #s(BMPAge) + 
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
AIC(model_full)
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) +  s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                            s(AI) + s(latitude) + #s(BMPAge) + 
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq
}


##
### WB
##
{
#### PHOSPHORUS
input <- P.data[which(P.data$BMPType == "WB"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                    s(AI) + s(latitude) + #s(BMPAge) + 
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
AIC(model_full)
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                            s(AI) + s(latitude) + #s(BMPAge) + 
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq

###### NITROGEN
input <- N.data[which(N.data$BMPType == "WB"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                    s(AI) + s(latitude) + #s(BMPAge) + 
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
AIC(model_full)
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) +  s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                            s(AI) + s(latitude) + #s(BMPAge) + 
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq
}

##
### GS
##

{
#### PHOSPHORUS
input <- P.data[which(P.data$BMPType == "GS"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                    s(AI) + s(latitude) + #s(BMPAge) + 
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
AIC(model_full)
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                            s(AI) + s(latitude) + #s(BMPAge) + 
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq

###### NITROGEN
input <- N.data[which(N.data$BMPType == "GS"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                    s(AI) + s(latitude) + #s(BMPAge) + 
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
AIC(model_full)
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) +  s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                            s(AI) + s(latitude) + #s(BMPAge) + 
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq

}

##
### BR
##

{
#### PHOSPHORUS
input <- P.data[which(P.data$BMPType == "BR"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                    s(AI) + s(latitude) + 
                    #s(BMPAge) + 
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
AIC(model_full)
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) + s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                            s(AI) + s(latitude) + 
                            #s(BMPAge) + 
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq

###### NITROGEN
input <- N.data[which(N.data$BMPType == "BR"),]
model_full <- gam((retention_percent) ~ s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                    s(AI) + s(latitude) + 
                    #s(BMPAge) + 
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
AIC(model_full)
summary(model_full)$r.sq
model_full_plus_FA <- gam((retention_percent) ~ s(flow_atten_percent) +  s(log(Conc_in)) +  s(log(Vol_in)) + s(log(Load_in)) + 
                            s(AI) + s(latitude) + #s(BMPAge) + 
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq

}









###### Grouped by BMPType attributes (Shape, wetness, vegetation) and solute type
{

############
##   "Bowls" = RP and DB and WB and BR
############
P.data <- data[which(data$Species == "TP" | data$Species == "PO4"),]
N.data <- data[which(data$Species != "TP" & data$Species != "PO4"),]


input <- P.data[which(P.data$BMPType == "RP" | P.data$BMPType == "DB" | P.data$BMPType == "WB" | P.data$BMPType == "BR"),]


### full model
model_full <- gam((retention_percent) ~ 
                    s(log(Conc_in)) + 
                    s(log(Vol_in)) + 
                    s(log(Load_in)) + 
                    s(AI) + 
                    s(latitude) + 
                    s(BMPAge) +
                    #s(Area_ha) +
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
#summary(model_full)
AIC(model_full)
summary(model_full)$r.sq
### full model plus flow attenuation
model_full_plus_FA <- gam((retention_percent) ~ 
                            s(flow_atten_percent) +  
                            s(log(Conc_in)) + 
                            s(log(Vol_in)) + 
                            s(log(Load_in)) + 
                            s(AI) + 
                            s(latitude) + 
                            s(BMPAge) +
                            #s(Area_ha) +
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
#summary(model_full_plus_FA)
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq



input <- N.data[which(N.data$BMPType == "RP" | N.data$BMPType == "DB" | N.data$BMPType == "WB" | N.data$BMPType == "BR"),]
### full model
model_full <- gam((retention_percent) ~ 
                    s(log(Conc_in)) + 
                    s(log(Vol_in)) + 
                    s(log(Load_in)) + 
                    s(AI) + 
                    s(latitude) + 
                    s(BMPAge) +
                    #s(Area_ha) +
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
#summary(model_full)
AIC(model_full)
summary(model_full)$r.sq
### full model plus flow attenuation
model_full_plus_FA <- gam((retention_percent) ~ 
                            s(flow_atten_percent) +  
                            s(log(Conc_in)) + 
                            s(log(Vol_in)) + 
                            s(log(Load_in)) + 
                            s(AI) + 
                            s(latitude) + 
                            s(BMPAge) +
                            #s(Area_ha) +
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
#summary(model_full_plus_FA)
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq



############
##   "Channels" = WC and GS
############

input <- P.data[which(P.data$BMPType == "WC" | P.data$BMPType == "GS" ),]


### full model
model_full <- gam((retention_percent) ~ 
                    s(log(Conc_in)) + 
                    s(log(Vol_in)) + 
                    s(log(Load_in)) + 
                    s(AI) + 
                    s(latitude) + 
                    s(BMPAge) +
                    #s(Area_ha) +
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
#summary(model_full)
AIC(model_full)
summary(model_full)$r.sq
### full model plus flow attenuation
model_full_plus_FA <- gam((retention_percent) ~ 
                            s(flow_atten_percent) +  
                            s(log(Conc_in)) + 
                            s(log(Vol_in)) + 
                            s(log(Load_in)) + 
                            s(AI) + 
                            s(latitude) + 
                            s(BMPAge) +
                            #s(Area_ha) +
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
#summary(model_full_plus_FA)
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq



input <- N.data[which(N.data$BMPType == "WC" | N.data$BMPType == "GS" ),]
### full model
model_full <- gam((retention_percent) ~ 
                    s(log(Conc_in)) + 
                    s(log(Vol_in)) + 
                    s(log(Load_in)) + 
                    s(AI) + 
                    s(latitude) + 
                    s(BMPAge) +
                    #s(Area_ha) +
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
#summary(model_full)
AIC(model_full)
summary(model_full)$r.sq
### full model plus flow attenuation
model_full_plus_FA <- gam((retention_percent) ~ 
                            s(flow_atten_percent) +  
                            s(log(Conc_in)) + 
                            s(log(Vol_in)) + 
                            s(log(Load_in)) + 
                            s(AI) + 
                            s(latitude) + 
                            s(BMPAge) +
                            #s(Area_ha) +
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
#summary(model_full_plus_FA)
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq


##########################################
############################################################################
##########################################



### wet/dry

input <- P.data[which(P.data$BMPType == "WC" | P.data$BMPType == "WB" | P.data$BMPType == "RP"),]


### full model
model_full <- gam((retention_percent) ~ 
                    s(log(Conc_in)) + 
                    s(log(Vol_in)) + 
                    s(log(Load_in)) + 
                    s(AI) + 
                    s(latitude) + 
                    s(BMPAge) +
                    #s(Area_ha) +
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
#summary(model_full)
AIC(model_full)
summary(model_full)$r.sq
### full model plus flow attenuation
model_full_plus_FA <- gam((retention_percent) ~ 
                            s(flow_atten_percent) +  
                            s(log(Conc_in)) + 
                            s(log(Vol_in)) + 
                            s(log(Load_in)) + 
                            s(AI) + 
                            s(latitude) + 
                            s(BMPAge) +
                            #s(Area_ha) +
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
#summary(model_full_plus_FA)
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq



input <- N.data[which(N.data$BMPType == "WB" | N.data$BMPType == "WC" | N.data$BMPType == "RP"),]
### full model
model_full <- gam((retention_percent) ~ 
                    s(log(Conc_in)) + 
                    s(log(Vol_in)) + 
                    s(log(Load_in)) + 
                    s(AI) + 
                    s(latitude) + 
                    s(BMPAge) +
                    #s(Area_ha) +
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
#summary(model_full)
AIC(model_full)
summary(model_full)$r.sq
### full model plus flow attenuation
model_full_plus_FA <- gam((retention_percent) ~ 
                            s(flow_atten_percent) +  
                            s(log(Conc_in)) + 
                            s(log(Vol_in)) + 
                            s(log(Load_in)) + 
                            s(AI) + 
                            s(latitude) + 
                            s(BMPAge) +
                            #s(Area_ha) +
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
#summary(model_full_plus_FA)
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq



############
##   "dry" = GS and DB and BR
############

input <- P.data[which(P.data$BMPType == "DB" | P.data$BMPType == "GS" | P.data$BMPType == "BR" ),]


### full model
model_full <- gam((retention_percent) ~ 
                    s(log(Conc_in)) + 
                    s(log(Vol_in)) + 
                    s(log(Load_in)) + 
                    s(AI) + 
                    s(latitude) + 
                    s(BMPAge) +
                    #s(Area_ha) +
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
#summary(model_full)
AIC(model_full)
summary(model_full)$r.sq
### full model plus flow attenuation
model_full_plus_FA <- gam((retention_percent) ~ 
                            s(flow_atten_percent) +  
                            s(log(Conc_in)) + 
                            s(log(Vol_in)) + 
                            s(log(Load_in)) + 
                            s(AI) + 
                            s(latitude) + 
                            s(BMPAge) +
                            #s(Area_ha) +
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
#summary(model_full_plus_FA)
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq



input <- N.data[which(N.data$BMPType == "GS" | N.data$BMPType == "DB" | N.data$BMPType == "BR"),]
### full model
model_full <- gam((retention_percent) ~ 
                    s(log(Conc_in)) + 
                    s(log(Vol_in)) + 
                    s(log(Load_in)) + 
                    s(AI) + 
                    s(latitude) + 
                    s(BMPAge) +
                    #s(Area_ha) +
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
#summary(model_full)
AIC(model_full)
summary(model_full)$r.sq
### full model plus flow attenuation
model_full_plus_FA <- gam((retention_percent) ~ 
                            s(flow_atten_percent) +  
                            s(log(Conc_in)) + 
                            s(log(Vol_in)) + 
                            s(log(Load_in)) + 
                            s(AI) + 
                            s(latitude) + 
                            s(BMPAge) +
                            #s(Area_ha) +
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
#summary(model_full_plus_FA)
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq


########################################################
#########################################################################
########################################################


### veg/non veg

input <- P.data[which(P.data$BMPType == "WC" | P.data$BMPType == "WB" | P.data$BMPType == "GS" | P.data$BMPType == "BR"),]


### full model
model_full <- gam((retention_percent) ~ 
                    s(log(Conc_in)) + 
                    s(log(Vol_in)) + 
                    s(log(Load_in)) + 
                    s(AI) + 
                    s(latitude) + 
                    s(BMPAge) +
                    #s(Area_ha) +
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
#summary(model_full)
AIC(model_full)
summary(model_full)$r.sq
### full model plus flow attenuation
model_full_plus_FA <- gam((retention_percent) ~ 
                            s(flow_atten_percent) +  
                            s(log(Conc_in)) + 
                            s(log(Vol_in)) + 
                            s(log(Load_in)) + 
                            s(AI) + 
                            s(latitude) + 
                            s(BMPAge) +
                            #s(Area_ha) +
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
#summary(model_full_plus_FA)
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq



input <- N.data[which(N.data$BMPType == "BR" | N.data$BMPType == "WC" | N.data$BMPType == "WB" | N.data$BMPType == "GS"),]
### full model
model_full <- gam((retention_percent) ~ 
                    s(log(Conc_in)) + 
                    s(log(Vol_in)) + 
                    s(log(Load_in)) + 
                    s(AI) + 
                    s(latitude) + 
                    s(BMPAge) +
                    #s(Area_ha) +
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
#summary(model_full)
AIC(model_full)
summary(model_full)$r.sq
### full model plus flow attenuation
model_full_plus_FA <- gam((retention_percent) ~ 
                            s(flow_atten_percent) +  
                            s(log(Conc_in)) + 
                            s(log(Vol_in)) + 
                            s(log(Load_in)) + 
                            s(AI) + 
                            s(latitude) + 
                            s(BMPAge) +
                            #s(Area_ha) +
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
#summary(model_full_plus_FA)
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq



############
##   "non-veg" = RP and DB
############

input <- P.data[which(P.data$BMPType == "DB" | P.data$BMPType == "RP"),]


### full model
model_full <- gam((retention_percent) ~ 
                    s(log(Conc_in)) + 
                    s(log(Vol_in)) + 
                    s(log(Load_in)) + 
                    s(AI) + 
                    s(latitude) + 
                    s(BMPAge) +
                    #s(Area_ha) +
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
#summary(model_full)
AIC(model_full)
summary(model_full)$r.sq
### full model plus flow attenuation
model_full_plus_FA <- gam((retention_percent) ~ 
                            s(flow_atten_percent) +  
                            s(log(Conc_in)) + 
                            s(log(Vol_in)) + 
                            s(log(Load_in)) + 
                            s(AI) + 
                            s(latitude) + 
                            s(BMPAge) +
                            #s(Area_ha) +
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
#summary(model_full_plus_FA)
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq



input <- N.data[which(N.data$BMPType == "RP" | N.data$BMPType == "DB"),]
### full model
model_full <- gam((retention_percent) ~ 
                    s(log(Conc_in)) + 
                    s(log(Vol_in)) + 
                    s(log(Load_in)) + 
                    s(AI) + 
                    s(latitude) + 
                    s(BMPAge) +
                    #s(Area_ha) +
                    s(Species, bs = "re"),
                  data = input, method = "REML", family = gaussian() )
#summary(model_full)
AIC(model_full)
summary(model_full)$r.sq
### full model plus flow attenuation
model_full_plus_FA <- gam((retention_percent) ~ 
                            s(flow_atten_percent) +  
                            s(log(Conc_in)) + 
                            s(log(Vol_in)) + 
                            s(log(Load_in)) + 
                            s(AI) + 
                            s(latitude) + 
                            s(BMPAge) +
                            #s(Area_ha) +
                            s(Species, bs = "re"),
                          data = input, method = "REML", family = gaussian() )
#summary(model_full_plus_FA)
AIC(model_full_plus_FA)
summary(model_full_plus_FA)$r.sq
}


