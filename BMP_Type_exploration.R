

### 
# Clean code for working up the clean data and running some comparative analyses

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/BMP_project")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")

library(tidyverse)
library(ggplot2)
library(viridis)
options(scipen=999)

Phos <- read.csv("BMP_P_clean.csv")
Nit <- read.csv("BMP_N_clean.csv")
data <- rbind(Phos, Nit)   ## merge N and P data

## Append BMP Age data
{bmp <- read.csv("BMPInfo.csv", header = TRUE)  %>%
  select(c('BMPID','SiteID', 'DateInstalled')) %>%
  mutate(YearInstalled = stringi::stri_sub(DateInstalled, -4,)) %>%
  mutate(YearInstalled = as.integer(YearInstalled))


data <- data %>%
  left_join(bmp, by = c("SiteID","BMPID"))  %>%
  mutate(year = stringi::stri_sub(DateStart, -2 ,)) %>%
  mutate(year0 = ifelse(year < 25, 20, 19)) %>%
  mutate(EventYear = paste(year0, year, sep = "")) %>%
  mutate(EventYear = as.integer(EventYear)) %>%
  mutate(BMPAge = EventYear - YearInstalled) %>%
  select(-c('year', 'year0'))}

## calculate FLOW ATTENUATION
data$flow_atten <- (data$Vol_in-data$Vol_out) 
data$flow_atten_percent <- (data$Vol_in-data$Vol_out) / data$Vol_in *100
## calculate solute retention
data$retention <- data$Load_in - data$Load_out
data$retention_percent <- (data$Load_in - data$Load_out)/data$Load_in*100


### filter out events with  outflow more than 110% of inflow
data10 <- data[which(data$flow_atten_percent > -10),]


###write.csv(data10, "filtered_BMPdata.csv")

## select BMP types of interest
our_types <- c("BI", "BR", "BS", "DB", "RP", "WB", "WC" )
select <- data10 %>%
  filter(BMPType %in% our_types)
select$BMPType <- factor(select$BMPType , levels=c('DB','RP','WB','BS', "BR", "BI", "WC"))
BMP_names <- c("Detention basin (dry)", "Retention pond", "Wetland basin", "Grass swale","Bioretention", "Grass strip", "Wetland channel")
#levels(select$BMPType) <- BMP_names

select$Species <- factor(select$Species, levels=c("ammonia_N", "Nitrate_N", "TN", "TKN", "TP.dis", "orthoP","TP"))
Species_names <-c("NH4", "NO3", "TN", "TKN", "disTP", "PO4", "TP")
levels(select$Species) <- Species_names


table(select$Species, select$BMPType)


### mass retention
ggplot(select, aes(x = BMPType, y = retention, fill = factor(BMPType))) +
  geom_boxplot(trim = TRUE) +
  scale_fill_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  coord_cartesian(ylim = c(-500,10000)) +
  #ylim(-100,500) +
  theme_bw(base_size = 16) +
  theme(legend.position = c(0.88,0.3) ) +
  facet_wrap(.~Species, nrow = 2, scales = "free") +
  theme(axis.text=element_text(size=8)) +
  ylab("Retention (g/event)") +
  xlab(" ")

### percent retention
ggplot(select, aes(x = BMPType, y = retention_percent, fill = factor(BMPType))) +
  geom_boxplot(trim = TRUE) +
  scale_fill_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  coord_cartesian(ylim = c(-200,100)) +
  #ylim(-100,500) +
  theme_bw(base_size = 16) +
  theme(legend.position = c(0.88,0.3) ) +
  facet_wrap(.~Species, nrow = 2, scales = "free") +
  theme(axis.text=element_text(size=8)) +
  geom_hline(yintercept = 0) +
  ylab("Percent retention") +
  xlab(" ")





#### percent retention vs age


## filter data, no age less than 0 (ie. no pre BMP conditions)
##              also no age more than 10yrs as these seam to buck the trend
df <- select[which(select$BMPAge > -1 & select$BMPAge < 11),]

ggplot(df, aes(x = BMPAge, y = retention_percent, color = BMPType)) +
  geom_point() +
  scale_color_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(ylim = c(-200,100)) +
  facet_wrap(.~Species, nrow = 2, scales = "free") +
  theme_bw(base_size = 16) +
  theme(legend.position = c(0.88,0.3) ) +
  ylab("Percent retention") +
  xlab("BMP Age")





### solute comparisons

##TP vs PO4

### find the complete cases
CC <- select %>% 
  select(c("SiteID", "EventID", "BMPID", "BMPType", "Species", "retention", "retention_percent")) %>%
  filter(Species  %in% c("TP", "PO4")) %>%
  distinct() %>%
  pivot_wider(names_from = Species, values_from = c("retention", "retention_percent"))
new <- CC %>% drop_na
##n = 608


plot(new$retention_percent_TP, new$retention_percent_PO4)

ggplot(new, aes(x = retention_percent_TP, y = retention_percent_PO4, color = BMPType)) +
  geom_point() +
  scale_color_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  #geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(ylim = c(-200,100), xlim = c(-200,100)) +
  facet_wrap(.~BMPType, nrow = 2, scales = "free") +
  theme_bw(base_size = 12) +
  theme(legend.position = c(0.88,0.2) ) +
  ylab("PO4 retention (%)") +
  xlab("TP retention (%)") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_abline(slope = 1, intercept = 0) 
  


##TN vs TP

### find the complete cases
CC <- select %>% 
  select(c("SiteID", "EventID", "BMPID", "BMPType", "Species", "retention", "retention_percent")) %>%
  filter(Species  %in% c("TN", "TP")) %>%
  distinct() %>%
  pivot_wider(names_from = Species, values_from = c("retention", "retention_percent"))
new <- CC %>% drop_na
## n = 565

ggplot(new, aes(x = retention_percent_TN, y = retention_percent_TP, color = BMPType)) +
  geom_point() +
  scale_color_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  #geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(ylim = c(-200,100), xlim = c(-200,100)) +
  facet_wrap(.~BMPType, nrow = 2, scales = "free") +
  theme_bw(base_size = 12) +
  theme(legend.position = c(0.88,0.2) ) +
  ylab("TP retention (%)") +
  xlab("TN retention (%)") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_abline(slope = 1, intercept = 0) 




### TP, TN, SRP, Nitrate, Ammonium
CC <- select %>% 
  select(c("SiteID", "EventID", "BMPID", "BMPType", "Species", "retention", "retention_percent")) %>%
  filter(Species  %in% c("TP", "PO4", "TN", "NO3", "NH4")) %>%
  distinct() %>%
  pivot_wider(names_from = Species, values_from = c("retention", "retention_percent"))
new <- CC %>% drop_na
## n = 145

CC <- select %>% 
  select(c("SiteID", "EventID", "BMPID", "BMPType", "Species", "retention", "retention_percent")) %>%
  filter(Species  %in% c("TP", "PO4", "TN", "NO3", "NH4", "disTP", "TKN")) %>%
  distinct() %>%
  pivot_wider(names_from = Species, values_from = c("retention", "retention_percent"))
new <- CC %>% drop_na
## n = 24

CC <- select %>% 
  select(c("SiteID", "EventID", "BMPID", "BMPType", "Species", "retention", "retention_percent")) %>%
  filter(Species  %in% c("TP", "PO4", "disTP" )) %>%
  distinct() %>%
  pivot_wider(names_from = Species, values_from = c("retention", "retention_percent"))
new <- CC %>% drop_na
## n = 24


CC <- select %>% 
  select(c("SiteID", "EventID", "BMPID", "BMPType", "Species", "retention", "retention_percent")) %>%
  filter(Species  %in% c("TP", "PO4", "TN", "NH4", "TKN")) %>%
  distinct() %>%
  pivot_wider(names_from = Species, values_from = c("retention", "retention_percent"))
new <- CC %>% drop_na
## n = 148


CC <- select %>% 
  select(c("SiteID", "EventID", "BMPID", "BMPType", "Species", "retention", "retention_percent")) %>%
  filter(Species  %in% c("TN", "NH4", "NO3")) %>%
  distinct() %>%
  pivot_wider(names_from = Species, values_from = c("retention", "retention_percent"))
new <- CC %>% drop_na
## n = 337



CC <- select %>% 
  select(c("SiteID", "EventID", "BMPID", "BMPType", "Species", "retention", "retention_percent")) %>%
  filter(Species  %in% c( "NO3", "NH4")) %>%
  distinct() %>%
  pivot_wider(names_from = Species, values_from = c("retention", "retention_percent"))
new <- CC %>% drop_na
## n = 337

ggplot(new, aes(x = retention_percent_NO3, y = retention_percent_NH4, color = BMPType)) +
  geom_point() +
  scale_color_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  #geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(ylim = c(-200,100), xlim = c(-200,100)) +
  facet_wrap(.~BMPType, nrow = 2, scales = "free") +
  theme_bw(base_size = 12) +
  theme(legend.position = c(0.88,0.2) ) +
  ylab("NH4 retention (%)") +
  xlab("NO3 retention (%)") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_abline(slope = 1, intercept = 0) 







