

### 
# Clean code for working up the clean data and running some comparative analyses

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/BMP_project")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")

library(tidyverse)
library(ggplot2)
library(viridis)
options(scipen=999)

select <- read.csv("BMP_Clean_filtered.csv", stringsAsFactors = TRUE)

### reorder factors
select$BMPType <- factor(select$BMPType , levels=c('DB','RP','WB', "WC",  "BS/BI", "BR"))
select$BMPType_name <- select$BMPType
BMP_names <- c("Detention basin (dry)", "Retention pond", "Wetland basin", "Wetland channel", "Grass strip/swale", "Bioretention")
levels(select$BMPType_name) <- BMP_names
select$Species <- factor(select$Species , levels=c("NH4", "NO3","TKN", "TN" , "TP", "PO4"))


### LOG mass retention
ggplot(select, aes(x = BMPType, y = log(retention), fill = factor(BMPType))) +
  geom_boxplot(trim = TRUE) +
  scale_fill_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  #coord_cartesian(ylim = c(-500,10000)) +
  #ylim(-100,500) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom" ) +
  facet_wrap(.~Species, nrow = 2, scales = "free") +
  theme(axis.text=element_text(size=8)) +
  ylab("log Retention (g/event)") +
  xlab(" ")

### mass retention
ggplot(select, aes(x = BMPType, y = (retention), fill = factor(BMPType))) +
  geom_boxplot(trim = TRUE) +
  scale_fill_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  coord_cartesian(ylim = c(-500,10000)) +
  #ylim(-100,500) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom" ) +
  facet_wrap(.~Species, nrow = 2, scales = "free") +
  theme(axis.text=element_text(size=8)) +
  ylab("Retention (g/event)") +
  xlab(" ")

### mass retention/area 

select$n.ret <- select$retention/select$Area_ha/10000 #retention in g/event/m2
hist(select$n.ret, breaks = 500, xlim =c(-5, 10))
ggplot(select, aes(x = BMPType, y = (n.ret), fill = factor(BMPType))) +
  geom_boxplot(trim = TRUE) +
  scale_fill_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  coord_cartesian(ylim = c(-1,5)) +
  #ylim(-100,500) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom" ) +
  facet_wrap(.~Species, nrow = 2, scales = "free") +
  theme(axis.text=element_text(size=8)) +
  ylab("Retention (g/event/m2)") +
  xlab(" ")

### percent retention
ggplot(select, aes(x = BMPType, y = retention_percent, fill = factor(BMPType))) +
  geom_boxplot(trim = TRUE) +
  scale_fill_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  coord_cartesian(ylim = c(-250,100)) +
  #ylim(-100,500) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom" ) +
  facet_wrap(.~Species, nrow = 2, scales = "free") +
  theme(axis.text=element_text(size=8)) +
  geom_hline(yintercept = 0) +
  ylab("Percent retention") +
  xlab(" ")


## percent retention points jitter
ggplot(select, aes(x = BMPType, y = retention_percent, fill = factor(BMPType))) +
  geom_violin() +
  geom_jitter(color = "black", size = 0.4, alpha = 0.8, width = 0.2)+
  scale_fill_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  coord_cartesian(ylim = c(-250,100)) +
  #ylim(-100,500) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom" ) +
  facet_wrap(.~Species, nrow = 2, scales = "free") +
  theme(axis.text=element_text(size=8)) +
  geom_hline(yintercept = 0) +
  ylab("Percent retention") +
  xlab(" ")

col <- viridis(6)
## percent retention points jitter
ggplot(select, aes(x = BMPType, y = retention_percent, color = factor(BMPType))) +
  geom_violin(color = "black") +
  #scale_fill_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  geom_jitter(size = 1, alpha = 1, width = 0.2)+
  scale_color_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  coord_cartesian(ylim = c(-250,100)) +
  #ylim(-100,500) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom" ) +
  facet_wrap(.~Species, nrow = 2, scales = "free") +
  theme(axis.text=element_text(size=8)) +
  geom_hline(yintercept = 0) +
  ylab("Percent retention") +
  xlab(" ")

### Percent retention no filter
full <- read.csv("BMP_Clean_full.csv", stringsAsFactors = TRUE)

### reorder factors
full$BMPType <- factor(full$BMPType , levels=c('DB','RP','WB', "WC",  "BS/BI", "BR"))
full$BMPType_name <- full$BMPType
BMP_names <- c("Detention basin (dry)", "Retention pond", "Wetland basin", "Wetland channel", "Grass strip/swale", "Bioretention")
levels(full$BMPType_name) <- BMP_names
full$Species <- factor(full$Species , levels=c("NH4", "NO3","TKN", "TN" , "TP", "PO4"))

ggplot(full, aes(x = BMPType, y = retention_percent, fill = factor(BMPType))) +
  geom_boxplot(trim = TRUE) +
  scale_fill_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  coord_cartesian(ylim = c(-250,100)) +
  #ylim(-100,500) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom" ) +
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
  theme(legend.position = "bottom" ) +
  ylab("Percent retention") +
  xlab("BMP Age")





### solute comparisons

##TP vs PO4

### find the complete cases
CC <- select %>% 
  select(c("SiteID", "EventID", "BMPID", "BMPType", "Species", "retention", "retention_percent", "BMPType_name")) %>%
  filter(Species  %in% c("TP", "PO4")) %>%
  distinct() %>%
  pivot_wider(names_from = Species, values_from = c("retention", "retention_percent"))
new <- CC %>% drop_na
##n = 608

plot(new$retention_percent_TP, new$retention_percent_PO4)

ggplot(new, aes(x = retention_percent_TP, y = retention_percent_PO4, color = BMPType_name)) +
  geom_point() +
  scale_color_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  #geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(ylim = c(-200,100), xlim = c(-200,100)) +
  facet_wrap(.~BMPType_name, nrow = 2, scales = "free") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none") +
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







