



### figure (2) retention by BMP type


setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")

library(tidyverse)
library(ggplot2)
library(viridis)
options(scipen=999)

## order = BR GS DB RP WB WC
pal <- c("#d6e2c7", "#94ac8b", "#fff3c8",  "#d7c5e6", "#c1c9e5", "#8f94c8" ) 


select <- read.csv("BMP_Clean_filtered.csv", stringsAsFactors = TRUE)


levels(select$BMPType)

### reorder factors

select$BMPType <- factor(select$BMPType , levels=c('BR','BS/BI','DB', "RP",  "WB", "WC"))
levels(select$BMPType) <- c('BR','GS','DB', "RP",  "WB", "WC")
BMP_names <- c("Bioretention","Grass strip/swale", "Detention basin (dry)", "Retention pond", "Wetland basin", "Wetland channel")
levels(select$BMPType_name) <- BMP_names
select$Species <- factor(select$Species , levels=c("TN","NH4", "NO3","TKN", "TP", "PO4"))



data <- select[which(select$Species != "TKN"),]

summary <- data %>%
  group_by(Species, BMPType) %>%
  summarise(count = n(), 
            retention_percent = median(retention)) %>%
  mutate(retention_percent = -200)


### percent retention
figure <- ggplot(data, aes(x = BMPType, y = retention_percent, fill = factor(BMPType))) +
  #geom_jitter(color = "black", size = 0.4, alpha = 0.8, width = 0.2)+
  geom_boxplot(coef=1.5, outlier.shape = NA) +
  scale_fill_manual(values = pal,  name = " ", labels = BMP_names) +
  coord_cartesian(ylim = c(-210,110)) +
  theme_bw(base_size = 10) +
  theme(legend.position = c(0.83,0.25), legend.key.size = unit(0.5, 'cm'),
        legend.text=element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(.~Species, nrow = 2, scales = "free") +
  theme(axis.text=element_text(size=8)) +
  geom_hline(yintercept = 0, size = .1) +
  ylab("Percent retention") +
  xlab(" ") +
  geom_text(data = summary, aes(label = count), 
            position = position_dodge(width = 1.0), size = 2.5, color = "gray50")

figure




tiff(filename = "figures/Retention_BMPType.tif", height=4, width=6, units= "in", res=800, compression= "lzw")

figure

dev.off()




### SUpplemental figure 2
summary <- data %>%
  group_by(Species, BMPType) %>%
  summarise(count = n(), 
            retention = median(retention)) %>%
  mutate(retention = -300)

figure <- ggplot(data, aes(x = BMPType, y = retention/1000, fill = factor(BMPType))) +
  #geom_jitter(color = "black", size = 0.4, alpha = 0.8, width = 0.2)+
  geom_boxplot(coef=1.5, outlier.shape = NA) +
  scale_fill_manual(values = pal,  name = " ", labels = BMP_names) +
  ylim(-1,3) + # chose not to use coorcartesian here, because in this case the outliers are ridiculous
  theme_bw(base_size = 10) +
  theme(legend.position = c(0.83,0.25), legend.key.size = unit(0.5, 'cm'),
        legend.text=element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(.~Species, nrow = 2, scales = "free") +
  theme(axis.text=element_text(size=8)) +
  geom_hline(yintercept = 0, size = .1) +
  ylab("retention (kg/event)") +
  xlab(" ") +
  geom_text(data = summary, aes(y = -0.8, label = count), 
            position = position_dodge(width = 1.0), size = 2.5, color = "gray40")

figure




tiff(filename = "figures/Retention_BMPType_supplement.tif", height=4, width=6, units= "in", res=800, compression= "lzw")

figure

dev.off()






### SUpplemental figure 3 (mass per area removed) g/m2
summary <- data %>%
  group_by(Species, BMPType) %>%
  summarise(count = n(), 
            retention = median(retention/Area_ha/10000, na.rm =TRUE)) 
Species <- summary$Species
BMPType <- summary$BMPType

summary <- data %>%
  filter(!is.na(Area_ha) ) %>%
  group_by(Species, BMPType) %>%
  summarise(count = n(), 
            retention = median(retention/Area_ha/10000)) 


count <- c(0, summary$count[1:5], 0, summary$count[6], 0, summary$count[7:9], 0, summary$count[10:14],
               0, summary$count[15:19], 0, summary$count[20:24])
retention <- rep(1,30)
summary <- data.frame(Species, BMPType, count, retention)


pal5 <- c("#d6e2c7", "#fff3c8", "#94ac8b", "#d7c5e6", "#c1c9e5", "#8f94c8" ) 

figure <- ggplot(data, aes(x = BMPType, y = retention/Area_ha/10000, fill = factor(BMPType))) +
  #geom_jitter(color = "black", size = 0.4, alpha = 0.8, width = 0.2)+
  geom_boxplot(coef=1.5, outlier.shape = NA) +
  scale_fill_manual(values = pal5,  name = " ", labels = BMP_names) +
  ylim(-0.5,2) + # chose not to use coorcartesian here, because in this case the outliers are ridiculous
  theme_bw(base_size = 10) +
  theme(legend.position = c(0.83,0.25), legend.key.size = unit(0.5, 'cm'),
        legend.text=element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(.~Species, nrow = 2, scales = "free") +
  theme(axis.text=element_text(size=8)) +
  geom_hline(yintercept = 0, size = .1) +
  ylab("retention (g/m2)") +
  xlab(" ") +
  geom_text(data = summary, aes(y = -0.4, label = count), 
            position = position_dodge(width = 1.0), size = 2.5, color = "gray40") +
  guides(fill = guide_legend(override.aes = list(fill = pal)))

figure




tiff(filename = "figures/Retention_BMPType_supplement2.tif", height=4, width=6, units= "in", res=800, compression= "lzw")

figure

dev.off()



summary <- data %>%
  group_by(Species) %>%
  summarise(mean = mean(retention_percent, na.rm = TRUE), 
            median_percent = median(retention_percent, na.rm = TRUE), 
            median_mass = median(retention, na.rm = TRUE),
            count = n())
summary




summary <- data %>%
  group_by(BMPType) %>%
  summarise(mean = mean(retention_percent, na.rm = TRUE), 
            median_percent = median(retention_percent, na.rm = TRUE), 
            median_mass = median(retention, na.rm = TRUE), 
            count = n())
summary


summary <- data %>%
  group_by(BMPType, Species) %>%
  summarise(count = n())
summary


library(rstatix)
library(ggpubr)
library(stringr)

data %>%
  group_by(BMPType,Species) %>%
  get_summary_stats(retention_percent, type = "median_mad")

data$Species_BMPType <-paste(data$Species, data$BMPType)
pwc <- data %>%
  pairwise_t_test(retention_percent ~  Species_BMPType, p.adjust.method = 'bonferroni')

test <- pwc %>%
  filter(str_starts(group1, "NH") & str_starts(group2, "NH") )










### anova and TUkey
library(multcompView)

TN <- data %>%
  filter(Species == "TN")

anova <- aov(retention_percent ~ BMPType, data = TN)
summary(anova)
TukeyHSD(anova)
tukey <- TukeyHSD(anova) 
cld <- multcompLetters4(anova, tukey)


NH4 <- data %>%
  filter(Species == "NH4")

anova <- aov(retention_percent ~ BMPType, data = NH4)
summary(anova)
TukeyHSD(anova)
tukey <- TukeyHSD(anova) 
cld <- multcompLetters4(anova, tukey)
cld


NO3 <- data %>%
  filter(Species == "NO3")

anova <- aov(retention_percent ~ BMPType, data = NO3)
summary(anova)
TukeyHSD(anova)
tukey <- TukeyHSD(anova) 
cld <- multcompLetters4(anova, tukey)
cld


TP <- data %>%
  filter(Species == "TP")

anova <- aov(retention_percent ~ BMPType, data = TP)
summary(anova)
TukeyHSD(anova)
tukey <- TukeyHSD(anova) 
cld <- multcompLetters4(anova, tukey)
cld


PO4 <- data %>%
  filter(Species == "PO4")

anova <- aov(retention_percent ~ BMPType, data = PO4)
summary(anova)
TukeyHSD(anova)
tukey <- TukeyHSD(anova) 
cld <- multcompLetters4(anova, tukey)
cld









