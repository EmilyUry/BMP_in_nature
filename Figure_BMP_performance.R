



### figure (2) retention by BMP type


setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")

library(tidyverse)
library(ggplot2)
options(scipen=999)

## order = BR GS DB RP WB WC
pal <- c("#d6e2c7", "#94ac8b", "#fff3c8",  "#d7c5e6", "#c1c9e5", "#8f94c8" ) 


select <- read.csv("BMP_Clean_filtered.csv", stringsAsFactors = TRUE)


levels(select$BMPType)

### reorder factors

select$BMPType <- factor(select$BMPType , levels=c('BR','BS/BI','DB', "RP",  "WB", "WC"))
levels(select$BMPType) <- c('BR','GS','DB', "RP",  "WB", "WC")
#BMP_names <- c("Bioretention","Grass strip/swale", "Detention basin (dry)", "Retention pond", "Wetland basin", "Wetland channel")
#levels(select$BMPType_name) <- BMP_names
select$Species <- factor(select$Species , levels=c("TN","NH4", "NO3","TKN", "TP", "PO4"))



data <- select[which(select$Species != "TKN"),]

length(unique(data$BMPID))
test <- data
test$BMP_event <- paste(test$BMPID, test$EventID, sep = "")
length(unique(test$BMP_event))


############## Table 1 #################################################

summaryn <- data %>%
  group_by(BMPType, BMPID) %>%
  summarise(count = n())

summary0 <- data %>%
  summarise(count = n(),
            median_retention = median(retention, na.rm = TRUE),
            IQ25 = quantile(retention, 0.25, na.rm = TRUE),
            IQ75 = quantile(retention, 0.75, na.rm = TRUE),
            median_retention_percent = median(retention_percent, na.rm = TRUE), 
            IQp25 = quantile(retention_percent, 0.25, na.rm = TRUE),
            IQp75 = quantile(retention_percent, 0.75, na.rm = TRUE))

summary1 <- data %>%
  group_by(Species) %>%
  summarise(count = n(),
            median_retention = median(retention, na.rm = TRUE),
            IQ25 = quantile(retention, 0.25, na.rm = TRUE),
            IQ75 = quantile(retention, 0.75, na.rm = TRUE),
            median_retention_percent = median(retention_percent, na.rm = TRUE), 
            IQp25 = quantile(retention_percent, 0.25, na.rm = TRUE),
            IQp75 = quantile(retention_percent, 0.75, na.rm = TRUE))

summary2 <- data %>%
  group_by(BMPType) %>%
  summarise(count = n(),
            median_retention = median(retention, na.rm = TRUE),
            IQ25 = quantile(retention, 0.25, na.rm = TRUE),
            IQ75 = quantile(retention, 0.75, na.rm = TRUE),
            median_retention_percent = median(retention_percent, na.rm = TRUE), 
            IQp25 = quantile(retention_percent, 0.25, na.rm = TRUE),
            IQp75 = quantile(retention_percent, 0.75, na.rm = TRUE))


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
            position = position_dodge(width = 1.0), size = 2.5, color = "gray30")

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
  coord_cartesian(ylim = c(-1,3)) +
  #ylim(-1,3) + # chose not to use coorcartesian here, because in this case the outliers are ridiculous
  theme_bw(base_size = 10) +
  theme(legend.position = c(0.84,0.25), legend.key.size = unit(0.5, 'cm'),
        legend.text=element_text(size=10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(.~Species, nrow = 2, scales = "free") +
  theme(axis.text=element_text(size=8)) +
  geom_hline(yintercept = 0, size = .1) +
  ylab("Retention (kg/event)") +
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

count <- c(0, 29, 21, 81, 45, 22, 0, 17, 0, 50, 48, 0,
           0, 17, 30, 65, 45, 0, 0, 107, 123, 134, 62, 22, 
           0, 77, 7, 91, 15, 0)
# count <- c(0, summary$count[1:5], 0, summary$count[6], 0, summary$count[7:9], 0, summary$count[10:14],
#                0, summary$count[15:19], 0, summary$count[20:24])
retention <- rep(1,30)
summary <- data.frame(Species, BMPType, count, retention)


pal5 <- c("#d6e2c7", "#fff3c8", "#94ac8b", "#d7c5e6", "#c1c9e5", "#8f94c8" ) 

figure <- ggplot(data, aes(x = BMPType, y = retention/Area_ha/10000, fill = factor(BMPType))) +
  #geom_jitter(color = "black", size = 0.4, alpha = 0.8, width = 0.2)+
  geom_boxplot(coef=1.5, outlier.shape = NA) +
  scale_fill_manual(values = pal5,  name = " ", labels = BMP_names) +
  coord_cartesian(ylim = c(0,3)) +
  #ylim(-0.5,1.1) + # chose not to use coorcartesian here, because in this case the outliers are ridiculous
  theme_bw(base_size = 10) +
  theme(legend.position = c(0.83,0.25), legend.key.size = unit(0.5, 'cm'),
        legend.text=element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(.~Species, nrow = 2, scales = "free") +
  theme(axis.text=element_text(size=8)) +
  geom_hline(yintercept = 0, size = .1) +
  ylab("Retention (g/m2)") +
  xlab(" ") +
  geom_text(data = summary, aes(y = -0.4, label = count), 
            position = position_dodge(width = 1.0), size = 2.5, color = "gray40") +
  guides(fill = guide_legend(override.aes = list(fill = pal)))

figure




tiff(filename = "figures/Retention_BMPType_supplement2.tif", height=4, width=6, units= "in", res=800, compression= "lzw")

figure

dev.off()







####### Figure -- source/sink behavior



summary <- data %>%
  group_by(Species, BMPType) %>%
  summarise(count = n(), 
            retention_mass = median(retention)) %>%
  mutate(retention_mass = -200)


data$source.sink <- ifelse(data$retention >= 0, "sink", "source")
summary <- data %>%
  group_by(Species, BMPType, source.sink) %>%
  summarize(Retention = median(retention),
            count = n()) 
summary$position = c(rep(c(3, -1), 6), rep(c(0.8, -0.5), 6), rep(c(1.5, -1.5), 6),
                     rep(c(0.4,-0.3), 6), rep(c(0.15, -0.1), 6))

label <- summary
label$Retention <- label$position

figure <- ggplot(summary, aes(x = BMPType, y = Retention/1000, fill = factor(BMPType))) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = pal,  name = " ", labels = BMP_names) +
    theme_bw(base_size = 10) +
  coord_cartesian() +
  theme(legend.position = c(0.85,0.25), legend.key.size = unit(0.5, 'cm'),
        legend.text=element_text(size=10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(.~Species, nrow = 2, scales = "free") +
  ylab("Retention (kg/event)") +
  xlab(" ") +
  geom_hline(yintercept = 0) +
  geom_text(data = label, aes(BMPType, Retention, label = count), 
            position = position_dodge(width = 1.0), 
            size = 2.5, color = "gray30")

figure

tiff(filename = "figures/Retention_BMPType_mass_source_sink.tif", height=4, width=6, units= "in", res=800, compression= "lzw")

figure

dev.off()











### metrics

data.mod <- data
#data.mod$retention_percent <- ifelse(data.mod$retention_percent < -250, -250, data.mod$retention_percent)

summary <- data %>%
  group_by(Species) %>%
  summarise(mean = mean(retention_percent, na.rm = TRUE), 
            sd  = sd(retention_percent, na.rm = TRUE),
            median_percent = median(retention_percent, na.rm = TRUE), 
            min_percent = min(retention_percent, na.rm = TRUE),
            max_percent = max(retention_percent, na.rm = TRUE),
            Q75 = quantile(retention_percent, 0.75, na.rm = TRUE),
            Q25 = quantile(retention_percent, 0.25, na.rm = TRUE),
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
print(summary, n = 30)


summary <- data %>%
  group_by(BMPType, Species) %>%
  summarise(mass = round(median(retention, na.rm = TRUE), 1),
            Q25 = round(quantile(retention, 0.25, na.rm = TRUE), 0),
            Q75 = round(quantile(retention, 0.75, na.rm = TRUE), 0))
print(summary, n = 30)


summary <- data %>%
  group_by(BMPType, Species) %>%
  summarise(percent = round(median(retention_percent, na.rm = TRUE), 0),
            Q25 = round(quantile(retention_percent, 0.25, na.rm = TRUE), 0),
            Q75 = round(quantile(retention_percent, 0.75, na.rm = TRUE), 0))
print(summary, n = 30)


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









