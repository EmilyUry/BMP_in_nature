

### figure (4) retention of N versus P and other species comparisons


setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")

library(tidyverse)
library(ggplot2)
library(viridis)
library(cowplot)
options(scipen=999)

select <- read.csv("BMP_Clean_filtered.csv", stringsAsFactors = TRUE)


levels(select$BMPType)

### reorder factors
select$BMPType <- factor(select$BMPType , levels=c('DB','RP','WB', "WC",  "BS/BI", "BR"))
levels(select$BMPType) <- c("DB", "RP", "WB", "WC", "GS", "BR")
BMP_names <- c("Detention basin (dry)", "Retention pond", "Wetland basin", "Wetland channel", "Grass strip/swale", "Bioretention")
levels(select$BMPType_name) <- BMP_names
select$Species <- factor(select$Species , levels=c("NH4", "NO3","TKN", "TN" , "TP", "PO4"))


data <- select[which(select$Species != "TKN"),]




### find the complete cases TN vs TP
CC <- data %>% 
  select(c("SiteID", "EventID", "BMPID", "BMPType", "Species", "retention", "retention_percent")) %>%
  filter(Species  %in% c("TN", "TP")) %>%
  distinct() %>%
  pivot_wider(names_from = Species, values_from = c("retention", "retention_percent"))
new <- CC %>% drop_na
## n = 565
labs <- c("DB", "RP", "WB", "WC", "GS", "BR")

scatter <- ggplot(new, aes(x = retention_percent_TN, y = retention_percent_TP, color = BMPType)) +
  geom_point() +
  scale_color_viridis(discrete= TRUE, name = " ", labels = labs, alpha = 0.8) +
  #geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(ylim = c(-100,100), xlim = c(-100,100)) +
  #facet_wrap(.~BMPType, nrow = 2, scales = "free") +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.11,0.8), 
        legend.background = element_rect(fill = "#00000022"), 
        legend.title = element_blank(), 
        legend.text = element_text(
          margin = margin(b = 0.1, unit = "cm"))) +
  ylab("TP retention (%)") +
  xlab("TN retention (%)") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  annotate('text', x = -100, y = 105, label =  "Only TP retained")
scatter

  

ggplot(new, aes(x = retention_percent_TN, y = retention_percent_TP, color = BMPType)) +
  geom_point() +
  scale_color_viridis(discrete= TRUE, name = "BMP Type", labels = BMP_names) +
  #geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(ylim = c(-200,100), xlim = c(-200,100)) +
  facet_wrap(.~BMPType, nrow = 2, scales = "free", labeller = labeller(BMP_names)) +
  theme_bw(base_size = 12) +
  theme(legend.position = c(0.88,0.2) ) +
  ylab("TP retention (%)") +
  xlab("TN retention (%)") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_abline(slope = 1, intercept = 0) 





##### key (inset)

triangle<-data.frame(c(0,100,0),c(0,100,100))
names(triangle) <- c('x', 'y')


triangle2<-data.frame(c(0,100,100),c(0,100,0))
names(triangle2) <- c('x', 'y')

triangle3<-data.frame(c(0,-100,-100),c(0,-100,0))
names(triangle3) <- c('x', 'y')

triangle4<-data.frame(c(0,-100,0),c(0,-100,-100))
names(triangle4) <- c('x', 'y')

inset <- ggplot(new, aes(x = retention_percent_TN, y = retention_percent_TP)) +
  geom_rect(aes(xmin = -100, xmax = 0, ymin = 0, ymax = 100), fill = "#ed7b89") +
  geom_rect(aes(xmin = 0, xmax = 100, ymin = -100, ymax = 0), fill = "#7482ec") +
  geom_polygon(data = triangle3, aes( x = x, y = y), fill = "#b2182bee") +  
  geom_polygon(data = triangle4, aes( x = x, y = y), fill = "#1c31ceee") +
  geom_polygon(data = triangle, aes( x = x, y = y), fill = "#fadade") +  
  geom_polygon(data = triangle2, aes( x = x, y = y), fill = "#ccd1f8") +
  geom_point(size = 0.4) +
  #scale_color_viridis(discrete= TRUE, name = " ", labels = labs, alpha = 0.8) +
  #geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(ylim = c(-115,115), xlim = c(-100,115)) +
  #facet_wrap(.~BMPType, nrow = 2, scales = "free") +
  theme_classic(base_size = 10) +
  ylab("TP retention (%)") +
  xlab("TN retention (%)") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  theme(plot.margin = margin(t=0.5,r = 0.3, b = 0.3, l = 0.3, unit = "cm")) #+
  # annotate('text', x = -68, y = 113, label =  "Only TP retained", fontface = "bold", size = 2) +
  # annotate('text', x = 52, y = 113, label =  "TP preferentially retained", fontface = "bold", size = 2) +
  # annotate('text', x = 115, y = 50, label =  "TN preferentially \n retained", fontface = "bold", angle = 270, size = 2) +
  # annotate('text', x = 107, y = -60, label =  "Only TN retained", fontface = "bold", angle = 270, size = 2) +
  # annotate('text', x = -60, y = -110, label =  "TN and TP released", fontface = "bold", size = 2) 
  # 
  # 



  
inset


#### TP vs TP

### find the complete cases TN vs TP
CC <- data %>% 
  select(c("SiteID", "EventID", "BMPID", "BMPType", "Species", "retention", "retention_percent")) %>%
  filter(Species  %in% c("TN", "TP")) %>%
  distinct() %>%
  pivot_wider(names_from = Species, values_from = c("retention", "retention_percent"))
new <- CC %>% drop_na
new <- new %>%
  mutate(quadrant = case_when(retention_percent_TN >= 0 & retention_percent_TP >= 0 
                            & retention_percent_TN >= retention_percent_TP        ~ "aQuad 1under",
                            retention_percent_TN >= 0 & retention_percent_TP >= 0 
                            & retention_percent_TN < retention_percent_TP        ~ "fQuad 1over",                              
                            retention_percent_TN <= 0 & retention_percent_TP >= 0~ "eQuad 2",
                            retention_percent_TN <= 0 & retention_percent_TP <= 0 
                            & retention_percent_TN >= retention_percent_TP        ~ "cQuad 3under",
                            retention_percent_TN <= 0 & retention_percent_TP <= 0 
                            & retention_percent_TN < retention_percent_TP        ~ "dQuad 3over",
                            retention_percent_TN >= 0 & retention_percent_TP <= 0 ~ "bQuad 4",
                            retention_percent_TN == retention_percent_TP ~ 'SAME'))

TN.TP <- ggplot(new, aes(x = BMPType, fill = quadrant)) +
  geom_bar(width = 0.75, position = 'fill',color = 'black') +
  theme_classic(base_size = 10) +
  scale_fill_manual(breaks = c("fQuad 1over", "aQuad 1under", "eQuad 2",
                               "dQuad 3over", "cQuad 3under", "bQuad 4"), 
                    values=c("#ccd1f8", "#fadade", "#7482ec",
                             '#1c31ceee', '#b2182bee', '#ed7b89'))+
  xlab(" ")+
  ylab("fraction")+
    theme(legend.position = 'none', 
        plot.margin = margin(t=0.5,r = 0.3, b = 0.3, l = 0.3, unit = "cm"))
TN.TP




#### PO4 vs TP

### find the complete cases TN vs TP
CC <- data %>% 
  select(c("SiteID", "EventID", "BMPID", "BMPType", "Species", "retention", "retention_percent")) %>%
  filter(Species  %in% c("PO4", "TP")) %>%
  distinct() %>%
  pivot_wider(names_from = Species, values_from = c("retention", "retention_percent"))
new2 <- CC %>% drop_na
new2 <- new2 %>%
  mutate(quadrant = case_when(retention_percent_PO4 >= 0 & retention_percent_TP >= 0 
                              & retention_percent_PO4 >= retention_percent_TP        ~ "aQuad 1under",
                              retention_percent_PO4 >= 0 & retention_percent_TP >= 0 
                              & retention_percent_PO4 < retention_percent_TP        ~ "fQuad 1over",                              
                              retention_percent_PO4 <= 0 & retention_percent_TP >= 0~ "eQuad 2",
                              retention_percent_PO4 <= 0 & retention_percent_TP <= 0 
                              & retention_percent_PO4 >= retention_percent_TP        ~ "cQuad 3under",
                              retention_percent_PO4 <= 0 & retention_percent_TP <= 0 
                              & retention_percent_PO4 < retention_percent_TP        ~ "dQuad 3over",
                              retention_percent_PO4 >= 0 & retention_percent_TP <= 0 ~ "bQuad 4",
                              retention_percent_PO4 == retention_percent_TP ~ 'SAME'))

PO4.TP <- ggplot(new2, aes(x = BMPType, fill = quadrant)) +
  geom_bar(width = 0.75, position = 'fill',color = 'black') +
  theme_classic(base_size = 10) +
  scale_fill_manual(breaks = c("fQuad 1over", "aQuad 1under", "eQuad 2",
                               "dQuad 3over", "cQuad 3under", "bQuad 4"), 
                    values=c("#ccd1f8", "#fadade", "#7482ec",
                             '#1c31ceee', '#b2182bee', '#ed7b89'))+
  xlab(" ")+
  ylab("fraction")+
  theme(legend.position = 'none', 
        plot.margin = margin(t=0.5,r = 0.3, b = 0.3, l = 0.3, unit = "cm"))


###### NH4 NO3

CC <- data %>% 
  select(c("SiteID", "EventID", "BMPID", "BMPType", "Species", "retention", "retention_percent")) %>%
  filter(Species  %in% c("NH4", "NO3")) %>%
  distinct() %>%
  pivot_wider(names_from = Species, values_from = c("retention", "retention_percent"))
new3 <- CC %>% drop_na
new3 <- new3 %>%
  mutate(quadrant = case_when(retention_percent_NO3 >= 0 & retention_percent_NH4 >= 0 
                              & retention_percent_NO3 >= retention_percent_NH4        ~ "aQuad 1under",
                              retention_percent_NO3 >= 0 & retention_percent_NH4 >= 0 
                              & retention_percent_NO3 < retention_percent_NH4        ~ "fQuad 1over",                              
                              retention_percent_NO3 <= 0 & retention_percent_NH4 >= 0~ "eQuad 2",
                              retention_percent_NO3 <= 0 & retention_percent_NH4 <= 0 
                              & retention_percent_NO3 >= retention_percent_NH4        ~ "cQuad 3under",
                              retention_percent_NO3 <= 0 & retention_percent_NH4 <= 0 
                              & retention_percent_NO3 < retention_percent_NH4        ~ "dQuad 3over",
                              retention_percent_NO3 >= 0 & retention_percent_NH4 <= 0 ~ "bQuad 4",
                              retention_percent_NO3 == retention_percent_NH4 ~ 'SAME'))

NO3.NH4 <- ggplot(new3, aes(x = BMPType, fill = quadrant)) +
  geom_bar(width = 0.75, position = 'fill',color = 'black') +
  theme_classic(base_size = 10) +
  scale_fill_manual(breaks = c("fQuad 1over", "aQuad 1under", "eQuad 2",
                               "dQuad 3over", "cQuad 3under", "bQuad 4"), 
                    values=c("#ccd1f8", "#fadade", "#7482ec",
                             '#1c31ceee', '#b2182bee', '#ed7b89'))+
  xlab(" ")+ 
  ylab("fraction") +
  theme(legend.position = 'none', 
        plot.margin = margin(t=0.5,r = 0.3, b = 0.3, l = 0.3, unit = "cm"))





plot_grid(inset, TN.TP, PO4.TP, NO3.NH4,
          labels = c("A) ","B) TN vs TP retention", "C) PO4 vs TP retention", 
                     "D) NO3 vs NH4 retention"),
          ncol = 2, rel_heights = c(1,1), label_size = 10)


tiff(filename = "figures/Solute_retention2.tif", height=4, width=6, units= "in", res=800, compression= "lzw")

plot_grid(inset, TN.TP, PO4.TP, NO3.NH4,
          labels = c("A) ","B) TN vs TP retention", "C) PO4 vs TP retention", 
                     "D) NO3 vs NH4 retention"),
          ncol = 2, rel_heights = c(1,1), label_size = 10)

dev.off()





#### stats


nrow(new$retention_percent_TP/new$retention_percent_TN > 1)
nrow(new)


x <- new[which(new$retention_percent_TP > new$retention_percent_TN),]
nrow(x)/nrow(new)





### supplement 1  NO3 vs TN


CC <- data %>% 
  select(c("SiteID", "EventID", "BMPID", "BMPType", "Species", "retention", "retention_percent")) %>%
  filter(Species  %in% c("TN", "NO3")) %>%
  distinct() %>%
  pivot_wider(names_from = Species, values_from = c("retention", "retention_percent"))
new5 <- CC %>% drop_na
new5 <- new5 %>%
  mutate(quadrant = case_when(retention_percent_NO3 >= 0 & retention_percent_TN >= 0 
                              & retention_percent_NO3 >= retention_percent_TN       ~ "aQuad 1under",
                              retention_percent_NO3 >= 0 & retention_percent_TN >= 0 
                              & retention_percent_NO3 < retention_percent_TN        ~ "fQuad 1over",                              
                              retention_percent_NO3 <= 0 & retention_percent_TN >= 0~ "eQuad 2",
                              retention_percent_NO3 <= 0 & retention_percent_TN <= 0 
                              & retention_percent_NO3 >= retention_percent_TN        ~ "cQuad 3under",
                              retention_percent_NO3 <= 0 & retention_percent_TN <= 0 
                              & retention_percent_NO3 < retention_percent_TN        ~ "dQuad 3over",
                              retention_percent_NO3 >= 0 & retention_percent_TN <= 0 ~ "bQuad 4",
                              retention_percent_NO3 == retention_percent_TN ~ 'SAME'))

TN.NO3 <- ggplot(new5, aes(x = BMPType, fill = quadrant)) +
  geom_bar(width = 0.75, position = 'fill',color = 'black') +
  theme_classic(base_size = 10) +
  scale_fill_manual(breaks = c("fQuad 1over", "aQuad 1under", "eQuad 2",
                               "dQuad 3over", "cQuad 3under", "bQuad 4"), 
                    values=c("#ccd1f8", "#fadade", "#7482ec",
                             '#1c31ceee', '#b2182bee', '#ed7b89'))+
  xlab(" ")+ 
  ylab("fraction") +
  theme(legend.position = 'none', 
        plot.margin = margin(t=0.5,r = 0.3, b = 0.3, l = 0.3, unit = "cm"))





CC <- data %>% 
  select(c("SiteID", "EventID", "BMPID", "BMPType", "Species", "retention", "retention_percent")) %>%
  filter(Species  %in% c("TN", "NH4")) %>%
  distinct() %>%
  pivot_wider(names_from = Species, values_from = c("retention", "retention_percent"))
new6 <- CC %>% drop_na
new6 <- new6 %>%
  mutate(quadrant = case_when(retention_percent_NH4 >= 0 & retention_percent_TN >= 0 
                              & retention_percent_NH4 >= retention_percent_TN       ~ "aQuad 1under",
                              retention_percent_NH4 >= 0 & retention_percent_TN >= 0 
                              & retention_percent_NH4 < retention_percent_TN        ~ "fQuad 1over",                              
                              retention_percent_NH4 <= 0 & retention_percent_TN >= 0~ "eQuad 2",
                              retention_percent_NH4 <= 0 & retention_percent_TN <= 0 
                              & retention_percent_NH4 >= retention_percent_TN        ~ "cQuad 3under",
                              retention_percent_NH4 <= 0 & retention_percent_TN <= 0 
                              & retention_percent_NH4 < retention_percent_TN        ~ "dQuad 3over",
                              retention_percent_NH4 >= 0 & retention_percent_TN <= 0 ~ "bQuad 4",
                              retention_percent_NH4 == retention_percent_TN ~ 'SAME'))

TN.NH4 <- ggplot(new6, aes(x = BMPType, fill = quadrant)) +
  geom_bar(width = 0.75, position = 'fill',color = 'black') +
  theme_classic(base_size = 10) +
  scale_fill_manual(breaks = c("fQuad 1over", "aQuad 1under", "eQuad 2",
                               "dQuad 3over", "cQuad 3under", "bQuad 4"), 
                    values=c("#ccd1f8", "#fadade", "#7482ec",
                             '#1c31ceee', '#b2182bee', '#ed7b89'))+
  xlab(" ")+ 
  ylab("fraction") +
  theme(legend.position = 'none', 
        plot.margin = margin(t=0.5,r = 0.3, b = 0.3, l = 0.3, unit = "cm"))









tiff(filename = "figures/Solute_retention_supp.tif", height=4, width=6, units= "in", res=800, compression= "lzw")

plot_grid(inset, TN.TP, PO4.TP, NO3.NH4, TN.NO3, TN.NH4,
          labels = c("A) ","B) TN vs TP", "C) TP vs PO4 ", 
                     "D) NH4 vs NO3" , "E) TN vs NO3", "F) TN vs NH4 "),
          ncol = 3, rel_heights = c(1,1), label_size = 10, label_x = 0, hjust = 0)

dev.off()






plot(new5$retention_percent_TN, new5$retention_percent_NO3, ylim = c(-100,100), xlim = c(-100,100))
abline(1,0)
abline(0,1)

plot(new6$retention_percent_TN, new6$retention_percent_NH4, ylim = c(-100,100), xlim = c(-100,100))
abline(1,0)
abline(0,1)






#### Supplement2 Unscaled versions


TN.TP <- ggplot(new, aes(x = BMPType, fill = quadrant)) +
  geom_bar(width = 0.75,color = 'black') +
  theme_classic(base_size = 10) +
  scale_fill_manual(breaks = c("Quad 1over", "Quad 1under", "Quad 2",
                               "Quad 3over", "Quad 3under", "Quad 4"), 
                    values=c("#CCF1F6", "#8AD6E0", "#E2D1F1",
                             '#FDDAE4', '#D4839B', '#FCEFBB'))+
  xlab(" ")+
  ylab("fraction")+
  theme(legend.position = 'none', 
        plot.margin = margin(t=0.5,r = 0.3, b = 0.3, l = 0.3, unit = "cm"))

PO4.TP <- ggplot(new2, aes(x = BMPType, fill = quadrant)) +
  geom_bar(width = 0.75,color = 'black') +
  theme_classic(base_size = 10) +
  scale_fill_manual(breaks = c("Quad 1over", "Quad 1under", "Quad 2",
                               "Quad 3over", "Quad 3under", "Quad 4"), 
                    values=c("#CCF1F6", "#8AD6E0", "#E2D1F1",
                             '#FDDAE4', '#D4839B', '#FCEFBB'))+
  xlab(" ")+
  ylab("fraction")+
  theme(legend.position = 'none', 
        plot.margin = margin(t=0.5,r = 0.3, b = 0.3, l = 0.3, unit = "cm"))


NO3.NH4 <- ggplot(new3, aes(x = BMPType, fill = quadrant)) +
  geom_bar(width = 0.75,color = 'black') +
  theme_classic(base_size = 10) +
  scale_fill_manual(breaks = c("Quad 1over", "Quad 1under", "Quad 2",
                               "Quad 3over", "Quad 3under", "Quad 4"), 
                    values=c("#CCF1F6", "#8AD6E0", "#E2D1F1",
                             '#FDDAE4', '#D4839B', '#FCEFBB'))+
  xlab(" ")+ 
  ylab("fraction") +
  theme(legend.position = 'none', 
        plot.margin = margin(t=0.5,r = 0.3, b = 0.3, l = 0.3, unit = "cm"))




plot_grid(inset, TN.TP, PO4.TP, NO3.NH4,
          labels = c(" ","TN vs TP retention", "PO4 vs TP retention", 
                     "NO3 vs NH4 retention"),
          ncol = 2, rel_heights = c(1,1), label_size = 10)


