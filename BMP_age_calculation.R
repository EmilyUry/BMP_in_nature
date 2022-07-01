


#### Aux data munging

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/BMP_project")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/BMP_project")

library(tidyverse)
library(ggplot2)
library(viridis)
options(scipen=999)

Phos <- read.csv("BMP_P_clean.csv")
Nit <- read.csv("BMP_N_clean.csv")
data <- rbind(Phos, Nit)


bmp <- read.csv("BMPInfo.csv", header = TRUE)  %>%
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
  select(-c('year', 'year0'))


hist(data$BMPAge, breaks = 50, xlim = c(-5, 15))


## BMP age