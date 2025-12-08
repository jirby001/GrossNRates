# File: CorrelationGraphs.R
# Project: Gross N transformation Rates
# Note: This script can be used see correlation between two variables
#       on a line graph
# Current Use: Gross N mineralization Rates vs Total N Concentration

rm(list=ls());setwd("/Users/jam925/GrossNRates"); graphics.off()

library(dplyr)
library(ggplot2)
library(readxl) #allows you to work with data from excel files
library("gridExtra")
library(tidyverse)
library(rstatix)
library(ggpubr)
library(gtsummary)
library("report")

#ranges: all = A1:T47, just winter A1:T24, just summer = A51:T74
d <- read_excel("GrossNRatesData.xlsx", sheet = "Sheet1")
d <- d %>% rename_all(funs(make.names(.)))# Remove spaces in column names
# winter 2023 air temperature day of sampling: 20.883750 C
# summer 2023 air temperature day of sampling: 24.58083 C

d <- d %>% mutate(Air_temp_C = case_when(
  Season == "Winter" ~ 20.883750,
  Season == "Summer" ~ 24.58083,
  TRUE ~ 1,
))


winter <- filter(d, Season %in% c("Winter")) 
summer <- filter(d, Season %in% c("Summer")) 
w_add <- filter(winter, Treatment %in% c("Winter+", "Summer+"))
s_add <- filter(summer, Treatment %in% c("Summer+", "Winter+"))


label <- expression(paste("Winter 2023 Addition plots Gross Nitrification vs ",NH[4]^"+"))
# plot(y ~ x)
plot(Gross.Min ~ Air_temp_C, data = d, 
     main = "GM vs air temp", 
           xlab = "air temp", ylab = "GM",
     pch = ifelse(d$Season == "Winter",19,17),
     col = ifelse(d$Treatment == "Control","#ffe599",
                  ifelse(d$Treatment == "Summer-","#f2a6a6",
                         ifelse(d$Treatment == "Winter-","#b0d2f2",
                                ifelse(d$Treatment == "Summer+","#e87e76","#329bea")))))




model<-lm(Gross.Min ~ `Air_temp_C`, data = d)
r2 <- summary(model)$r.squared
p <- paste(" p = " ,format(summary(model)$coefficients[2, "Pr(>|t|)"], 
                           digits = 3))
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))

abline(model)
text(x = 22, y = 0.8, labels = mylabel)
text(x = 22, y = 0.6, labels = p)

model <- lm(`Gross Nit` ~ `Water content %` + `MB N` + 
              NH4, data = winter) #multiple linear regression

# Add a legend
legend("topright", legend = 
         c("Sampled in Winter 2023", "Sampled in Summer 2023", "Control", "Summer Addition", "Summer Exclusion", "Winter Addition", "Winter Exclusion"), 
       col = c("black", "black", "#ffe599", "#e87e76", "#f2a6a6","#329bea", "#b0d2f2"),
       pch = c(19, 17, 15, 15, 15, 15, 15))

#old way was not creating stepwise model, and just using model for everything else
