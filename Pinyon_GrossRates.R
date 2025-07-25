# File: MultiGraphComparisons.R
# Project: Gross N transformations
# Note: Adapted from old S22 v W23 N Cocnentrations file
#   Used to generate and compare measurements/graphs next to each other
#       

rm(list=ls());graphics.off()

#######Load Libraries####
library(here)
library(dplyr)
library(ggplot2)
library(readxl) #allows you to work with data from excel files
library("gridExtra")
library(tidyverse)
library(rstatix)
library(ggpubr)
library(gtsummary)
library(car)
library("report")
library("olsrr") #test the model for normality
library(nlme)
library(lme4)
library(plyr)
library(patchwork)
library(multcomp)
library(DescTools)
library(lsmeans)
library(pracma)

#######Obtain Data######

d <- read_excel("GrossNRatesData.xlsx", sheet = "Sheet1")
d <- d %>% rename_all(funs(make.names(.)))# Remove spaces in column names

winter <- filter(d, Season %in% c("Winter")) #winter season only
summer <- filter(d, Season %in% c("Summer")) #summer season only


#######Comparison Graphs ###########

winter_summary <- winter %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    w_mean = mean(as.numeric(ratio)),
    w_se = sd(as.numeric(ratio)) / sqrt(n())
  )

summer_summary <- summer %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    s_mean = mean(as.numeric(ratio)),
    s_se = sd(as.numeric(ratio)) / sqrt(n())
  )



#expression(~paste(mu, "g N", " g soil"^-1))
(plot1 <- ggplot(winter, aes(x = Treatment, y = as.numeric(ratio), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75, binwidth = 1)+
    geom_errorbar(data = winter_summary, aes(y = w_mean, ymin = w_mean - w_se, ymax = w_mean + w_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    labs(y=expression(~paste("Ratio of Gross N Mineralization : Gross Nitrification")))  + 
    xlab("") +
    scale_y_continuous(limits = c(0, 60))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 60, label = "Winter", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 60, label = "A)",size = 4, fontface=2)+ 
    annotate("text", x = 1.25, y = 2, label = "a",size = 3.5)+ 
    annotate("text", x = 2.25, y = 2, label = "a",size = 3.5)+ 
    annotate("text", x = 3.25, y = 2, label = "a",size = 3.5)+ 
    annotate("text", x = 4.25, y = 8, label = "b",size = 3.5)+ 
    annotate("text", x = 5.25, y = 2, label = "a",size = 3.5)+ 
    scale_x_discrete(labels=c("Control" ,"Summer-","Summer+","Winter-","Winter+"))+
    theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
          panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
          panel.grid.major=element_blank(),
          legend.position = "none",
          legend.key = element_rect(color = NA),
          text = element_text(size=8),
          axis.title.x = element_text(size=8), #size of x-axis title
          axis.title.y = element_text(size=10), #size of y-axis title
          axis.text.x = element_text(size = 12, angle = 25, hjust = 1),
          axis.text.y = element_text(size=12)))


(plot2 <- ggplot(summer, aes(x = Treatment, y = as.numeric(ratio), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75, binwidth = 1)+
    geom_errorbar(data = summer_summary, aes(y = s_mean, ymin = s_mean - s_se, ymax = s_mean + s_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    #labs(y=expression(~paste(mu, "g N", "gdw"^-1)))  + 
    ylab("")+
    xlab("") +
    scale_y_continuous(limits = c(0, 75))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 75, label = "Summer", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 75, label = "B)",size = 4, fontface=2)+ 
    annotate("text", x = 1.3, y = 9, label = "ab",size = 4)+ 
    annotate("text", x = 2.25, y = 35, label = "b",size = 4)+ 
    annotate("text", x = 3.3, y = 9, label = "ab",size = 4)+ 
    annotate("text", x = 4.3, y = 7, label = "ab",size = 4)+ 
    annotate("text", x = 5.3, y = 8, label = "ab",size = 4)+ 
    scale_x_discrete(labels=c("Control" ,"Summer-","Summer+","Winter-","Winter+"))+
    theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
          panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
          panel.grid.major=element_blank(),
          legend.position = "none",
          legend.key = element_rect(color = NA),
          text = element_text(size=8),
          axis.title.x = element_text(size=8), #size of x-axis title
          axis.title.y = element_text(size=12), #size of y-axis title
          axis.text.x = element_text(size = 12, angle = 25, hjust = 1),
          axis.text.y = element_text(size=12)))

combined <- ggarrange(plot1, plot2, ncol = 2, nrow = 1)
combined
#export ratio: 750 to 600

#### Correlation Graphs ####

d <- read_excel("GrossNRatesData.xlsx", sheet = "Pinyon EA", range = "A1:I153")
d <- d %>% rename_all(funs(make.names(.)))
rain <- read_excel("GrossNRatesData.xlsx", sheet = "Sheet4", range = "G11:H65")



# Define the color palette
treatment_colors <- c("Control" = "#ffe599", 
                      "Summer-" = "#f2a6a6", 
                      "Winter-" = "#b0d2f2", 
                      "Summer+" = "#e87e76", 
                      "Winter+" = "#329bea") 


label2 = expression(paste("Average ", delta^{15}, "N vs Cumulative Rain per Water Year"))
l2 <-  expression(paste(delta^{15}, "N (\u2030)"))
#label = expression(paste("Gross Nitrification (", mu, "g N-NO"[3]^-1, " g soil"^-1, " hr"^-1, ")"))
#l2 <- expression(paste(mu, "g NH"[4]^"+", " g soil"^-1))

model <- lmer(as.numeric(d15N) ~ as.numeric(Rain) + (1 | Sample.ID), data = d)
r2 <- summary(model)$r.squared
p <- paste(" p = " ,format(summary(model)$coefficients[2, "Pr(>|t|)"], 
                           digits = 3))
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))

(plot1 <- ggplot(d, aes(x = as.numeric(Rain), y = as.numeric(d15N), color = Treatment)) +
  geom_point(size = 3) + # Adjust point size as needed
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  scale_color_manual(values = treatment_colors) +
  labs(
    x = "Cumulative Rain per Water Year (mm)",
    y = l2
  ) +
  annotate("text", x = 300, y = 7.5, label = "mylabel") +
  annotate("text", x = 300, y = 7.2, label = "p") +
  theme(
    panel.background = element_rect(colour = "black", fill = "NA", size = .7),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none",
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 10)
  ))

combined <- ggarrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)
combined


#### GWC, NO3, NH4 plot

(plot3 <- ggplot(winter, aes(x = Treatment, y = as.numeric(NO3), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75)+
    geom_errorbar(data = winter_summary, aes(y = w_mean, ymin = w_mean - w_se, ymax = w_mean + w_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    labs(y=expression(~paste(mu, "g NO"[3]^”-”, "-N g soil"^-1)))  + 
    xlab("") +
    scale_y_continuous(limits = c(0, 4))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 4, label = "Winter", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 4, label = "C)",size = 4, fontface=2)+ 
    annotate("text", x = 1.35, y = 0.7, label = "abc",size = 4)+ 
    annotate("text", x = 2.2, y = 0.6, label = "a",size = 4)+ 
    annotate("text", x = 3.3, y = 0.7, label = "abc",size = 4)+ 
    annotate("text", x = 4.2, y = 2.8, label = "d",size = 4)+ 
    annotate("text", x = 5.2, y = 0.6, label = "ab",size = 4)+ 
    scale_x_discrete(labels=c("Control" ,"Summer-","Summer+","Winter-","Winter+"))+
    theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
          panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
          panel.grid.major=element_blank(),
          legend.position = "none",
          legend.key = element_rect(color = NA),
          text = element_text(size=8),
          axis.title.x = element_text(size=8), #size of x-axis title
          axis.title.y = element_text(size=10), #size of y-axis title
          axis.text.x = element_text(size = 12, angle = 25, hjust = 1),
          axis.text.y = element_text(size=12)))


(plot4 <- ggplot(summer, aes(x = Treatment, y = as.numeric(NO3), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75)+
    geom_errorbar(data = summer_summary, aes(y = s_mean, ymin = s_mean - s_se, ymax = s_mean + s_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    #labs(y=expression(~paste(mu, "g N", "gdw"^-1)))  + 
    ylab("")+
    xlab("") +
    scale_y_continuous(limits = c(0, 4))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 4, label = "Summer", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 4, label = "D)",size = 4, fontface=2)+ 
    annotate("text", x = 1.2, y = 1.6, label = "cd",size = 4)+ 
    annotate("text", x = 2.23, y = 0.8, label = "ab",size = 4)+ 
    annotate("text", x = 3.25, y = 1.7, label = "bcd",size = 4)+ 
    annotate("text", x = 4.2, y = 2.3, label = "d",size = 4)+ 
    annotate("text", x = 5.2, y = 1.9, label = "cd",size = 4)+ 
    scale_x_discrete(labels=c("Control" ,"Summer-","Summer+","Winter-","Winter+"))+
    theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
          panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
          panel.grid.major=element_blank(),
          legend.position = "none",
          legend.key = element_rect(color = NA),
          text = element_text(size=8),
          axis.title.x = element_text(size=8), #size of x-axis title
          axis.title.y = element_text(size=12), #size of y-axis title
          axis.text.x = element_text(size = 12, angle = 25, hjust = 1),
          axis.text.y = element_text(size=12)))
(plot1 <- ggplot(winter, aes(x = Treatment, y = as.numeric(NH4), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75)+
    geom_errorbar(data = winter_summary, aes(y = w_mean, ymin = w_mean - w_se, ymax = w_mean + w_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    labs(y=expression(~paste(mu, "g NH"[4]^"+", "-N g soil"^-1)))  + 
    xlab("") +
    scale_y_continuous(limits = c(0, 4))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 4, label = "Winter", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 4, label = "A)",size = 4, fontface=2)+ 
    annotate("text", x = 1.3, y = 1, label = "a",size = 4)+ 
    annotate("text", x = 2.3, y = 1.9, label = "b",size = 4)+ 
    annotate("text", x = 3.3, y = 1.4, label = "ab",size = 4)+ 
    annotate("text", x = 4.3, y = 1.7, label = "ab",size = 4)+ 
    annotate("text", x = 5.3, y = 1.5, label = "ab",size = 4)+ 
    scale_x_discrete(labels=c("Control" ,"Summer-","Summer+","Winter-","Winter+"))+
    theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
          panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
          panel.grid.major=element_blank(),
          legend.position = "none",
          legend.key = element_rect(color = NA),
          text = element_text(size=8),
          axis.title.x = element_text(size=8), #size of x-axis title
          axis.title.y = element_text(size=10), #size of y-axis title
          axis.text.x = element_text(size = 12, angle = 25, hjust = 1),
          axis.text.y = element_text(size=12)))


(plot2 <- ggplot(summer, aes(x = Treatment, y = as.numeric(NH4), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75)+
    geom_errorbar(data = summer_summary, aes(y = s_mean, ymin = s_mean - s_se, ymax = s_mean + s_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    #labs(y=expression(~paste(mu, "g N", "gdw"^-1)))  + 
    ylab("")+
    xlab("") +
    scale_y_continuous(limits = c(0, 4))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 4, label = "Summer", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 4, label = "B)",size = 4, fontface=2)+ 
    annotate("text", x = 1.3, y = 1, label = "ab",size = 4)+ 
    annotate("text", x = 2.23, y = 2, label = "ab",size = 4)+ 
    annotate("text", x = 3.2, y = 1.5, label = "ab",size = 4)+ 
    annotate("text", x = 4.2, y = 2.2, label = "ab",size = 4)+ 
    annotate("text", x = 5.25, y = 1.8, label = "ab",size = 4)+ 
    scale_x_discrete(labels=c("Control" ,"Summer-","Summer+","Winter-","Winter+"))+
    theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
          panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
          panel.grid.major=element_blank(),
          legend.position = "none",
          legend.key = element_rect(color = NA),
          text = element_text(size=8),
          axis.title.x = element_text(size=8), #size of x-axis title
          axis.title.y = element_text(size=12), #size of y-axis title
          axis.text.x = element_text(size = 12, angle = 25, hjust = 1),
          axis.text.y = element_text(size=12)))


combined <- ggarrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)
combined

