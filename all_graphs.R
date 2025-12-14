## All graphs in manuscript order

rm(list=ls());graphics.off()
library(here)
library(dplyr)
library(ggplot2)
library(readxl) #allows you to work with data from excel files
library("gridExtra")
library(tidyverse)
library(ggpubr)
library("report")

d <- read_excel("GrossNRatesData.xlsx", sheet = "Sheet1")
d <- d %>% rename_all(funs(make.names(.)))# Remove spaces in column names

winter <- filter(d, Season %in% c("Winter")) #winter season only
summer <- filter(d, Season %in% c("Summer")) #summer season only

####Fig 3 - Soil Moisture####
winter$Season <- "Winter"
summer$Season <- "Summer"
combined_data <- bind_rows(winter, summer)

# Summarize data
combined_summary <- combined_data %>%
  group_by(Season, Treatment) %>%
  summarise(
    mean_GWC = mean(as.numeric(GWC)),
    se_GWC = sd(as.numeric(GWC)) / sqrt(n())
  )


(gwc <- ggplot(combined_data, aes(x = Treatment, y = as.numeric(GWC), fill = Season)) +
  geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75, binwidth = 0.15, position = position_dodge(0.8)) +
  geom_errorbar(data = combined_summary, aes(y = mean_GWC, ymin = mean_GWC - se_GWC, ymax = mean_GWC + se_GWC, group = Season),
                width = 0.2, position = position_dodge(0.8)) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, position = position_dodge(0.8)) +
  labs(y = "Soil gravimetric water content (%)", x = "") +
  scale_y_continuous(limits = c(0, 7.5)) +
  scale_fill_manual(values = c("Winter" = "#046C9A", "Summer" = "#A42820")) +
  theme_minimal() +
  annotate("text", x = 1.4, y = 4.5, label = "a",size = 4)+ 
  annotate("text", x = 2.4, y = 3.8, label = "ab",size = 4)+ 
  annotate("text", x = 3.4, y = 4.83, label = "a",size = 4)+ 
  annotate("text", x = 4.4, y = 2.53, label = "bc",size = 4)+ 
  annotate("text", x = 5.4, y = 4.8, label = "a",size = 4)+ 
    annotate("text", x = 1, y = 1.7, label = "c",size = 4)+ 
    annotate("text", x = 2, y = 0.4, label = "d",size = 4)+ 
    annotate("text", x = 3, y = 3.6, label = "ab",size = 4)+ 
    annotate("text", x = 4, y = 1.4, label = "cd",size = 4)+ 
    annotate("text", x = 5, y = 1.2, label = "cd",size = 4)+
  theme(
    panel.background = element_rect(colour = "black", fill = NA, size = 0.7),
    legend.position = "top",
    panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
    panel.grid.major=element_blank(),
    text = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 25, hjust = 1),
    axis.text.y = element_text(size = 12)
  ))


#ooriginal
winter_summary <- winter %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    w_mean = mean(as.numeric(GWC)),
    w_se = sd(as.numeric(GWC)) / sqrt(n())
  )

(gwc_w <- ggplot(winter, aes(x = Treatment, y = as.numeric(GWC), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75, binwidth = 0.1)+
    geom_errorbar(data = winter_summary, aes(y = w_mean, ymin = w_mean - w_se, ymax = w_mean + w_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    labs(y=expression(~paste("Soil gravimetric water content %")))  + 
    xlab("") +
    scale_y_continuous(limits = c(0, 7.5))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 7.5, label = "Winter", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 7.5, label = "A)",size = 4, fontface=2)+ 
    annotate("text", x = 1.2, y = 4.3, label = "a",size = 4)+ 
    annotate("text", x = 2.2, y = 3.8, label = "ab",size = 4)+ 
    annotate("text", x = 3.2, y = 4.83, label = "a",size = 4)+ 
    annotate("text", x = 4.2, y = 2.53, label = "bc",size = 4)+ 
    annotate("text", x = 5.2, y = 4.45, label = "a",size = 4)+ 
    scale_x_discrete(labels=c("Control" ,"Summer-","Summer+","Winter-","Winter+"))+
    theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
          panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
          panel.grid.major=element_blank(),
          legend.position = "none",
          legend.key = element_rect(color = NA),
          text = element_text(size=8),
          axis.title.x = element_text(size=8), #size of x-axis title
          axis.title.y = element_text(size=10), #size of y-axis title
          axis.text.x = element_text(size = 15, angle = 25, hjust = 1),
          axis.text.y = element_text(size=12)))

summer_summary <- summer %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    s_mean = mean(as.numeric(GWC)),
    s_se = sd(as.numeric(GWC)) / sqrt(n())
  )

(gwc_s <- ggplot(summer, aes(x = Treatment, y = as.numeric(GWC), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75, binwidth = 0.1)+
    geom_errorbar(data = summer_summary, aes(y = s_mean, ymin = s_mean - s_se, ymax = s_mean + s_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    #labs(y=expression(~paste(mu, "g N", "gdw"^-1)))  + 
    ylab("")+
    xlab("") +
    scale_y_continuous(limits = c(0, 7.5))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 7.5, label = "Summer", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 7.5, label = "B)",size = 4, fontface=2)+ 
    annotate("text", x = 1.25, y = 1.8, label = "c",size = 4)+ 
    annotate("text", x = 2.25, y = 0.5, label = "d",size = 4)+ 
    annotate("text", x = 3.25, y = 3.7, label = "ab",size = 4)+ 
    annotate("text", x = 4.25, y = 1.4, label = "cd",size = 4)+ 
    annotate("text", x = 5.25, y = 1.25, label = "cd",size = 4)+ 
    scale_x_discrete(labels=c("Control" ,"Summer-","Summer+","Winter-","Winter+"))+
    theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
          panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
          panel.grid.major=element_blank(),
          legend.position = "none",
          legend.key = element_rect(color = NA),
          text = element_text(size=8),
          axis.title.x = element_text(size=8), #size of x-axis title
          axis.title.y = element_text(size=12), #size of y-axis title
          axis.text.x = element_text(size = 15, angle = 25, hjust = 1),
          axis.text.y = element_text(size=12)))
(gwc <- ggarrange(gwc_w, gwc_s, ncol = 2, nrow = 1))
ggsave(gwc, path = "Graphs", file = "gwc.jpeg", width = 8, height = 7.5, units = "in")


#### Fig 4 - NH4 NO3 ####
winter_summary <- winter %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    w_mean = mean(as.numeric(NH4)),
    w_se = sd(as.numeric(NH4)) / sqrt(n())
  )
(NH4_w <- ggplot(winter, aes(x = Treatment, y = as.numeric(NH4), fill = Treatment))+
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
   annotate("text", x = .75, y = 4, label = "C)",size = 4, fontface=2)+ 
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
         axis.title.y = element_text(size=15), #size of y-axis title
         axis.text.x = element_text(size = 15, angle = 25, hjust = 1),
         axis.text.y = element_text(size=15)))

summer_summary <- summer %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    s_mean = mean(as.numeric(NH4)),
    s_se = sd(as.numeric(NH4)) / sqrt(n())
  )

(NH4_s <- ggplot(summer, aes(x = Treatment, y = as.numeric(NH4), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75)+
    geom_errorbar(data = summer_summary, aes(y = s_mean, ymin = s_mean - s_se, ymax = s_mean + s_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    ylab("")+
    xlab("") +
    scale_y_continuous(limits = c(0, 4))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 4, label = "Summer", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 4, label = "D)",size = 4, fontface=2)+ 
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
          axis.title.y = element_text(size=15), #size of y-axis title
          axis.text.x = element_text(size = 15, angle = 25, hjust = 1),
          axis.text.y = element_text(size=15)))

winter_summary <- winter %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    w_mean = mean(as.numeric(NO3)),
    w_se = sd(as.numeric(NO3)) / sqrt(n())
  )

(NO3_w <- ggplot(winter, aes(x = Treatment, y = as.numeric(NO3), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75)+
    geom_errorbar(data = winter_summary, aes(y = w_mean, ymin = w_mean - w_se, ymax = w_mean + w_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    labs(y=expression(~paste(mu, "g NO"[3]^"-", "-N g soil"^-1)))  + 
    xlab("") +
    scale_y_continuous(limits = c(0, 4))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 4, label = "Winter", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 4, label = "E)",size = 4, fontface=2)+ 
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
          axis.title.y = element_text(size=15), #size of y-axis title
          axis.text.x = element_text(size = 15, angle = 25, hjust = 1),
          axis.text.y = element_text(size=15)))

summer_summary <- summer %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    s_mean = mean(as.numeric(NO3)),
    s_se = sd(as.numeric(NO3)) / sqrt(n())
  )

(NO3_s <- ggplot(summer, aes(x = Treatment, y = as.numeric(NO3), fill = Treatment))+
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
    annotate("text", x = .75, y = 4, label = "F)",size = 4, fontface=2)+ 
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
          axis.text.x = element_text(size = 15, angle = 25, hjust = 1),
          axis.text.y = element_text(size=15)))

(NH4_NO3 <- ggarrange(NH4_w, NH4_s, NO3_w, NO3_s, ncol = 2, nrow = 2, common.legend = TRUE, legend = "none"))

ggsave(NH4_NO3, path = "Graphs", file = "NH4_NO3.jpeg", width = 8, height = 7.5, units = "in")

#### Fig 5 - GM GN ####
winter_summary <- winter %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    w_mean = mean(as.numeric(Gross.Min)),
    w_se = sd(as.numeric(Gross.Min)) / sqrt(n())
  )

(GM_w <- ggplot(winter, aes(x = Treatment, y = as.numeric(Gross.Min), fill = Treatment))+
   geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75, binwidth = 0.015, dotsize = 1.5)+
   geom_errorbar(data = winter_summary, aes(y = w_mean, ymin = w_mean - w_se, ymax = w_mean + w_se), width = 0.2) +
   stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
   labs(y=expression(~paste("Gross N Mineralization rate (",mu, "g N",  " g soil"^-1, "h"^-1, ")")))  + 
   xlab("") +
   scale_y_continuous(limits = c(0, 1))+ #scales the y axis for you
   scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                     labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                     guide = guide_legend(override.aes = list(colour = NA)))+
   annotate("text", x = 3, y = 1, label = "Winter", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
   annotate("text", x = .75, y = 1, label = "A)",size = 4, fontface=2)+ 
   annotate("text", x = 1.3, y = 0.325, label = "ab",size = 3.5)+ 
   annotate("text", x = 2.3, y = 0.321, label = "ab",size = 3.5)+ 
   annotate("text", x = 3.35, y = 0.25, label = "abc",size = 3.5)+ 
   annotate("text", x = 4.2, y = 0.15, label = "c",size = 3.5)+ 
   annotate("text", x = 5.3, y = 0.255, label = "abc",size = 3.5)+ 
   scale_x_discrete(labels=c("Control" ,"Summer-","Summer+","Winter-","Winter+"))+
   theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
         panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
         panel.grid.major=element_blank(),
         legend.position = "none",
         legend.key = element_rect(color = NA),
         text = element_text(size=8),
         axis.title.x = element_text(size=8), #size of x-axis title
         axis.title.y = element_text(size=11), #size of y-axis title
         axis.text.x = element_text(size = 12, angle = 25, hjust = 1),
         axis.text.y = element_text(size=12)))

summer_summary <- summer %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    s_mean = mean(as.numeric(Gross.Min)),
    s_se = sd(as.numeric(Gross.Min)) / sqrt(n())
  )

(GM_s <- ggplot(summer, aes(x = Treatment, y = as.numeric(Gross.Min), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75, binwidth = 0.015, dotsize = 1.5)+
    geom_errorbar(data = summer_summary, aes(y = s_mean, ymin = s_mean - s_se, ymax = s_mean + s_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    #labs(y=expression(~paste(mu, "g N", "gdw"^-1)))  + 
    ylab("")+
    xlab("") +
    scale_y_continuous(limits = c(0, 1))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 1, label = "Summer", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 1, label = "B)",size = 4, fontface=2)+ 
    annotate("text", x = 1.3, y = 0.44, label = "ad",size = 4)+ 
    annotate("text", x = 2.33, y = 0.295, label = "abc",size = 4)+ 
    annotate("text", x = 3.2, y = 0.72, label = "d",size = 4)+ 
    annotate("text", x = 4.29, y = 0.23, label = "b",size = 4)+ 
    annotate("text", x = 5.33, y = 0.36, label = "ab",size = 4)+ 
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

winter_summary <- winter %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    w_mean = mean(as.numeric(Gross.Nit)),
    w_se = sd(as.numeric(Gross.Nit)) / sqrt(n())
  )
(GN_w <- ggplot(winter, aes(x = Treatment, y = as.numeric(Gross.Nit), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75, binwidth = 0.009, dotsize = 1.3)+
    geom_errorbar(data = winter_summary, aes(y = w_mean, ymin = w_mean - w_se, ymax = w_mean + w_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    labs(y=expression(~paste("Gross Nitrification rate (",mu, "g NO"[3]^"-",  "-N g soil"^-1, "h"^-1, ")")))  + 
    xlab("") +
    scale_y_continuous(limits = c(0, 0.5))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 0.5, label = "Winter", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 0.5, label = "C)",size = 4, fontface=2)+ 
    annotate("text", x = 1.245, y = 0.207, label = "a",size = 3.5)+ 
    annotate("text", x = 2.24, y = 0.217, label = "a",size = 3.5)+ 
    annotate("text", x = 3.25, y = 0.147, label = "ab",size = 3.5)+ 
    annotate("text", x = 4.25, y = 0.032, label = "cd",size = 3.5)+ 
    annotate("text", x = 5.27, y = 0.152, label = "ab",size = 3.5)+ 
    scale_x_discrete(labels=c("Control" ,"Summer-","Summer+","Winter-","Winter+"))+
    theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
          panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
          panel.grid.major=element_blank(),
          legend.position = "none",
          legend.key = element_rect(color = NA),
          text = element_text(size=8),
          axis.title.x = element_text(size=8), #size of x-axis title
          axis.title.y = element_text(size=11.5), #size of y-axis title
          axis.text.x = element_text(size = 12, angle = 25, hjust = 1),
          axis.text.y = element_text(size=12)))

summer_summary <- summer %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    s_mean = mean(as.numeric(Gross.Nit)),
    s_se = sd(as.numeric(Gross.Nit)) / sqrt(n())
  )
(GN_s <- ggplot(summer, aes(x = Treatment, y = as.numeric(Gross.Nit), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75, binwidth = 0.009, dotsize = 1.3)+
    geom_errorbar(data = summer_summary, aes(y = s_mean, ymin = s_mean - s_se, ymax = s_mean + s_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    #labs(y=expression(~paste(mu, "g N", "gdw"^-1)))  + 
    ylab("")+
    xlab("") +
    scale_y_continuous(limits = c(0, 0.5))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 0.5, label = "Summer", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 0.5, label = "D)",size = 4, fontface=2)+ 
    annotate("text", x = 1.33, y = 0.1, label = "bcd",size = 4)+ 
    annotate("text", x = 2.245, y = 0.027, label = "d",size = 4)+ 
    annotate("text", x = 3.305, y = 0.137, label = "bcd",size = 4)+ 
    annotate("text", x = 4.32, y = 0.065, label = "bcd",size = 4)+ 
    annotate("text", x = 5.3, y = 0.09, label = "bcd",size = 4)+ 
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

(GM_GN <- ggarrange(GM_w, GM_s, GN_w, GN_s, ncol = 2, nrow = 2, common.legend = TRUE, legend = "none"))

ggsave(GM_GN, path = "Graphs", file = "GM_GN.jpeg", width = 8, height = 7.5, units = "in")

#### Fig 7 - Net rates ####

winter_summary <- winter %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    w_mean = mean(as.numeric(Net.Min)),
    w_se = sd(as.numeric(Net.Min)) / sqrt(n())
  )
(Net_Min_w <- ggplot(winter, aes(x = Treatment, y = as.numeric(Net.Min), fill = Treatment))+
   geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75)+
   geom_errorbar(data = winter_summary, aes(y = w_mean, ymin = w_mean - w_se, ymax = w_mean + w_se), width = 0.2) +
   stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
   labs(y=expression(~paste("Net N Mineralization (", mu, "g N", " g soil"^-1, "d"^-1,")")))  + 
   xlab("") +
   scale_y_continuous(limits = c(-0.1, 1))+ #scales the y axis for you
   scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                     labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                     guide = guide_legend(override.aes = list(colour = NA)))+
   annotate("text", x = 3, y = 1, label = "Winter", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
   annotate("text", x = .75, y = 1, label = "A)",size = 4, fontface=2)+ 
   annotate("text", x = 1.3, y = 0.15, label = "a",size = 4)+ 
   annotate("text", x = 2.3, y = 0.2, label = "a",size = 4)+ 
   annotate("text", x = 3.3, y = 0.05, label = "a",size = 4)+ 
   annotate("text", x = 4.3, y = 0.2, label = "a",size = 4)+ 
   annotate("text", x = 5.3, y = 0.02, label = "a",size = 4)+ 
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

summer_summary <- summer %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    s_mean = mean(as.numeric(Net.Min)),
    s_se = sd(as.numeric(Net.Min)) / sqrt(n())
  )
(Net_Min_s <- ggplot(summer, aes(x = Treatment, y = as.numeric(Net.Min), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75)+
    geom_errorbar(data = summer_summary, aes(y = s_mean, ymin = s_mean - s_se, ymax = s_mean + s_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    labs(y="")  + 
    xlab("") +
    scale_y_continuous(limits = c(-0.1, 1))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 1, label = "Summer", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 1, label = "B)",size = 4, fontface=2)+ 
    annotate("text", x = 1.3, y = 0.15, label = "a",size = 4)+ 
    annotate("text", x = 2.3, y = 0.4, label = "a",size = 4)+ 
    annotate("text", x = 3.3, y = 0.1, label = "a",size = 4)+ 
    annotate("text", x = 4.3, y = 0.5, label = "a",size = 4)+ 
    annotate("text", x = 5.3, y = 0.2, label = "a",size = 4)+ 
    scale_x_discrete(labels=c("Control" ,"Summer-","Summer+","Winter-","Winter+"))+
    theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
          panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
          panel.grid.major=element_blank(),
          legend.position = "none",
          legend.key = element_rect(color = NA),
          text = element_text(size=8),
         axis.title.x = element_text(size=8), #size of x-axis title
         axis.title.y = element_text(size=11), #size of y-axis title
         axis.text.x = element_text(size = 12, angle = 25, hjust = 1),
         axis.text.y = element_text(size=12)))

winter_summary <- winter %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    w_mean = mean(as.numeric(Net.Nit)),
    w_se = sd(as.numeric(Net.Nit)) / sqrt(n())
  )
(Net_Nit_w <- ggplot(winter, aes(x = Treatment, y = as.numeric(Net.Nit), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75)+
    geom_errorbar(data = winter_summary, aes(y = w_mean, ymin = w_mean - w_se, ymax = w_mean + w_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    labs(y=expression(~paste("Net Nitrification (", mu, "g N", " g soil"^-1, "d"^-1,")")))  + 
    xlab("") +
    scale_y_continuous(limits = c(-0.1, 0.6))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 0.6, label = "Winter", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 0.6, label = "C)",size = 4, fontface=2)+ 
    annotate("text", x = 1.35, y = 0.05, label = "ab",size = 4)+ 
    annotate("text", x = 2.35, y = 0.02, label = "a",size = 4)+ 
    annotate("text", x = 3.45, y = 0.02, label = "ab",size = 4)+ 
    annotate("text", x = 4.3, y = 0.09, label = "b",size = 4)+ 
    annotate("text", x = 5.45, y = 0.02, label = "ab",size = 4)+ 
    scale_x_discrete(labels=c("Control" ,"Summer-","Summer+","Winter-","Winter+"))+
    theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
          panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
          panel.grid.major=element_blank(),
          legend.position = "none",
          legend.key = element_rect(color = NA),
          text = element_text(size=8),
          axis.title.x = element_text(size=8), #size of x-axis title
          axis.title.y = element_text(size=13), #size of y-axis title
          axis.text.x = element_text(size = 12, angle = 25, hjust = 1),
          axis.text.y = element_text(size=12)))

summer_summary <- summer %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    s_mean = mean(as.numeric(Net.Nit)),
    s_se = sd(as.numeric(Net.Nit)) / sqrt(n())
  )
(Net_Nit_s <- ggplot(summer, aes(x = Treatment, y = as.numeric(Net.Nit), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75)+
    geom_errorbar(data = summer_summary, aes(y = s_mean, ymin = s_mean - s_se, ymax = s_mean + s_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    labs(y="")  + 
    xlab("") +
    scale_y_continuous(limits = c(-0.1, 0.6))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 0.6, label = "Summer", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 0.6, label = "D)",size = 4, fontface=2)+ 
    annotate("text", x = 1.3, y = 0.15, label = "a",size = 4)+ 
    annotate("text", x = 2.3, y = 0.2, label = "a",size = 4)+ 
    annotate("text", x = 3.3, y = 0.05, label = "a",size = 4)+ 
    annotate("text", x = 4.3, y = 0.38, label = "a",size = 4)+ 
    annotate("text", x = 5.3, y = 0.15, label = "a",size = 4)+ 
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

(Net_rates <- ggarrange(Net_Min_w, Net_Min_s, Net_Nit_w, Net_Nit_s, ncol = 2, nrow = 2, common.legend = TRUE, legend = "none"))

ggsave(Net_rates, path = "Graphs", file = "Net_rates.jpeg", width = 8, height = 7.5, units = "in")

#### C:N ####
winter_summary <- winter %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    w_mean = mean(as.numeric(C.N)),
    w_se = sd(as.numeric(C.N)) / sqrt(n())
  )

(CN_w <- ggplot(winter, aes(x = Treatment, y = as.numeric(C.N), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75, binwidth = 0.3)+
    geom_errorbar(data = winter_summary, aes(y = w_mean, ymin = w_mean - w_se, ymax = w_mean + w_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    labs(y=expression(~paste("C:N")))  + 
    xlab("") +
    scale_y_continuous(limits = c(0, 17))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 17, label = "Winter", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 17, label = "A)",size = 4, fontface=2)+ 
    annotate("text", x = 1.2, y = 6.3, label = "a",size = 4)+ 
    annotate("text", x = 2.2, y = 7.1, label = "a",size = 4)+ 
    annotate("text", x = 3.2, y = 6.2, label = "a",size = 4)+ 
    annotate("text", x = 4.2, y = 6.3, label = "a",size = 4)+ 
    annotate("text", x = 5.2, y = 6.5, label = "a",size = 4)+ 
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

summer_summary <- summer %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    s_mean = mean(as.numeric(C.N)),
    s_se = sd(as.numeric(C.N)) / sqrt(n())
  )

(CN_s <- ggplot(summer, aes(x = Treatment, y = as.numeric(C.N), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75, binwidth = 0.3)+
    geom_errorbar(data = summer_summary, aes(y = s_mean, ymin = s_mean - s_se, ymax = s_mean + s_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    ylab("")+
    xlab("") +
    scale_y_continuous(limits = c(0, 17))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 17, label = "Summer", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 17, label = "B)",size = 4, fontface=2)+ 
    annotate("text", x = 1.2, y = 8.75, label = "a",size = 4)+ 
    annotate("text", x = 2.25, y = 14.2, label = "b",size = 4)+ 
    annotate("text", x = 3.25, y = 13.9, label = "b",size = 4)+ 
    annotate("text", x = 4.25, y = 13.1, label = "b",size = 4)+ 
    annotate("text", x = 5.25, y = 14.1, label = "b",size = 4)+ 
    scale_x_discrete(labels=c("Control" ,"Summer-","Summer+","Winter-","Winter+"))+
    theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
          panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
          panel.grid.major=element_blank(),
          legend.position = c(0.75, 0.25),
          legend.key = element_rect(color = NA),
          text = element_text(size=8),
          axis.title.x = element_text(size=8), #size of x-axis title
          axis.title.y = element_text(size=12), #size of y-axis title
          axis.text.x = element_text(size = 12, angle = 25, hjust = 1),
          axis.text.y = element_text(size=12)))
(CN <- ggarrange(CN_w, CN_s, ncol = 2, nrow = 1))

ggsave(CN, path = "Graphs", file = "CN.jpeg", width = 8, height = 7.5, units = "in")
#### MB ####

winter_summary <- winter %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    s_mean = mean(as.numeric(MB.C)),
    s_se = sd(as.numeric(MB.C)) / sqrt(n())
  )

ggplot(summer, aes(x = Treatment, y = as.numeric(MB.N), fill = Treatment))+
  geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75, binwidth = 0.3)

#### Organic N ####
winter_summary <- winter %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    w_mean = mean(as.numeric(organic.N)),
    w_se = sd(as.numeric(organic.N)) / sqrt(n())
  )

(Org_w <- ggplot(winter, aes(x = Treatment, y = as.numeric(organic.N), fill = Treatment))+
   geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75, binwidth = 1)+
   geom_errorbar(data = winter_summary, aes(y = w_mean, ymin = w_mean - w_se, ymax = w_mean + w_se), width = 0.2) +
   stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
   labs(y=expression(~paste("Organic N (", mu, "g N g soil"^-1, ")")))  + 
   xlab("") +
   scale_y_continuous(limits = c(0, 60))+ #scales the y axis for you
   scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                     labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                     guide = guide_legend(override.aes = list(colour = NA)))+
   annotate("text", x = 3, y = 60, label = "Winter", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
   annotate("text", x = .75, y = 60, label = "A)",size = 4, fontface=2)+ 
   annotate("text", x = 1.3, y = 29, label = "a",size = 4)+ 
   annotate("text", x = 2.35, y = 20, label = "ab",size = 4)+ 
   annotate("text", x = 3.3, y = 24, label = "ab",size = 4)+ 
   annotate("text", x = 4.2, y = 14, label = "b",size = 4)+ 
   annotate("text", x = 5.3, y = 20, label = "ab",size = 4)+ 
   scale_x_discrete(labels=c("Control" ,"Summer-","Summer+","Winter-","Winter+"))+
   theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
         panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
         panel.grid.major=element_blank(),
         legend.position = c(0.75, 0.75),
         legend.key = element_rect(color = NA),
         text = element_text(size=8),
         axis.title.x = element_text(size=8), #size of x-axis title
         axis.title.y = element_text(size=10), #size of y-axis title
         axis.text.x = element_text(size = 12, angle = 25, hjust = 1),
         axis.text.y = element_text(size=12)))

summer_summary <- summer %>%
  group_by(Treatment) %>%
  dplyr::summarise(
    s_mean = mean(as.numeric(organic.N)),
    s_se = sd(as.numeric(organic.N)) / sqrt(n())
  )
(Org_s <- ggplot(summer, aes(x = Treatment, y = as.numeric(organic.N), fill = Treatment))+
    geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 0.75, binwidth = 1)+
    geom_errorbar(data = summer_summary, aes(y = s_mean, ymin = s_mean - s_se, ymax = s_mean + s_se), width = 0.2) +
    stat_summary(fun = mean, geom = "crossbar", width = 0.5) +
    #labs(y=expression(~paste(mu, "g N", "gdw"^-1)))  + 
    ylab("")+
    xlab("") +
    scale_y_continuous(limits = c(0, 60))+ #scales the y axis for you
    scale_fill_manual(values=c("#EBCC2A","#F8AFA8", "#A42820", "#ABDDDE", "#046C9A"), 
                      labels = c("Control" ,"Summer-","Summer+","Winter-","Winter+"),
                      guide = guide_legend(override.aes = list(colour = NA)))+
    annotate("text", x = 3, y = 60, label = "Summer", size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = .75, y = 60, label = "B)",size = 4, fontface=2)+ 
    annotate("text", x = 1.2, y = 28, label = "a",size = 4)+ 
    annotate("text", x = 2.2, y = 29, label = "a",size = 4)+ 
    annotate("text", x = 3.3, y = 30, label = "ab",size = 4)+ 
    annotate("text", x = 4.2, y = 15, label = "b",size = 4)+ 
    annotate("text", x = 5.2, y = 25, label = "ab",size = 4)+ 
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

(organicN <- ggarrange(Org_w, Org_s, ncol = 2, nrow = 1))


ggsave(organicN, path = "Graphs", file = "organicN.jpeg", width = 8, height = 7.5, units = "in")

####################
####CORRELATIONS####
####################

treatment_colors <- c("Control" = "#ffe599", 
                      "Summer-" = "#f2a6a6", 
                      "Winter-" = "#b0d2f2", 
                      "Summer+" = "#e87e76", 
                      "Winter+" = "#329bea") 
#### GM vs C:N ####
lab1=expression(~paste("Gross N Mineralization rate (",mu, "g N g soil"^-1, "h"^-1, ")"))
model<-lm(as.numeric(Gross.Min) ~ as.numeric(C.N), data = summer)
r2 <- summary(model)$r.squared
p <- paste(" p = " ,format(summary(model)$coefficients[2, "Pr(>|t|)"], 
                           digits = 3))
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))

(gm_gwc_w <-ggplot(winter, aes(x = as.numeric(C.N), y = as.numeric(Gross.Min), color = Treatment)) +
    geom_point(size = 3) + # Adjust point size as needed
    scale_color_manual(values = treatment_colors)+
    scale_y_continuous(limits = c(0, 1.15))+
    annotate("text", x = 7, y = 1.15, label = "Winter", 
             size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = 3, y = 1.15, label = "A)",size = 4, fontface=2)+
    scale_x_continuous(limits = c(3, 11))+
    labs(
      x = "C:N",
      y = lab1
    ) +
    annotate("text", x = 4, y = 0.9, label = mylabel)+
    annotate("text", x = 4, y = 0.85, label = p)+
    theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
          panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
          panel.grid.major=element_blank(),  
          legend.position = c(0.75, 0.75),
          axis.title.x = element_text(size=10), #size of x-axis title
          axis.title.y = element_text(size=10), #size of y-axis title
          axis.text.x = element_text(size = 10), #size of x-axis text
          axis.text.y = element_text(size=10), plot.title = element_text(size = 10)))


###################
#### Gross vs GWC####

lab1=expression(~paste("Gross N Mineralization rate (",mu, "g N g soil"^-1, "h"^-1, ")"))
model<-lm(as.numeric(Gross.Min) ~ as.numeric(GWC), data = winter)
r2 <- summary(model)$r.squared
p <- paste(" p = " ,format(summary(model)$coefficients[2, "Pr(>|t|)"], 
                           digits = 3))
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))

(GM_GWC_w <-ggplot(winter, aes(x = as.numeric(GWC), y = as.numeric(Gross.Min), color = Treatment)) +
   geom_point(size = 3) + # Adjust point size as needed
   scale_color_manual(values = treatment_colors)+
   scale_y_continuous(limits = c(0, 1.15))+
   annotate("text", x = 3, y = 1.15, label = "Winter", 
            size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
   annotate("text", x = 0, y = 1.15, label = "A)",size = 4, fontface=2)+
   scale_x_continuous(limits = c(0, 6))+
   labs(
     x = "Soil gravimetric water content %",
     y = lab1
   ) +
   annotate("text", x = 1, y = 0.9, label = mylabel)+
   annotate("text", x = 1, y = 0.85, label = p)+
   theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
         panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
         panel.grid.major=element_blank(),  
         legend.position = c(0.75, 0.75),
         axis.title.x = element_text(size=10), #size of x-axis title
         axis.title.y = element_text(size=10), #size of y-axis title
         axis.text.x = element_text(size = 10), #size of x-axis text
         axis.text.y = element_text(size=10), plot.title = element_text(size = 10)))


model<-lm(as.numeric(Gross.Min) ~ as.numeric(GWC), data = summer)
r2 <- summary(model)$r.squared
p <- paste(" p = " ,format(summary(model)$coefficients[2, "Pr(>|t|)"], 
                           digits = 3))
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))


(GM_GWC_s <-ggplot(summer, aes(x = as.numeric(GWC), y = as.numeric(Gross.Min), color = Treatment)) +
    geom_point(size = 3) + # Adjust point size as needed
    geom_smooth(method = lm, se = T, colour = "black")+
    scale_color_manual(values = treatment_colors)+
    scale_y_continuous(limits = c(0, 1.15))+
    annotate("text", x = 3, y = 1.15, label = "Summer", 
             size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = 0, y = 1.15, label = "D)",size = 4, fontface=2)+
    scale_x_continuous(limits = c(0, 6))+
    labs(
      x = "Soil gravimetric water content %",
      y = ""
    ) +
    annotate("text", x = 1, y = 0.9, label = mylabel)+
    annotate("text", x = 1, y = 0.85, label = "p < 0.0001")+
    theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
          panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
          panel.grid.major=element_blank(),  
          legend.position = "none",
          axis.title.x = element_text(size=10), #size of x-axis title
          axis.title.y = element_text(size=10), #size of y-axis title
          axis.text.x = element_text(size = 10), #size of x-axis text
          axis.text.y = element_text(size=10), plot.title = element_text(size = 10)))


lab1=expression(~paste("Gross Nitrification rate (",mu, "g NO"[3]^"-",  "-N g soil"^-1, "h"^-1, ")"))
model<-lm(as.numeric(Gross.Nit) ~ as.numeric(GWC), data = winter)
r2 <- summary(model)$r.squared
p <- paste(" p = " ,format(summary(model)$coefficients[2, "Pr(>|t|)"], 
                           digits = 3))
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))

(GN_GWC_w <-ggplot(winter, aes(x = as.numeric(GWC), y = as.numeric(Gross.Nit), color = Treatment)) +
    geom_point(size = 3) + # Adjust point size as needed
    scale_color_manual(values = treatment_colors)+
    scale_y_continuous(limits = c(0, 0.4))+
    annotate("text", x = 3, y = 0.4, label = "Winter", 
             size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = 0, y = 0.4, label = "C)",size = 4, fontface=2)+
    scale_x_continuous(limits = c(0, 6))+
    labs(
      x = "Soil gravimetric water content %",
      y = lab1
    ) +
    annotate("text", x = 1, y = 0.35, label = mylabel)+
    annotate("text", x = 1, y = 0.33, label = p)+
    theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
          panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
          panel.grid.major=element_blank(),  
          legend.position = "none",
          axis.title.x = element_text(size=10), #size of x-axis title
          axis.title.y = element_text(size=10), #size of y-axis title
          axis.text.x = element_text(size = 10), #size of x-axis text
          axis.text.y = element_text(size=10), plot.title = element_text(size = 10)))
  
model<-lm(as.numeric(Gross.Nit) ~ as.numeric(GWC), data = summer)
r2 <- summary(model)$r.squared
p <- paste(" p = " ,format(summary(model)$coefficients[2, "Pr(>|t|)"], 
                           digits = 3))
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))


(GN_GWC_s <-ggplot(summer, aes(x = as.numeric(GWC), y = as.numeric(Gross.Nit), color = Treatment)) +
    geom_point(size = 3) + # Adjust point size as needed
    geom_smooth(method = lm, se = T, colour = "black")+
    scale_color_manual(values = treatment_colors)+
    scale_y_continuous(limits = c(0, 0.4))+
    annotate("text", x = 3, y = 0.4, label = "Summer", 
             size = 3.5, fontface=2)+ #adds the sub graphs for the figure at top left
    annotate("text", x = 0, y = 0.4, label = "D)",size = 4, fontface=2)+
    scale_x_continuous(limits = c(0, 6))+
    labs(
      x = "Soil gravimetric water content %",
      y = ""
    ) +
    annotate("text", x = 1, y = 0.35, label = mylabel)+
    annotate("text", x = 1, y = 0.33, label = "p < 0.0001")+
    theme(panel.background = element_rect(colour = "black", fill = "NA", size=.7),
          panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
          panel.grid.major=element_blank(),  
          legend.position = "none",
          axis.title.x = element_text(size=10), #size of x-axis title
          axis.title.y = element_text(size=10), #size of y-axis title
          axis.text.x = element_text(size = 10), #size of x-axis text
          axis.text.y = element_text(size=10), plot.title = element_text(size = 10)))


(Gross_GWC <- ggarrange(GM_GWC_w, GM_GWC_s, GN_GWC_w, GN_GWC_s, ncol = 2, nrow = 2))

ggsave(Gross_GWC, path = "Graphs", file = "Gross_GWC.jpeg", width = 8, height = 7.5, units = "in")
