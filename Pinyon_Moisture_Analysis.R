#copied from alex's PinyonMoisture.R, adjusted for rain measurements from log 2

rm(list=ls());graphics.off()

#read in the data
library(writexl)
library(openxlsx)
library(dplyr)
library(lubridate)
library(readxl)
setwd("/Users/jam925/");
library(ggplot2)

# P2 <- read.delim("Pinyon2_Table1.dat",skip=1,header=TRUE,sep=",")        
# 
#   p1 <- data.frame(P2$TIMESTAMP, P2$VW_Avg,P2$VW_2_Avg,P2$T108_C_Avg,P2$T108_C_2_Avg)
#   p1$Plot <- "1"
#   p1$Treatment <- "Winter-"
#   colnames(p1) <- c("Timestamp","VWC2cm","VWC10cm", "Temp2cm","Temp10cm","Plot","Treatment")
#   head(p1)
#   p3 <- data.frame(P2$TIMESTAMP, P2$VW_3_Avg,P2$VW_4_Avg,P2$T108_C_3_Avg,P2$T108_C_4_Avg)
#   p3$Plot <- "3"
#   p3$Treatment <- "Summer+"
#   colnames(p3) <- c("Timestamp","VWC2cm","VWC10cm", "Temp2cm","Temp10cm","Plot","Treatment")
#   head(p3)

P2 <- read_excel("Desktop/Project Documents/Pinyon Data Loggers/Collected 2:16:24/Pinyon_Temperature.xlsx", 
                 sheet = "Sheet 1")
P22 <- subset(P2, P2.TIMESTAMP > "2022-01-01 12:00:00")

d <- data.frame(P22$P2.TIMESTAMP, P22$P2.T108_C_Avg, P22$Rain_mm_Tot)
colnames(d) <- c("Timestamp", "Temperature", "Rain")

d$Timestamp <- ymd_hms(d$Timestamp)
d$date <- as.Date(d$Timestamp)
d$Rain <- as.numeric(d$Rain)
d$Temperature <- as.numeric(d$Temperature)


daily_data <- d %>%
  group_by(date) %>%
  dplyr::summarize(
    daily_avg_temp = mean(Temperature, na.rm = TRUE),
    daily_total_rain = sum(Rain, na.rm = TRUE)
  )

ggplot(data = daily_data, aes(x = date)) +
  geom_line(aes(y = daily_avg_temp, color = "Temperature"), size = 1) +
  geom_segment(aes(x = date, xend = date, y = 0, yend = daily_total_rain, color = "Rain"), size = 1) +
  scale_x_date(date_breaks = "6 month", date_labels = "%b %Y") +
  scale_y_continuous(
    name = "Daily Average Temperature (°C)",
    sec.axis = sec_axis(~ ., name = "Daily Precipitation (mm)"),
    limits = c(0, 100)
  ) +
  scale_color_manual(
    name = "",
    values = c("Temperature" = "red", "Rain" = "blue"),
    labels = c("", "")
  ) +
  labs(
    title = "",
    x = "Date"
  ) +
  theme(
    axis.title.y = element_text(color = "red"),
    axis.title.y.right = element_text(color = "blue"),
    axis.text.y = element_text(size = 10),  # Increase the size of the left y-axis units
    axis.text.y.right = element_text(size = 10),  # Increase the size of the right y-axis units
    axis.text.x = element_text(size=10),
    legend.position = "bottom",
    panel.background = element_blank()
  )




# m_daily <- moisture %>%
#   mutate(Date = P2.TIMESTAMP) %>%
#   group_by(Date) %>%
#   summarise(P2.VW_Avg = sum(P2.VW_Avg))

# total_rain <- sum(rain_daily$Total_Rain_mm)
# frequency <- length(rain_daily$Date)
# rain_per_event <- total_rain/frequency #annual
# 
# winter_months <- c("November", "December", "January", "February", "March", "April", "May")
# winter_rain <- 0 #total
# summer_rain <- 0
# winter_freq <- 0
# summer_freq <- 0
# winter_annual <- array(0, 74)
# summer_annual <- array(0, 43)
# w_index <- 1
# s_index <- 1
# 
# for (j in  1:frequency){
#   if(months(rain_daily$Date[j]) %in% winter_months){
#     winter_rain <- winter_rain + rain_daily$Total_Rain_mm[j]
#     winter_freq <- winter_freq + 1
#     winter_annual[w_index] <- rain_daily$Total_Rain_mm[j]
#     w_index <- w_index + 1
#   }else{
#     summer_rain <- summer_rain + rain_daily$Total_Rain_mm[j]
#     summer_freq <- summer_freq + 1
#     summer_annual[s_index] <- rain_daily$Total_Rain_mm[j]
#     s_index <- s_index + 1
#   }
# }


#plot for rain graph
ggplot(rain_daily, aes(x = Date, y = Total_Rain_mm)) +
  geom_bar(stat = "identity") +
  scale_x_date(date_breaks = "6 month", date_labels = "%b %Y") +
  labs(x = "", y = " Rainfall (mm)", title = "Rainfall Events at Pinyon Flats from 2020-2023") +
  theme(plot.title = element_text(size = 15), 
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size=6.5), 
        panel.background = element_blank())


ggplot(rain_daily, aes(x= Date)) +
  geom_bar(aes(y= Total_Rain_mm/4, fill = "Total_Rain_mm"), stat="identity")  +
  geom_line(aes(y = Temperature, color = "Temperature")) +
  geom_point(aes(y = Temperature)) +
  scale_x_discrete(limits = month.name) +
  scale_y_continuous(
    expression("Average Monthly Temperature " ( degree*C)), 
    sec.axis = sec_axis(~ . * 4, name = "Monthly Precipitation (mm)")
  ) +
  scale_colour_manual("", values = c("Temperature" = "black")) +
  scale_fill_manual("", values = "orange") +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, hjust=1),
        legend.position = "bottom") +
  facet_wrap(~Year)



ggplot(m_daily, aes(x = Date, y = P2.VW_Avg)) +
  geom_bar(stat = "identity") +
  scale_x_date(date_breaks = "6 month", date_labels = "%b %Y") +
  labs(x = "", y = " Soil Moisture Average", title = "Soil Moisture at Pinyon Flats from 2020-2023") +
  theme(plot.title = element_text(size = 15), 
        axis.title = element_text(size = 15),
        axis.text.x = element_text(size=6.5), 
        panel.background = element_blank())



