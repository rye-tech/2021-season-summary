#load libraries #############################

library(ggplot2)
library(scales)
library(grid)
library(dplyr)
library(here)


##### set working directory ###############################

setwd(here())

getwd()

# loading in  data ##############

# load tidied data

rm(list =ls())

load("data/eos-met/EOS_MET_20191120-20201219.RData")


df1 <- eos_met_2020
rm(eos_met_2020)


colnames(df1)

# [1] "datetime"  "TIMESTAMP" "RECORD"    "BattV"     "AirTC"     "RH"        "BP_mmHg"  
# [8] "WS_ms"     "WindDir"   "PAR_Den"   "PAR_Tot"   "SlrkW"     "SlrMJ"     "Rain_mm" 

summary(df1$datetime)

# season time bounds
t1 <-  "2020-05-01 00:00:00"
t2 <- "2020-12-02 00:00:00"



# plotting Air temperature ####

summary(df1$AirTC)

p = ggplot(df1, aes(datetime, AirTC))
a1=p + geom_point(aes(), size = 0.25, color = "red3") +
  xlab("") + #last x label sets the time axis label
  ylab("Air Temp (DegC)")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(4, 40) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(a1)


# plotting Wind speed ####

summary(df1$WS_ms)

p = ggplot(df1, aes(datetime, WS_ms))
a2=p + geom_point(aes(), size = 0.25, color = "blue2") +
  xlab("") + #last x label sets the time axis label
  ylab("Wind Speed (m/s)")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 16) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(a2)


# plotting Wind direction ####

summary(df1$WindDir)

p = ggplot(df1, aes(datetime, WindDir))
a3=p + geom_point(aes(), size = 0.25, color = "steel blue") +
  xlab("") + #last x label sets the time axis label
  ylab("Wind Dir (deg)")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 360) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(a3)



# plotting rain accumulation ####

summary(df1$Rain_mm)

p = ggplot(df1, aes(datetime, Rain_mm))
a4=p + geom_point(aes(), size = 0.25, color = "blue") +
  xlab("") + #last x label sets the time axis label
  ylab("Rain Accum (mm)")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 2) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(a4)




# plotting atmospheric pressure with x-axis and months ####



df1$BP_mbar <- df1$BP_mmHg * 1.33322

summary(df1$BP_mbar)

p = ggplot(df1, aes(datetime, BP_mbar))
a5=p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab("EOS Met May to Dec 2020") + #last x label sets the time axis label
  ylab("Air Press (mbar)")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(990, 1040) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(a5)




# plotting relative humidity ####

summary(df1$RH)

p = ggplot(df1, aes(datetime, RH))
a6=p + geom_point(aes(), size = 0.25, color = "purple2") +
  xlab("") + #last x label sets the time axis label
  ylab("Relative Humidity (%)")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 100) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(a6)



# plotting photosynthetically active radiation ####

summary(df1$PAR_Den)

p = ggplot(df1, aes(datetime, PAR_Den))
a7=p + geom_point(aes(), size = 0.25, color = "gold") +
  xlab("") + #last x label sets the time axis label
  ylab("PAR Density (umol/s*m^2)")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 2800) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(a7)




# saving plots into one file ####



#the following plots aligned time series into one .png with 4K HD aspect ratios

# annual plots

# ggsave("plots/eos-met/airT_ws_wdir_press_Dec_01_2019_to_Dec_01_2020_eos-met.png",
#        plot = grid.draw(rbind(ggplotGrob(a1), ggplotGrob(a2), ggplotGrob(a3), ggplotGrob(a5), 
#                               size = "last")), width = 6.65, height = 3.5)
# 
# 
# 
# ggsave("plots/eos-met/par_rain_rh_press_Dec_01_2019_to_Dec_01_2020_eos-met.png",
#        plot = grid.draw(rbind(ggplotGrob(a7), ggplotGrob(a4), ggplotGrob(a6), ggplotGrob(a5), 
#                               size = "first")), width = 6.65, height = 3.5)


#last 6 months plotted

ggsave("plots/eos-met/airT_ws_wdir_press_May_01_2020_to_Dec_01_2020_eos-met.png",
       plot = grid.draw(rbind(ggplotGrob(a1), ggplotGrob(a2), ggplotGrob(a3), ggplotGrob(a5), 
                              size = "last")), width = 6.65, height = 3.5)



ggsave("plots/eos-met/par_rain_rh_press_May_01_2020_to_Dec_01_2020_eos-met.png",
       plot = grid.draw(rbind(ggplotGrob(a7), ggplotGrob(a4), ggplotGrob(a6), ggplotGrob(a5), 
                              size = "first")), width = 6.65, height = 3.5)







