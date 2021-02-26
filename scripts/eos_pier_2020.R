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

load("data/eos-pier/EOS_YSI_20191120-20201219.RData")


df1 <- eos_pier_2020
rm(eos_pier_2020)


summary(df1$datetime)

# season time bounds
t1 <-  "2019-12-01 00:00:00"
t2 <- "2020-12-02 00:00:00"



# plotting oxygen ####

summary(df1$o2_mg_l)

p = ggplot(df1, aes(datetime, o2_mg_l))
a1=p + geom_point(aes(), size = 0.25, color = "blue") +
  xlab("") + #last x label sets the time axis label
  ylab("DO (mg/L)")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(5, 10) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(a1)


# plotting pH ####

summary(df1$pH)

p = ggplot(df1, aes(datetime, pH))
a2=p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab("") + #last x label sets the time axis label
  ylab("pH")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(7.5, 8.1) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(a2)


# plotting salinity ####

summary(df1$sss)

p = ggplot(df1, aes(datetime, sss))
a3=p + geom_point(aes(), size = 0.25, color = "steel blue") +
  xlab("") + #last x label sets the time axis label
  ylab("Sal (PSS)")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(16, 33) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(a3)



# plotting temperature ####

summary(df1$sst)

p = ggplot(df1, aes(datetime, sst))
a4=p + geom_point(aes(), size = 0.25, color = "red") +
  xlab("") + #last x label sets the time axis label
  ylab("Temp (degC)")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(9, 22) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(a4)




# plotting temperature with x-axis and months ####


p = ggplot(df1, aes(datetime, sst))
a5=p + geom_point(aes(), size = 0.25, color = "red") +
  xlab("EOS Pier Dec 2019 to Dec 2020") + #last x label sets the time axis label
  ylab("Temp (degC)")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(9, 22) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(a5)




# plotting depth ####

summary(df1$Depth)

p = ggplot(df1, aes(datetime, Depth))
a6=p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("EOS Pier Dec 2019 to Dec 2020") + #last x label sets the time axis label
  ylab("Depth (m)")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 3) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(a6)



# plotting turbidity ####

summary(df1$Turb)

p = ggplot(df1, aes(datetime, Turb))
a7=p + geom_point(aes(), size = 0.25, color = "brown") +
  xlab("") + #last x label sets the time axis label
  ylab("Turb (NTU)")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 250) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(a7)



# plotting chlorophyll ####

summary(df1$chl_ugl)

p = ggplot(df1, aes(datetime, chl_ugl))
a8=p + geom_point(aes(), size = 0.25, color = "green3") +
  xlab("") + #last x label sets the time axis label
  ylab("Chl (ug/L)")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 40) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(a8)




# saving plots into one file ####



#the following plots aligned time series into one .png with 4K HD aspect ratios


ggsave("plots/eos-pier/O_pH_T_S_Dec_01_2019_to_Dec_01_2020_eos-pier.png",
       plot = grid.draw(rbind(ggplotGrob(a1), ggplotGrob(a2), ggplotGrob(a3), ggplotGrob(a5), 
                              size = "last")), width = 6.65, height = 3.5)



ggsave("plots/eos-pier/O_S_T_D_Dec_01_2019_to_Dec_01_2020_eos-pier.png",
       plot = grid.draw(rbind(ggplotGrob(a1), ggplotGrob(a3), ggplotGrob(a4), ggplotGrob(a6), 
                              size = "first")), width = 6.65, height = 3.5)


ggsave("plots/eos-pier/trb_chl_S_T_D_Dec_01_2019_to_Dec_01_2020_eos-pier.png",
       plot = grid.draw(rbind(ggplotGrob(a7), ggplotGrob(a8), ggplotGrob(a3),
                              ggplotGrob(a4), ggplotGrob(a6),
                              size = "first")), width = 6.65, height = 3.5)
