# plotting SeaFET/CTD Combo and SeapHOx together




# load packages -----------------------------------------------------------



library(ggplot2)
library(scales)
library(grid)
library(lubridate)
library(dplyr)
library(here)




# set working directory ---------------------------------------------------



# use at your peril

# rm(list=ls())

setwd(here())

getwd()





# load tidied data --------------------------------------------------------





# pre-deployment bay water bath check samples
load("data/chk-samples/bath.check.samples-2020.RData")




#load BOB predeployment common bath 
load("tidied-data/bob/seafet/bob-pre.deploy.bath-2020-prcsd.RData")

bob.pre.deploy.bath <- df1
rm(df1)




#load predeployment bay water bath
load("tidied-data/mari/mari-baywater-bath-predeploy.2020.sphx.RData")
mari.pre.deploy.bath <- df1
rm(df1)




# building comparison plots -----------------------------------------------






# bob date bounds

head(bob.pre.deploy.bath)
"2020-07-24 06:00:00"

tail(bob.pre.deploy.bath)
"2020-09-24 05:00:00"


# mari date bounds

head(mari.pre.deploy.bath)
"2020-07-24 02:39:08"

tail(mari.pre.deploy.bath)
"2020-09-24 05:00:01"



#event time bounds
t1 <-  '2020-07-24 01:00:00'
t2 <- '2020-09-24 06:00:00'





p = ggplot(mari.pre.deploy.bath, aes(datetime))
pre.b.phs = p + geom_point(aes(y = pH_int), size = 0.25, color = "seagreen") +
  geom_point(aes(y = pH_ext), size = 0.25, color = "green") +
  geom_point(data = bob.pre.deploy.bath, aes(x=datetime, y= pH_int), size = 0.25,
             color = "deepskyblue4")  +
  geom_point(data = bob.pre.deploy.bath, aes(x=datetime, y= pH_ext), size = 0.25,
             color = "deepskyblue")  +
  xlab("") + #last x label sets the time axis label
  ylab("pH int (mari dark green, bob dark blue)\n ext (mari light green, bob light blue)\n check samples (black)\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(8.0, 8.2) +
  geom_point(data = bth.chk.df, aes(x=datetime, y= pH), shape = 18, size = 2,
             color = "black", show.legend = F)  +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  theme(legend.title=element_blank(), legend.position = "right") 


print(pre.b.phs)




p = ggplot(mari.pre.deploy.bath, aes(datetime, ctd_sal))
pre.b.s = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  geom_point(data = bob.pre.deploy.bath, aes(x=datetime, y= ctd_sal), size = 0.25,
             color = "green")  +
  xlab("") + #last x label sets the time axis label
  ylab("S (bob light, mari dark)\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(25, 33) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(pre.b.s)




p = ggplot(mari.pre.deploy.bath, aes(datetime, ctd_temp))
pre.b.t.dt = p + geom_point(aes(), size = 0.25, color = "red4") +
  geom_point(data = bob.pre.deploy.bath, aes(x=datetime, y= ctd_temp), size = 0.25,
             color = "red")  +
  xlab("Pre-Deployment Bay Water Bath BOB & MARI 2020") + #last x label sets the time axis label
  ylab("T (bob thermistor light, mari ctd dark)\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(16, 22) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(pre.b.t.dt)














# saving plots ------------------------------------------------------------


ggsave("plots/BOB-n-MARI-pre-deployment_bath_pHs_T_S_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.b.phs), 
                              ggplotGrob(pre.b.s), ggplotGrob(pre.b.t.dt),
                              size = "first")), width = 6.65, height = 3.5)































