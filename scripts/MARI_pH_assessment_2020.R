#load libraries #############################

library(ggplot2)
library(scales)
library(grid)
library(lubridate)
library(dplyr)
library(here)



##### set working directory ###############################

# clear work space at your peril...
# rm(list=ls())

setwd(here())

getwd()


# load scatter plot regression function #####################################################

# found this nifty function here:
# https://community.rstudio.com/t/annotate-ggplot2-with-regression-equation-and-r-squared/6112/7 



ggplotRegression <- function(dat, xvar, yvar){
  
  fml <- paste(yvar, "~", xvar)
  
  fit <- lm(fml, dat)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}



######load tidied data ####################################

#load instrument specifications
#load("data/mari.2020.screen.RData")


#load predeployment bay water bath
load("tidied-data/mari/mari-baywater-bath-predeploy.2020.sphx.RData")
pre.deploy.bath <- df1
rm(df1)

# bath check samples
load("data/chk-samples/bath.check.samples-2020.RData")


#load predeployment Dickson Standard Run
load("tidied-data/mari/mari-pre-deploy-dickson-run-prcsd-2020.RData")
pre.dickson <- df1
rm(df1)


#intstrument data
# load("tidied-data/mari.2020.RData")
# sfx.df <- data
# rm(data)

#load MID Deployment Field Bath (5/21/2020)
#load("tidied-data/mari-field-bath-20200521.sphx.RData")
#field.bath <- df1
#rm(df1)

# field check samples
#load("tidied-data/mari-check.samples-2020.RData")



#load POST deployment common bath and check samples
# load("tidied-data/mari-common-bath-post.deploy.2020.sphx.RData")
# post.deploy.bath <- df1
# rm(df1)

#load POST deployment Dickson Standard Run
# load("tidied-data/mari-post-deploy-dickson-run-prcsd-2020.RData")
# post.dickson <- df1
# rm(df1)


#adjusted data with local calibration constant
# DID NOT PROVE TO BE BETTER THAN THE FACTORY CALIBRATION

# load("2020 data/mari.2020.adj.final.RData")
# adj.sfx.df <- df1
# rm(df1)

# PRE deployment bath plots ####################################################################


head(pre.deploy.bath)
"2020-07-24 02:39:08"

tail(pre.deploy.bath)
"2020-09-24 05:00:01"

#event time bounds
t1 <-  '2020-07-24 01:39:08'
t2 <- '2020-09-24 06:00:01'

max_int_v <- max(pre.deploy.bath$pH_int_v)
min_int_v <- min(pre.deploy.bath$pH_int_v)

p = ggplot(pre.deploy.bath, aes(x = datetime))
pre.b.int.v = p + geom_point(aes(y = pH_int_v), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(min_int_v, max_int_v) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(pre.b.int.v)

max_ext_v <- max(pre.deploy.bath$pH_ext_v)
min_ext_v <- min(pre.deploy.bath$pH_ext_v)



p = ggplot(pre.deploy.bath, aes(datetime, pH_ext_v))
pre.b.ext.v = p + geom_point(aes(), size = 0.25, color = "green") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(min_ext_v, max_ext_v) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(pre.b.ext.v)

summary(pre.deploy.bath$abs_v_diff)

p = ggplot(pre.deploy.bath, aes(datetime, abs_v_diff))
pre.b.abs.v_diff =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.03, 0.065) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.04, linetype="dashed", 
             color = "yellow", size=1) + 
  geom_hline(yintercept=0.065, linetype="dashed", 
             color = "red", size=1) +
  labs(caption = "yellow conservative line suggested by Rivest et al. 2016. may not apply to estuaries.")

print(pre.b.abs.v_diff)



p = ggplot(pre.deploy.bath, aes(datetime))
pre.b.phs = p + geom_point(aes(y = pH_int), size = 0.25, color = "seagreen") +
  geom_point(aes(y = pH_ext), size = 0.25, color = "green") +
  xlab("") + #last x label sets the time axis label
  ylab("pH int (dark green)\n ext (light green)\n check samples (black)\n")+ 
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



p = ggplot(pre.deploy.bath, aes(datetime, abs_pH_diff))
pre.b.pH.diff = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("electrode abs diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.1) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=0.5) +
  labs(caption = "red line threshold suggested by Seabird Electronics as suspect")


print(pre.b.pH.diff)


p = ggplot(pre.deploy.bath, aes(datetime, ctd_sal))
pre.b.s = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab("") + #last x label sets the time axis label
  ylab("S\n")+ 
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


p = ggplot(pre.deploy.bath, aes(datetime, ctd_sal))
pre.b.s.dt = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab("Pre-Deployment Bay Water Bath MARI 2020") + #last x label sets the time axis label
  ylab("S\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(25, 33) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(pre.b.s.dt)


summary(pre.deploy.bath$ctd_temp)

p = ggplot(pre.deploy.bath, aes(datetime, ctd_temp))
pre.b.t.dt = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("Pre-Deployment Bay Water Bath MARI 2020") + #last x label sets the time axis label
  ylab("T\n")+ 
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




# regression plots

# combine check samples to instrument data in a new data frame

# df1 <- left_join(pre.deploy.bath, bth.chk.df, by = "datetime")
# 
# 
# # Discrete spec pH regressed versus pH
# 
# k0.int.pre.b.benchmark <- ggplotRegression(df1, "pH", "pH_int_v")
# 
# print(k0.int.pre.b.benchmark)
# 
# 
# k0.ext.pre.b.benchmark <- ggplotRegression(df1, "pH", "pH_ext_v")
# 
# print(k0.ext.pre.b.benchmark)
# 
# rm(df1)






# PRE deployment Dickson TRIS Seawater Plots ##############################


#creating Dickson Standard Line to be plotted alongside instrument data

# test_data <- read.csv("test_data_dickson_line.csv",
#                       header=T, stringsAsFactors=F, sep=",")
# 
# save(test_data, file = "dickson.line.data-test.RData")

# df = data frame that has data of interest
# T_DegC = temperature of thermistor local to instrument, example call:  "df$ctd_temp"  
# pH_int & pH_ext = pH of internal and external electrode, example call: "df$pH_int" 
# sal_std = all seawater TRIS buffer Dickson standards are mixed to Nominal salinity of 35
# offset_std = offset (not needed with Dickson Standard)

sal_std <- 35  
offset_std <- 0 #for Dickson standard

#df <- test_data

pre.dickson$DL = (11911.08 - 18.2499*sal_std - 0.039336*sal_std^2)/(pre.dickson$pH_temp + 273.15) + 
  (-366.27059 + 0.53993607*sal_std + 0.00016329*sal_std^2) + 
  (64.52243 - 0.084041*sal_std)*log(pre.dickson$pH_temp+273.15) - 0.11149858*(pre.dickson$pH_temp +273.15)  +  offset_std


pre.dickson$DL_pH_diff_int <- abs(pre.dickson$pH_int_cell - pre.dickson$DL)
pre.dickson$DL_pH_diff_ext <- abs(pre.dickson$pH_ext_cell - pre.dickson$DL)

#determine data time bounds

head(pre.dickson)

"2020-07-03 00:27:29"

tail(pre.dickson)

"2020-07-24 01:20:15"

#event time bounds
t1 <-  '2020-07-02 23:27:29'
t2 <- '2020-07-24 02:20:15'

# t3 <- '2020-03-08 20:00:00'

summary(pre.dickson$pH_int_v)

# Min.         1st Qu.          Median            Mean         3rd Qu.            Max. 
# -0.894020000000 -0.892518500000 -0.891735000000 -0.891662267373 -0.890885000000 -0.888929000000  

# will add this in later when we determine our screens with more seasons of data
#print(mari.2020.screen.df$int.v.min)
# -1.06
#print(mari.2020.screen.df$int.v.max) 
# -1.02


#adjusting plotting range for pH_int_v because it is out of range...

p = ggplot(pre.dickson, aes(x = datetime))
pre.d.int.v = p + geom_point(aes(y = pH_int_v), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-0.895, -0.887) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(pre.d.int.v)


summary(pre.dickson$pH_ext_v)
# Min.         1st Qu.          Median            Mean         3rd Qu.            Max. 
# -0.857622000000 -0.848725500000 -0.847968000000 -0.847801702184 -0.846808000000 -0.844947000000 

#adjusting plotting range for pH_int_v because it is out of range...

#print(mari.2020.screen.df$ext.v.min)
# -1.01
#print(mari.2020.screen.df$ext.v.max) 
# -0.97

p = ggplot(pre.dickson, aes(datetime, pH_ext_v))
pre.d.ext.v = p + geom_point(aes(), size = 0.25, color = "green") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-0.855, -0.843) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(pre.d.ext.v)




p = ggplot(pre.dickson, aes(datetime, abs_v_diff))
pre.d.v_diff =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.03, 0.065) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.04, linetype="dashed", 
             color = "yellow", size=1) + 
  geom_hline(yintercept=0.065, linetype="dashed", 
             color = "red", size=1) +
  labs(caption = "yellow conservative line suggested by Rivest et al. 2016. may not apply to estuaries.")

print(pre.d.v_diff)

summary(pre.dickson$pH_int_cell)
summary(pre.dickson$pH_ext_cell)


p = ggplot(pre.dickson, aes(x = datetime))
pre.d.pHs = p + geom_point(aes(y = pH_int_cell), size = 0.25, color = "seagreen") +
  geom_point(aes(y = pH_ext_cell), size = 0.25, color = "green") +
  geom_point(aes(y = DL), size = 0.25, color = "black") +
  xlab(" ") + #last x label sets the time axis label
  ylab("pH int (light green)\n ext (dark green)\n TRIS (black)\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(8.1, 8.4) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(pre.d.pHs)



p = ggplot(pre.dickson, aes(datetime, abs_pH_diff))
pre.d.pH.diff = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("abs electrode diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.1) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=0.5) +
  labs(caption = "red line threshold suggested by Seabird Electronics as suspect")

print(pre.d.pH.diff)




p = ggplot(pre.dickson, aes(x =datetime))
pre.d.DL.diff = p + geom_point(aes(y = DL_pH_diff_int), size = 0.25, color = "seagreen") +
  geom_point(aes(datetime, DL_pH_diff_ext), size = 0.25, color = "green") +
  xlab("Pre-Deployment Dickson TRIS Run for MARI 2020") + #last x label sets the time axis label
  ylab("abs offset from Dickson\n pH_int (dark green) pH_ext (light green) \n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.1) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=1.0)


print(pre.d.DL.diff)

summary(pre.dickson$pH_temp)

# Min.       1st Qu.        Median          Mean       3rd Qu.          Max. 
# 17.0306000000 17.9843000000 18.4853000000 18.4283678359 18.9045000000 20.9791000000

p = ggplot(pre.dickson, aes(datetime, pH_temp))
pre.d.t.dt = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("Pre-Deployment Dickson TRIS Run for MARI 2020") + #last x label sets the time axis label
  ylab("T\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(17.0, 21.0) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  labs(caption = "note: temperature from local thermistor on SeaFET or SeapHOx. Salinity is nominally 35 PSU")

print(pre.d.t.dt)

# no salinity plot, salinity is a nominal 35



# saving plots of pre deployment runs ##############

ls()

# [1] "bth.chk.df"       "ggplotRegression" "max_ext_v"       
# [4] "max_int_v"        "min_ext_v"        "min_int_v"       
# [7] "offset_std"       "p"                "pre.b.abs.v_diff"
# [10] "pre.b.ext.v"      "pre.b.int.v"      "pre.b.pH.diff"   
# [13] "pre.b.phs"        "pre.b.s"          "pre.b.s.dt"      
# [16] "pre.b.t.dt"       "pre.d.DL.diff"    "pre.d.ext.v"     
# [19] "pre.d.int.v"      "pre.d.pH.diff"    "pre.d.pHs"       
# [22] "pre.d.t.dt"       "pre.d.v_diff"     "pre.deploy.bath" 
# [25] "pre.dickson"      "sal_std"          "t1"              
# [28] "t2"                




ggsave("plots/mari/MARI-pre.deployment_dickson.std.volts.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.int.v), ggplotGrob(pre.d.ext.v),
                              ggplotGrob(pre.d.v_diff), ggplotGrob(pre.d.t.dt), 
                              size = "first")), width = 6.65, height = 3.5)



ggsave("plots/mari/MARI-pre.deployment_dickson.std.pHs.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.pHs), ggplotGrob(pre.d.pH.diff),
                              ggplotGrob(pre.d.t.dt), 
                              size = "first")), width = 6.65, height = 3.5)

# save pre-deployment bath plots

ggsave("plots/mari/MARI-pre-deployment_bath_Volt_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.b.int.v), ggplotGrob(pre.b.ext.v),
                              ggplotGrob(pre.b.abs.v_diff), ggplotGrob(pre.b.s.dt),
                              size = "first")), width = 6.65, height = 3.5)

ggsave("plots/mari/MARI-pre-deployment_bath_pHs_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.b.phs), ggplotGrob(pre.b.pH.diff),
                              ggplotGrob(pre.b.s.dt),
                              size = "first")), width = 6.65, height = 3.5)

ggsave("plots/mari/MARI-pre-deployment_bath_T_S_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.b.s), ggplotGrob(pre.b.t.dt),
                              size = "first")), width = 6.65, height = 3.5)

ggsave("plots/mari/MARI-pre-deployment_bath_pHs_T_S_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.b.phs), ggplotGrob(pre.b.pH.diff),
                              ggplotGrob(pre.b.s), ggplotGrob(pre.b.t.dt),
                              size = "first")), width = 6.65, height = 3.5)









# adjusted PRE deployment Dickson standard plots to more stable section #######################

t3 <- '2020-03-08 20:00:00'
pre.dickson.trim <- filter(pre.dickson, datetime > t3)

p = ggplot(pre.dickson.trim, aes(x = datetime))
pre.d.int.v.trim = p + geom_point(aes(y = pH_int_v), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t3, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.02, -1.01) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(pre.d.int.v.trim)


p = ggplot(pre.dickson.trim, aes(datetime, pH_ext_v))
pre.d.ext.v.trim = p + geom_point(aes(), size = 0.25, color = "green") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t3, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-0.97, -0.965) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(pre.d.ext.v.trim)




p = ggplot(pre.dickson.trim, aes(datetime, abs_v_diff))
pre.d.v_diff.trim =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t3, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.03, 0.065) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.04, linetype="dashed", 
             color = "yellow", size=1) + 
  geom_hline(yintercept=0.065, linetype="dashed", 
             color = "red", size=1) +
  labs(caption = "yellow conservative line suggested by Rivest et al. 2016. may not apply to estuaries.")

print(pre.d.v_diff.trim)






p = ggplot(pre.dickson.trim, aes(x = datetime))
pre.d.pHs.trim = p + geom_point(aes(y = pH_int_cell), size = 0.25, color = "seagreen") +
  geom_point(aes(y = pH_ext_cell), size = 0.25, color = "green") +
  geom_point(aes(y = DL), size = 0.25, color = "black") +
  xlab(" ") + #last x label sets the time axis label
  ylab("pH comparison\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t3, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(8.2, 8.5) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(pre.d.pHs.trim)


p = ggplot(pre.dickson.trim, aes(datetime, abs_pH_diff))
pre.d.pH.diff.trim = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("abs electrode diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t3, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.1) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=1.0) +
  labs(caption = "red line threshold suggested by Seabird Electronics as suspect")

print(pre.d.pH.diff.trim)



p = ggplot(pre.dickson.trim, aes(x =datetime))
pre.d.DL.diff.trim = p + geom_point(aes(y = DL_pH_diff_int), size = 0.25, color = "seagreen") +
  geom_point(aes(datetime, DL_pH_diff_ext), size = 0.25, color = "green") +
  xlab("") + #last x label sets the time axis label
  ylab("abs offset from Dickson\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t3, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.1) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=1.0)


print(pre.d.DL.diff.trim)


p = ggplot(pre.dickson.trim, aes(datetime, pH_temp))
pre.d.t.dt.trim = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("Pre-Deployment Dickson Standard Run for MARI March 8-9th 2020") + #last x label sets the time axis label
  ylab("T\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t3, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(14.0, 17.0) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  labs(caption = "note: temperature from local thermistor on SeaFET or SeapHOx. Salinity is nominally 35 PSU")

print(pre.d.t.dt.trim)

# no salinity plot, salinity is a nominal 35


# regression plots

# Dickson predicted versus pH
pre.d.DL.v.pH.int.trim <- ggplotRegression(pre.dickson.trim, "DL", "pH_int_v")

print(pre.d.DL.v.pH.int.trim)


pre.d.DL.v.pH.ext.trim <- ggplotRegression(pre.dickson.trim, "DL", "pH_ext_v")

print(pre.d.DL.v.pH.ext.trim)



# Dickson predicted versus pH
pre.d.pH.int.v.Volt.int.trim <- ggplotRegression(pre.dickson.trim, "pH_int_cell", "pH_int_v")

print(pre.d.pH.int.v.Volt.int.trim)


pre.d.pH.ext.v.Volt.ext.trim <- ggplotRegression(pre.dickson.trim, "pH_ext_cell", "pH_ext_v")

print(pre.d.pH.ext.v.Volt.ext.trim)




# DEPLOYMENT PLOTS #####################################################################

# find data time bounds
head(sfx.df) 
tail(sfx.df)

#sfx season time bounds
#2020-03-11 19:20:00
#2020-10-09 18:20:00


#event time bounds
t1 <-  '2020-03-11 00:00:00'
t2 <- '2020-10-11 00:00:00'
#t2 <- '2020-03-26 00:00:00'

summary(sfx.df)

# datetime                      ctd_temp         pH_ext          pH_int         pH_ext_v          pH_int_v     
# Min.   :2020-03-11 19:20:00   Min.   :10.95   Min.   :6.133   Min.   :7.608   Min.   :-1.0072   Min.   :-1.056  
# 1st Qu.:2020-05-03 17:45:00   1st Qu.:13.72   1st Qu.:7.729   1st Qu.:7.727   1st Qu.:-0.9974   1st Qu.:-1.048  
# Median :2020-06-25 19:30:00   Median :15.28   Median :7.802   Median :7.804   Median :-0.9930   Median :-1.044  
# Mean   :2020-06-25 19:02:42   Mean   :15.31   Mean   :7.816   Mean   :7.819   Mean   :-0.9923   Mean   :-1.043  
# 3rd Qu.:2020-08-17 19:55:00   3rd Qu.:16.81   3rd Qu.:7.912   3rd Qu.:7.905   3rd Qu.:-0.9870   3rd Qu.:-1.038  
# Max.   :2020-10-09 18:20:00   Max.   :20.41   Max.   :8.146   Max.   :8.133   Max.   :-0.9754   Max.   :-1.026  

# pH_temp        press_dbar       ctd_sal           cond_Sm         ctd_o2_ml_l      ctd_o2_mg_l           RH       
# Min.   :10.99   Min.   : 0.01   Min.   : 0.2497   Min.   :0.04037   Min.   : 3.431   Min.   : 4.903   Min.   :0.000  
# 1st Qu.:13.74   1st Qu.:16.60   1st Qu.:26.7176   1st Qu.:3.35087   1st Qu.: 4.636   1st Qu.: 6.625   1st Qu.:0.200  
# Median :15.30   Median :17.40   Median :28.8362   Median :3.59112   Median : 4.959   Median : 7.087   Median :2.000  
# Mean   :15.33   Mean   :17.40   Mean   :27.9815   Mean   :3.53537   Mean   : 5.057   Mean   : 7.226   Mean   :2.219  
# 3rd Qu.:16.82   3rd Qu.:18.23   3rd Qu.:30.2007   3rd Qu.:3.84145   3rd Qu.: 5.425   3rd Qu.: 7.752   3rd Qu.:4.100  
# Max.   :20.35   Max.   :20.62   Max.   :32.0586   Max.   :4.08976   Max.   :13.234   Max.   :18.912   Max.   :5.900  

# pH_int_temp        QARTOD         pH_diff       
# Min.   : 9.30   Min.   :1.000   Min.   :0.00000  
# 1st Qu.:13.80   1st Qu.:1.000   1st Qu.:0.01260  
# Median :15.40   Median :1.000   Median :0.02320  
# Mean   :15.39   Mean   :1.076   Mean   :0.03047  
# 3rd Qu.:16.90   3rd Qu.:1.000   3rd Qu.:0.03660  
# Max.   :20.40   Max.   :3.000   Max.   :1.82620 

p = ggplot(sfx.df, aes(datetime, pH_int_v))
sfx.int.v =p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.06, -1.02) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(sfx.int.v)



p = ggplot(sfx.df, aes(datetime, pH_ext_v))
sfx.ext.v = p + geom_point(aes(), size = 0.25, color = "green") +
  xlab("") + #last x label sets the time axis label
  ylab("V ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.015, -0.97) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(sfx.ext.v)

p = ggplot(sfx.df, aes(datetime, abs_v_diff))
sfx.v_diff =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.03, 0.065) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.04, linetype="dashed", 
             color = "yellow", size=1) + 
  geom_hline(yintercept=0.065, linetype="dashed", 
             color = "red", size=1) +
  labs(caption = "yellow conservative line suggested by Rivest et al. 2016. may not apply to estuaries.")

print(sfx.v_diff)

p = ggplot(sfx.df, aes(datetime, pH_int))
sfx.pH_int = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("pH int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(7.2, 8.2) +
  geom_point(data = chk.df, aes(x=datetime, y= pH.check.median), shape = 18, size = 1,
             color = "black", show.legend = F)  +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(sfx.pH_int)



p = ggplot(sfx.df, aes(datetime, pH_ext))
sfx.pH_ext = p + geom_point(aes(), size = 0.25, color = "green") +
  xlab("") + #last x label sets the time axis label
  ylab("pH ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(7.2, 8.2) +
  geom_point(data = chk.df, aes(x=datetime, y= pH.check.median), shape = 18, size = 1,
             color = "black", show.legend = F)  +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(sfx.pH_ext)



p = ggplot(sfx.df, aes(datetime))
sfx.phs = p + geom_point(aes(y = pH_int), size = 0.25, color = "seagreen") +
  geom_point(aes(y = pH_ext), size = 0.25, color = "green") +
  xlab("") + #last x label sets the time axis label
  ylab("pH int, ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(7.2, 8.2) +
  geom_point(data = chk.df, aes(x=datetime, y= pH.check.median), shape = 18, size = 1,
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


print(sfx.phs)


summary(sfx.df$abs_pH_diff)

p = ggplot(sfx.df, aes(datetime, abs_pH_diff))
sfx.pH.diff = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("pH abs-diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.35) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=1.0) +
  labs(caption = "red line threshold suggested by Seabird Electronics as suspect")


print(sfx.pH.diff)


p = ggplot(sfx.df, aes(datetime, abs_pH_diff))
sfx.pH.diff.no.note = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("pH abs-diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.35) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=1.0)


print(sfx.pH.diff.no.note)



p = ggplot(sfx.df, aes(datetime, ctd_sal))
sfx.sal = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("S\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(8, 32) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(sfx.sal)


p = ggplot(sfx.df, aes(datetime, ctd_sal))
sfx.sal.dt = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab("Deployment MARI 2020") + #last x label sets the time axis label
  ylab("S\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(8, 32) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(sfx.sal.dt)



p = ggplot(sfx.df, aes(datetime, ctd_temp))
sfx.temp = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("") + #last x label sets the time axis label
  ylab("T\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(9, 21) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(sfx.temp)


p = ggplot(sfx.df, aes(datetime, ctd_o2_mg_l))
sfx.o2 = p + geom_point(aes(), size = 0.25, color = "red") +
  xlab("Deployment MARI 2020") + #last x label sets the time axis label
  ylab("O2 (mg/L)\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(4, 10) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(sfx.o2)


#regression plots

sfx.ph.int.v.o2 <- ggplotRegression(sfx.df, "pH_int", "ctd_o2_mg_l")

print(sfx.ph.int.v.o2)


sfx.ph.ext.v.o2 <- ggplotRegression(sfx.df, "pH_ext", "ctd_o2_mg_l")

print(sfx.ph.ext.v.o2)


#removing outliers

max_o2 = 10

min_pH = 7

df1 <- filter(sfx.df, ctd_o2_mg_l < max_o2)
df1 <- filter(df1, pH_ext > min_pH)

sfx.ph.int.v.o2.ol.rm <- ggplotRegression(df1, "pH_int", "ctd_o2_mg_l")

print(sfx.ph.int.v.o2.ol.rm)

sfx.ph.ext.v.o2.ol.rm <- ggplotRegression(df1, "pH_ext", "ctd_o2_mg_l")

print(sfx.ph.ext.v.o2.ol.rm)


# combine check samples to instrument data in a new data frame

# round sampling times of check samples since there is a slight mismatch in time

df3 <- chk.df
  
df3$datetime2 <- round_date(chk.df$datetime, "20 mins")

View(df3)

df2 <- left_join(sfx.df, df3, by = c("datetime" = "datetime2"))

View(df2)

# Discrete spec pH regressed versus pH

spc.ph.v.sfx.ph.int.v <- ggplotRegression(df2, "pH.check.median", "pH_int_v")

print(spc.ph.v.sfx.ph.int.v)


spc.ph.v.sfx.ph.ext.v <- ggplotRegression(df2, "pH.check.median", "pH_ext_v")

print(spc.ph.v.sfx.ph.ext.v)


spc.ph.v.sfx.ph.int <- ggplotRegression(df2, "pH.check.median", "pH_int")

print(spc.ph.v.sfx.ph.int)


spc.ph.v.sfx.ph.ext <- ggplotRegression(df2, "pH.check.median", "pH_ext")

print(spc.ph.v.sfx.ph.ext)


# subsetting more for k0
pH_max <- 8.1
pH_min <- 7.9


df2 <- filter(df2, pH_int > pH_min & pH_int < pH_max)


# Discrete spec pH regressed versus pH

spc.ph.v.sfx.ph.int.v.2 <- ggplotRegression(df2, "pH.check.median", "pH_int_v")

print(spc.ph.v.sfx.ph.int.v.2)


spc.ph.v.sfx.ph.ext.v.2 <- ggplotRegression(df2, "pH.check.median", "pH_ext_v")

print(spc.ph.v.sfx.ph.ext.v.2)


spc.ph.v.sfx.ph.int.2 <- ggplotRegression(df2, "pH.check.median", "pH_int")

print(spc.ph.v.sfx.ph.int.2)


spc.ph.v.sfx.ph.ext.2 <- ggplotRegression(df2, "pH.check.median", "pH_ext")

print(spc.ph.v.sfx.ph.ext.2)


rm(df2)
rm(df3)



# ADJUSTED DEPLOYMENT PLOTS #####################################################################

# find data time bounds
head(adj.sfx.df) 
tail(adj.sfx.df)

#adj.sfx season time bounds
#2020-03-11 19:20:00
#2020-10-09 18:20:00


#event time bounds
t1 <-  '2020-03-11 00:00:00'
t2 <- '2020-10-11 00:00:00'
#t2 <- '2020-03-26 00:00:00'

summary(adj.sfx.df)

# datetime                      ctd_temp         pH_ext          pH_int         pH_ext_v          pH_int_v     
# Min.   :2020-03-11 19:20:00   Min.   :10.95   Min.   :6.133   Min.   :7.608   Min.   :-1.0072   Min.   :-1.056  
# 1st Qu.:2020-05-03 17:45:00   1st Qu.:13.72   1st Qu.:7.729   1st Qu.:7.727   1st Qu.:-0.9974   1st Qu.:-1.048  
# Median :2020-06-25 19:30:00   Median :15.28   Median :7.802   Median :7.804   Median :-0.9930   Median :-1.044  
# Mean   :2020-06-25 19:02:42   Mean   :15.31   Mean   :7.816   Mean   :7.819   Mean   :-0.9923   Mean   :-1.043  
# 3rd Qu.:2020-08-17 19:55:00   3rd Qu.:16.81   3rd Qu.:7.912   3rd Qu.:7.905   3rd Qu.:-0.9870   3rd Qu.:-1.038  
# Max.   :2020-10-09 18:20:00   Max.   :20.41   Max.   :8.146   Max.   :8.133   Max.   :-0.9754   Max.   :-1.026  

# pH_temp        press_dbar       ctd_sal           cond_Sm         ctd_o2_ml_l      ctd_o2_mg_l           RH       
# Min.   :10.99   Min.   : 0.01   Min.   : 0.2497   Min.   :0.04037   Min.   : 3.431   Min.   : 4.903   Min.   :0.000  
# 1st Qu.:13.74   1st Qu.:16.60   1st Qu.:26.7176   1st Qu.:3.35087   1st Qu.: 4.636   1st Qu.: 6.625   1st Qu.:0.200  
# Median :15.30   Median :17.40   Median :28.8362   Median :3.59112   Median : 4.959   Median : 7.087   Median :2.000  
# Mean   :15.33   Mean   :17.40   Mean   :27.9815   Mean   :3.53537   Mean   : 5.057   Mean   : 7.226   Mean   :2.219  
# 3rd Qu.:16.82   3rd Qu.:18.23   3rd Qu.:30.2007   3rd Qu.:3.84145   3rd Qu.: 5.425   3rd Qu.: 7.752   3rd Qu.:4.100  
# Max.   :20.35   Max.   :20.62   Max.   :32.0586   Max.   :4.08976   Max.   :13.234   Max.   :18.912   Max.   :5.900  

# pH_int_temp        QARTOD         pH_diff       
# Min.   : 9.30   Min.   :1.000   Min.   :0.00000  
# 1st Qu.:13.80   1st Qu.:1.000   1st Qu.:0.01260  
# Median :15.40   Median :1.000   Median :0.02320  
# Mean   :15.39   Mean   :1.076   Mean   :0.03047  
# 3rd Qu.:16.90   3rd Qu.:1.000   3rd Qu.:0.03660  
# Max.   :20.40   Max.   :3.000   Max.   :1.82620 

p = ggplot(adj.sfx.df, aes(datetime, pH_int_v))
adj.sfx.int.v =p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.06, -1.02) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(adj.sfx.int.v)



p = ggplot(adj.sfx.df, aes(datetime, pH_ext_v))
adj.sfx.ext.v = p + geom_point(aes(), size = 0.25, color = "green") +
  xlab("") + #last x label sets the time axis label
  ylab("V ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.015, -0.97) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(adj.sfx.ext.v)

p = ggplot(adj.sfx.df, aes(datetime, abs_v_diff))
adj.sfx.v_diff =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.03, 0.065) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.04, linetype="dashed", 
             color = "yellow", size=1) + 
  geom_hline(yintercept=0.065, linetype="dashed", 
             color = "red", size=1) +
  labs(caption = "yellow conservative line suggested by Rivest et al. 2016. may not apply to estuaries.")

print(adj.sfx.v_diff)

p = ggplot(adj.sfx.df, aes(datetime, pH_int_cell))
adj.sfx.pH_int = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("pH int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(7.5, 8.5) +
  geom_point(data = chk.df, aes(x=datetime, y= pH.check.median), shape = 18, size = 1,
             color = "black", show.legend = F)  +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(adj.sfx.pH_int)



p = ggplot(adj.sfx.df, aes(datetime, pH_ext_cell))
adj.sfx.pH_ext = p + geom_point(aes(), size = 0.25, color = "green") +
  xlab("") + #last x label sets the time axis label
  ylab("pH ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(7.5, 8.5) +
  geom_point(data = chk.df, aes(x=datetime, y= pH.check.median), shape = 18, size = 1,
             color = "black", show.legend = F)  +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(adj.sfx.pH_ext)



p = ggplot(adj.sfx.df, aes(datetime))
adj.sfx.phs = p + geom_point(aes(y = pH_int_cell), size = 0.25, color = "seagreen") +
  geom_point(aes(y = pH_ext_cell), size = 0.25, color = "green") +
  xlab("") + #last x label sets the time axis label
  ylab("pH int, ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(7.5, 8.5) +
  geom_point(data = chk.df, aes(x=datetime, y= pH.check.median), shape = 18, size = 1,
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


print(adj.sfx.phs)


summary(adj.sfx.df$abs_pH_diff)

p = ggplot(adj.sfx.df, aes(datetime, abs_pH_diff))
adj.sfx.pH.diff = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("pH abs-diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.35) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=1.0) +
  labs(caption = "red line threshold suggested by Seabird Electronics as suspect")


print(adj.sfx.pH.diff)


p = ggplot(adj.sfx.df, aes(datetime, abs_pH_diff))
adj.sfx.pH.diff.no.note = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("pH abs-diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.35) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=1.0)


print(adj.sfx.pH.diff.no.note)



p = ggplot(adj.sfx.df, aes(datetime, ctd_sal))
adj.sfx.sal = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("S\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(8, 32) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(adj.sfx.sal)


p = ggplot(adj.sfx.df, aes(datetime, ctd_sal))
adj.sfx.sal.dt = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab("Deployment MARI 2020") + #last x label sets the time axis label
  ylab("S\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(8, 32) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(adj.sfx.sal.dt)



p = ggplot(adj.sfx.df, aes(datetime, ctd_temp))
adj.sfx.temp = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("") + #last x label sets the time axis label
  ylab("T\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(9, 21) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(adj.sfx.temp)


p = ggplot(adj.sfx.df, aes(datetime, ctd_o2_mg_l))
adj.sfx.o2 = p + geom_point(aes(), size = 0.25, color = "red") +
  xlab("Deployment MARI 2020") + #last x label sets the time axis label
  ylab("O2 (mg/L)\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(4, 10) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(adj.sfx.o2)


#regression plots

adj.sfx.ph.int.v.o2 <- ggplotRegression(adj.sfx.df, "pH_int_cell", "ctd_o2_mg_l")

print(adj.sfx.ph.int.v.o2)


adj.sfx.ph.ext.v.o2 <- ggplotRegression(adj.sfx.df, "pH_ext_cell", "ctd_o2_mg_l")

print(adj.sfx.ph.ext.v.o2)


#removing outliers

max_o2 = 10

min_pH = 7

df1 <- filter(adj.sfx.df, ctd_o2_mg_l < max_o2)
df1 <- filter(df1, pH_ext > min_pH)

adj.sfx.ph.int.v.o2.ol.rm <- ggplotRegression(df1, "pH_int", "ctd_o2_mg_l")

print(adj.sfx.ph.int.v.o2.ol.rm)

adj.sfx.ph.ext.v.o2.ol.rm <- ggplotRegression(df1, "pH_ext", "ctd_o2_mg_l")

print(adj.sfx.ph.ext.v.o2.ol.rm)


# combine check samples to instrument data in a new data frame

# round sampling times of check samples since there is a slight mismatch in time

df3 <- chk.df

df3$datetime2 <- round_date(chk.df$datetime, "20 mins")

View(df3)

df2 <- left_join(adj.sfx.df, df3, by = c("datetime" = "datetime2"))

View(df2)

# Discrete spec pH regressed versus pH


spc.ph.v.adj.sfx.ph.int <- ggplotRegression(df2, "pH.check.median", "pH_int_cell")

print(spc.ph.v.adj.sfx.ph.int)


spc.ph.v.adj.sfx.ph.ext <- ggplotRegression(df2, "pH.check.median", "pH_ext_cell")

print(spc.ph.v.adj.sfx.ph.ext)

# 
# # subsetting more for k0
# pH_max <- 8.1
# pH_min <- 7.9
# 
# 
# df2 <- filter(df2, pH_int > pH_min & pH_int < pH_max)
# 
# 
# # Discrete spec pH regressed versus pH
# 
# spc.ph.v.adj.sfx.ph.int.v.2 <- ggplotRegression(df2, "pH.check.median", "pH_int_v")
# 
# print(spc.ph.v.adj.sfx.ph.int.v.2)
# 
# 
# spc.ph.v.adj.sfx.ph.ext.v.2 <- ggplotRegression(df2, "pH.check.median", "pH_ext_v")
# 
# print(spc.ph.v.adj.sfx.ph.ext.v.2)
# 
# 
# spc.ph.v.adj.sfx.ph.int.2 <- ggplotRegression(df2, "pH.check.median", "pH_int")
# 
# print(spc.ph.v.adj.sfx.ph.int.2)
# 
# 
# spc.ph.v.adj.sfx.ph.ext.2 <- ggplotRegression(df2, "pH.check.median", "pH_ext")
# 
# print(spc.ph.v.adj.sfx.ph.ext.2)
# 

rm(df2)
rm(df3)



####  MID DEPLOYMENT FIELD BATH PLOTS #########################################


head(field.bath)
"2020-05-21 17:27:00"

tail(field.bath)
"2020-05-21 18:05:00"

#event time bounds
t1 <-  '2020-05-21 17:26:00'
t2 <- '2020-05-21 18:06:00'


p = ggplot(field.bath, aes(x = datetime))
mid.b.int.v = p + geom_point(aes(y = pH_int_v), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(mari.2020.screen.df$int.v.min, mari.2020.screen.df$int.v.max) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(mid.b.int.v)


p = ggplot(field.bath, aes(datetime, pH_ext_v))
mid.b.ext.v = p + geom_point(aes(), size = 0.25, color = "green") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(mari.2020.screen.df$ext.v.min, mari.2020.screen.df$ext.v.max) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(mid.b.ext.v)

summary(field.bath$abs_v_diff)

p = ggplot(field.bath, aes(datetime, abs_v_diff))
mid.b.abs.v_diff =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.03, 0.065) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.04, linetype="dashed", 
             color = "yellow", size=1) + 
  geom_hline(yintercept=0.065, linetype="dashed", 
             color = "red", size=1) +
  labs(caption = "yellow conservative line suggested by Rivest et al. 2016. may not apply to estuaries.")

print(mid.b.abs.v_diff)



p = ggplot(field.bath, aes(datetime))
mid.b.phs = p + geom_point(aes(y = pH_int), size = 0.25, color = "seagreen") +
  geom_point(aes(y = pH_ext), size = 0.25, color = "green") +
  xlab("") + #last x label sets the time axis label
  ylab("pH int, ext, dickson\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(7.6, 7.8) +
  geom_point(data = bth.chk.df, aes(x=datetime, y= pH.check.median), shape = 18, size = 1,
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


print(mid.b.phs)



p = ggplot(field.bath, aes(datetime, abs_pH_diff))
mid.b.pH.diff = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("electrode abs diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.2) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=0.5) +
  labs(caption = "red line threshold suggested by Seabird Electronics as suspect")


print(mid.b.pH.diff)


p = ggplot(field.bath, aes(datetime, ctd_sal))
mid.b.s = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab("") + #last x label sets the time axis label
  ylab("S\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(15, 34) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(mid.b.s)


p = ggplot(field.bath, aes(datetime, ctd_sal))
mid.b.s.dt = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab("Mid-Deployment Common Bath for MARI 5/21/2020") + #last x label sets the time axis label
  ylab("S\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(15, 34) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(mid.b.s.dt)


summary(field.bath$ctd_temp)

p = ggplot(field.bath, aes(datetime, ctd_temp))
mid.b.t.dt = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("Mid-Deployment Common Bath for MARI 5/21/2020") + #last x label sets the time axis label
  ylab("T\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(15, 17) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(mid.b.t.dt)






# combine check samples to instrument data in a new data frame

df1 <- left_join(field.bath, bth.chk.df, by = "datetime")


# Discrete spec pH regressed versus pH

k0.int.mid.b.benchmark <- ggplotRegression(df1, "pH.check.median", "pH_int_v")

print(k0.int.mid.b.benchmark)


k0.ext.mid.b.benchmark <- ggplotRegression(df1, "pH.check.median", "pH_ext_v")

print(k0.ext.mid.b.benchmark)


# removing outlier bath sample

# substitute for t1 and t2 in stabilized pH measurement
t3 <- '2020-05-21 00:00:00'
t4 <- '2020-05-21 18:00:00'

df1 <- filter(df1, datetime > t3 & datetime < t4)





# Discrete spec pH regressed versus pH

k0.int.mid.b.benchmark.2pts <- ggplotRegression(df1, "pH.check.median", "pH_int_v")

print(k0.int.mid.b.benchmark.2pts)


k0.ext.mid.b.benchmark.2pts <- ggplotRegression(df1, "pH.check.median", "pH_ext_v")

print(k0.ext.mid.b.benchmark.2pts)


rm(df1)



############# POST DEPLOYMENT BATH PLOTS  ##########################################


head(post.deploy.bath)
"2020-10-30 21:40:00"

tail(post.deploy.bath)
"2020-11-07 02:40:00"

#event time bounds
t1 <-  '2020-10-30 20:40:00'
t2 <- '2020-11-07 03:40:00'


p = ggplot(post.deploy.bath, aes(x = datetime))
post.b.int.v = p + geom_point(aes(y = pH_int_v), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(mari.2020.screen.df$int.v.min, mari.2020.screen.df$int.v.max) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(post.b.int.v)


p = ggplot(post.deploy.bath, aes(datetime, pH_ext_v))
post.b.ext.v = p + geom_point(aes(), size = 0.25, color = "green") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(mari.2020.screen.df$ext.v.min, mari.2020.screen.df$ext.v.max) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(post.b.ext.v)

summary(post.deploy.bath$abs_v_diff)

p = ggplot(post.deploy.bath, aes(datetime, abs_v_diff))
post.b.abs.v_diff =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.03, 0.065) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.04, linetype="dashed", 
             color = "yellow", size=1) + 
  geom_hline(yintercept=0.065, linetype="dashed", 
             color = "red", size=1) +
  labs(caption = "yellow conservative line suggested by Rivest et al. 2016. may not apply to estuaries.")

print(post.b.abs.v_diff)



p = ggplot(post.deploy.bath, aes(datetime))
post.b.phs = p + geom_point(aes(y = pH_int), size = 0.25, color = "seagreen") +
  geom_point(aes(y = pH_ext), size = 0.25, color = "green") +
  xlab("") + #last x label sets the time axis label
  ylab("pH int, ext, dickson\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(7.8, 8.2) +
  geom_point(data = bth.chk.df, aes(x=datetime, y= pH.check.median), shape = 18, size = 1,
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


print(post.b.phs)



p = ggplot(post.deploy.bath, aes(datetime, abs_pH_diff))
post.b.pH.diff = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("electrode abs diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.1) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=0.5) +
  labs(caption = "red line threshold suggested by Seabird Electronics as suspect")


print(post.b.pH.diff)


p = ggplot(post.deploy.bath, aes(datetime, ctd_sal))
post.b.s = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab("") + #last x label sets the time axis label
  ylab("S\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(30, 35) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(post.b.s)


p = ggplot(post.deploy.bath, aes(datetime, ctd_sal))
post.b.s.dt = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab("Post-Deployment Common Bath for MARI 2020") + #last x label sets the time axis label
  ylab("S\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(30, 35) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(post.b.s.dt)


summary(post.deploy.bath$ctd_temp)

p = ggplot(post.deploy.bath, aes(datetime, ctd_temp))
post.b.t.dt = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("Post-Deployment Common Bath for MARI 2020") + #last x label sets the time axis label
  ylab("T\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(12, 18) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(post.b.t.dt)


# regression plots

# combine check samples to instrument data in a new data frame

df1 <- left_join(post.deploy.bath, bth.chk.df, by = "datetime")


# Discrete spec pH regressed versus pH

k0.int.post.b.benchmark <- ggplotRegression(df1, "pH.check.median", "pH_int_v")

print(k0.int.post.b.benchmark)


k0.ext.post.b.benchmark <- ggplotRegression(df1, "pH.check.median", "pH_ext_v")

print(k0.int.post.b.benchmark)

rm(df1)



############# POST DEPLOYMENT DICKSON STD RUN PLOTS #################################

#creating Dickson Standard Line to be plotted alongside instrument data

# test_data <- read.csv("test_data_dickson_line.csv",
#                       header=T, stringsAsFactors=F, sep=",")
# 
# save(test_data, file = "dickson.line.data-test.RData")

# df = data frame that has data of interest
# T_DegC = temperature of thermistor local to instrument, example call:  "df$ctd_temp"  
# pH_int & pH_ext = pH of internal and external electrode, example call: "df$pH_int" 
# sal_std = all seawater TRIS buffer Dickson standards are mixed to Nominal salinity of 35
# offset_std = offset (not needed with Dickson Standard)

sal_std <- 35  
offset_std <- 0 #for Dickson standard

#df <- test_data

post.dickson$DL = (11911.08 - 18.2499*sal_std - 0.039336*sal_std^2)/(post.dickson$pH_temp + 273.15) + 
  (-366.27059 + 0.53993607*sal_std + 0.00016329*sal_std^2) + 
  (64.52243 - 0.084041*sal_std)*log(post.dickson$pH_temp+273.15) - 0.11149858*(post.dickson$pH_temp +273.15)  +  offset_std


post.dickson$DL_pH_diff_int <- abs(post.dickson$pH_int_cell - post.dickson$DL)
post.dickson$DL_pH_diff_ext <- abs(post.dickson$pH_ext_cell - post.dickson$DL)

#determine data time bounds

head(post.dickson)
"2020-11-07 03:10:00"


tail(post.dickson)
"2020-11-13 03:00:00"


#event time bounds
t1 <-  '2020-11-07 02:10:00'
t2 <- '2020-11-13 04:00:00'

# for trim plots

t3 <- '2020-11-12 00:00:00' # substitute for t1 in stabilized pH measurement
t4 <- '2020-11-13 00:00:00'

summary(post.dickson$pH_int_v)
# Min.        1st Qu.         Median           Mean        3rd Qu.           Max. 
# -1.01679800000 -1.01376125000 -1.01336950000 -1.01338753125 -1.01296650000 -1.01211900000 

print(mari.2020.screen.df$int.v.min)
# -1.06
print(mari.2020.screen.df$int.v.max) 
# -1.02


#adjusting plotting range for pH_int_v because it is out of range...

p = ggplot(post.dickson, aes(x = datetime))
post.d.int.v = p + geom_point(aes(y = pH_int_v), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.02, -1.01) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(post.d.int.v)


summary(post.dickson$pH_ext_v)
# Min.         1st Qu.          Median            Mean         3rd Qu.            Max. 
# -0.967675000000 -0.966529000000 -0.966390000000 -0.966413097581 -0.966322000000 -0.966094000000 

#adjusting plotting range for pH_int_v because it is out of range...

print(mari.2020.screen.df$ext.v.min)
# -1.01
print(mari.2020.screen.df$ext.v.max) 
# -0.97

p = ggplot(post.dickson, aes(datetime, pH_ext_v))
post.d.ext.v = p + geom_point(aes(), size = 0.25, color = "green") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-0.97, -0.965) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(post.d.ext.v)




p = ggplot(post.dickson, aes(datetime, abs_v_diff))
post.d.v_diff =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.03, 0.065) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.04, linetype="dashed", 
             color = "yellow", size=1) + 
  geom_hline(yintercept=0.065, linetype="dashed", 
             color = "red", size=1) +
  labs(caption = "yellow conservative line suggested by Rivest et al. 2016. may not apply to estuaries.")

print(post.d.v_diff)



p = ggplot(post.dickson, aes(x = datetime))
post.d.pHs = p + geom_point(aes(y = pH_int_cell), size = 0.25, color = "seagreen") +
  geom_point(aes(y = pH_ext_cell), size = 0.25, color = "green") +
  geom_point(aes(y = DL), size = 0.25, color = "black") +
  xlab(" ") + #last x label sets the time axis label
  ylab("pH int, ext, dickson\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(8.2, 8.5) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(post.d.pHs)



p = ggplot(post.dickson, aes(datetime, abs_pH_diff))
post.d.pH.diff = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("abs electrode diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.1) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=0.5) +
  labs(caption = "red line threshold suggested by Seabird Electronics as suspect")

print(post.d.pH.diff)




p = ggplot(post.dickson, aes(x =datetime))
post.d.DL.diff = p + geom_point(aes(y = DL_pH_diff_int), size = 0.25, color = "seagreen") +
  geom_point(aes(datetime, DL_pH_diff_ext), size = 0.25, color = "green") +
  xlab("Post-Deployment Dickson Standard Run for MARI 2020") + #last x label sets the time axis label
  ylab("abs offset from Dickson\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.1) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=1.0)


print(post.d.DL.diff)

summary(post.dickson$pH_temp)


p = ggplot(post.dickson, aes(datetime, pH_temp))
post.d.t.dt = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("Post-Deployment Dickson Standard Run for MARI Nov 7-13th 2020") + #last x label sets the time axis label
  ylab("T\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(14.0, 18.0) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  labs(caption = "note: temperature from local thermistor on SeaFET or SeapHOx. Salinity is nominally 35 PSU")

print(post.d.t.dt)

# no salinity plot, salinity is a nominal 35


# adjusted POST deployment Dickson standard plots to more stable section #######################

# substitute for t1 and t2 in stabilized pH measurement
t3 <- '2020-11-12 00:00:00'
t4 <- '2020-11-13 00:00:00'

post.dickson.trim <- filter(post.dickson, datetime > t3 & datetime < t4)

p = ggplot(post.dickson.trim, aes(x = datetime))
post.d.int.v.trim = p + geom_point(aes(y = pH_int_v), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t3, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t4, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.02, -1.01) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(post.d.int.v.trim)


p = ggplot(post.dickson.trim, aes(datetime, pH_ext_v))
post.d.ext.v.trim = p + geom_point(aes(), size = 0.25, color = "green") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t3, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t4, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-0.97, -0.965) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(post.d.ext.v.trim)




p = ggplot(post.dickson.trim, aes(datetime, abs_v_diff))
post.d.v_diff.trim =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t3, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t4, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.03, 0.065) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.04, linetype="dashed", 
             color = "yellow", size=1) + 
  geom_hline(yintercept=0.065, linetype="dashed", 
             color = "red", size=1) +
  labs(caption = "yellow conservative line suggested by Rivest et al. 2016. may not apply to estuaries.")

print(post.d.v_diff.trim)






p = ggplot(post.dickson.trim, aes(x = datetime))
post.d.pHs.trim = p + geom_point(aes(y = pH_int_cell), size = 0.25, color = "seagreen") +
  geom_point(aes(y = pH_ext_cell), size = 0.25, color = "green") +
  geom_point(aes(y = DL), size = 0.25, color = "black") +
  xlab(" ") + #last x label sets the time axis label
  ylab("pH comparison\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t3, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t4, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(8.2, 8.5) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(post.d.pHs.trim)


p = ggplot(post.dickson.trim, aes(datetime, abs_pH_diff))
post.d.pH.diff.trim = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("abs electrode diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t3, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t4, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.1) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=1.0) +
  labs(caption = "red line threshold suggested by Seabird Electronics as suspect")

print(post.d.pH.diff.trim)



p = ggplot(post.dickson.trim, aes(x =datetime))
post.d.DL.diff.trim = p + geom_point(aes(y = DL_pH_diff_int), size = 0.25, color = "seagreen") +
  geom_point(aes(datetime, DL_pH_diff_ext), size = 0.25, color = "green") +
  xlab("") + #last x label sets the time axis label
  ylab("abs offset from Dickson\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t3, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t4, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.1) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=1.0)


print(post.d.DL.diff.trim)


p = ggplot(post.dickson.trim, aes(datetime, pH_temp))
post.d.t.dt.trim = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("Post-Deployment Dickson Standard Run for MARI Nov 12-13th 2020") + #last x label sets the time axis label
  ylab("T\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t3, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t4, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(14.0, 17.0) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  labs(caption = "note: temperature from local thermistor on SeaFET or SeapHOx. Salinity is nominally 35 PSU")

print(post.d.t.dt.trim)



# no salinity plot, salinity is a nominal 35


# regression plots

# Dickson predicted versus pH
# post.d.DL.v.pH.int.trim <- ggplotRegression(post.dickson.trim, "DL", "pH_int_v")
# 
# print(post.d.DL.v.pH.int.trim)
# 
# 
# post.d.DL.v.pH.ext.trim <- ggplotRegression(post.dickson.trim, "DL", "pH_ext_v")
# 
# print(post.d.DL.v.pH.ext.trim)



##### arranging and printing plots to .png files ######################################

#the following plots aligned time series into one .png with 4K HD aspect ratios

ls() # to pull object names 

# PREDEPLOYMENT BATH #######################################################

# "Pre.Bath...." Predeployment Bath plots
# "pre.b.abs.v_diff"        "pre.b.ext.v"            
# "pre.b.int.v"             "pre.b.pH.diff"           "pre.b.phs"              
# "pre.b.s"                 "pre.b.s.dt"              "pre.b.t.dt"     

ggsave("qaqc_plots/2020/MARI-pre-deployment_bath_Volt_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.b.int.v), ggplotGrob(pre.b.ext.v),
                              ggplotGrob(pre.b.abs.v_diff), ggplotGrob(pre.b.s.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-pre-deployment_bath_pHs_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.b.phs), ggplotGrob(pre.b.pH.diff),
                              ggplotGrob(pre.b.s.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-pre-deployment_bath_T_S_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.b.s), ggplotGrob(pre.b.t.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-pre-deployment_bath_pHs_T_S_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.b.phs), ggplotGrob(pre.b.pH.diff),
                              ggplotGrob(pre.b.s), ggplotGrob(pre.b.t.dt),
                              size = "last")), width = 6.65, height = 3.5)


# regression  plots

ggsave("qaqc_plots/2020/MARI-pre-deployment_bath_disrete.spec.pH.v.sfx.pH_2020.png",
       plot = grid.draw(rbind(ggplotGrob(k0.int.pre.b.benchmark), ggplotGrob(k0.ext.pre.b.benchmark),
                              size = "last")), width = 5, height = 5)


# PREDEPLOYMENT DICKSON STD RUN #######################################################


# "Pre.Dickson...." Pre deployment Dickson std run
# "pre.d.DL.diff"           "pre.d.DL.diff.trim"      "pre.d.DL.v.pH.ext.trim" 
# "pre.d.DL.v.pH.int.trim"  "pre.d.ext.v"             "pre.d.ext.v.trim"       
# "pre.d.int.v"             "pre.d.int.v.trim"        "pre.d.pH.diff"          
# "pre.d.pH.diff.trim"      "pre.d.pHs"               "pre.d.pHs.trim"         
# "pre.d.t.dt"              "pre.d.t.dt.trim"         "pre.d.v_diff"           
# "pre.d.v_diff.trim"       "pre.deploy.bath"         "pre.dickson"            
# "pre.dickson.trim"        "sal_std"                 


ggsave("qaqc_plots/2020/MARI-pre-deployment_DSR_Volt_T_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.int.v), ggplotGrob(pre.d.ext.v),
                              ggplotGrob(pre.d.v_diff), ggplotGrob(pre.d.t.dt),
                              size = "last")), width = 6.65, height = 3.5)


ggsave("qaqc_plots/2020/MARI-pre-deployment_DSR_pHs_DL_T_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.pHs), ggplotGrob(pre.d.pH.diff),
                              ggplotGrob(pre.d.DL.diff), ggplotGrob(pre.d.t.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-pre-deployment_DSR_pHs_T_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.pHs), ggplotGrob(pre.d.pH.diff),
                              ggplotGrob(pre.d.t.dt),
                              size = "last")), width = 6.65, height = 3.5)

# trimmed plots

ggsave("qaqc_plots/2020/MARI-pre-deployment_DSR_Volt_T_2020-trim.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.int.v.trim), ggplotGrob(pre.d.ext.v.trim),
                              ggplotGrob(pre.d.v_diff.trim), ggplotGrob(pre.d.t.dt.trim),
                              size = "last")), width = 6.65, height = 3.5)


ggsave("qaqc_plots/2020/MARI-pre-deployment_DSR_pHs_DL_T_2020-trim.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.pHs.trim), ggplotGrob(pre.d.pH.diff.trim),
                              ggplotGrob(pre.d.DL.diff.trim), ggplotGrob(pre.d.t.dt.trim),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-pre-deployment_DSR_pHs_T_2020-trim.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.pHs.trim), ggplotGrob(pre.d.pH.diff.trim),
                              ggplotGrob(pre.d.t.dt.trim),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-pre-deployment_DSR_DL.v.pHs_2020-trim.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.DL.v.pH.int.trim), ggplotGrob(pre.d.DL.v.pH.ext.trim),
                              size = "last")), width = 5, height = 5)


ggsave("qaqc_plots/2020/MARI-pre-deployment_DSR_pHs.v.Volts_2020-trim.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.pH.int.v.Volt.int.trim), 
                              ggplotGrob(pre.d.pH.ext.v.Volt.ext.trim),
                              size = "last")), width = 5, height = 5)


# DEPLOYMENT #######################################################


# "SeapHOx...." Deployment data for SeapHOx
# "sfx.df"                 
# "sfx.ext.v"               "sfx.int.v"               "sfx.o2"                 
# "sfx.pH.diff"             "sfx.ph.ext.v.o2"         "sfx.ph.ext.v.o2.ol.rm"  
# "sfx.ph.int.v.o2"         "sfx.ph.int.v.o2.ol.rm"   "sfx.pH_ext"             
# "sfx.pH_int"              "sfx.phs"                 "sfx.sal"                
# "sfx.sal.dt"              "sfx.temp"                "sfx.v_diff"             
# "spc.ph.v.sfx.ph.ext"     "spc.ph.v.sfx.ph.ext.2"   "spc.ph.v.sfx.ph.ext.v"  
# "spc.ph.v.sfx.ph.ext.v.2" "spc.ph.v.sfx.ph.int"     "spc.ph.v.sfx.ph.int.2"  
# "spc.ph.v.sfx.ph.int.v"   "spc.ph.v.sfx.ph.int.v.2"  


ggsave("qaqc_plots/2020/MARI-deployment_Volt_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(sfx.int.v), ggplotGrob(sfx.ext.v),
                              ggplotGrob(sfx.v_diff), ggplotGrob(sfx.sal.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-deployment_pHs_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(sfx.pH_int), ggplotGrob(sfx.pH_ext),
                              ggplotGrob(sfx.pH.diff), ggplotGrob(sfx.sal.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-deployment_pHs_T_S_O2_2020.png",
       plot = grid.draw(rbind(ggplotGrob(sfx.phs), ggplotGrob(sfx.pH.diff.no.note),
                        ggplotGrob(sfx.sal), ggplotGrob(sfx.temp),
                        ggplotGrob(sfx.o2),
                        size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-deployment_pHs_T_S_O2_2020-2.png",
       plot = grid.draw(rbind(ggplotGrob(sfx.phs), 
                              ggplotGrob(sfx.sal), ggplotGrob(sfx.temp),
                              ggplotGrob(sfx.o2),
                              size = "last")), width = 6.65, height = 3.5)

# regression  plots

ggsave("qaqc_plots/2020/MARI-deployment_pH_v_o2_2020.png",
       plot = grid.draw(rbind(ggplotGrob(sfx.ph.int.v.o2), ggplotGrob(sfx.ph.ext.v.o2),
                              size = "last")), width = 5, height = 5)

ggsave("qaqc_plots/2020/MARI-deployment_pH_v_o2_no_outliers_2020.png",
       plot = grid.draw(rbind(ggplotGrob(sfx.ph.int.v.o2.ol.rm), ggplotGrob(sfx.ph.ext.v.o2.ol.rm),
                              size = "last")), width = 5, height = 5)

ggsave("qaqc_plots/2020/MARI-deployment_spc.pH_v_sfx.pH.int_2020.png",
       plot = grid.draw(rbind(ggplotGrob(spc.ph.v.sfx.ph.int.v), ggplotGrob(spc.ph.v.sfx.ph.int.v.2),
                              ggplotGrob(spc.ph.v.sfx.ph.int),
                              size = "last")), width = 5, height = 5)

ggsave("qaqc_plots/2020/MARI-deployment_spc.pH_v_sfx.pH.ext_2020.png",
       plot = grid.draw(rbind(ggplotGrob(spc.ph.v.sfx.ph.ext.v), ggplotGrob(spc.ph.v.sfx.ph.ext.v.2),
                              ggplotGrob(spc.ph.v.sfx.ph.ext),
                              size = "last")), width = 5, height = 5)

# NO IMPROVEMENT ADJUSTED DEPLOYMENT #######################################################

# ADJUSTED DEPLOYMENT DATA
# "adj.sfx.df"              "adj.sfx.ext.v"           "adj.sfx.int.v"          
# "adj.sfx.o2"              "adj.sfx.pH.diff"         "adj.sfx.pH.diff.no.note"
# "adj.sfx.pH_ext"          "adj.sfx.pH_int"          "adj.sfx.phs"            
# "adj.sfx.sal"             "adj.sfx.sal.dt"          "adj.sfx.temp"           
# "adj.sfx.v_diff"   
# "spc.ph.v.adj.sfx.ph.ext" "spc.ph.v.adj.sfx.ph.int"

# 
# 
# 
# ggsave("qaqc_plots/2020/MARI-ADJ-deployment_pHs_Sal_2020.png",
#        plot = grid.draw(rbind(ggplotGrob(adj.sfx.pH_int), ggplotGrob(adj.sfx.pH_ext),
#                               ggplotGrob(adj.sfx.pH.diff), ggplotGrob(adj.sfx.sal.dt),
#                               size = "last")), width = 6.65, height = 3.5)
# 
# ggsave("qaqc_plots/2020/MARI-ADJ-deployment_pHs_T_S_O2_2020-2.png",
#        plot = grid.draw(rbind(ggplotGrob(adj.sfx.phs), 
#                               ggplotGrob(adj.sfx.sal), ggplotGrob(adj.sfx.temp),
#                               ggplotGrob(adj.sfx.o2),
#                               size = "last")), width = 6.65, height = 3.5)
# 
# #regression plots
# 
# ggsave("qaqc_plots/2020/MARI-ADJ-deployment_spc.pH_v_sfx.pHs_2020.png",
#        plot = grid.draw(rbind(ggplotGrob(spc.ph.v.adj.sfx.ph.int), 
#                               ggplotGrob(spc.ph.v.adj.sfx.ph.ext),
#                               size = "last")), width = 5, height = 5)
# 
# 



# MID DEPLOYMENT BATH #######################################################

# "Mid.Bath...."   Mid Deployment Field Bath plot objects 
# "mid.b.abs.v_diff"       "mid.b.ext.v"            "mid.b.int.v"           
# "mid.b.pH.diff"          "mid.b.phs"              "mid.b.s"               
# "mid.b.s.dt"             "mid.b.t.dt"  
# "k0.ext.mid.b.benchmark"      "k0.ext.mid.b.benchmark.2pts"
# "k0.int.mid.b.benchmark"     
# "k0.int.mid.b.benchmark.2pts" 


ggsave("qaqc_plots/2020/MARI-mid-deployment_bath_Volt_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(mid.b.int.v), ggplotGrob(mid.b.ext.v),
                              ggplotGrob(mid.b.abs.v_diff), ggplotGrob(mid.b.s.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-mid-deployment_bath_pHs_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(mid.b.phs), ggplotGrob(mid.b.pH.diff),
                              ggplotGrob(mid.b.s.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-mid-deployment_bath_T_S_2020.png",
       plot = grid.draw(rbind(ggplotGrob(mid.b.s), ggplotGrob(mid.b.t.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-mid-deployment_bath_pHs_T_S_2020.png",
       plot = grid.draw(rbind(ggplotGrob(mid.b.phs), ggplotGrob(mid.b.pH.diff),
                              ggplotGrob(mid.b.s), ggplotGrob(mid.b.t.dt),
                              size = "last")), width = 6.65, height = 3.5)


# regression  plots

ggsave("qaqc_plots/2020/MARI-mid-deployment_bath_disrete.spec.pH.v.sfx.pH_2020.png",
       plot = grid.draw(rbind(ggplotGrob(k0.int.mid.b.benchmark), ggplotGrob(k0.ext.mid.b.benchmark),
                              size = "last")), width = 5, height = 5)


ggsave("qaqc_plots/2020/MARI-mid-deployment_bath_disrete.spec.pH.v.sfx.pH_2020-2pts.png",
       plot = grid.draw(rbind(ggplotGrob(k0.int.mid.b.benchmark.2pts), ggplotGrob(k0.ext.mid.b.benchmark.2pts),
                              size = "last")), width = 5, height = 5)


# POST DEPLOYMENT BATH #######################################################

# "Post.Bath...."   Post Deployment Bath plot objects              
# "post.b.abs.v_diff"       "post.b.ext.v"           
# "post.b.int.v"            "post.b.pH.diff"          "post.b.phs"             
# "post.b.s"                "post.b.s.dt"             "post.b.t.dt"  
# "k0.ext.post.b.benchmark" "k0.int.post.b.benchmark"

ggsave("qaqc_plots/2020/MARI-post-deployment_bath_Volt_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(post.b.int.v), ggplotGrob(post.b.ext.v),
                              ggplotGrob(post.b.abs.v_diff), ggplotGrob(post.b.s.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-post-deployment_bath_pHs_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(post.b.phs), ggplotGrob(post.b.pH.diff),
                              ggplotGrob(post.b.s.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-post-deployment_bath_T_S_2020.png",
       plot = grid.draw(rbind(ggplotGrob(post.b.s), ggplotGrob(post.b.t.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-post-deployment_bath_pHs_T_S_2020.png",
       plot = grid.draw(rbind(ggplotGrob(post.b.phs), ggplotGrob(post.b.pH.diff),
                              ggplotGrob(post.b.s), ggplotGrob(post.b.t.dt),
                              size = "last")), width = 6.65, height = 3.5)

# regression  plots

ggsave("qaqc_plots/2020/MARI-post-deployment_bath_disrete.spec.pH.v.sfx.pH_2020.png",
       plot = grid.draw(rbind(ggplotGrob(k0.int.post.b.benchmark), ggplotGrob(k0.ext.post.b.benchmark),
                              size = "last")), width = 5, height = 5)

# POST DEPLOYMENT DICKSON STANDARD RUN #######################################################

# "Post.Dickson...." Post deployment Dickson Standard plot objects
# "post.d.DL.diff"          "post.d.DL.diff.trim"     "post.d.DL.v.pH.ext.trim"
# "post.d.DL.v.pH.int.trim" "post.d.ext.v"            "post.d.ext.v.trim"      
# "post.d.int.v"            "post.d.int.v.trim"       "post.d.pH.diff"         
# "post.d.pH.diff.trim"     "post.d.pHs"              "post.d.pHs.trim"        
# "post.d.t.dt"             "post.d.t.dt.trim"        "post.d.v_diff"          
# "post.d.v_diff.trim"      "post.deploy.bath"        "post.dickson"           
# "post.dickson.trim"       



ggsave("qaqc_plots/2020/MARI-post-deployment_DSR_Volt_T_2020.png",
       plot = grid.draw(rbind(ggplotGrob(post.d.int.v), ggplotGrob(post.d.ext.v),
                              ggplotGrob(post.d.v_diff), ggplotGrob(post.d.t.dt),
                              size = "last")), width = 6.65, height = 3.5)


ggsave("qaqc_plots/2020/MARI-post-deployment_DSR_pHs_DL_T_2020.png",
       plot = grid.draw(rbind(ggplotGrob(post.d.pHs), ggplotGrob(post.d.pH.diff),
                              ggplotGrob(post.d.DL.diff), ggplotGrob(post.d.t.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-post-deployment_DSR_pHs_T_2020.png",
       plot = grid.draw(rbind(ggplotGrob(post.d.pHs), ggplotGrob(post.d.pH.diff),
                              ggplotGrob(post.d.t.dt),
                              size = "last")), width = 6.65, height = 3.5)

# trimmed plots

ggsave("qaqc_plots/2020/MARI-post-deployment_DSR_Volt_T_2020-trim.png",
       plot = grid.draw(rbind(ggplotGrob(post.d.int.v.trim), ggplotGrob(post.d.ext.v.trim),
                              ggplotGrob(post.d.v_diff.trim), ggplotGrob(post.d.t.dt.trim),
                              size = "last")), width = 6.65, height = 3.5)


ggsave("qaqc_plots/2020/MARI-post-deployment_DSR_pHs_DL_T_2020-trim.png",
       plot = grid.draw(rbind(ggplotGrob(post.d.pHs.trim), ggplotGrob(post.d.pH.diff.trim),
                              ggplotGrob(post.d.DL.diff.trim), ggplotGrob(post.d.t.dt.trim),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-post-deployment_DSR_pHs_T_2020-trim.png",
       plot = grid.draw(rbind(ggplotGrob(post.d.pHs.trim), ggplotGrob(post.d.pH.diff.trim),
                              ggplotGrob(post.d.t.dt.trim),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-post-deployment_DSR_DL.v.pHs_2020-trim.png",
       plot = grid.draw(rbind(ggplotGrob(post.d.DL.v.pH.int.trim), ggplotGrob(post.d.DL.v.pH.ext.trim),
                              size = "last")), width = 5, height = 5)





# FINAL QC CHECK AND FLAGGING ##################################################################

rm(list=ls())

setwd("C:/Users/915712257/Box Sync/Inbox/oceanographic work/MARI SeapHOx assessment")


#intstrument data
load("2020 data/mari.2020.RData")
sfx.df <- data
rm(data)

#list vars in data frame

# ls(sfx.df)
#  "abs_pH_diff" "abs_v_diff"  "cond_Sm"    
#  "ctd_o2_mg_l" "ctd_o2_ml_l" "ctd_sal"    
#  "ctd_temp"    "datetime"    "pH_diff"    
#  "pH_ext"      "pH_ext_v"    "pH_int"     
#  "pH_int_temp" "pH_int_v"    "pH_temp"    
#  "press_dbar"  "QARTOD"      "RH"         
#  "volt_diff" 

#reorder and pull select vars

sfx.df <- select(sfx.df, datetime,
              pH_int_v, pH_ext_v, abs_v_diff, ctd_temp, ctd_sal,
              pH_int, pH_ext, abs_pH_diff,
              ctd_o2_mg_l, press_dbar, pH_temp, cond_Sm)




# QARTOD FLAGS
# '1' DATA HAS PASSED REQUIRED QC
# '2' DATA HAS NOT BEEN EVALUATED
# '3' QUESTIONABLE/SUSPECT
# '4' BAD DATA
# '9' MISSING DATA


summary(sfx.df$pH_int)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 7.608   7.727   7.804   7.819   7.904   8.133 

summary(sfx.df$pH_ext)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 7.495   7.729   7.802   7.816   7.912   8.146



# SeaFET sensor specifications 
# measure pH from 6.5 to 9.0
# measure salinity from 20 to 40 PSU
# measure temperature from 0 to 50 degC


sfx.df <- sfx.df %>%
  mutate(
    pH_int_flag = case_when(
      pH_int > 8.6 ~ "4",
      pH_int <= 8.6 & pH_int > 8.4 ~ "3",
      pH_int <= 8.4 & pH_int >= 7.4 ~ "1",
      pH_int < 7.4  & pH_int >= 7.0 ~ "3",
      pH_int < 7.0 ~ "4"
    )
  )


#verify  flag worked

df1 <- filter(sfx.df, pH_int_flag == "4") # 0 observations flagged for removal

df1 <- arrange(df1, pH_int)

df2 <- filter(sfx.df, pH_int_flag == "3") # 0 observations flagged as suspect

df2 <- arrange(df2, pH_int)

df3 <- filter(sfx.df, pH_int_flag == "1")

df3 <- arrange(df3, pH_int)

x <- sum(nrow(df1), nrow(df2), nrow(df3))

rm(x, df1, df2, df3)



sfx.df <- sfx.df %>%
  mutate(
    pH_ext_flag = case_when(
      pH_ext > 8.6 ~ "4",
      pH_ext <= 8.6 & pH_ext > 8.4 ~ "3",
      pH_ext <= 8.4 & pH_ext >= 7.4 ~ "1",
      pH_ext < 7.4  & pH_ext >= 7.0 ~ "3",
      pH_ext < 7.0 ~ "4"
    )
  )


#verify  flag worked

df1 <- filter(sfx.df, pH_ext_flag == "4") # 0 observations flagged for removal

df1 <- arrange(df1, pH_ext)

df2 <- filter(sfx.df, pH_ext_flag == "3") # 0 observations flagged as suspect

df2 <- arrange(df2, pH_ext)

df3 <- filter(sfx.df, pH_ext_flag == "1")

df3 <- arrange(df3, pH_ext)

x <- sum(nrow(df1), nrow(df2), nrow(df3))

rm(x, df1, df2, df3)




# salinity flag for SeaFET instruments


sfx.df <- sfx.df %>%
  mutate(
    pH_salt_flag = case_when(
      ctd_sal > 40 ~ "4",
      ctd_sal <= 40 & ctd_sal > 35 ~ "3",
      ctd_sal <= 35 & ctd_sal >= 20 ~ "1",
      ctd_sal < 20 & ctd_sal >= 10 ~ "3",
      ctd_sal < 10 ~ "4"
    )
  )

#verify  flag worked

df1 <- filter(sfx.df, pH_salt_flag == "4") # 0 observations flagged for removal

df1 <- arrange(df1, ctd_sal)

df2 <- filter(sfx.df, pH_salt_flag == "3") # 493 observations flagged as suspect

df2 <- arrange(df2, ctd_sal)

df3 <- filter(sfx.df, pH_salt_flag == "1") # 14753 observations flagged for keeping

df3 <- arrange(df3, ctd_sal)

x <- sum(nrow(df1), nrow(df2), nrow(df3))

rm(x, df1, df2, df3)



# sensor specifactions maximum ranges == "4" (0 to 450 micromol per kg O2.... Which converted to 7.1995 mg/L O2...)

sfx.df <- sfx.df %>%
  mutate(
    o2_flag = case_when(
      ctd_o2_mg_l > 16 ~ "4",
      ctd_o2_mg_l < 16 ~ "1"
    )
  )

df1 <- filter(sfx.df, o2_flag == "4") #flagged 9 observations for removal

df1 <- arrange(df1, ctd_o2_mg_l)

rm(df1)



pH_int_flag_cols <- c(14,16,17)

sfx.df$pH_int_final_flag <- do.call(pmax, sfx.df[,pH_int_flag_cols])

pH_ext_flag_cols <- c(15:17)

sfx.df$pH_ext_final_flag <- do.call(pmax, sfx.df[,pH_ext_flag_cols])

sfx.df$final_flag <- do.call(pmax, sfx.df[,14:17])


save(sfx.df, file = 'mari.2020.all.combined.prcsd.flagged.RData')

rm(list=ls())

load('mari.2020.all.combined.prcsd.flagged.RData')

# remove all rows of data where pH external is flagged for removal 
sfx.df <- filter(sfx.df, pH_ext_final_flag != "4")

# replace pH internal flagged data with NA's only
sfx.df <- mutate(sfx.df, pH_int = ifelse(pH_int_final_flag == "4", NA, pH_int))

save(sfx.df, file = 'mari.2020.select.combined.prcsd.flagged.RData')

load('mari.2020.select.combined.prcsd.flagged.RData')

write.csv(sfx.df, "2020 data/mari-2020-final.flagged.data.csv", row.names = F)


rm(list=ls())






# plotting ALL flagged combined data #################################################




setwd("C:/Users/915712257/Box Sync/Inbox/oceanographic work/MARI SeapHOx assessment")

rm(list=ls())

#regression plot function (click carat to expand)
ggplotRegression <- function(dat, xvar, yvar){
  
  fml <- paste(yvar, "~", xvar)
  
  fit <- lm(fml, dat)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


load('mari.2020.all.combined.prcsd.flagged.RData')


# field check samples
load("2020 data/mari-check.samples-2020.RData")

# find data time bounds
head(sfx.df) 
tail(sfx.df)

#sfx season time bounds
#2020-03-11 19:20:00
#2020-10-09 18:20:00


#event time bounds
t1 <-  '2020-03-11 00:00:00'
t2 <- '2020-10-11 00:00:00'
#t2 <- '2020-03-26 00:00:00'

summary(sfx.df)


# datetime                      pH_int_v         pH_ext_v         abs_v_diff         ctd_temp    
# Min.   :2020-03-11 19:20:00   Min.   :-1.056   Min.   :-1.0072   Min.   :0.04633   Min.   :10.95  
# 1st Qu.:2020-05-03 17:45:00   1st Qu.:-1.048   1st Qu.:-0.9974   1st Qu.:0.04970   1st Qu.:13.73  
# Median :2020-06-25 19:30:00   Median :-1.044   Median :-0.9930   Median :0.05033   Median :15.28  
# Mean   :2020-06-25 19:02:42   Mean   :-1.043   Mean   :-0.9923   Mean   :0.05059   Mean   :15.31  
# 3rd Qu.:2020-08-17 19:55:00   3rd Qu.:-1.038   3rd Qu.:-0.9870   3rd Qu.:0.05122   3rd Qu.:16.81  
# Max.   :2020-10-09 18:20:00   Max.   :-1.026   Max.   :-0.9754   Max.   :0.06226   Max.   :20.41  

# ctd_sal          pH_int          pH_ext       abs_pH_diff      ctd_o2_mg_l       press_dbar   
# Min.   :10.20   Min.   :7.608   Min.   :7.495   Min.   :0.0000   Min.   : 4.903   Min.   :14.74  
# 1st Qu.:26.71   1st Qu.:7.727   1st Qu.:7.729   1st Qu.:0.0126   1st Qu.: 6.625   1st Qu.:16.60  
# Median :28.83   Median :7.804   Median :7.802   Median :0.0233   Median : 7.087   Median :17.40  
# Mean   :27.98   Mean   :7.819   Mean   :7.816   Mean   :0.0304   Mean   : 7.225   Mean   :17.42  
# 3rd Qu.:30.20   3rd Qu.:7.904   3rd Qu.:7.912   3rd Qu.:0.0367   3rd Qu.: 7.748   3rd Qu.:18.23  
# Max.   :32.06   Max.   :8.133   Max.   :8.146   Max.   :0.3075   Max.   :18.912   Max.   :20.62

# pH_temp         cond_Sm      pH_int_flag        pH_ext_flag        pH_salt_flag      
# Min.   :10.99   Min.   :1.409   Length:15246       Length:15246       Length:15246      
# 1st Qu.:13.74   1st Qu.:3.350   Class :character   Class :character   Class :character  
# Median :15.30   Median :3.591   Mode  :character   Mode  :character   Mode  :character  
# Mean   :15.33   Mean   :3.535                                                           
# 3rd Qu.:16.82   3rd Qu.:3.841                                                           
# Max.   :20.35   Max.   :4.090                                                           




# color mappings for both electrode types ######

color_group_int <- c("seagreen","yellow2","red2")

color_group_ext <- c("green","yellow2","red2")

#olor_group_both <- c(color_group_int, color_group_ext)



# Voltages #########

p = ggplot(sfx.df, aes(datetime, pH_int_v))
sfx.int.v = p + 
  geom_point(aes(color = factor(pH_int_final_flag)), shape =21, size = 0.25) +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.06, -1.02) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        legend.position = "none",
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  scale_color_manual(values = color_group_int)

print(sfx.int.v)




p = ggplot(sfx.df, aes(datetime, pH_ext_v))
sfx.ext.v = p + 
  geom_point(aes(color = factor(pH_ext_final_flag)), shape =21, size = 0.25) +
  xlab("") + #last x label sets the time axis label
  ylab("V ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.015, -0.97) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        legend.position = "none",
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  scale_color_manual(values = color_group_ext)

print(sfx.ext.v)



p = ggplot(sfx.df, aes(datetime, abs_v_diff))
sfx.v_diff =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.03, 0.065) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.04, linetype="dashed", 
             color = "yellow", size=1) + 
  geom_hline(yintercept=0.065, linetype="dashed", 
             color = "red", size=1) +
  labs(caption = "yellow conservative line suggested by Rivest et al. 2016. may not apply to estuaries.")

print(sfx.v_diff)


# pH plots #########


p = ggplot(sfx.df, aes(datetime, pH_int))

sfx.pH_int = p + 
  geom_point(aes(color = factor(pH_int_final_flag)), shape =21, size = 0.25) +
  xlab(" ") + #last x label sets the time axis label
  ylab("pH int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(7.2, 8.2) +
  geom_point(data = chk.df, aes(x=datetime, y= pH.check.median), shape = 18, size = 1,
             color = "black", show.legend = F)  +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        legend.position = "none",
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  scale_color_manual(values = color_group_int)

print(sfx.pH_int)



p = ggplot(sfx.df, aes(datetime, pH_ext))
sfx.pH_ext = p +
  geom_point(aes(color = factor(pH_ext_final_flag)), shape =21, size = 0.25) +
  xlab("") + #last x label sets the time axis label
  ylab("pH ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(7.2, 8.2) +
  geom_point(data = chk.df, aes(x=datetime, y= pH.check.median), shape = 18, size = 1,
             color = "black", show.legend = F)  +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        legend.position = "none",
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  scale_color_manual(values = color_group_ext)

print(sfx.pH_ext)


summary(sfx.df$abs_pH_diff)

p = ggplot(sfx.df, aes(datetime, abs_pH_diff))
sfx.pH.diff = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("pH abs-diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.35) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=1.0) +
  labs(caption = "red line threshold suggested by Seabird Electronics as suspect")


print(sfx.pH.diff)


p = ggplot(sfx.df, aes(datetime, abs_pH_diff))
sfx.pH.diff.no.note = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("pH abs-diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.35) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=1.0)


print(sfx.pH.diff.no.note)


# environmental plots ##########################################################


p = ggplot(sfx.df, aes(datetime, ctd_sal))
sfx.sal = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("S\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(8, 32) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))




print(sfx.sal)


p = ggplot(sfx.df, aes(datetime, ctd_sal))
sfx.sal.dt = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab("Deployment MARI 2020") + #last x label sets the time axis label
  ylab("S\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(8, 32) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(sfx.sal.dt)



p = ggplot(sfx.df, aes(datetime, ctd_temp))
sfx.temp = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("") + #last x label sets the time axis label
  ylab("T\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(9, 21) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(sfx.temp)


p = ggplot(sfx.df, aes(datetime, ctd_o2_mg_l))
sfx.o2 = p + geom_point(aes(), size = 0.25, color = "red") +
  xlab("Deployment MARI 2020") + #last x label sets the time axis label
  ylab("O2 (mg/L)\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(4, 10) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(sfx.o2)


#regression plots ######################################

sfx.ph.int.v.o2 <- ggplotRegression(sfx.df, "pH_int", "ctd_o2_mg_l")

print(sfx.ph.int.v.o2)


sfx.ph.ext.v.o2 <- ggplotRegression(sfx.df, "pH_ext", "ctd_o2_mg_l")

print(sfx.ph.ext.v.o2)


#removing outliers ###############################

max_o2 = 10

min_pH = 7

df1 <- filter(sfx.df, ctd_o2_mg_l < max_o2)
df1 <- filter(df1, pH_ext > min_pH)

sfx.ph.int.v.o2.ol.rm <- ggplotRegression(df1, "pH_int", "ctd_o2_mg_l")

print(sfx.ph.int.v.o2.ol.rm)

sfx.ph.ext.v.o2.ol.rm <- ggplotRegression(df1, "pH_ext", "ctd_o2_mg_l")

print(sfx.ph.ext.v.o2.ol.rm)


# combine check samples to instrument data in a new data frame

# round sampling times of check samples since there is a slight mismatch in time

df3 <- chk.df

df3$datetime2 <- round_date(chk.df$datetime, "20 mins")

#View(df3)

df2 <- left_join(sfx.df, df3, by = c("datetime" = "datetime2"))

#View(df2)

# Discrete spec pH regressed versus pH ###############################################

spc.ph.v.sfx.ph.int.v <- ggplotRegression(df2, "pH.check.median", "pH_int_v")

print(spc.ph.v.sfx.ph.int.v)


spc.ph.v.sfx.ph.ext.v <- ggplotRegression(df2, "pH.check.median", "pH_ext_v")

print(spc.ph.v.sfx.ph.ext.v)


spc.ph.v.sfx.ph.int <- ggplotRegression(df2, "pH.check.median", "pH_int")

print(spc.ph.v.sfx.ph.int)


spc.ph.v.sfx.ph.ext <- ggplotRegression(df2, "pH.check.median", "pH_ext")

print(spc.ph.v.sfx.ph.ext)



rm(df2)
rm(df3)



# # subsetting more for k0 #######################################
# pH_max <- 8.1
# pH_min <- 7.9
# 
# 
# df2 <- filter(df2, pH_int > pH_min & pH_int < pH_max)
# 
# 
# # Discrete spec pH regressed versus pH
# 
# spc.ph.v.sfx.ph.int.v.2 <- ggplotRegression(df2, "pH.check.median", "pH_int_v")
# 
# print(spc.ph.v.sfx.ph.int.v.2)
# 
# 
# spc.ph.v.sfx.ph.ext.v.2 <- ggplotRegression(df2, "pH.check.median", "pH_ext_v")
# 
# print(spc.ph.v.sfx.ph.ext.v.2)




# DEPLOYMENT QC Plots ALL DATA #######################################################


# "SeapHOx...." Deployment data for SeapHOx
# "sfx.df"                 
# "sfx.ext.v"               "sfx.int.v"               "sfx.o2"                 
# "sfx.pH.diff"             "sfx.ph.ext.v.o2"         "sfx.ph.ext.v.o2.ol.rm"  
# "sfx.ph.int.v.o2"         "sfx.ph.int.v.o2.ol.rm"   "sfx.pH_ext"             
# "sfx.pH_int"              "sfx.phs"                 "sfx.sal"                
# "sfx.sal.dt"              "sfx.temp"                "sfx.v_diff"             
# "spc.ph.v.sfx.ph.ext"     "spc.ph.v.sfx.ph.ext.2"   "spc.ph.v.sfx.ph.ext.v"  
# "spc.ph.v.sfx.ph.ext.v.2" "spc.ph.v.sfx.ph.int"     "spc.ph.v.sfx.ph.int.2"  
# "spc.ph.v.sfx.ph.int.v"   "spc.ph.v.sfx.ph.int.v.2"  


ggsave("qaqc_plots/2020/MARI-deployment_Volt_Sal_2020-all-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sfx.int.v), ggplotGrob(sfx.ext.v),
                              ggplotGrob(sfx.v_diff), ggplotGrob(sfx.sal.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-deployment_pHs_Sal_2020-all-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sfx.pH_int), ggplotGrob(sfx.pH_ext),
                              ggplotGrob(sfx.pH.diff), ggplotGrob(sfx.sal.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-deployment_pHs_T_S_O2_2020-all-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sfx.pH_int), ggplotGrob(sfx.pH_ext),
                              ggplotGrob(sfx.sal), ggplotGrob(sfx.temp),
                              ggplotGrob(sfx.o2),
                              size = "last")), width = 6.65, height = 3.5)

# regression  plots

ggsave("qaqc_plots/2020/MARI-deployment_pH_v_o2_2020-all-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sfx.ph.int.v.o2), ggplotGrob(sfx.ph.ext.v.o2),
                              size = "last")), width = 5, height = 5)

ggsave("qaqc_plots/2020/MARI-deployment_pH_v_o2_no_outliers_2020-all-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sfx.ph.int.v.o2.ol.rm), ggplotGrob(sfx.ph.ext.v.o2.ol.rm),
                              size = "last")), width = 5, height = 5)


#"spc.ph.v.sfx.ph.ext"          "spc.ph.v.sfx.ph.ext.v" 
# "spc.ph.v.sfx.ph.int"          "spc.ph.v.sfx.ph.int.v"     

ggsave("qaqc_plots/2020/MARI-deployment_spc.pH_v_sfX.pH.int_2020-all-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(spc.ph.v.sfx.ph.int.v), 
                              ggplotGrob(spc.ph.v.sfx.ph.int),
                              size = "last")), width = 5, height = 5)

ggsave("qaqc_plots/2020/MARI-deployment_spc.pH_v_sfX.pH.ext_2020-all-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(spc.ph.v.sfx.ph.ext.v), 
                              ggplotGrob(spc.ph.v.sfx.ph.ext),
                              size = "last")), width = 5, height = 5)




#plotting SELECT flagged data ###################################################

#set working directory and clear workspace

setwd("C:/Users/915712257/Box Sync/Inbox/oceanographic work/MARI SeapHOx assessment")

rm(list=ls())

#regression plot function (click carat to expand)
ggplotRegression <- function(dat, xvar, yvar){
  
  fml <- paste(yvar, "~", xvar)
  
  fit <- lm(fml, dat)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


load('mari.2020.select.combined.prcsd.flagged.RData')


# field check samples
load("2020 data/mari-check.samples-2020.RData")



# find data time bounds
head(sfx.df) 
tail(sfx.df)

#sfx season time bounds
#2020-03-11 19:20:00
#2020-10-09 18:20:00


#event time bounds
t1 <-  '2020-03-11 00:00:00'
t2 <- '2020-10-11 00:00:00'
#t2 <- '2020-03-26 00:00:00'

summary(sfx.df)


# datetime                      pH_int_v         pH_ext_v         abs_v_diff         ctd_temp        ctd_sal     
# Min.   :2020-03-11 19:20:00   Min.   :-1.056   Min.   :-1.0072   Min.   :0.04633   Min.   :10.95   Min.   :10.20  
# 1st Qu.:2020-05-03 17:00:00   1st Qu.:-1.048   1st Qu.:-0.9974   1st Qu.:0.04970   1st Qu.:13.73   1st Qu.:26.71  
# Median :2020-06-25 19:20:00   Median :-1.044   Median :-0.9930   Median :0.05033   Median :15.28   Median :28.83  
# Mean   :2020-06-25 18:58:54   Mean   :-1.043   Mean   :-0.9923   Mean   :0.05059   Mean   :15.31   Mean   :27.98  
# 3rd Qu.:2020-08-17 20:40:00   3rd Qu.:-1.038   3rd Qu.:-0.9870   3rd Qu.:0.05122   3rd Qu.:16.81   3rd Qu.:30.20  
# Max.   :2020-10-09 18:20:00   Max.   :-1.026   Max.   :-0.9754   Max.   :0.06226   Max.   :20.41   Max.   :32.06 
# 
# pH_int          pH_ext       abs_pH_diff       ctd_o2_mg_l       press_dbar       pH_temp         cond_Sm     
# Min.   :7.608   Min.   :7.495   Min.   :0.00000   Min.   : 4.903   Min.   :14.74   Min.   :10.99   Min.   :1.409  
# 1st Qu.:7.727   1st Qu.:7.729   1st Qu.:0.01260   1st Qu.: 6.625   1st Qu.:16.60   1st Qu.:13.74   1st Qu.:3.350  
# Median :7.804   Median :7.802   Median :0.02330   Median : 7.087   Median :17.40   Median :15.30   Median :3.591  
# Mean   :7.819   Mean   :7.816   Mean   :0.03038   Mean   : 7.219   Mean   :17.42   Mean   :15.33   Mean   :3.535  
# 3rd Qu.:7.904   3rd Qu.:7.912   3rd Qu.:0.03670   3rd Qu.: 7.747   3rd Qu.:18.23   3rd Qu.:16.82   3rd Qu.:3.841  
# Max.   :8.133   Max.   :8.146   Max.   :0.30750   Max.   :14.912   Max.   :20.62   Max.   :20.35   Max.   :4.090 

# color mappings for both electrode types ######

color_group_int <- c("seagreen","yellow2","red2")

color_group_ext <- c("green","yellow2","red2")

#olor_group_both <- c(color_group_int, color_group_ext)



# Voltages #########

p = ggplot(sfx.df, aes(datetime, pH_int_v))
sfx.int.v = p + 
  geom_point(aes(color = factor(pH_int_final_flag)), shape =21, size = 0.25) +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.06, -1.02) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        legend.position = "none",
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  scale_color_manual(values = color_group_int)

print(sfx.int.v)




p = ggplot(sfx.df, aes(datetime, pH_ext_v))
sfx.ext.v = p + 
  geom_point(aes(color = factor(pH_ext_final_flag)), shape =21, size = 0.25) +
  xlab("") + #last x label sets the time axis label
  ylab("V ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.015, -0.97) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        legend.position = "none",
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  scale_color_manual(values = color_group_ext)

print(sfx.ext.v)



p = ggplot(sfx.df, aes(datetime, abs_v_diff))
sfx.v_diff =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.03, 0.065) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.04, linetype="dashed", 
             color = "yellow", size=1) + 
  geom_hline(yintercept=0.065, linetype="dashed", 
             color = "red", size=1) +
  labs(caption = "yellow conservative line suggested by Rivest et al. 2016. may not apply to estuaries.")

print(sfx.v_diff)


# pH plots #########


p = ggplot(sfx.df, aes(datetime, pH_int))
sfx.pH_int = p +
  geom_point(aes(color = factor(pH_int_final_flag)), shape =21, size = 0.25) +
  xlab(" ") + #last x label sets the time axis label
  ylab("pH int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(7.2, 8.2) +
  geom_point(data = chk.df, aes(x=datetime, y= pH.check.median), shape = 18, size = 1,
             color = "black", show.legend = F)  +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        legend.position = "none",
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  scale_color_manual(values = color_group_int)

print(sfx.pH_int)



p = ggplot(sfx.df, aes(datetime, pH_ext))
sfx.pH_ext = p +
  geom_point(aes(color = factor(pH_ext_final_flag)), shape =21, size = 0.25) +
  xlab("") + #last x label sets the time axis label
  ylab("pH ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(7.2, 8.2) +
  geom_point(data = chk.df, aes(x=datetime, y= pH.check.median), shape = 18, size = 1,
             color = "black", show.legend = F)  +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        legend.position = "none",
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  scale_color_manual(values = color_group_ext)

print(sfx.pH_ext)


summary(sfx.df$abs_pH_diff)

p = ggplot(sfx.df, aes(datetime, abs_pH_diff))
sfx.pH.diff = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("pH abs-diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.35) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=1.0) +
  labs(caption = "red line threshold suggested by Seabird Electronics as suspect")


print(sfx.pH.diff)


p = ggplot(sfx.df, aes(datetime, abs_pH_diff))
sfx.pH.diff.no.note = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("pH abs-diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 0.35) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size=1.0)


print(sfx.pH.diff.no.note)


# environmental plots ##########################################################


p = ggplot(sfx.df, aes(datetime, ctd_sal))
sfx.sal = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("S\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(8, 32) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))




print(sfx.sal)


p = ggplot(sfx.df, aes(datetime, ctd_sal))
sfx.sal.dt = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab("Deployment MARI 2020") + #last x label sets the time axis label
  ylab("S\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(8, 32) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(sfx.sal.dt)



p = ggplot(sfx.df, aes(datetime, ctd_temp))
sfx.temp = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("") + #last x label sets the time axis label
  ylab("T\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(9, 21) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(sfx.temp)


p = ggplot(sfx.df, aes(datetime, ctd_o2_mg_l))
sfx.o2 = p + geom_point(aes(), size = 0.25, color = "red") +
  xlab("Deployment MARI 2020") + #last x label sets the time axis label
  ylab("O2 (mg/L)\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(4, 10) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size =5),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size =5,lineheight=3))


print(sfx.o2)


#regression plots ######################################

sfx.ph.int.v.o2 <- ggplotRegression(sfx.df, "pH_int", "ctd_o2_mg_l")

print(sfx.ph.int.v.o2)


sfx.ph.ext.v.o2 <- ggplotRegression(sfx.df, "pH_ext", "ctd_o2_mg_l")

print(sfx.ph.ext.v.o2)


#removing outliers ###############################

max_o2 = 10

min_pH = 7

df1 <- filter(sfx.df, ctd_o2_mg_l < max_o2)
df1 <- filter(df1, pH_ext > min_pH)

sfx.ph.int.v.o2.ol.rm <- ggplotRegression(df1, "pH_int", "ctd_o2_mg_l")

print(sfx.ph.int.v.o2.ol.rm)

sfx.ph.ext.v.o2.ol.rm <- ggplotRegression(df1, "pH_ext", "ctd_o2_mg_l")

print(sfx.ph.ext.v.o2.ol.rm)


# combine check samples to instrument data in a new data frame

# round sampling times of check samples since there is a slight mismatch in time

df3 <- chk.df

df3$datetime2 <- round_date(chk.df$datetime, "20 mins")

#View(df3)

df2 <- left_join(sfx.df, df3, by = c("datetime" = "datetime2"))

#View(df2)

# Discrete spec pH regressed versus pH ###############################################

spc.ph.v.sfx.ph.int.v <- ggplotRegression(df2, "pH.check.median", "pH_int_v")

print(spc.ph.v.sfx.ph.int.v)


spc.ph.v.sfx.ph.ext.v <- ggplotRegression(df2, "pH.check.median", "pH_ext_v")

print(spc.ph.v.sfx.ph.ext.v)


spc.ph.v.sfx.ph.int <- ggplotRegression(df2, "pH.check.median", "pH_int")

print(spc.ph.v.sfx.ph.int)


spc.ph.v.sfx.ph.ext <- ggplotRegression(df2, "pH.check.median", "pH_ext")

print(spc.ph.v.sfx.ph.ext)



rm(df2)
rm(df3)



# # subsetting more for k0 #######################################
# pH_max <- 8.1
# pH_min <- 7.9
# 
# 
# df2 <- filter(df2, pH_int > pH_min & pH_int < pH_max)
# 
# 
# # Discrete spec pH regressed versus pH
# 
# spc.ph.v.sfx.ph.int.v.2 <- ggplotRegression(df2, "pH.check.median", "pH_int_v")
# 
# print(spc.ph.v.sfx.ph.int.v.2)
# 
# 
# spc.ph.v.sfx.ph.ext.v.2 <- ggplotRegression(df2, "pH.check.median", "pH_ext_v")
# 
# print(spc.ph.v.sfx.ph.ext.v.2)




# DEPLOYMENT QC Plots SELECT DATA #######################################################


# "SeapHOx...." Deployment data for SeapHOx
# "sfx.df"                 
# "sfx.ext.v"               "sfx.int.v"               "sfx.o2"                 
# "sfx.pH.diff"             "sfx.ph.ext.v.o2"         "sfx.ph.ext.v.o2.ol.rm"  
# "sfx.ph.int.v.o2"         "sfx.ph.int.v.o2.ol.rm"   "sfx.pH_ext"             
# "sfx.pH_int"              "sfx.phs"                 "sfx.sal"                
# "sfx.sal.dt"              "sfx.temp"                "sfx.v_diff"             
# "spc.ph.v.sfx.ph.ext"     "spc.ph.v.sfx.ph.ext.2"   "spc.ph.v.sfx.ph.ext.v"  
# "spc.ph.v.sfx.ph.ext.v.2" "spc.ph.v.sfx.ph.int"     "spc.ph.v.sfx.ph.int.2"  
# "spc.ph.v.sfx.ph.int.v"   "spc.ph.v.sfx.ph.int.v.2"  

ggsave("qaqc_plots/2020/MARI-deployment_Volt_Sal_2020-select-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sfx.int.v), ggplotGrob(sfx.ext.v),
                              ggplotGrob(sfx.v_diff), ggplotGrob(sfx.sal.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-deployment_pHs_Sal_2020-select-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sfx.pH_int), ggplotGrob(sfx.pH_ext),
                              ggplotGrob(sfx.pH.diff), ggplotGrob(sfx.sal.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/MARI-deployment_pHs_T_S_O2_2020-select-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sfx.pH_int), ggplotGrob(sfx.pH_ext),
                              ggplotGrob(sfx.sal), ggplotGrob(sfx.temp),
                              ggplotGrob(sfx.o2),
                              size = "last")), width = 6.65, height = 3.5)

# regression  plots

ggsave("qaqc_plots/2020/MARI-deployment_pH_v_o2_2020-select-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sfx.ph.int.v.o2), ggplotGrob(sfx.ph.ext.v.o2),
                              size = "last")), width = 5, height = 5)

ggsave("qaqc_plots/2020/MARI-deployment_pH_v_o2_no_outliers_2020-select-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sfx.ph.int.v.o2.ol.rm), ggplotGrob(sfx.ph.ext.v.o2.ol.rm),
                              size = "last")), width = 5, height = 5)


#"spc.ph.v.sfx.ph.ext"          "spc.ph.v.sfx.ph.ext.v" 
# "spc.ph.v.sfx.ph.int"          "spc.ph.v.sfx.ph.int.v"     

ggsave("qaqc_plots/2020/MARI-deployment_spc.pH_v_sfX.pH.int_2020-select-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(spc.ph.v.sfx.ph.int.v), 
                              ggplotGrob(spc.ph.v.sfx.ph.int),
                              size = "last")), width = 5, height = 5)

ggsave("qaqc_plots/2020/MARI-deployment_spc.pH_v_sfX.pH.ext_2020-select-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(spc.ph.v.sfx.ph.ext.v), 
                              ggplotGrob(spc.ph.v.sfx.ph.ext),
                              size = "last")), width = 5, height = 5)



# all all all done folks