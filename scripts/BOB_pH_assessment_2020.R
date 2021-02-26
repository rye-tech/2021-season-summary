#load libraries #############################

library(ggplot2)
library(scales)
library(grid)
library(lubridate)
library(dplyr)
library(here)


##### set working directory ###############################

# use at your peril

# rm(list=ls())

setwd(here())

getwd()

# load regressing scatter plots function #########################################

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
# load("tidied-data/bob.2020.screen.RData")



# pre-deployment bay water bath check samples
load("data/chk-samples/bath.check.samples-2020.RData")

#tag for samples can be tied to time series
# chk.df$datetime.tag <- round_date(chk.df$datetime, "20 min")


#tag for samples can be tied to time series
bth.chk.df$datetime.tag <- bth.chk.df$datetime

#load predeployment Dickson Standard Run
load("tidied-data/bob/seafet/bob.dickson.run.predeploy.2020.sft.RData")

pre.dickson <- df
rm(df)






#load predeployment common bath 
load("tidied-data/bob/seafet/bob-pre.deploy.bath-2020-prcsd.RData")

pre.deploy.bath <- df1
rm(df1)




#### combine seafet and ctd data #####################
# # load seafet data
# load("2020 data/bob.seafet.pro.2020.RData")
# 
# # load ctd data and prep to merge
# load("2020 data/bob ctd raw/bob.ctd.2020.RData")
# df2 <- data
# rm(data)
# df2$datetime.tag <- round_date(df2$datetime, "20 min")
# 
# 
# # combine data files
# df <- left_join(df1, df2, by = "datetime.tag")
# 
# df <- select(df, datetime.x, datetime.tag,
#               pH_int_v, pH_ext_v, abs_v_diff, pH_temp, ctd_sal,
#               pH_int_cell, pH_ext_cell, abs_pH_diff,
#               sst, o2, chla, turb)
# 
# df <- df %>%
#   rename(datetime = "datetime.x",
#          ctd_temp =  "sst",
#          ctd_o2_mg_l = "o2",
#          ctd_chla = "chla",
#          ctd_turb = "turb")
# 
# 
# sft.df <- df
# rm(df, df1, df2)
# 
# save(sft.df, file = 'bob.2020.all.combined.prcsd.RData')

###### load combined seafet file ############################################
#load('bob.2020.all.combined.prcsd.RData')


#load MID Deployment Field Bath (N/A)



#load POST deployment common bath 
# load("tidied-data/bob-post.deploy.bath-2020-prcsd.RData")
# 
# df1 <- df1 %>%
#   rename(datetime = "datetime.x")
# 
# df1 <- select(df1, datetime, datetime.tag,
#               pH_int_v, pH_ext_v, abs_v_diff, pH_temp, ctd_sal,
#               pH_int_cell, pH_ext_cell, abs_pH_diff)
# 
# post.deploy.bath <- df1
# rm(df1)


#load POST deployment Dickson Standard Run
# load("tidied-data/bob-post-deploy-dickson-run-prcsd-2020.RData")
# 
# df1 <- select(df1, datetime,
#               pH_int_v, pH_ext_v, abs_v_diff, pH_temp,
#               pH_int, pH_ext, abs_pH_diff)
# 
# 
# post.dickson <- df1
# rm(df1)


#adjusted data with local calibration constant
# load("2020 data/bob.2020.adj.final.RData")
# adj.sft.df <- df1
# rm(df1)



# PRE deployment Dickson Standard Run Assessment ######################


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


pre.dickson$DL_pH_diff_int <- abs(pre.dickson$pH_int - pre.dickson$DL)
pre.dickson$DL_pH_diff_ext <- abs(pre.dickson$pH_ext - pre.dickson$DL)

#determine data time bounds

head(pre.dickson)
#"2020-07-03 00:40:00"
tail(pre.dickson)
#"2020-07-10 21:20:00"

#event time bounds
t1 <-  '2020-07-02 23:40:00'
t2 <- '2020-07-10 22:20:00'

# getting rid of erroneous data at end of run
# pre.dickson <- filter(pre.dickson, datetime < t2)

# adjust ylim with min and max from here
summary(pre.dickson$pH_int_v)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.9253 -0.9237 -0.9231 -0.9229 -0.9222 -0.9211 

# print(bob.2020.screen.df$int.v.min)
# # -1.06
# print(bob.2020.screen.df$int.v.max) 
# # -1.02


#adjusting plotting range for pH_int_v because it is out of range...

p = ggplot(pre.dickson, aes(x = datetime))
pre.d.int.v = p + geom_point(aes(y = pH_int_v), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-0.9253, -0.9211) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(pre.d.int.v)

#adjust ylim to min and max from here
summary(pre.dickson$pH_ext_v)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.8837 -0.8811 -0.8801 -0.8799 -0.8790 -0.8759 

#adjusting plotting range for pH_int_v because it is out of range...

print(bob.2020.screen.df$ext.v.min)
# -1.01
print(bob.2020.screen.df$ext.v.max) 
# -0.99

p = ggplot(pre.dickson, aes(datetime, pH_ext_v))
pre.d.ext.v = p + geom_point(aes(), size = 0.25, color = "green") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-0.8837, -0.8759) +
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



p = ggplot(pre.dickson, aes(x = datetime))
pre.d.pHs = p + geom_point(aes(y = pH_int), size = 0.25, color = "seagreen") +
  geom_point(aes(y = pH_ext), size = 0.25, color = "green") +
  geom_point(aes(y = DL), size = 0.25, color = "black") +
  xlab(" ") + #last x label sets the time axis label
  ylab("pH int, ext, dickson\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(8.0, 8.5) +
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

print(pre.d.pH.diff)




p = ggplot(pre.dickson, aes(x =datetime))
pre.d.DL.diff = p + geom_point(aes(y = DL_pH_diff_int), size = 0.25, color = "seagreen") +
  geom_point(aes(datetime, DL_pH_diff_ext), size = 0.25, color = "green") +
  xlab("Pre-Deployment Dickson Standard Run for BOB 2020") + #last x label sets the time axis label
  ylab("abs offset from Dickson\n")+
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
             color = "red", size=1.0)


print(pre.d.DL.diff)

summary(pre.dickson$pH_temp)


p = ggplot(pre.dickson, aes(datetime, pH_temp))
pre.d.t.dt = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("Pre-Deployment Dickson Standard Run for BOB 2020") + #last x label sets the time axis label
  ylab("T\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(15.0, 21.0) +
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


# adjusted PRE deployment Dickson standard plots to more stable section #######################

# for setting up a regression when electrodes have stabilized
t2 <-  '2020-04-22 22:00:00'
t3 <- '2020-04-22 00:00:00'
pre.dickson.trim <- filter(pre.dickson, datetime > t3)

p = ggplot(pre.dickson.trim, aes(x = datetime))
pre.d.int.v.trim = p + geom_point(aes(y = pH_int_v), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t3, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.05, -1.04) +
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
  ylim(-1.02, -0.99) +
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
pre.d.pHs.trim = p + geom_point(aes(y = pH_int), size = 0.25, color = "seagreen") +
  geom_point(aes(y = pH_ext), size = 0.25, color = "green") +
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
  xlab("Pre-Deployment Dickson Standard Run for BOB April 22nd 2020") + #last x label sets the time axis label
  ylab("T\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t3, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(16.0, 20.0) +
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
pre.d.pH.int.v.Volt.int.trim <- ggplotRegression(pre.dickson.trim, "pH_int", "pH_int_v")

print(pre.d.pH.int.v.Volt.int.trim)


pre.d.pH.ext.v.Volt.ext.trim <- ggplotRegression(pre.dickson.trim, "pH_ext", "pH_ext_v")

print(pre.d.pH.ext.v.Volt.ext.trim)









# PRE deployment bath plots ####################################################################


head(pre.deploy.bath)
"2020-07-24 06:00:00"

tail(pre.deploy.bath)
"2020-09-24 05:00:00"

#event time bounds
t1 <-  '2020-07-24 05:00:00'
t2 <- '2020-09-24 06:00:00'

# adjust ylim with min and max
summary(pre.deploy.bath$pH_int_v)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.9362 -0.9342 -0.9311 -0.9323 -0.9307 -0.9300 



p = ggplot(pre.deploy.bath, aes(x = datetime))
pre.b.int.v = p + geom_point(aes(y = pH_int_v), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-0.9362, -0.9300) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(pre.b.int.v)

# adjust ylim with min and max
summary(pre.deploy.bath$pH_ext_v)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.8835 -0.8814 -0.8794 -0.8800 -0.8787 -0.8776



p = ggplot(pre.deploy.bath, aes(datetime, pH_ext_v))
pre.b.ext.v = p + geom_point(aes(), size = 0.25, color = "green") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-0.8835, -0.8776) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(pre.b.ext.v)

# leave ylim at 0.03 and max at 0.065
# just check to see if they are in range
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


#need to adjust plots below similar to mari plots

p = ggplot(pre.deploy.bath, aes(datetime))
pre.b.phs = p + geom_point(aes(y = pH_int), size = 0.25, color = "seagreen") +
  geom_point(aes(y = pH_ext), size = 0.25, color = "green") +
  xlab("") + #last x label sets the time axis label
  ylab("pH int (dark green)\n ext (light green)\n check samples (black)\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(8.0, 8.2) +
  geom_point(data = bth.chk.df, aes(x=datetime, y= pH), shape = 18, size = 1,
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
  ylim(25, 34) +
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
  xlab("Pre-Deployment Bay Water Bath for BOB 2020") + #last x label sets the time axis label
  ylab("S\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(25, 34) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(pre.b.s.dt)


summary(pre.deploy.bath$pH_temp)

p = ggplot(pre.deploy.bath, aes(datetime, pH_temp))
pre.b.t.dt = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("Pre-Deployment Bay Water Bath for BOB 2020") + #last x label sets the time axis label
  ylab("SeaFET T\n")+ 
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

# df1 <- left_join(pre.deploy.bath, bth.chk.df, by = "datetime.tag")
# 
# 
# # Discrete spec pH regressed versus pH
# 
# k0.int.pre.b.benchmark <- ggplotRegression(df1, "pH.check.median", "pH_int_v")
# 
# print(k0.int.pre.b.benchmark)
# 
# 
# k0.ext.pre.b.benchmark <- ggplotRegression(df1, "pH.check.median", "pH_ext_v")
# 
# print(k0.ext.pre.b.benchmark)
# 
# rm(df1)
# 


# saving plots of predeployment runs ------------------------------------------------

ls()

# [1] "bth.chk.df"       "offset_std"       "p"                "pre.b.abs.v_diff" "pre.b.ext.v"     
# [6] "pre.b.int.v"      "pre.b.pH.diff"    "pre.b.phs"        "pre.b.s"          "pre.b.s.dt"      
# [11] "pre.b.t.dt"       "pre.d.DL.diff"    "pre.d.ext.v"      "pre.d.int.v"      "pre.d.pH.diff"   
# [16] "pre.d.pHs"        "pre.d.t.dt"       "pre.d.v_diff"     "pre.deploy.bath"  "pre.dickson"     
# [21] "sal_std"          "t1"               "t2"



ggsave("plots/bob/BOB-pre.deployment_dickson.std.volts.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.int.v), ggplotGrob(pre.d.ext.v),
                              ggplotGrob(pre.d.v_diff), ggplotGrob(pre.d.t.dt), 
                              size = "first")), width = 6.65, height = 3.5)



ggsave("plots/bob/BOB-pre.deployment_dickson.std.pHs.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.pHs), ggplotGrob(pre.d.pH.diff),
                              ggplotGrob(pre.d.t.dt), 
                              size = "first")), width = 6.65, height = 3.5)

# save pre-deployment bath plots

ggsave("plots/bob/BOB-pre-deployment_bath_Volt_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.b.int.v), ggplotGrob(pre.b.ext.v),
                              ggplotGrob(pre.b.abs.v_diff), ggplotGrob(pre.b.s.dt),
                              size = "first")), width = 6.65, height = 3.5)

ggsave("plots/bob/BOB-pre-deployment_bath_pHs_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.b.phs), ggplotGrob(pre.b.pH.diff),
                              ggplotGrob(pre.b.s.dt),
                              size = "first")), width = 6.65, height = 3.5)

ggsave("plots/bob/BOB-pre-deployment_bath_T_S_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.b.s), ggplotGrob(pre.b.t.dt),
                              size = "first")), width = 6.65, height = 3.5)

ggsave("plots/bob/BOB-pre-deployment_bath_pHs_T_S_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.b.phs), ggplotGrob(pre.b.pH.diff),
                              ggplotGrob(pre.b.s), ggplotGrob(pre.b.t.dt),
                              size = "first")), width = 6.65, height = 3.5)



# DEPLOYMENT PLOTS #####################################################################

# find data time bounds
head(sft.df) 
tail(sft.df)

#sft season time bounds
"2020-05-01 20:20:04"
"2020-10-24 17:00:13"


#event time bounds
t1 <-  "2020-05-01 20:00:04"
t2 <- "2020-10-24 17:20:13"
#t2 <- '2020-03-26 00:00:00'

summary(sft.df)

# datetime                    datetime.tag                    pH_int_v         pH_ext_v      
# Min.   :2020-05-01 20:20:04   Min.   :2020-05-01 20:20:00   Min.   :-1.188   Min.   :-1.0605  
# 1st Qu.:2020-05-28 02:55:06   1st Qu.:2020-05-28 02:55:00   1st Qu.:-1.147   1st Qu.:-1.0366  
# Median :2020-06-23 09:30:08   Median :2020-06-23 09:30:00   Median :-1.097   Median :-1.0225  
# Mean   :2020-07-05 07:20:27   Mean   :2020-07-05 07:20:19   Mean   :-1.107   Mean   :-1.0238  
# 3rd Qu.:2020-08-06 20:15:10   3rd Qu.:2020-08-06 20:15:00   3rd Qu.:-1.072   3rd Qu.:-1.0117  
# Max.   :2020-10-24 17:00:13   Max.   :2020-10-24 17:00:00   Max.   :-1.052   Max.   :-0.9899  

# abs_v_diff         pH_temp         ctd_sal         pH_int_cell     pH_ext_cell   
# Min.   :0.04988   Min.   :12.77   Min.   : 0.9547   Min.   :5.815   Min.   :6.406  
# 1st Qu.:0.05738   1st Qu.:15.89   1st Qu.:19.3933   1st Qu.:6.523   1st Qu.:7.480  
# Median :0.06306   Median :16.99   Median :24.4275   Median :7.381   Median :7.729  
# Mean   :0.08363   Mean   :17.12   Mean   :23.1226   Mean   :7.209   Mean   :7.677  
# 3rd Qu.:0.12561   3rd Qu.:18.31   3rd Qu.:27.2102   3rd Qu.:7.832   3rd Qu.:7.862  
# Max.   :0.14279   Max.   :21.79   Max.   :31.7812   Max.   :8.186   Max.   :8.235

# abs_pH_diff         ctd_temp      ctd_o2_mg_l        ctd_chla          ctd_turb     
# Min.   :0.00000   Min.   :12.67   Min.   : 5.258   Min.   : 0.7123   Min.   : 1.248  
# 1st Qu.:0.02759   1st Qu.:15.80   1st Qu.: 7.287   1st Qu.: 1.5256   1st Qu.: 3.938  
# Median :0.05962   Median :16.93   Median : 7.744   Median : 1.9048   Median : 5.672  
# Mean   :0.47858   Mean   :17.04   Mean   : 7.788   Mean   : 3.2223   Mean   : 9.572  
# 3rd Qu.:1.26959   3rd Qu.:18.24   3rd Qu.: 8.245   3rd Qu.: 3.0994   3rd Qu.:12.369  
# Max.   :1.58917   Max.   :22.21   Max.   :10.202   Max.   :49.2383   Max.   :24.700 

p = ggplot(sft.df, aes(datetime, pH_int_v))
sft.int.v =p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.2, -1.05) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))


print(sft.int.v)



p = ggplot(sft.df, aes(datetime, pH_ext_v))
sft.ext.v = p + geom_point(aes(), size = 0.25, color = "green") +
  xlab("") + #last x label sets the time axis label
  ylab("V ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.065, -0.98) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(sft.ext.v)

p = ggplot(sft.df, aes(datetime, abs_v_diff))
sft.v_diff =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.03, 0.08) +
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

print(sft.v_diff)


p = ggplot(sft.df, aes(datetime, abs_v_diff))
sft.v_diff.2 =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.03, 0.3) +
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

print(sft.v_diff.2)



p = ggplot(sft.df, aes(datetime, pH_int_cell))
sft.pH_int = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("pH int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(5.8, 8.3) +
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


print(sft.pH_int)



p = ggplot(sft.df, aes(datetime, pH_ext_cell))
sft.pH_ext = p + geom_point(aes(), size = 0.25, color = "green") +
  xlab("") + #last x label sets the time axis label
  ylab("pH ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(5.8, 8.3) +
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

print(sft.pH_ext)



p = ggplot(sft.df, aes(datetime))
sft.phs = p + geom_point(aes(y = pH_int_cell), size = 0.25, color = "seagreen") +
  geom_point(aes(y = pH_ext_cell), size = 0.25, color = "green") +
  xlab("") + #last x label sets the time axis label
  ylab("pH int, ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(5.8, 8.3) +
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


print(sft.phs)


summary(sft.df$abs_pH_diff)

p = ggplot(sft.df, aes(datetime, abs_pH_diff))
sft.pH.diff = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("pH abs-diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 1.6) +
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


print(sft.pH.diff)


p = ggplot(sft.df, aes(datetime, abs_pH_diff))
sft.pH.diff.no.note = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("pH abs-diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 1.6) +
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


print(sft.pH.diff.no.note)



p = ggplot(sft.df, aes(datetime, ctd_sal))
sft.sal = p + geom_point(aes(), size = 0.25, color = "seagreen") +
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

print(sft.sal)


p = ggplot(sft.df, aes(datetime, ctd_sal))
sft.sal.dt = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab("Deployment BOB 2020") + #last x label sets the time axis label
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

print(sft.sal.dt)



p = ggplot(sft.df, aes(datetime, pH_temp))
sft.temp = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("") + #last x label sets the time axis label
  ylab("T\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(12, 22) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(sft.temp)


p = ggplot(sft.df, aes(datetime, ctd_o2_mg_l))
sft.o2 = p + geom_point(aes(), size = 0.25, color = "red") +
  xlab("Deployment BOB 2020") + #last x label sets the time axis label
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


print(sft.o2)


#regression plots

sft.ph.int.v.o2 <- ggplotRegression(sft.df, "pH_int_cell", "ctd_o2_mg_l")

print(sft.ph.int.v.o2)


sft.ph.ext.v.o2 <- ggplotRegression(sft.df, "pH_ext_cell", "ctd_o2_mg_l")

print(sft.ph.ext.v.o2)


#removing outliers

max_o2 = 10

min_pH = 7

df1 <- filter(sft.df, ctd_o2_mg_l < max_o2)
df1 <- filter(df1, pH_ext_cell > min_pH)

sft.ph.int.v.o2.ol.rm <- ggplotRegression(df1, "pH_int_cell", "ctd_o2_mg_l")

print(sft.ph.int.v.o2.ol.rm)

sft.ph.ext.v.o2.ol.rm <- ggplotRegression(df1, "pH_ext_cell", "ctd_o2_mg_l")

print(sft.ph.ext.v.o2.ol.rm)


# combine check samples to instrument data in a new data frame

# round sampling times of check samples since there is a slight mismatch in time

df3 <- chk.df
  
df2 <- left_join(sft.df, df3, by = "datetime.tag")

View(df2)

# Discrete spec pH regressed versus pH

spc.ph.v.sft.ph.int.v <- ggplotRegression(df2, "pH.check.median", "pH_int_v")

print(spc.ph.v.sft.ph.int.v)


spc.ph.v.sft.ph.ext.v <- ggplotRegression(df2, "pH.check.median", "pH_ext_v")

print(spc.ph.v.sft.ph.ext.v)


spc.ph.v.sft.ph.int <- ggplotRegression(df2, "pH.check.median", "pH_int_cell")

print(spc.ph.v.sft.ph.int)


spc.ph.v.sft.ph.ext <- ggplotRegression(df2, "pH.check.median", "pH_ext_cell")

print(spc.ph.v.sft.ph.ext)


# # subsetting more for k0 if needed
# pH_max <- 8.1
# pH_min <- 7.9
# 
# 
# df2 <- filter(df2, pH_int > pH_min & pH_int < pH_max)
# 
# 
# # Discrete spec pH regressed versus pH
# 
# spc.ph.v.sft.ph.int.v.2 <- ggplotRegression(df2, "pH.check.median", "pH_int_v")
# 
# print(spc.ph.v.sft.ph.int.v.2)
# 
# 
# spc.ph.v.sft.ph.ext.v.2 <- ggplotRegression(df2, "pH.check.median", "pH_ext_v")
# 
# print(spc.ph.v.sft.ph.ext.v.2)
# 
# 
# spc.ph.v.sft.ph.int.2 <- ggplotRegression(df2, "pH.check.median", "pH_int")
# 
# print(spc.ph.v.sft.ph.int.2)
# 
# 
# spc.ph.v.sft.ph.ext.2 <- ggplotRegression(df2, "pH.check.median", "pH_ext")
# 
# print(spc.ph.v.sft.ph.ext.2)
# 

rm(df1,df2,df3)



# ADJUSTED DEPLOYMENT PLOTS #####################################################################

# find data time bounds
head(adj.sft.df) 
tail(adj.sft.df)

#adj.sft season time bounds
#2020-03-11 19:20:00
#2020-10-09 18:20:00


#event time bounds
t1 <-  '2020-03-11 00:00:00'
t2 <- '2020-10-11 00:00:00'
#t2 <- '2020-03-26 00:00:00'

summary(adj.sft.df)

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

p = ggplot(adj.sft.df, aes(datetime, pH_int_v))
adj.sft.int.v =p + geom_point(aes(), size = 0.25, color = "seagreen") +
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


print(adj.sft.int.v)



p = ggplot(adj.sft.df, aes(datetime, pH_ext_v))
adj.sft.ext.v = p + geom_point(aes(), size = 0.25, color = "green") +
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

print(adj.sft.ext.v)

p = ggplot(adj.sft.df, aes(datetime, abs_v_diff))
adj.sft.v_diff =p + geom_point(aes(), size = 0.25, color = "black") +
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

print(adj.sft.v_diff)

p = ggplot(adj.sft.df, aes(datetime, pH_int_cell))
adj.sft.pH_int = p + geom_point(aes(), size = 0.25, color = "seagreen") +
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


print(adj.sft.pH_int)



p = ggplot(adj.sft.df, aes(datetime, pH_ext_cell))
adj.sft.pH_ext = p + geom_point(aes(), size = 0.25, color = "green") +
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

print(adj.sft.pH_ext)



p = ggplot(adj.sft.df, aes(datetime))
adj.sft.phs = p + geom_point(aes(y = pH_int_cell), size = 0.25, color = "seagreen") +
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


print(adj.sft.phs)


summary(adj.sft.df$abs_pH_diff)

p = ggplot(adj.sft.df, aes(datetime, abs_pH_diff))
adj.sft.pH.diff = p + geom_point(aes(), size = 0.25, color = "black") +
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


print(adj.sft.pH.diff)


p = ggplot(adj.sft.df, aes(datetime, abs_pH_diff))
adj.sft.pH.diff.no.note = p + geom_point(aes(), size = 0.25, color = "black") +
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


print(adj.sft.pH.diff.no.note)



p = ggplot(adj.sft.df, aes(datetime, ctd_sal))
adj.sft.sal = p + geom_point(aes(), size = 0.25, color = "seagreen") +
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

print(adj.sft.sal)


p = ggplot(adj.sft.df, aes(datetime, ctd_sal))
adj.sft.sal.dt = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab("Deployment BOB 2020") + #last x label sets the time axis label
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

print(adj.sft.sal.dt)



p = ggplot(adj.sft.df, aes(datetime, pH_temp))
adj.sft.temp = p + geom_point(aes(), size = 0.25, color = "red4") +
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

print(adj.sft.temp)


p = ggplot(adj.sft.df, aes(datetime, ctd_o2_mg_l))
adj.sft.o2 = p + geom_point(aes(), size = 0.25, color = "red") +
  xlab("Deployment BOB 2020") + #last x label sets the time axis label
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


print(adj.sft.o2)


#regression plots

adj.sft.ph.int.v.o2 <- ggplotRegression(adj.sft.df, "pH_int_cell", "ctd_o2_mg_l")

print(adj.sft.ph.int.v.o2)


adj.sft.ph.ext.v.o2 <- ggplotRegression(adj.sft.df, "pH_ext_cell", "ctd_o2_mg_l")

print(adj.sft.ph.ext.v.o2)


#removing outliers

max_o2 = 10

min_pH = 7

df1 <- filter(adj.sft.df, ctd_o2_mg_l < max_o2)
df1 <- filter(df1, pH_ext > min_pH)

adj.sft.ph.int.v.o2.ol.rm <- ggplotRegression(df1, "pH_int", "ctd_o2_mg_l")

print(adj.sft.ph.int.v.o2.ol.rm)

adj.sft.ph.ext.v.o2.ol.rm <- ggplotRegression(df1, "pH_ext", "ctd_o2_mg_l")

print(adj.sft.ph.ext.v.o2.ol.rm)


# combine check samples to instrument data in a new data frame

# round sampling times of check samples since there is a slight mismatch in time

df3 <- chk.df

df3$datetime2 <- round_date(chk.df$datetime, "20 mins")

View(df3)

df2 <- left_join(adj.sft.df, df3, by = c("datetime" = "datetime2"))

View(df2)

# Discrete spec pH regressed versus pH


spc.ph.v.adj.sft.ph.int <- ggplotRegression(df2, "pH.check.median", "pH_int_cell")

print(spc.ph.v.adj.sft.ph.int)


spc.ph.v.adj.sft.ph.ext <- ggplotRegression(df2, "pH.check.median", "pH_ext_cell")

print(spc.ph.v.adj.sft.ph.ext)

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
# spc.ph.v.adj.sft.ph.int.v.2 <- ggplotRegression(df2, "pH.check.median", "pH_int_v")
# 
# print(spc.ph.v.adj.sft.ph.int.v.2)
# 
# 
# spc.ph.v.adj.sft.ph.ext.v.2 <- ggplotRegression(df2, "pH.check.median", "pH_ext_v")
# 
# print(spc.ph.v.adj.sft.ph.ext.v.2)
# 
# 
# spc.ph.v.adj.sft.ph.int.2 <- ggplotRegression(df2, "pH.check.median", "pH_int")
# 
# print(spc.ph.v.adj.sft.ph.int.2)
# 
# 
# spc.ph.v.adj.sft.ph.ext.2 <- ggplotRegression(df2, "pH.check.median", "pH_ext")
# 
# print(spc.ph.v.adj.sft.ph.ext.2)
# 

rm(df2)
rm(df3)



#### NO MID DEPLOYMENT FIELD BATH PLOTS #########################################
# 
# 
# head(field.bath)
# "2020-05-21 17:27:00"
# 
# tail(field.bath)
# "2020-05-21 18:05:00"
# 
# #event time bounds
# t1 <-  '2020-05-21 17:26:00'
# t2 <- '2020-05-21 18:06:00'
# 
# 
# p = ggplot(field.bath, aes(x = datetime))
# mid.b.int.v = p + geom_point(aes(y = pH_int_v), size = 0.25, color = "seagreen") +
#   xlab(" ") + #last x label sets the time axis label
#   ylab("V int\n")+ 
#   scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
#   xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
#          as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
#   ylim(bob.2020.screen.df$int.v.min, bob.2020.screen.df$int.v.max) +
#   theme_minimal() +
#   theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
#         panel.grid.minor = element_line(colour = "grey", size = 0.25), 
#         axis.title.x=element_blank(), 
#         axis.text.x=element_blank(), 
#         axis.text.y = element_text(size = 5),
#         axis.ticks.x=element_blank(), 
#         axis.title.y = element_text(size =5,lineheight=3))
# 
# 
# print(mid.b.int.v)
# 
# 
# p = ggplot(field.bath, aes(datetime, pH_ext_v))
# mid.b.ext.v = p + geom_point(aes(), size = 0.25, color = "green") +
#   xlab(" ") + #last x label sets the time axis label
#   ylab("V ext\n")+ 
#   scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
#   xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
#          as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
#   ylim(bob.2020.screen.df$ext.v.min, bob.2020.screen.df$ext.v.max) +
#   theme_minimal() +
#   theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
#         panel.grid.minor = element_line(colour = "grey", size = 0.25), 
#         axis.title.x=element_blank(), 
#         axis.text.x=element_blank(), 
#         axis.text.y = element_text(size = 5),
#         axis.ticks.x=element_blank(), 
#         axis.title.y = element_text(size =5,lineheight=3))
# 
# 
# print(mid.b.ext.v)
# 
# summary(field.bath$abs_v_diff)
# 
# p = ggplot(field.bath, aes(datetime, abs_v_diff))
# mid.b.abs.v_diff =p + geom_point(aes(), size = 0.25, color = "black") +
#   xlab("") + #last x label sets the time axis label
#   ylab("V abs-diff\n")+ 
#   scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
#   xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
#          as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
#   ylim(0.03, 0.065) +
#   theme_minimal() +
#   theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
#         panel.grid.minor = element_line(colour = "grey", size = 0.25), 
#         axis.title.x=element_blank(), 
#         axis.text.x=element_blank(), 
#         axis.text.y = element_text(size = 5),
#         axis.ticks.x=element_blank(), 
#         axis.title.y = element_text(size =5,lineheight=3)) +
#   geom_hline(yintercept=0.04, linetype="dashed", 
#              color = "yellow", size=1) + 
#   geom_hline(yintercept=0.065, linetype="dashed", 
#              color = "red", size=1) +
#   labs(caption = "yellow conservative line suggested by Rivest et al. 2016. may not apply to estuaries.")
# 
# print(mid.b.abs.v_diff)
# 
# 
# 
# p = ggplot(field.bath, aes(datetime))
# mid.b.phs = p + geom_point(aes(y = pH_int), size = 0.25, color = "seagreen") +
#   geom_point(aes(y = pH_ext), size = 0.25, color = "green") +
#   xlab("") + #last x label sets the time axis label
#   ylab("pH int, ext, dickson\n")+ 
#   scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
#   xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
#          as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
#   ylim(7.6, 7.8) +
#   geom_point(data = bth.chk.df, aes(x=datetime, y= pH.check.median), shape = 18, size = 1,
#              color = "black", show.legend = F)  +
#   theme_minimal() +
#   theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
#         panel.grid.minor = element_line(colour = "grey", size = 0.25),
#         axis.title.x=element_blank(), 
#         axis.text.x=element_blank(), 
#         axis.text.y = element_text(size = 5),
#         axis.ticks.x=element_blank(), 
#         axis.title.y = element_text(size =5,lineheight=3)) +
#   theme(legend.title=element_blank(), legend.position = "right") 
# 
# 
# print(mid.b.phs)
# 
# 
# 
# p = ggplot(field.bath, aes(datetime, abs_pH_diff))
# mid.b.pH.diff = p + geom_point(aes(), size = 0.25, color = "black") +
#   xlab("") + #last x label sets the time axis label
#   ylab("electrode abs diff\n")+
#   scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
#   xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
#          as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
#   ylim(0, 0.2) +
#   theme_minimal() +
#   theme(panel.grid.major = element_line(colour = "black", size = 0.25),  #theme for bottom plot
#         panel.grid.minor = element_line(colour = "grey", size = 0.25),
#         axis.title.x= element_blank(),
#         axis.text.x= element_blank(),
#         axis.text.y = element_text(size =5),
#         axis.ticks.x=element_blank(),
#         axis.title.y = element_text(size =5,lineheight=3)) +
#   geom_hline(yintercept=0.05, linetype="dashed", 
#              color = "red", size=0.5) +
#   labs(caption = "red line threshold suggested by Seabird Electronics as suspect")
# 
# 
# print(mid.b.pH.diff)
# 
# 
# p = ggplot(field.bath, aes(datetime, ctd_sal))
# mid.b.s = p + geom_point(aes(), size = 0.25, color = "seagreen") +
#   xlab("") + #last x label sets the time axis label
#   ylab("S\n")+ 
#   scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
#   xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
#          as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
#   ylim(15, 34) +
#   theme_minimal() +
#   theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
#         panel.grid.minor = element_line(colour = "grey", size = 0.25), 
#         axis.title.x=element_blank(), 
#         axis.text.x=element_blank(), 
#         axis.text.y = element_text(size = 5),
#         axis.ticks.x=element_blank(), 
#         axis.title.y = element_text(size =5,lineheight=3))
# 
# print(mid.b.s)
# 
# 
# p = ggplot(field.bath, aes(datetime, ctd_sal))
# mid.b.s.dt = p + geom_point(aes(), size = 0.25, color = "seagreen") +
#   xlab("Mid-Deployment Common Bath for BOB 5/21/2020") + #last x label sets the time axis label
#   ylab("S\n")+ 
#   scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
#   xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
#          as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
#   ylim(15, 34) +
#   theme_minimal() +
#   theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
#         panel.grid.minor = element_line(colour = "grey", size = 0.25), 
#         axis.title.x= element_text(size =10,lineheight=3),
#         axis.text.x= element_text(size =5),
#         axis.text.y = element_text(size = 5),
#         axis.ticks.x=element_blank(), 
#         axis.title.y = element_text(size =5,lineheight=3))
# 
# print(mid.b.s.dt)
# 
# 
# summary(field.bath$pH_temp)
# 
# p = ggplot(field.bath, aes(datetime, pH_temp))
# mid.b.t.dt = p + geom_point(aes(), size = 0.25, color = "red4") +
#   xlab("Mid-Deployment Common Bath for BOB 5/21/2020") + #last x label sets the time axis label
#   ylab("T\n")+ 
#   scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
#   xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
#          as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
#   ylim(15, 17) +
#   theme_minimal() +
#   theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
#         panel.grid.minor = element_line(colour = "grey", size = 0.25), 
#         axis.title.x= element_text(size =10,lineheight=3),
#         axis.text.x= element_text(size =5),
#         axis.text.y = element_text(size = 5),
#         axis.ticks.x=element_blank(), 
#         axis.title.y = element_text(size =5,lineheight=3))
# 
# print(mid.b.t.dt)
# 
# 
# 
# 
# 
# 
# # combine check samples to instrument data in a new data frame
# 
# df1 <- left_join(field.bath, bth.chk.df, by = "datetime")
# 
# 
# # Discrete spec pH regressed versus pH
# 
# k0.int.mid.b.benchmark <- ggplotRegression(df1, "pH.check.median", "pH_int_v")
# 
# print(k0.int.mid.b.benchmark)
# 
# 
# k0.ext.mid.b.benchmark <- ggplotRegression(df1, "pH.check.median", "pH_ext_v")
# 
# print(k0.ext.mid.b.benchmark)
# 
# 
# # removing outlier bath sample
# 
# # substitute for t1 and t2 in stabilized pH measurement
# t3 <- '2020-05-21 00:00:00'
# t4 <- '2020-05-21 18:00:00'
# 
# df1 <- filter(df1, datetime > t3 & datetime < t4)
# 
# 
# 
# 
# 
# # Discrete spec pH regressed versus pH
# 
# k0.int.mid.b.benchmark.2pts <- ggplotRegression(df1, "pH.check.median", "pH_int_v")
# 
# print(k0.int.mid.b.benchmark.2pts)
# 
# 
# k0.ext.mid.b.benchmark.2pts <- ggplotRegression(df1, "pH.check.median", "pH_ext_v")
# 
# print(k0.ext.mid.b.benchmark.2pts)
# 
# 
# rm(df1)
# 


############# POST DEPLOYMENT BATH PLOTS  ##########################################


head(post.deploy.bath)
"2020-10-30 21:40:04"

tail(post.deploy.bath)
"2020-11-07 02:30:11"

#event time bounds
t1 <-  '2020-10-30 20:40:04'
t2 <- '2020-11-07 03:30:11'


summary(post.deploy.bath)

# datetime                    datetime.tag                    pH_int_v         pH_ext_v     
# Min.   :2020-10-30 21:40:04   Min.   :2020-10-30 21:40:00   Min.   :-1.150   Min.   :-1.017  
# 1st Qu.:2020-11-01 20:15:06   1st Qu.:2020-11-01 20:15:00   1st Qu.:-1.148   1st Qu.:-1.016  
# Median :2020-11-03 18:50:08   Median :2020-11-03 18:50:00   Median :-1.147   Median :-1.016  
# Mean   :2020-11-03 17:46:10   Mean   :2020-11-03 17:46:32   Mean   :-1.148   Mean   :-1.015  
# 3rd Qu.:2020-11-05 17:25:10   3rd Qu.:2020-11-05 17:25:00   3rd Qu.:-1.147   3rd Qu.:-1.015  
# Max.   :2020-11-07 02:30:13   Max.   :2020-11-07 02:40:00   Max.   :-1.145   Max.   :-1.012  


# abs_v_diff        pH_temp         ctd_sal       pH_int_cell     pH_ext_cell   
# Min.   :0.1295   Min.   :13.64   Min.   :31.69   Min.   :6.480   Min.   :7.950  
# 1st Qu.:0.1314   1st Qu.:14.50   1st Qu.:32.12   1st Qu.:6.506   1st Qu.:7.987  
# Median :0.1321   Median :14.99   Median :32.55   Median :6.519   Median :7.997  
# Mean   :0.1321   Mean   :15.17   Mean   :32.49   Mean   :6.517   Mean   :7.999  
# 3rd Qu.:0.1329   3rd Qu.:15.71   3rd Qu.:32.90   3rd Qu.:6.529   3rd Qu.:8.011  
# Max.   :0.1339   Max.   :17.29   Max.   :33.05   Max.   :6.554   Max.   :8.041  

# abs_pH_diff   
# Min.   :1.437  
# 1st Qu.:1.462  
# Median :1.490  
# Mean   :1.481  
# 3rd Qu.:1.497  
# Max.   :1.503 




p = ggplot(post.deploy.bath, aes(x = datetime))
post.b.int.v = p + geom_point(aes(y = pH_int_v), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.16, -1.14) +
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
  ylim(-1.02, -1.01) +
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
  ylim(0.03, 0.3) +
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
post.b.ext_ph = p + geom_point(aes(y = pH_ext_cell), size = 0.25, color = "green") +
  xlab("") + #last x label sets the time axis label
  ylab("pH int, ext, dickson\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(7.9, 8.1) +
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


print(post.b.ext_ph)

p = ggplot(post.deploy.bath, aes(datetime))
post.b.phs = p + geom_point(aes(y = pH_int_cell), size = 0.25, color = "seagreen") +
  geom_point(aes(y = pH_ext_cell), size = 0.25, color = "green") +
  xlab("") + #last x label sets the time axis label
  ylab("pH int, ext, dickson\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(6.4, 8.2) +
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
  ylim(0, 1.6) +
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



p = ggplot(post.deploy.bath, aes(datetime, abs_pH_diff))
post.b.pH.diff.2 = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("electrode abs diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(1.4, 1.6) +
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


print(post.b.pH.diff.2)


p = ggplot(post.deploy.bath, aes(datetime, ctd_sal))
post.b.s = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab("") + #last x label sets the time axis label
  ylab("S\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(31, 34) +
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
  xlab("Post-Deployment Common Bath for BOB 2020") + #last x label sets the time axis label
  ylab("S\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(31, 34) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_text(size =10,lineheight=3),
        axis.text.x= element_text(size =5),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(post.b.s.dt)


summary(post.deploy.bath$pH_temp)

p = ggplot(post.deploy.bath, aes(datetime, pH_temp))
post.b.t.dt = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("Post-Deployment Common Bath for BOB 2020") + #last x label sets the time axis label
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

df1 <- left_join(post.deploy.bath, bth.chk.df, by = "datetime.tag")


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


post.dickson$DL_pH_diff_int <- abs(post.dickson$pH_int - post.dickson$DL)
post.dickson$DL_pH_diff_ext <- abs(post.dickson$pH_ext - post.dickson$DL)

#determine data time bounds

head(post.dickson)
"2020-11-07 02:40:04"


tail(post.dickson)
"2020-11-12 21:10:08"


#event time bounds
t1 <-  '2020-11-07 02:30:04'
t2 <- '2020-11-12 21:20:08'

# for trim plots

# t3 <- '2020-11-12 00:00:00' # substitute for t1 in stabilized pH measurement
# t4 <- '2020-11-13 00:00:00'

summary(post.dickson$pH_int_v)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.130  -1.126  -1.126  -1.126  -1.125  -1.124  

print(bob.2020.screen.df$int.v.min)
# -1.06
print(bob.2020.screen.df$int.v.max) 
# -1.02


#adjusting plotting range for pH_int_v because it is out of range...

p = ggplot(post.dickson, aes(x = datetime))
post.d.int.v = p + geom_point(aes(y = pH_int_v), size = 0.25, color = "seagreen") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.135, -1.12) +
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

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.9985 -0.9955 -0.9950 -0.9952 -0.9948 -0.9943 

#adjusting plotting range for pH_int_v because it is out of range...

print(bob.2020.screen.df$ext.v.min)
# -1.01
print(bob.2020.screen.df$ext.v.max) 
# -0.97

p = ggplot(post.dickson, aes(datetime, pH_ext_v))
post.d.ext.v = p + geom_point(aes(), size = 0.25, color = "green") +
  xlab(" ") + #last x label sets the time axis label
  ylab("V ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.0, -0.99) +
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
  ylim(0.03, 0.3) +
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


summary(post.dickson$abs_v_diff)

p = ggplot(post.dickson, aes(datetime, abs_v_diff))
post.d.v_diff.2 =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.12, 0.14) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) 

print(post.d.v_diff.2)



summary(post.dickson$pH_int)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.817   6.885   6.898   6.896   6.910   6.923 

summary(post.dickson$pH_ext)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.276   8.373   8.385   8.384   8.398   8.409


p = ggplot(post.dickson, aes(x = datetime))
post.d.ext_pH = p +
  geom_point(aes(y = pH_ext), size = 0.25, color = "green") +
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


print(post.d.ext_pH)


p = ggplot(post.dickson, aes(x = datetime))
post.d.pHs = p + geom_point(aes(y = pH_int), size = 0.25, color = "seagreen") +
  geom_point(aes(y = pH_ext), size = 0.25, color = "green") +
  geom_point(aes(y = DL), size = 0.25, color = "black") +
  xlab(" ") + #last x label sets the time axis label
  ylab("pH int, ext, dickson\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(6.5, 8.5) +
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
  ylim(0, 1.5) +
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


p = ggplot(post.dickson, aes(datetime, abs_pH_diff))
post.d.pH.diff.2 = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("abs electrode diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(1.4, 1.6) +
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

print(post.d.pH.diff.2)



p = ggplot(post.dickson, aes(x =datetime))
post.d.DL.diff = p + geom_point(aes(y = DL_pH_diff_int), size = 0.25, color = "seagreen") +
  geom_point(aes(datetime, DL_pH_diff_ext), size = 0.25, color = "green") +
  xlab("Post-Deployment Dickson Standard Run for BOB 2020") + #last x label sets the time axis label
  ylab("abs offset from Dickson\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 1.65) +
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


p = ggplot(post.dickson, aes(x =datetime))
post.d.DL.diff.ext = p +
  geom_point(aes(datetime, DL_pH_diff_ext), size = 0.25, color = "green") +
  xlab("Post-Deployment Dickson Standard Run for BOB 2020") + #last x label sets the time axis label
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


print(post.d.DL.diff.ext)


summary(post.dickson$pH_temp)


p = ggplot(post.dickson, aes(datetime, pH_temp))
post.d.t.dt = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("Post-Deployment Dickson Standard Run for BOB Nov 7-12th 2020") + #last x label sets the time axis label
  ylab("T\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(14.0, 19.0) +
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
  xlab("Post-Deployment Dickson Standard Run for BOB Nov 12-13th 2020") + #last x label sets the time axis label
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
post.d.DL.v.pH.int.trim <- ggplotRegression(post.dickson.trim, "DL", "pH_int_v")

print(post.d.DL.v.pH.int.trim)


post.d.DL.v.pH.ext.trim <- ggplotRegression(post.dickson.trim, "DL", "pH_ext_v")

print(post.d.DL.v.pH.ext.trim)



##### arranging and printing plots to .png files ######################################

#the following plots aligned time series into one .png with 4K HD aspect ratios

ls() # to pull object names


# PRE DEPLOYMENT BATH ######################################################################

# "Pre.Bath...." Predeployment Bath plots
# "pre.b.abs.v_diff"        "pre.b.ext.v"            
# "pre.b.int.v"             "pre.b.pH.diff"           "pre.b.phs"              
# "pre.b.s"                 "pre.b.s.dt"              "pre.b.t.dt"     

ggsave("qaqc_plots/2020/BOB-pre-deployment_bath_Volt_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.b.int.v), ggplotGrob(pre.b.ext.v),
                              ggplotGrob(pre.b.abs.v_diff), ggplotGrob(pre.b.s.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-pre-deployment_bath_pHs_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.b.phs), ggplotGrob(pre.b.pH.diff),
                              ggplotGrob(pre.b.s.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-pre-deployment_bath_T_S_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.b.s), ggplotGrob(pre.b.t.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-pre-deployment_bath_pHs_T_S_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.b.phs), ggplotGrob(pre.b.pH.diff),
                              ggplotGrob(pre.b.s), ggplotGrob(pre.b.t.dt),
                              size = "last")), width = 6.65, height = 3.5)


# regression  plots

ggsave("qaqc_plots/2020/BOB-pre-deployment_bath_disrete.spec.pH.v.sft.pH_2020.png",
       plot = grid.draw(rbind(ggplotGrob(k0.int.pre.b.benchmark), ggplotGrob(k0.ext.pre.b.benchmark),
                              size = "last")), width = 5, height = 5)

# PRE DEPLOYMENT DICKSON STD RUN ######################################################################

# "Pre.Dickson...." Pre deployment Dickson std run
# "pre.d.DL.diff"                "pre.d.DL.diff.trim"           "pre.d.DL.v.pH.ext.trim"      
# "pre.d.DL.v.pH.int.trim"       "pre.d.ext.v"                  "pre.d.ext.v.trim"            
# "pre.d.int.v"                  "pre.d.int.v.trim"             "pre.d.pH.diff"               
# "pre.d.pH.diff.trim"           "pre.d.pH.ext.v.Volt.ext.trim" "pre.d.pH.int.v.Volt.int.trim"
# "pre.d.pHs"                    "pre.d.pHs.trim"               "pre.d.t.dt"                  
# "pre.d.t.dt.trim"              "pre.d.v_diff"                 "pre.d.v_diff.trim"           
# "pre.deploy.bath"              "pre.dickson"                  "pre.dickson.trim" 


ggsave("qaqc_plots/2020/BOB-pre-deployment_DSR_Volt_T_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.int.v), ggplotGrob(pre.d.ext.v),
                              ggplotGrob(pre.d.v_diff), ggplotGrob(pre.d.t.dt),
                              size = "last")), width = 6.65, height = 3.5)


ggsave("qaqc_plots/2020/BOB-pre-deployment_DSR_pHs_DL_T_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.pHs), ggplotGrob(pre.d.pH.diff),
                              ggplotGrob(pre.d.DL.diff), ggplotGrob(pre.d.t.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-pre-deployment_DSR_pHs_T_2020.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.pHs), ggplotGrob(pre.d.pH.diff),
                              ggplotGrob(pre.d.t.dt),
                              size = "last")), width = 6.65, height = 3.5)

# trimmed plots

ggsave("qaqc_plots/2020/BOB-pre-deployment_DSR_Volt_T_2020-trim.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.int.v.trim), ggplotGrob(pre.d.ext.v.trim),
                              ggplotGrob(pre.d.v_diff.trim), ggplotGrob(pre.d.t.dt.trim),
                              size = "last")), width = 6.65, height = 3.5)


ggsave("qaqc_plots/2020/BOB-pre-deployment_DSR_pHs_DL_T_2020-trim.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.pHs.trim), ggplotGrob(pre.d.pH.diff.trim),
                              ggplotGrob(pre.d.DL.diff.trim), ggplotGrob(pre.d.t.dt.trim),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-pre-deployment_DSR_pHs_T_2020-trim.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.pHs.trim), ggplotGrob(pre.d.pH.diff.trim),
                              ggplotGrob(pre.d.t.dt.trim),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-pre-deployment_DSR_DL.v.pHs_2020-trim.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.DL.v.pH.int.trim), ggplotGrob(pre.d.DL.v.pH.ext.trim),
                              size = "last")), width = 5, height = 5)


ggsave("qaqc_plots/2020/BOB-pre-deployment_DSR_pHs.v.Volts_2020-trim.png",
       plot = grid.draw(rbind(ggplotGrob(pre.d.pH.int.v.Volt.int.trim), 
                              ggplotGrob(pre.d.pH.ext.v.Volt.ext.trim),
                              size = "last")), width = 5, height = 5)


# DEPLOYMENT ######################################################################

# "SeaFET...." Deployment data for SeaFET
#  "sft.ext.v"                   
# "sft.int.v"                    "sft.o2"                       "sft.pH.diff"                 
# "sft.pH.diff.no.note"          "sft.ph.ext.v.o2"              "sft.ph.ext.v.o2.ol.rm"       
# "sft.ph.int.v.o2"              "sft.ph.int.v.o2.ol.rm"        "sft.pH_ext"                  
# "sft.pH_int"                   "sft.phs"                      "sft.sal"                     
# "sft.sal.dt"                   "sft.temp"                     "sft.v_diff"                  
# "sft.v_diff.2"                 "spc.ph.v.sft.ph.ext"          "spc.ph.v.sft.ph.ext.v" 
# "spc.ph.v.sft.ph.int"          "spc.ph.v.sft.ph.int.v"       
# 


ggsave("qaqc_plots/2020/BOB-deployment_Volt_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(sft.int.v), ggplotGrob(sft.ext.v),
                              ggplotGrob(sft.v_diff), ggplotGrob(sft.sal.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-deployment_Volt_Sal_2020-2.png",
       plot = grid.draw(rbind(ggplotGrob(sft.int.v), ggplotGrob(sft.ext.v),
                              ggplotGrob(sft.v_diff.2), ggplotGrob(sft.sal.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-deployment_pHs_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(sft.pH_int), ggplotGrob(sft.pH_ext),
                              ggplotGrob(sft.pH.diff), ggplotGrob(sft.sal.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-deployment_pHs_T_S_O2_2020.png",
       plot = grid.draw(rbind(ggplotGrob(sft.phs), ggplotGrob(sft.pH.diff.no.note),
                        ggplotGrob(sft.sal), ggplotGrob(sft.temp),
                        ggplotGrob(sft.o2),
                        size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-deployment_pHs_T_S_O2_2020-2.png",
       plot = grid.draw(rbind(ggplotGrob(sft.phs), 
                              ggplotGrob(sft.sal), ggplotGrob(sft.temp),
                              ggplotGrob(sft.o2),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-deployment_pH-ext_T_S_O2_2020.png",
       plot = grid.draw(rbind(ggplotGrob(sft.pH_ext), ggplotGrob(sft.pH.diff.no.note),
                              ggplotGrob(sft.sal), ggplotGrob(sft.temp),
                              ggplotGrob(sft.o2),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-deployment_pH-ext_T_S_O2_2020-2.png",
       plot = grid.draw(rbind(ggplotGrob(sft.pH_ext), 
                              ggplotGrob(sft.sal), ggplotGrob(sft.temp),
                              ggplotGrob(sft.o2),
                              size = "last")), width = 6.65, height = 3.5)

# regression  plots

ggsave("qaqc_plots/2020/BOB-deployment_pH_v_o2_2020.png",
       plot = grid.draw(rbind(ggplotGrob(sft.ph.int.v.o2), ggplotGrob(sft.ph.ext.v.o2),
                              size = "last")), width = 5, height = 5)

ggsave("qaqc_plots/2020/BOB-deployment_pH_v_o2_no_outliers_2020.png",
       plot = grid.draw(rbind(ggplotGrob(sft.ph.int.v.o2.ol.rm), ggplotGrob(sft.ph.ext.v.o2.ol.rm),
                              size = "last")), width = 5, height = 5)


#"spc.ph.v.sft.ph.ext"          "spc.ph.v.sft.ph.ext.v" 
# "spc.ph.v.sft.ph.int"          "spc.ph.v.sft.ph.int.v"     

ggsave("qaqc_plots/2020/BOB-deployment_spc.pH_v_sft.pH.int_2020.png",
       plot = grid.draw(rbind(ggplotGrob(spc.ph.v.sft.ph.int.v), 
                              ggplotGrob(spc.ph.v.sft.ph.int),
                              size = "last")), width = 5, height = 5)

ggsave("qaqc_plots/2020/BOB-deployment_spc.pH_v_sft.pH.ext_2020.png",
       plot = grid.draw(rbind(ggplotGrob(spc.ph.v.sft.ph.ext.v), 
                              ggplotGrob(spc.ph.v.sft.ph.ext),
                              size = "last")), width = 5, height = 5)

# ADJUSTED DEPLOYMENT ######################################################################

# DATA NOT ADJUSTED AS OF 4/16/2020
# ADJUSTED DEPLOYMENT DATA
# "adj.sft.df"              "adj.sft.ext.v"           "adj.sft.int.v"          
# "adj.sft.o2"              "adj.sft.pH.diff"         "adj.sft.pH.diff.no.note"
# "adj.sft.pH_ext"          "adj.sft.pH_int"          "adj.sft.phs"            
# "adj.sft.sal"             "adj.sft.sal.dt"          "adj.sft.temp"           
# "adj.sft.v_diff"   
# "spc.ph.v.adj.sft.ph.ext" "spc.ph.v.adj.sft.ph.int"


# ggsave("qaqc_plots/2020/BOB-ADJ-deployment_pHs_Sal_2020.png",
#        plot = grid.draw(rbind(ggplotGrob(adj.sft.pH_int), ggplotGrob(adj.sft.pH_ext),
#                               ggplotGrob(adj.sft.pH.diff), ggplotGrob(adj.sft.sal.dt),
#                               size = "last")), width = 6.65, height = 3.5)
# 
# ggsave("qaqc_plots/2020/BOB-ADJ-deployment_pHs_T_S_O2_2020-2.png",
#        plot = grid.draw(rbind(ggplotGrob(adj.sft.phs), 
#                               ggplotGrob(adj.sft.sal), ggplotGrob(adj.sft.temp),
#                               ggplotGrob(adj.sft.o2),
#                               size = "last")), width = 6.65, height = 3.5)
# 
# #regression plots
# 
# ggsave("qaqc_plots/2020/BOB-ADJ-deployment_spc.pH_v_sft.pHs_2020.png",
#        plot = grid.draw(rbind(ggplotGrob(spc.ph.v.adj.sft.ph.int), 
#                               ggplotGrob(spc.ph.v.adj.sft.ph.ext),
#                               size = "last")), width = 5, height = 5)
# 
# 

# NO MID DEPLOYMENT FOR BOB 2020 ######################################################################

# "Mid.Bath...."   Mid Deployment Field Bath plot objects 
# "mid.b.abs.v_diff"       "mid.b.ext.v"            "mid.b.int.v"           
# "mid.b.pH.diff"          "mid.b.phs"              "mid.b.s"               
# "mid.b.s.dt"             "mid.b.t.dt"  
# "k0.ext.mid.b.benchmark"      "k0.ext.mid.b.benchmark.2pts"
# "k0.int.mid.b.benchmark"     
# "k0.int.mid.b.benchmark.2pts" 

# 
# ggsave("qaqc_plots/2020/BOB-mid-deployment_bath_Volt_Sal_2020.png",
#        plot = grid.draw(rbind(ggplotGrob(mid.b.int.v), ggplotGrob(mid.b.ext.v),
#                               ggplotGrob(mid.b.abs.v_diff), ggplotGrob(mid.b.s.dt),
#                               size = "last")), width = 6.65, height = 3.5)
# 
# ggsave("qaqc_plots/2020/BOB-mid-deployment_bath_pHs_Sal_2020.png",
#        plot = grid.draw(rbind(ggplotGrob(mid.b.phs), ggplotGrob(mid.b.pH.diff),
#                               ggplotGrob(mid.b.s.dt),
#                               size = "last")), width = 6.65, height = 3.5)
# 
# ggsave("qaqc_plots/2020/BOB-mid-deployment_bath_T_S_2020.png",
#        plot = grid.draw(rbind(ggplotGrob(mid.b.s), ggplotGrob(mid.b.t.dt),
#                               size = "last")), width = 6.65, height = 3.5)
# 
# ggsave("qaqc_plots/2020/BOB-mid-deployment_bath_pHs_T_S_2020.png",
#        plot = grid.draw(rbind(ggplotGrob(mid.b.phs), ggplotGrob(mid.b.pH.diff),
#                               ggplotGrob(mid.b.s), ggplotGrob(mid.b.t.dt),
#                               size = "last")), width = 6.65, height = 3.5)
# 
# 
# # regression  plots
# 
# ggsave("qaqc_plots/2020/BOB-mid-deployment_bath_disrete.spec.pH.v.sft.pH_2020.png",
#        plot = grid.draw(rbind(ggplotGrob(k0.int.mid.b.benchmark), ggplotGrob(k0.ext.mid.b.benchmark),
#                               size = "last")), width = 5, height = 5)
# 
# 
# ggsave("qaqc_plots/2020/BOB-mid-deployment_bath_disrete.spec.pH.v.sft.pH_2020-2pts.png",
#        plot = grid.draw(rbind(ggplotGrob(k0.int.mid.b.benchmark.2pts), ggplotGrob(k0.ext.mid.b.benchmark.2pts),
#                               size = "last")), width = 5, height = 5)
# 



# POST DEPLOYMENT BATH ######################################################################

# "Post.Bath...."   Post Deployment Bath plot objects              
# "k0.ext.post.b.benchmark"           
# "k0.int.post.b.benchmark"                                   
# "post.b.abs.v_diff"            "post.b.ext.v"                 "post.b.ext_ph"               
# "post.b.int.v"                 "post.b.pH.diff"               "post.b.pH.diff.2"            
# "post.b.phs"                   "post.b.s"                     "post.b.s.dt"                 
# "post.b.t.dt"                 
# 

ggsave("qaqc_plots/2020/BOB-post-deployment_bath_Volt_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(post.b.int.v), ggplotGrob(post.b.ext.v),
                              ggplotGrob(post.b.abs.v_diff), ggplotGrob(post.b.s.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-post-deployment_bath_pHs_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(post.b.phs), ggplotGrob(post.b.pH.diff),
                              ggplotGrob(post.b.s.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-post-deployment_bath_pHs_Sal_2020-2.png",
       plot = grid.draw(rbind(ggplotGrob(post.b.phs), ggplotGrob(post.b.pH.diff.2),
                              ggplotGrob(post.b.s.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-post-deployment_bath_pH_ext_Sal_2020.png",
       plot = grid.draw(rbind(ggplotGrob(post.b.ext_ph), 
                              ggplotGrob(post.b.s.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-post-deployment_bath_T_S_2020.png",
       plot = grid.draw(rbind(ggplotGrob(post.b.s), ggplotGrob(post.b.t.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-post-deployment_bath_pHs_T_S_2020.png",
       plot = grid.draw(rbind(ggplotGrob(post.b.phs), ggplotGrob(post.b.pH.diff),
                              ggplotGrob(post.b.s), ggplotGrob(post.b.t.dt),
                              size = "last")), width = 6.65, height = 3.5)


ggsave("qaqc_plots/2020/BOB-post-deployment_bath_pH_ext_T_S_2020.png",
       plot = grid.draw(rbind(ggplotGrob(post.b.ext_ph), 
                              ggplotGrob(post.b.s), ggplotGrob(post.b.t.dt),
                              size = "last")), width = 6.65, height = 3.5)

# regression  plots

ggsave("qaqc_plots/2020/BOB-post-deployment_bath_disrete.spec.pH.v.sft.pH_2020.png",
       plot = grid.draw(rbind(ggplotGrob(k0.int.post.b.benchmark), ggplotGrob(k0.ext.post.b.benchmark),
                              size = "last")), width = 5, height = 5)

# POST DEPLOYMENT DICKSON STANDARD RUN DSR ###############################################################

# "Post.Dickson...." Post deployment Dickson Standard plot objects
#
# "post.d.DL.diff"               "post.d.DL.diff.ext"          
# "post.d.ext.v"                 "post.d.ext_pH"                "post.d.int.v"                
# "post.d.pH.diff"               "post.d.pH.diff.2"             "post.d.pHs"                  
# "post.d.t.dt"                  "post.d.v_diff"                "post.deploy.bath"            
# "post.dickson"                  "post.d.v_diff.2" 


ggsave("qaqc_plots/2020/BOB-post-deployment_DSR_Volt_T_2020.png",
       plot = grid.draw(rbind(ggplotGrob(post.d.int.v), ggplotGrob(post.d.ext.v),
                              ggplotGrob(post.d.v_diff), ggplotGrob(post.d.t.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-post-deployment_DSR_Volt_T_2020-2.png",
       plot = grid.draw(rbind(ggplotGrob(post.d.int.v), ggplotGrob(post.d.ext.v),
                              ggplotGrob(post.d.v_diff.2), ggplotGrob(post.d.t.dt),
                              size = "last")), width = 6.65, height = 3.5)


ggsave("qaqc_plots/2020/BOB-post-deployment_DSR_pHs_DL_T_2020.png",
       plot = grid.draw(rbind(ggplotGrob(post.d.pHs), ggplotGrob(post.d.pH.diff),
                              ggplotGrob(post.d.DL.diff), ggplotGrob(post.d.t.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-post-deployment_DSR_pHs_DL_T_2020-2.png",
       plot = grid.draw(rbind(ggplotGrob(post.d.pHs), ggplotGrob(post.d.pH.diff.2),
                              ggplotGrob(post.d.DL.diff), ggplotGrob(post.d.t.dt),
                              size = "last")), width = 6.65, height = 3.5)


ggsave("qaqc_plots/2020/BOB-post-deployment_DSR_pH_ext_DL_T_2020.png",
       plot = grid.draw(rbind(ggplotGrob(post.d.ext_pH), 
                              ggplotGrob(post.d.DL.diff.ext), ggplotGrob(post.d.t.dt),
                              size = "last")), width = 6.65, height = 3.5)


ggsave("qaqc_plots/2020/BOB-post-deployment_DSR_pH_ext_T_2020.png",
       plot = grid.draw(rbind(ggplotGrob(post.d.ext_pH), 
                              ggplotGrob(post.d.t.dt),
                              size = "last")), width = 6.65, height = 3.5)

# trimmed plots

# ggsave("qaqc_plots/2020/BOB-post-deployment_DSR_Volt_T_2020-trim.png",
#        plot = grid.draw(rbind(ggplotGrob(post.d.int.v.trim), ggplotGrob(post.d.ext.v.trim),
#                               ggplotGrob(post.d.v_diff.trim), ggplotGrob(post.d.t.dt.trim),
#                               size = "last")), width = 6.65, height = 3.5)
# 
# 
# ggsave("qaqc_plots/2020/BOB-post-deployment_DSR_pHs_DL_T_2020-trim.png",
#        plot = grid.draw(rbind(ggplotGrob(post.d.pHs.trim), ggplotGrob(post.d.pH.diff.trim),
#                               ggplotGrob(post.d.DL.diff.trim), ggplotGrob(post.d.t.dt.trim),
#                               size = "last")), width = 6.65, height = 3.5)
# 
# ggsave("qaqc_plots/2020/BOB-post-deployment_DSR_pHs_T_2020-trim.png",
#        plot = grid.draw(rbind(ggplotGrob(post.d.pHs.trim), ggplotGrob(post.d.pH.diff.trim),
#                               ggplotGrob(post.d.t.dt.trim),
#                               size = "last")), width = 6.65, height = 3.5)
# 
# ggsave("qaqc_plots/2020/BOB-post-deployment_DSR_DL.v.pHs_2020-trim.png",
#        plot = grid.draw(rbind(ggplotGrob(post.d.DL.v.pH.int.trim), ggplotGrob(post.d.DL.v.pH.ext.trim),
#                               size = "last")), width = 5, height = 5)




# FINAL QC CHECK AND FLAGGING ##################################################################

rm(list=ls())

load('bob.2020.all.combined.prcsd.RData')


#added this into data handling script
# 

# sft.df <- sft.df %>%
#   group_by(datetime.tag) %>%
#   summarise(pH_int_v = median(pH_int_v),
#             pH_ext_v = median(pH_ext_v),
#             abs_v_diff = median(abs_v_diff),
#             pH_temp = median(pH_temp), 
#             ctd_sal = median(ctd_sal),
#             pH_int_cell =  median(pH_int_cell), 
#             pH_ext_cell =  median(pH_ext_cell), 
#             abs_pH_diff =  median(abs_pH_diff),
#             ctd_temp =  median(ctd_temp),
#             ctd_o2_mg_l = median(ctd_o2_mg_l),
#             ctd_chla =  median(ctd_chla),
#             ctd_turb =  median(ctd_turb))

# sft.df <- sft.df %>%                     
#   rename(datetime = "datetime.tag")

#QARTOD FLAGS
# '1' DATA HAS PASSED REQUIRED QC
# '2' DATA HAS NOT BEEN EVALUATED
# '3' QUESTIONABLE/SUSPECT
# '4' BAD DATA
# '9' MISSING DATA


summary(sft.df$pH_int_cell)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.815   6.523   7.381   7.209   7.832   8.183 

summary(sft.df$pH_ext_cell)
#  Min.   1st Qu.  Median    Mean   3rd Qu.    Max. 
# 6.407   7.480   7.729     7.677   7.862     8.207



# SeaFET sensor specifications 
# measure pH from 6.5 to 9.0
# measure salinity from 20 to 40 PSU
# measure temperature from 0 to 50 degC


sft.df <- sft.df %>%
  mutate(
    pH_int_flag = case_when(
      pH_int_cell > 8.6 ~ "4",
      pH_int_cell <= 8.6 & pH_int_cell > 8.4 ~ "3",
      pH_int_cell <= 8.4 & pH_int_cell >= 7.4 ~ "1",
      pH_int_cell < 7.4  & pH_int_cell >= 7.0 ~ "3",
      pH_int_cell < 7.0 ~ "4"
    )
  )


#verify  flag worked

df1 <- filter(sft.df, pH_int_flag == "4")

df1 <- arrange(df1, pH_int_cell)

df2 <- filter(sft.df, pH_int_flag == "3")

df2 <- arrange(df2, pH_int_cell)

df3 <- filter(sft.df, pH_int_flag == "1")

df3 <- arrange(df3, pH_int_cell)

x <- sum(nrow(df1), nrow(df2), nrow(df3))

rm(x, df1, df2, df3)



sft.df <- sft.df %>%
  mutate(
    pH_ext_flag = case_when(
      pH_ext_cell > 8.6 ~ "4",
      pH_ext_cell <= 8.6 & pH_ext_cell > 8.4 ~ "3",
      pH_ext_cell <= 8.4 & pH_ext_cell >= 7.4 ~ "1",
      pH_ext_cell < 7.4  & pH_ext_cell >= 7.0 ~ "3",
      pH_ext_cell < 7.0 ~ "4"
    )
  )


#verify  flag worked

df1 <- filter(sft.df, pH_ext_flag == "4")

df1 <- arrange(df1, pH_ext_cell)

df2 <- filter(sft.df, pH_ext_flag == "3")

df2 <- arrange(df2, pH_ext_cell)

df3 <- filter(sft.df, pH_ext_flag == "1")

df3 <- arrange(df3, pH_ext_cell)

x <- sum(nrow(df1), nrow(df2), nrow(df3))

rm(x, df1, df2, df3)




# salinity flag for SeaFET instruments


sft.df <- sft.df %>%
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

df1 <- filter(sft.df, pH_salt_flag == "4")

df1 <- arrange(df1, ctd_sal)

df2 <- filter(sft.df, pH_salt_flag == "3")

df2 <- arrange(df2, ctd_sal)

df3 <- filter(sft.df, pH_salt_flag == "1")

df3 <- arrange(df3, ctd_sal)

x <- sum(nrow(df1), nrow(df2), nrow(df3))

rm(x, df1, df2, df3)



# sensor specifactions maximum ranges == "4"

sft.df <- sft.df %>%
  mutate(
    o2_flag = case_when(
      ctd_o2_mg_l > 16 ~ "4",
      ctd_o2_mg_l < 16 ~ "1"
    )
  )

df1 <- filter(sft.df, o2_flag == "4")

df1 <- arrange(df1, ctd_o2_mg_l)

rm(df1)



pH_int_flag_cols <- c(14,16,17)

sft.df$pH_int_final_flag <- do.call(pmax, sft.df[,pH_int_flag_cols])

pH_ext_flag_cols <- c(15:17)

sft.df$pH_ext_final_flag <- do.call(pmax, sft.df[,pH_ext_flag_cols])

sft.df$final_flag <- do.call(pmax, sft.df[,14:17])


save(sft.df, file = 'bob.2020.all.combined.prcsd.flagged.RData')

rm(list=ls())

load('bob.2020.all.combined.prcsd.flagged.RData')

# remove all rows of data where pH external is flagged for removal 
sft.df <- filter(sft.df, pH_ext_final_flag != "4")

# replace pH internal flagged data with NA's only
sft.df <- mutate(sft.df, pH_int_cell = ifelse(pH_int_final_flag == "4", NA, pH_int_cell))

save(sft.df, file = 'bob.2020.select.combined.prcsd.flagged.RData')

rm(list=ls())






# plotting ALL flagged combined data #################################################

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




load('bob.2020.all.combined.prcsd.flagged.RData')

# field check samples
load("2020 data/bob-check.samples-2020.RData")

#tag for samples can be tied to time series
chk.df$datetime.tag <- round_date(chk.df$datetime, "20 min")



# find data time bounds
head(sft.df) 
tail(sft.df)

#sft season time bounds
"2020-05-01 20:20:04"
"2020-10-24 17:00:13"


#event time bounds
t1 <-  "2020-05-01 20:00:04"
t2 <- "2020-10-24 17:20:13"
#t2 <- '2020-03-26 00:00:00'

summary(sft.df)

# datetime                    datetime.tag                    pH_int_v         pH_ext_v      
# Min.   :2020-05-01 20:20:04   Min.   :2020-05-01 20:20:00   Min.   :-1.188   Min.   :-1.0605  
# 1st Qu.:2020-05-28 02:55:06   1st Qu.:2020-05-28 02:55:00   1st Qu.:-1.147   1st Qu.:-1.0366  
# Median :2020-06-23 09:30:08   Median :2020-06-23 09:30:00   Median :-1.097   Median :-1.0225  
# Mean   :2020-07-05 07:20:27   Mean   :2020-07-05 07:20:19   Mean   :-1.107   Mean   :-1.0238  
# 3rd Qu.:2020-08-06 20:15:10   3rd Qu.:2020-08-06 20:15:00   3rd Qu.:-1.072   3rd Qu.:-1.0117  
# Max.   :2020-10-24 17:00:13   Max.   :2020-10-24 17:00:00   Max.   :-1.052   Max.   :-0.9899  

# abs_v_diff         pH_temp         ctd_sal         pH_int_cell     pH_ext_cell   
# Min.   :0.04988   Min.   :12.77   Min.   : 0.9547   Min.   :5.815   Min.   :6.406  
# 1st Qu.:0.05738   1st Qu.:15.89   1st Qu.:19.3933   1st Qu.:6.523   1st Qu.:7.480  
# Median :0.06306   Median :16.99   Median :24.4275   Median :7.381   Median :7.729  
# Mean   :0.08363   Mean   :17.12   Mean   :23.1226   Mean   :7.209   Mean   :7.677  
# 3rd Qu.:0.12561   3rd Qu.:18.31   3rd Qu.:27.2102   3rd Qu.:7.832   3rd Qu.:7.862  
# Max.   :0.14279   Max.   :21.79   Max.   :31.7812   Max.   :8.186   Max.   :8.235

# abs_pH_diff         ctd_temp      ctd_o2_mg_l        ctd_chla          ctd_turb     
# Min.   :0.00000   Min.   :12.67   Min.   : 5.258   Min.   : 0.7123   Min.   : 1.248  
# 1st Qu.:0.02759   1st Qu.:15.80   1st Qu.: 7.287   1st Qu.: 1.5256   1st Qu.: 3.938  
# Median :0.05962   Median :16.93   Median : 7.744   Median : 1.9048   Median : 5.672  
# Mean   :0.47858   Mean   :17.04   Mean   : 7.788   Mean   : 3.2223   Mean   : 9.572  
# 3rd Qu.:1.26959   3rd Qu.:18.24   3rd Qu.: 8.245   3rd Qu.: 3.0994   3rd Qu.:12.369  
# Max.   :1.58917   Max.   :22.21   Max.   :10.202   Max.   :49.2383   Max.   :24.700 


color_group <- c("seagreen","yellow2","red2")

p = ggplot(sft.df, aes(datetime, pH_int_v))
sft.int.v = p + 
  geom_point(aes(color = factor(pH_int_flag)), shape =21, size = 0.25) +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.2, -1.05) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        legend.position = "none",
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  scale_color_manual(values = color_group) 


print(sft.int.v)


color_group <- c("green","yellow2","red2")

p = ggplot(sft.df, aes(datetime, pH_ext_v))
sft.ext.v = p + 
  geom_point(aes(color = factor(pH_ext_flag)), shape =21, size = 0.25) +
  xlab("") + #last x label sets the time axis label
  ylab("V ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.065, -0.98) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        legend.position = "none",
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  scale_color_manual(values = color_group) 

print(sft.ext.v)


p = ggplot(sft.df, aes(datetime, abs_v_diff))
sft.v_diff =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.03, 0.08) +
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

print(sft.v_diff)





p = ggplot(sft.df, aes(datetime, abs_v_diff))
sft.v_diff.2 =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.03, 0.3) +
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

print(sft.v_diff.2)



color_group <- c("seagreen","yellow2","red2")

p = ggplot(sft.df, aes(datetime, pH_int_cell))
sft.pH_int = p + 
  geom_point(aes(color = factor(pH_int_flag)), shape =21, size = 0.25) +
  xlab(" ") + #last x label sets the time axis label
  ylab("pH int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(5.8, 8.3) +
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
  scale_color_manual(values = color_group) 


print(sft.pH_int)


color_group <- c("green","yellow2","red2")

p = ggplot(sft.df, aes(datetime, pH_ext_cell))
sft.pH_ext = p + 
  geom_point(aes(color = factor(pH_ext_flag)), shape =21, size = 0.25) +
  xlab("") + #last x label sets the time axis label
  ylab("pH ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(5.8, 8.3) +
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
  scale_color_manual(values = color_group) 

print(sft.pH_ext)

# 
# color_group <- c("seagreen","yellow2","red2")
# color_group <- c("green","yellow2","red2")
# 
# p = ggplot(sft.df, aes(datetime))
# sft.phs = p + geom_point(aes(y = pH_int_cell), size = 0.25, color = "seagreen") +
#   geom_point(aes(y = pH_ext_cell), size = 0.25, color = "green") +
#   xlab("") + #last x label sets the time axis label
#   ylab("pH int, ext\n")+ 
#   scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
#   xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
#          as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
#   ylim(5.8, 8.3) +
#   geom_point(data = chk.df, aes(x=datetime, y= pH.check.median), shape = 18, size = 1,
#              color = "black", show.legend = F)  +
#   theme_minimal() +
#   theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
#         panel.grid.minor = element_line(colour = "grey", size = 0.25),
#         axis.title.x=element_blank(), 
#         axis.text.x=element_blank(), 
#         axis.text.y = element_text(size = 5),
#         axis.ticks.x=element_blank(), 
#         axis.title.y = element_text(size =5,lineheight=3)) +
#   theme(legend.title=element_blank(), legend.position = "right") 
# 
# 
# print(sft.phs)


summary(sft.df$abs_pH_diff)

p = ggplot(sft.df, aes(datetime, abs_pH_diff))
sft.pH.diff = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("pH abs-diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 1.6) +
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


print(sft.pH.diff)


p = ggplot(sft.df, aes(datetime, abs_pH_diff))
sft.pH.diff.no.note = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("pH abs-diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 1.6) +
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


print(sft.pH.diff.no.note)



p = ggplot(sft.df, aes(datetime, ctd_sal))
sft.sal = p + geom_point(aes(), size = 0.25, color = "seagreen") +
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

print(sft.sal)


p = ggplot(sft.df, aes(datetime, ctd_sal))
sft.sal.dt = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab("Deployment BOB 2020") + #last x label sets the time axis label
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

print(sft.sal.dt)



p = ggplot(sft.df, aes(datetime, pH_temp))
sft.temp = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("") + #last x label sets the time axis label
  ylab("T\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(12, 22) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(sft.temp)


p = ggplot(sft.df, aes(datetime, ctd_o2_mg_l))
sft.o2 = p + geom_point(aes(), size = 0.25, color = "red") +
  xlab("Deployment BOB 2020") + #last x label sets the time axis label
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


print(sft.o2)


# combine check samples to instrument data in a new data frame

# round sampling times of check samples since there is a slight mismatch in time

df3 <- chk.df

df3 <- select(df3, -datetime)

df3 <- df3 %>%                     
  rename(datetime = "datetime.tag")

df2 <- left_join(sft.df, df3, by = "datetime")

View(df2)

# Discrete spec pH regressed versus pH

spc.ph.v.sft.ph.int.v <- ggplotRegression(df2, "pH.check.median", "pH_int_v")

print(spc.ph.v.sft.ph.int.v)


spc.ph.v.sft.ph.ext.v <- ggplotRegression(df2, "pH.check.median", "pH_ext_v")

print(spc.ph.v.sft.ph.ext.v)


spc.ph.v.sft.ph.int <- ggplotRegression(df2, "pH.check.median", "pH_int_cell")

print(spc.ph.v.sft.ph.int)


spc.ph.v.sft.ph.ext <- ggplotRegression(df2, "pH.check.median", "pH_ext_cell")

print(spc.ph.v.sft.ph.ext)





# "SeaFET...." Deployment data for SeaFET
#  "sft.ext.v"                   
# "sft.int.v"                    "sft.o2"                       "sft.pH.diff"                 
# "sft.pH.diff.no.note"          "sft.ph.ext.v.o2"              "sft.ph.ext.v.o2.ol.rm"       
# "sft.ph.int.v.o2"              "sft.ph.int.v.o2.ol.rm"        "sft.pH_ext"                  
# "sft.pH_int"                   "sft.phs"                      "sft.sal"                     
# "sft.sal.dt"                   "sft.temp"                     "sft.v_diff"                  
# "sft.v_diff.2"                 "spc.ph.v.sft.ph.ext"          "spc.ph.v.sft.ph.ext.v" 
# "spc.ph.v.sft.ph.int"          "spc.ph.v.sft.ph.int.v"       
# 


ggsave("qaqc_plots/2020/BOB-deployment_Volt_Sal_2020-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sft.int.v), ggplotGrob(sft.ext.v),
                              ggplotGrob(sft.v_diff), ggplotGrob(sft.sal.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-deployment_Volt_Sal_2020-2-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sft.int.v), ggplotGrob(sft.ext.v),
                              ggplotGrob(sft.v_diff.2), ggplotGrob(sft.sal.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-deployment_pHs_Sal_2020-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sft.pH_int), ggplotGrob(sft.pH_ext),
                              ggplotGrob(sft.pH.diff), ggplotGrob(sft.sal.dt),
                              size = "last")), width = 6.65, height = 3.5)


ggsave("qaqc_plots/2020/BOB-deployment_pH-ext_T_S_O2_2020-2-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sft.pH_ext), 
                              ggplotGrob(sft.sal), ggplotGrob(sft.temp),
                              ggplotGrob(sft.o2),
                              size = "last")), width = 6.65, height = 3.5)

# regression  plots

#"spc.ph.v.sft.ph.ext"          "spc.ph.v.sft.ph.ext.v" 
# "spc.ph.v.sft.ph.int"          "spc.ph.v.sft.ph.int.v"     

ggsave("qaqc_plots/2020/BOB-deployment_spc.pH_v_sft.pH.int_2020-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(spc.ph.v.sft.ph.int.v), 
                              ggplotGrob(spc.ph.v.sft.ph.int),
                              size = "last")), width = 5, height = 5)

ggsave("qaqc_plots/2020/BOB-deployment_spc.pH_v_sft.pH.ext_2020-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(spc.ph.v.sft.ph.ext.v), 
                              ggplotGrob(spc.ph.v.sft.ph.ext),
                              size = "last")), width = 5, height = 5)



#plotting SELECT flagged data ###################################################


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


load('bob.2020.select.combined.prcsd.flagged.RData')

write.csv(sft.df, "2020 data/bob-2020-final.flagged.data.csv", 
          row.names = F)


# field check samples
load("2020 data/bob-check.samples-2020.RData")

#tag for samples can be tied to time series
chk.df$datetime.tag <- round_date(chk.df$datetime, "20 min")



# find data time bounds
head(sft.df) 
tail(sft.df)

#sft season time bounds
"2020-05-01 20:20:04"
"2020-10-24 17:00:13"


#event time bounds
t1 <-  "2020-05-01 20:00:04"
t2 <- "2020-10-24 17:20:13"
#t2 <- '2020-03-26 00:00:00'

summary(sft.df)

# datetime                    datetime.tag                    pH_int_v         pH_ext_v      
# Min.   :2020-05-01 20:20:04   Min.   :2020-05-01 20:20:00   Min.   :-1.188   Min.   :-1.0605  
# 1st Qu.:2020-05-28 02:55:06   1st Qu.:2020-05-28 02:55:00   1st Qu.:-1.147   1st Qu.:-1.0366  
# Median :2020-06-23 09:30:08   Median :2020-06-23 09:30:00   Median :-1.097   Median :-1.0225  
# Mean   :2020-07-05 07:20:27   Mean   :2020-07-05 07:20:19   Mean   :-1.107   Mean   :-1.0238  
# 3rd Qu.:2020-08-06 20:15:10   3rd Qu.:2020-08-06 20:15:00   3rd Qu.:-1.072   3rd Qu.:-1.0117  
# Max.   :2020-10-24 17:00:13   Max.   :2020-10-24 17:00:00   Max.   :-1.052   Max.   :-0.9899  

# abs_v_diff         pH_temp         ctd_sal         pH_int_cell     pH_ext_cell   
# Min.   :0.04988   Min.   :12.77   Min.   : 0.9547   Min.   :5.815   Min.   :6.406  
# 1st Qu.:0.05738   1st Qu.:15.89   1st Qu.:19.3933   1st Qu.:6.523   1st Qu.:7.480  
# Median :0.06306   Median :16.99   Median :24.4275   Median :7.381   Median :7.729  
# Mean   :0.08363   Mean   :17.12   Mean   :23.1226   Mean   :7.209   Mean   :7.677  
# 3rd Qu.:0.12561   3rd Qu.:18.31   3rd Qu.:27.2102   3rd Qu.:7.832   3rd Qu.:7.862  
# Max.   :0.14279   Max.   :21.79   Max.   :31.7812   Max.   :8.186   Max.   :8.235

# abs_pH_diff         ctd_temp      ctd_o2_mg_l        ctd_chla          ctd_turb     
# Min.   :0.00000   Min.   :12.67   Min.   : 5.258   Min.   : 0.7123   Min.   : 1.248  
# 1st Qu.:0.02759   1st Qu.:15.80   1st Qu.: 7.287   1st Qu.: 1.5256   1st Qu.: 3.938  
# Median :0.05962   Median :16.93   Median : 7.744   Median : 1.9048   Median : 5.672  
# Mean   :0.47858   Mean   :17.04   Mean   : 7.788   Mean   : 3.2223   Mean   : 9.572  
# 3rd Qu.:1.26959   3rd Qu.:18.24   3rd Qu.: 8.245   3rd Qu.: 3.0994   3rd Qu.:12.369  
# Max.   :1.58917   Max.   :22.21   Max.   :10.202   Max.   :49.2383   Max.   :24.700 


color_group <- c("seagreen","yellow2","red2")

p = ggplot(sft.df, aes(datetime, pH_int_v))
sft.int.v = p + 
  geom_point(aes(color = factor(pH_int_flag)), shape =21, size = 0.25) +
  xlab(" ") + #last x label sets the time axis label
  ylab("V int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.2, -1.05) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        legend.position = "none",
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  scale_color_manual(values = color_group) 


print(sft.int.v)


color_group <- c("green","yellow2","red2")

p = ggplot(sft.df, aes(datetime, pH_ext_v))
sft.ext.v = p + 
  geom_point(aes(color = factor(pH_ext_flag)), shape =21, size = 0.25) +
  xlab("") + #last x label sets the time axis label
  ylab("V ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(-1.065, -0.98) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
        panel.grid.minor = element_line(colour = "grey", size = 0.25),
        legend.position = "none",
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3)) +
  scale_color_manual(values = color_group) 

print(sft.ext.v)


p = ggplot(sft.df, aes(datetime, abs_v_diff))
sft.v_diff =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.03, 0.08) +
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

print(sft.v_diff)





p = ggplot(sft.df, aes(datetime, abs_v_diff))
sft.v_diff.2 =p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("V abs-diff\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0.03, 0.3) +
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

print(sft.v_diff.2)



color_group <- c("seagreen","yellow2","red2")

p = ggplot(sft.df, aes(datetime, pH_int_cell))
sft.pH_int = p + 
  geom_point(aes(color = factor(pH_int_flag)), shape =21, size = 0.25) +
  xlab(" ") + #last x label sets the time axis label
  ylab("pH int\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(5.8, 8.3) +
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
  scale_color_manual(values = color_group) 


print(sft.pH_int)


color_group <- c("green","yellow2","red2")

p = ggplot(sft.df, aes(datetime, pH_ext_cell))
sft.pH_ext = p + 
  geom_point(aes(color = factor(pH_ext_flag)), shape =21, size = 0.25) +
  xlab("") + #last x label sets the time axis label
  ylab("pH ext\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(5.8, 8.3) +
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
  scale_color_manual(values = color_group) 

print(sft.pH_ext)

# 
# color_group <- c("seagreen","yellow2","red2")
# color_group <- c("green","yellow2","red2")
# 
# p = ggplot(sft.df, aes(datetime))
# sft.phs = p + geom_point(aes(y = pH_int_cell), size = 0.25, color = "seagreen") +
#   geom_point(aes(y = pH_ext_cell), size = 0.25, color = "green") +
#   xlab("") + #last x label sets the time axis label
#   ylab("pH int, ext\n")+ 
#   scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
#   xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
#          as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
#   ylim(5.8, 8.3) +
#   geom_point(data = chk.df, aes(x=datetime, y= pH.check.median), shape = 18, size = 1,
#              color = "black", show.legend = F)  +
#   theme_minimal() +
#   theme(panel.grid.major = element_line(colour = "black", size = 0.25), 
#         panel.grid.minor = element_line(colour = "grey", size = 0.25),
#         axis.title.x=element_blank(), 
#         axis.text.x=element_blank(), 
#         axis.text.y = element_text(size = 5),
#         axis.ticks.x=element_blank(), 
#         axis.title.y = element_text(size =5,lineheight=3)) +
#   theme(legend.title=element_blank(), legend.position = "right") 
# 
# 
# print(sft.phs)


summary(sft.df$abs_pH_diff)

p = ggplot(sft.df, aes(datetime, abs_pH_diff))
sft.pH.diff = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("pH abs-diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 1.6) +
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


print(sft.pH.diff)


p = ggplot(sft.df, aes(datetime, abs_pH_diff))
sft.pH.diff.no.note = p + geom_point(aes(), size = 0.25, color = "black") +
  xlab("") + #last x label sets the time axis label
  ylab("pH abs-diff\n")+
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(0, 1.6) +
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


print(sft.pH.diff.no.note)



p = ggplot(sft.df, aes(datetime, ctd_sal))
sft.sal = p + geom_point(aes(), size = 0.25, color = "seagreen") +
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

print(sft.sal)


p = ggplot(sft.df, aes(datetime, ctd_sal))
sft.sal.dt = p + geom_point(aes(), size = 0.25, color = "seagreen") +
  xlab("Deployment BOB 2020") + #last x label sets the time axis label
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

print(sft.sal.dt)



p = ggplot(sft.df, aes(datetime, pH_temp))
sft.temp = p + geom_point(aes(), size = 0.25, color = "red4") +
  xlab("") + #last x label sets the time axis label
  ylab("T\n")+ 
  scale_x_datetime(labels=date_format("%m"), breaks = date_breaks("1 month"), expand=c(0,0)) +
  xlim(c(as.POSIXct(t1, format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct(t2, format = "%Y-%m-%d %H:%M:%S"))) +
  ylim(12, 22) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "black", size = 0.25), #theme for top two plots
        panel.grid.minor = element_line(colour = "grey", size = 0.25), 
        axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_text(size = 5),
        axis.ticks.x=element_blank(), 
        axis.title.y = element_text(size =5,lineheight=3))

print(sft.temp)


p = ggplot(sft.df, aes(datetime, ctd_o2_mg_l))
sft.o2 = p + geom_point(aes(), size = 0.25, color = "red") +
  xlab("Deployment BOB 2020") + #last x label sets the time axis label
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


print(sft.o2)


# combine check samples to instrument data in a new data frame

# round sampling times of check samples since there is a slight mismatch in time

df3 <- chk.df

df3 <- select(df3, -datetime)

df3 <- df3 %>%                     
  rename(datetime = "datetime.tag")

df2 <- left_join(sft.df, df3, by = "datetime")

View(df2)

# Discrete spec pH regressed versus pH

spc.ph.v.sft.ph.int.v <- ggplotRegression(df2, "pH.check.median", "pH_int_v")

print(spc.ph.v.sft.ph.int.v)


spc.ph.v.sft.ph.ext.v <- ggplotRegression(df2, "pH.check.median", "pH_ext_v")

print(spc.ph.v.sft.ph.ext.v)


spc.ph.v.sft.ph.int <- ggplotRegression(df2, "pH.check.median", "pH_int_cell")

print(spc.ph.v.sft.ph.int)


spc.ph.v.sft.ph.ext <- ggplotRegression(df2, "pH.check.median", "pH_ext_cell")

print(spc.ph.v.sft.ph.ext)





# "SeaFET...." Deployment data for SeaFET
#  "sft.ext.v"                   
# "sft.int.v"                    "sft.o2"                       "sft.pH.diff"                 
# "sft.pH.diff.no.note"          "sft.ph.ext.v.o2"              "sft.ph.ext.v.o2.ol.rm"       
# "sft.ph.int.v.o2"              "sft.ph.int.v.o2.ol.rm"        "sft.pH_ext"                  
# "sft.pH_int"                   "sft.phs"                      "sft.sal"                     
# "sft.sal.dt"                   "sft.temp"                     "sft.v_diff"                  
# "sft.v_diff.2"                 "spc.ph.v.sft.ph.ext"          "spc.ph.v.sft.ph.ext.v" 
# "spc.ph.v.sft.ph.int"          "spc.ph.v.sft.ph.int.v"       
# 


ggsave("qaqc_plots/2020/BOB-deployment_Volt_Sal_2020-select-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sft.int.v), ggplotGrob(sft.ext.v),
                              ggplotGrob(sft.v_diff), ggplotGrob(sft.sal.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-deployment_Volt_Sal_2020-2-select-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sft.int.v), ggplotGrob(sft.ext.v),
                              ggplotGrob(sft.v_diff.2), ggplotGrob(sft.sal.dt),
                              size = "last")), width = 6.65, height = 3.5)

ggsave("qaqc_plots/2020/BOB-deployment_pHs_Sal_2020-select-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sft.pH_int), ggplotGrob(sft.pH_ext),
                              ggplotGrob(sft.pH.diff), ggplotGrob(sft.sal.dt),
                              size = "last")), width = 6.65, height = 3.5)


ggsave("qaqc_plots/2020/BOB-deployment_pH-ext_T_S_O2_2020-2-select-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(sft.pH_ext), 
                              ggplotGrob(sft.sal), ggplotGrob(sft.temp),
                              ggplotGrob(sft.o2),
                              size = "last")), width = 6.65, height = 3.5)

# regression  plots

#"spc.ph.v.sft.ph.ext"          "spc.ph.v.sft.ph.ext.v" 
# "spc.ph.v.sft.ph.int"          "spc.ph.v.sft.ph.int.v"     

ggsave("qaqc_plots/2020/BOB-deployment_spc.pH_v_sft.pH.int_2020-select-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(spc.ph.v.sft.ph.int.v), 
                              ggplotGrob(spc.ph.v.sft.ph.int),
                              size = "last")), width = 5, height = 5)

ggsave("qaqc_plots/2020/BOB-deployment_spc.pH_v_sft.pH.ext_2020-select-flagged.png",
       plot = grid.draw(rbind(ggplotGrob(spc.ph.v.sft.ph.ext.v), 
                              ggplotGrob(spc.ph.v.sft.ph.ext),
                              size = "last")), width = 5, height = 5)


























