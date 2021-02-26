
#combining data from text files

setwd("C:/Users/915712257/Box Sync/Inbox/oceanographic work/MARI SeapHOx assessment/2019 data")


rm(list = ls())

library(dplyr)
library(lubridate)

list.files()

# [1] "20190311-20190521 MARI data raw_final.csv"                               
# [2] "20190521-20190731 MARI data raw.csv"                                 
# [3] "20190731-20191009 MARI data raw.csv" 
# "mari-buoy_field_samples_metadata_2019.csv" 
# "mari field bath 201900521.csv"



#read in field check sample data for mari 2019 ########################################

#read data tables and match var names
df1 = read.csv("mari-buoy_field_samples_metadata_2019.csv",
               header=T, stringsAsFactors=F, sep=",")

df1$datetime <- as.POSIXct(paste(df1$Date,df1$UTC), format = "%m/%d/%Y %H:%M") 

df1$pH.check.median <- as.numeric(df1$pH.check.median)


head(df1)
# 
# Date   UTC Sample.. Cast_Sal Cast_Temp pH.check.median   pH.check1   pH.check2   pH.check3
# 3/11/2019 19:38   M-0058      NaN       NaN             NaN         NaN         NaN         NaN


chk.df <- df1
rm(df1)

#clear workspace and reload tidied data
save(chk.df, file = "mari-check.samples-2019.RData")

rm(list=ls())

load("mari-check.samples-2019.RData")

rm(list=ls())


#read in bath check sample data for mari 2019 ########################################

#read data tables and match var names
df1 = read.csv("mari-buoy_bath_samples_metadata_2019.csv",
               header=T, stringsAsFactors=F, sep=",")

df1$datetime <- as.POSIXct(paste(df1$Date,df1$UTC), format = "%m/%d/%Y %H:%M") 

df1$pH.check.median <- as.numeric(df1$pH.check.median)


head(df1)
# 
# Date   UTC Sample.. Cast_Sal Cast_Temp pH.check.median   pH.check1   pH.check2   pH.check3
# 3/11/2019 19:38   M-0058      NaN       NaN             NaN         NaN         NaN         NaN


bth.chk.df <- df1
rm(df1)

#clear workspace and reload tidied data
save(bth.chk.df, file = "mari-bath.check.samples-2019.RData")

rm(list=ls())

load("mari-bath.check.samples-2019.RData")

rm(list=ls())


# PRE deployment common bath run 2019 ############################################

#pull in instrument data

#read data tables and match var names
df1 = read.csv("pre-deployment.mari-2019/mari predeployment bath 20190301-20190307.csv",
               header=T, stringsAsFactors=F, sep=",")


#get var names by printing data
head(df1)
# 

# FrameSync      DateTime..UTC. Sample.Number.... Error.Flags.... Temperature..Celsius.
# 1 SSPHOX01004 03/01/2019 04:25:42                 1              20               16.2112
# 
# External.pH..pH. Internal.pH..pH. External.pH..Volt. Internal.pH..Volt. pH.Temperature..Celsius.
# 1           7.8070           8.0059          -0.996100          -1.031704                  16.5352
# 
# Pressure..Decibar. Salinity..psu. Conductivity..S.m. Oxygen..ml.L. Relative.Humidity....
# 1              0.219        32.7005            4.15122         5.679                     0
#
# Int.Temperature..Celsius.
# 1                      16.9

df1 <- df1 %>%                     
  rename(datetime = 'DateTime..UTC.',
         sample.num = 'Sample.Number....',
         sbe_err = 'Error.Flags....', 
         ctd_temp = 'Temperature..Celsius.', 
         pH_ext = 'External.pH..pH.', 
         pH_int = 'Internal.pH..pH.', 
         pH_ext_v = "External.pH..Volt.",
         pH_int_v = 'Internal.pH..Volt.', 
         pH_temp = 'pH.Temperature..Celsius.', 
         press_dbar = 'Pressure..Decibar.',
         ctd_sal = 'Salinity..psu.',
         cond_Sm = "Conductivity..S.m.",
         ctd_o2_ml_l = 'Oxygen..ml.L.', 
         RH = "Relative.Humidity....",
         pH_int_temp = 'Int.Temperature..Celsius.')

df1 <- select(df1, -FrameSync, -sbe_err, -sample.num, -press_dbar, -cond_Sm, -ctd_o2_ml_l, -RH, -pH_int_temp)


#setting date time to POSIXct and arranging data by datetime

df1$datetime <- as.POSIXct(df1$datetime, format = "%m/%d/%Y %H:%M") 

#have data ascend in time

df1 <- df1 %>%
  arrange(datetime)

#calculate difference between pH electrodes
df1$pH_diff <- df1$pH_int - df1$pH_ext
df1$abs_pH_diff <- abs(df1$pH_int - df1$pH_ext)

df1$volt_diff <- df1$pH_ext_v - df1$pH_int_v
df1$abs_v_diff <- abs(df1$pH_ext_v - df1$pH_int_v)

#saving final test proof data for seafetV2 translator script
save(df1, file = "mari-common-bath-predeploy.2019.sphx.RData")

rm(list=ls())


#double check looks good
load("mari-common-bath-predeploy.2019.sphx.RData")




rm(list=ls())



# PRE deployment dickson standard run 2019 ############################################

#read data tables and match var names
df1 = read.csv("pre-deployment.mari-2019/mari dickson_standard_run 20190308-20190309.csv",
               header=T, stringsAsFactors=F, sep=",")

#get var names by printing data
head(df1)
# 

# FrameSync      DateTime..UTC. Sample.Number.... Error.Flags.... Temperature..Celsius. External.pH..pH.
# 1 SSPHOX01004 03/08/2019 04:40:59              2017              20               14.9507           8.2547
# 
# Internal.pH..pH. External.pH..Volt. Internal.pH..Volt. pH.Temperature..Celsius. Pressure..Decibar. Salinity..psu.
# 1           8.2892          -0.967675          -1.016114                  16.4852              0.160        28.0675
# 
# Conductivity..S.m. Oxygen..ml.L. Relative.Humidity.... Int.Temperature..Celsius.
# 1            3.51603         5.970                     0                      15.3


df1 <- df1 %>%                     
  rename(datetime = 'DateTime..UTC.',
         sample.num = 'Sample.Number....',
         sbe_err = 'Error.Flags....', 
         ctd_temp = 'Temperature..Celsius.', 
         og_pH_ext = 'External.pH..pH.', #origninal pH value based on wrong temp
         og_pH_int = 'Internal.pH..pH.', #origninal pH value based on wrong temp
         pH_ext_v = "External.pH..Volt.",
         pH_int_v = 'Internal.pH..Volt.', 
         pH_temp = 'pH.Temperature..Celsius.', 
         press_dbar = 'Pressure..Decibar.',
         ctd_sal = 'Salinity..psu.',
         cond_Sm = "Conductivity..S.m.",
         ctd_o2_ml_l = 'Oxygen..ml.L.', 
         RH = "Relative.Humidity....",
         pH_int_temp = 'Int.Temperature..Celsius.')

df1 <- select(df1, -FrameSync, -sbe_err, -sample.num, -press_dbar, -cond_Sm, -ctd_o2_ml_l, -RH, -pH_int_temp)


#setting date time to POSIXct and arranging data by datetime

df1$datetime <- as.POSIXct(df1$datetime, format = "%m/%d/%Y %H:%M") 

#have data ascend in time

df1 <- df1 %>%
  arrange(datetime)


#saving final test proof data for seafetV2 translator script
save(df1, file = "mari.dickson.run.predeploy.2019.sphx.RData")

rm(list=ls())



load("mari.dickson.run.predeploy.2019.sphx.RData")


#for calculations to be accurate, greatest digit count is 22, deafault is 7
options(digits = 15)

# PRE deployment set up seafetV2 processing algorithm mari 2019 ###################

# data and calculations pulled from Seabird Application Note 99 
# see: https://www.seabird.com/application-notes

# creating calibration data frame
# can pull data from seafet summary reports or raw .sbsdata for V2
# in summary report I assumed KDF0 and KDF2 were for the DuraFET internal electrode reference 
# will know if orignal and newly added pH values match

mari.2019.cal.df <- data.frame(k0_int.mari.2019.sbe = -1.47344,
                         k2_int.mari.2019.sbe = -0.00110563, 
                         k0_ext.mari.2019.sbe = -1.463136,
                         k2_ext.mari.2019.sbe = -0.001000847,
                         stringsAsFactors = FALSE)


# defining constants for seafetV2 ################

# calibration constants specific to each instrument's calibration file
# usually in xml format before raw data or in a separate xml data file


k0_int <- mari.2019.cal.df$k0_int.mari.2019.sbe 
k2_int <- mari.2019.cal.df$k2_int.mari.2019.sbe

k0_ext <- mari.2019.cal.df$k0_ext.mari.2019.sbe
k2_ext <- mari.2019.cal.df$k2_ext.mari.2019.sbe

# data value calls

T_degC <- df1$pH_temp

T_k <- T_degC + 273.15 # TdegC converted to Kelvin

# for dickson standard specify 35
S_psu <- 35  

#final algortihm test with mari.data
#S_psu <- df1$ctd_sal

#theoretical constants

Rgas <- 8.3144621 # R is the universal gas constant # units J /(K mol))

Frdy_c <- 96485.365 # Faraday constant # units Coulombs/mole C/mol


# defining oceanographic constants

# expected nernstian slope response
Snernst = Rgas * T_k * log(10) / Frdy_c 

# Total chloride in seawater
Cl_T <- (0.99889/35.453) * (S_psu/1.80655) * ((1000) / (1000-(1.005*S_psu))) 


#sample ionic strength
Ion_str <- (19.924*S_psu) / (1000-(1.005*S_psu))

#Debye-Huckel constant for activity of HCl

A_DH <- (0.0000034286 * T_degC^2) + (0.00067524 * T_degC) + 0.49172143

#logarithm of HCl activity coefficient
log_HCl_ac <- ((-A_DH * sqrt(Ion_str))/(1 + (1.394 * sqrt(Ion_str)))) +((0.08885 - (0.000111 * T_degC)) * Ion_str)

#total sulfate in seawater
Sulf8_T <- (0.1400/96.062) * (S_psu/1.80655)  

# Acid dissociation constant of HSO4-
K_sulf8 <- (1-(0.001005 * S_psu)) * exp(((-4276.1/T_k) + 141.328 - (23.093*log(T_k))) +
                                          (((-13856/T_k) +324.57-(47.986*log(T_k)))*sqrt(Ion_str)) +
                                          (((35474/T_k) - 771.54 + (114.723 * log(T_k))) * Ion_str) -
                                          ((2698/T_k) * Ion_str^1.5) + ((1776/T_k) * Ion_str^2))

#pH internal caclulation ################################

# electrode cell voltage junction to pH

df1$pH_int_cell <- ((df1$pH_int_v - k0_int) - (k2_int * T_degC)) / Snernst

df1$pH_int_cell <- round(df1$pH_int_cell, digits = 4)


# pH external caclulation ###########################

# electrode cell voltage junction to pH

df1$pH_ext_cell <- (((df1$pH_ext_v - k0_ext) - (k2_ext * T_degC)) / Snernst) +
  log10(Cl_T) + (2 * log_HCl_ac) - log10(1+(Sulf8_T/K_sulf8)) - log10((1000-(1.005*S_psu)) / 1000)

df1$pH_ext_cell <- round(df1$pH_ext_cell, digits = 4)


#calculate difference between pH electrodes
df1$pH_diff <- df1$pH_int_cell - df1$pH_ext_cell
df1$abs_pH_diff <- abs(df1$pH_int_cell - df1$pH_ext_cell)

df1$volt_diff <- df1$pH_ext_v - df1$pH_int_v
df1$abs_v_diff <- abs(df1$pH_ext_v - df1$pH_int_v)


# save mari 2019 PRE deployment dickson std run #################################

#clear workspace and reload tidied data
save(df1, file = "mari-pre-deploy-dickson-run-prcsd-2019.RData")

rm(list=ls())

load("mari-pre-deploy-dickson-run-prcsd-2019.RData")

# subsetting predeployment  dickson standard run to get median pH value from
# last 278 samples, or 4.6333 hours of run
t3 <- '2019-03-08 20:00:00'
df1 <- filter(df1, datetime > t3)

save(df1, file = "mari-pre-deploy-dickson-run-prcsd-calibration-08mar2019.RData")

median.dickson.value.int <- median(df1$pH_int_cell)
print(median.dickson.value.int) 
#8.3305

median.dickson.value.ext <- median(df1$pH_ext_cell)
print(median.dickson.value.ext) 
#8.3564

median.temp.value <- median(df1$pH_temp) 
print(median.temp.value) 
#16.0377

rm(list=ls())



# DEPLOYMENT instrument data tidying ####################################################


mari.2019.screen.df <- data.frame(k0_int.mari.2019.eos = -1.50105017881742,
                                  k2_int.mari.2019.eos = -0.00110563, 
                                  k0_ext.mari.2019.eos = -1.48621134712642,
                                  k2_ext.mari.2019.eos = -0.001000847,
                                  int.v.min = -1.06,
                                  int.v.max =  -1.02,
                                  ext.v.min = -1.01,
                                  ext.v.max =  -0.97,
                                  stringsAsFactors = FALSE)

#clear workspace and reload tidied data
save(mari.2019.screen.df, file = "mari.2019.screen.RData")

rm(list=ls())

load("mari.2019.screen.RData")


#read data tables and match var names
df1 = read.csv("20190311-20190521 MARI data raw_final.csv",
               header=T, stringsAsFactors=F, sep=",")

#get var names by printing data
head(df1)
# 

# FrameSync      DateTime..UTC. Sample.Number.... Error.Flags.... Temperature..Celsius. External.pH..pH.
# 1 SSPHOX01004 03/11/2019 19:20:01              3413              20               12.1862           7.8925
# 
# Internal.pH..pH. External.pH..Volt. Internal.pH..Volt. pH.Temperature..Celsius. Pressure..Decibar.
# 1           7.9485          -0.986572          -1.036893                  12.7169             15.635
# 
# Salinity..psu. Conductivity..S.m. Oxygen..ml.L. Relative.Humidity.... Int.Temperature..Celsius.
# 1        23.5597            2.81143         6.203                     0                      15.3 


# create variable with oxygen converted to mg per liter

df1$Oxygen..mg.L. <- 1.42903*df1$Oxygen..ml.L.

df1 <- df1 %>%                     
  rename(datetime = 'DateTime..UTC.',
         sbe_err = 'Error.Flags....', 
         ctd_temp = 'Temperature..Celsius.', 
         pH_ext = 'External.pH..pH.',
         pH_int = 'Internal.pH..pH.',
         pH_ext_v = "External.pH..Volt.",
         pH_int_v = 'Internal.pH..Volt.', 
         pH_temp = 'pH.Temperature..Celsius.', 
         press_dbar = 'Pressure..Decibar.',
         ctd_sal = 'Salinity..psu.',
         cond_Sm = "Conductivity..S.m.",
         ctd_o2_ml_l = 'Oxygen..ml.L.', 
         ctd_o2_mg_l = 'Oxygen..mg.L.',
         RH = "Relative.Humidity....",
         pH_int_temp = 'Int.Temperature..Celsius.')

df1 <- select(df1, -FrameSync, -sbe_err, -Sample.Number....)




df2 = read.csv("20190521-20190731 MARI data raw.csv",
               header=T, stringsAsFactors=F, sep=",")

head(df2)

# DateTime..UTC.00.00. Error.Flags.... Temperature..Celsius. External.pH..pH. Internal.pH..pH. External.pH..Volt.
# 1  05/21/2019 20:00:01              20               15.0302           7.8871           7.8151          -0.984715
# 
# Internal.pH..Volt. pH.Temperature..Celsius. Pressure..Decibar. Salinity..psu. Conductivity..S.m. Oxygen..ml.L.
# 1          -1.043181                  15.1217             17.923        23.8620            3.04211        12.021
# 
# Oxygen.mg.L. Relative.Humidity.... Int.Temperature..Celsius. QARTOD
# 1    17.178370                     1                      15.2      3

df2 <- df2 %>%                     
  rename(datetime = 'DateTime..UTC.00.00.',
         sbe_err = 'Error.Flags....', 
         ctd_temp = 'Temperature..Celsius.', 
         pH_ext = 'External.pH..pH.',
         pH_int = 'Internal.pH..pH.',
         pH_ext_v = "External.pH..Volt.",
         pH_int_v = 'Internal.pH..Volt.', 
         pH_temp = 'pH.Temperature..Celsius.', 
         press_dbar = 'Pressure..Decibar.',
         ctd_sal = 'Salinity..psu.',
         cond_Sm = "Conductivity..S.m.",
         ctd_o2_ml_l = 'Oxygen..ml.L.', 
         ctd_o2_mg_l = 'Oxygen.mg.L.',
         RH = "Relative.Humidity....",
         pH_int_temp = 'Int.Temperature..Celsius.',
         QARTOD = 'QARTOD')

df2 <- select(df2, -sbe_err)




df3 = read.csv("20190731-20191009 MARI data raw.csv",
               header=T, stringsAsFactors=F, sep=",")


head(df3)

# DateTime..UTC.00.00. Temperature..Celsius. External.pH..pH. Internal.pH..pH. External.pH..Volt. Internal.pH..Volt.
# 1  07/31/2019 18:40:00               17.1073           7.9241           7.8764          -0.986307          -1.038726
# 
# pH.Temperature..Celsius. Pressure..Decibar. Salinity..psu. Conductivity..S.m. Oxygen..ml.L. Oxygen..mg.L.
# 1                  18.3569             17.070        29.9288            3.91244        13.234      18.91178
# 
# Relative.Humidity.... Int.Temperature..Celsius. QARTOD
# 1                   4.3                      18.9      3


df3 <- df3 %>%                     
  rename(datetime = 'DateTime..UTC.00.00.',
         ctd_temp = 'Temperature..Celsius.', 
         pH_ext = 'External.pH..pH.',
         pH_int = 'Internal.pH..pH.',
         pH_ext_v = "External.pH..Volt.",
         pH_int_v = 'Internal.pH..Volt.', 
         pH_temp = 'pH.Temperature..Celsius.', 
         press_dbar = 'Pressure..Decibar.',
         ctd_sal = 'Salinity..psu.',
         cond_Sm = "Conductivity..S.m.",
         ctd_o2_ml_l = 'Oxygen..ml.L.', 
         ctd_o2_mg_l = 'Oxygen..mg.L.',
         RH = "Relative.Humidity....",
         pH_int_temp = 'Int.Temperature..Celsius.',
         QARTOD = 'QARTOD')



#combine into one data frame

data <- bind_rows(df1,df2, df3, .id= NULL)

#setting date time to POSIXct and arranging data by datetime

data$datetime <- as.POSIXct(data$datetime, format = "%m/%d/%Y %H:%M") 

#have data ascend in time

data <- data %>%
  arrange(datetime)

#calculate difference between pH electrodes

data$pH_diff <- data$pH_int - data$pH_ext
data$abs_pH_diff <- abs(data$pH_int - data$pH_ext)

data$volt_diff <- data$pH_ext_v - data$pH_int_v
data$abs_v_diff <- data$pH_ext_v - data$pH_int_v

#flag out bad voltages

data <- filter(data, pH_int_v > mari.2019.screen.df$int.v.min & pH_int_v < mari.2019.screen.df$int.v.max)

data <- filter(data, pH_ext_v > mari.2019.screen.df$ext.v.min & pH_ext_v < mari.2019.screen.df$ext.v.max)

#clear workspace and reload tidied data
save(data, file = "mari.2019.RData")

rm(list=ls())

load("mari.2019.RData")

rm(list=ls())

#save as csv

#write.csv(data, "mari_2019.csv")


# ADJUSTED DEPLOYMENT DATA BASED ON CALIBRATION K0 (E*) VALUES #########

rm(list=ls())

load("mari.2019.screen.RData")
load("mari.2019.RData")

#read data tables and match var names
df1 = data
rm(data)

#get var names by printing data
head(df1)
# 

# FrameSync      DateTime..UTC. Sample.Number.... Error.Flags.... Temperature..Celsius. External.pH..pH.
# 1 SSPHOX01004 03/08/2019 04:40:59              2017              20               14.9507           8.2547
# 
# Internal.pH..pH. External.pH..Volt. Internal.pH..Volt. pH.Temperature..Celsius. Pressure..Decibar. Salinity..psu.
# 1           8.2892          -0.967675          -1.016114                  16.4852              0.160        28.0675
# 
# Conductivity..S.m. Oxygen..ml.L. Relative.Humidity.... Int.Temperature..Celsius.
# 1            3.51603         5.970                     0                      15.3


df1 <- df1 %>%                     
  rename(og_pH_ext = 'pH_ext', #origninal pH value based on SBE calibration
         og_pH_int = 'pH_int') #origninal pH value based on SBE calibration
         

df1 <- select(df1, -QARTOD, -volt_diff, -pH_diff)



#for calculations to be accurate, greatest digit count is 22, deafault is 7
options(digits = 15)

# ADJUSTED DEPLOYMENT set up seafetV2 processing algorithm mari 2019 ###################

# data and calculations pulled from Seabird Application Note 99 
# see: https://www.seabird.com/application-notes

# creating calibration data frame
# can pull data from seafet summary reports or raw .sbsdata for V2
# in summary report I assumed KDF0 and KDF2 were for the DuraFET internal electrode reference 

# mari.2019.cal.df <- data.frame(k0_int.mari.2019.sbe = -1.47344,
#                                k2_int.mari.2019.sbe = -0.00110563,
#                                k0_ext.mari.2019.sbe = -1.463136,
#                                k2_ext.mari.2019.sbe = -0.001000847,
#                                stringsAsFactors = FALSE)


# defining constants for seafetV2 ################

# calibration constants specific to each instrument's calibration file
# usually in xml format before raw data or in a separate xml data file


k0_int <- mari.2019.screen.df$k0_int.mari.2019.eos 
k2_int <- mari.2019.screen.df$k2_int.mari.2019.eos

k0_ext <- mari.2019.screen.df$k0_ext.mari.2019.eos
k2_ext <- mari.2019.screen.df$k2_ext.mari.2019.eos

# data value calls

T_degC <- df1$ctd_temp

T_k <- T_degC + 273.15 # TdegC converted to Kelvin

# for dickson standard specify 35
S_psu <- df1$ctd_sal  

#final algortihm test with mari.data
#S_psu <- df1$ctd_sal

#theoretical constants

Rgas <- 8.3144621 # R is the universal gas constant # units J /(K mol))

Frdy_c <- 96485.365 # Faraday constant # units Coulombs/mole C/mol


# defining oceanographic constants

# expected nernstian slope response
Snernst = Rgas * T_k * log(10) / Frdy_c 

# Total chloride in seawater
Cl_T <- (0.99889/35.453) * (S_psu/1.80655) * ((1000) / (1000-(1.005*S_psu))) 


#sample ionic strength
Ion_str <- (19.924*S_psu) / (1000-(1.005*S_psu))

#Debye-Huckel constant for activity of HCl

A_DH <- (0.0000034286 * T_degC^2) + (0.00067524 * T_degC) + 0.49172143

#logarithm of HCl activity coefficient
log_HCl_ac <- ((-A_DH * sqrt(Ion_str))/(1 + (1.394 * sqrt(Ion_str)))) +((0.08885 - (0.000111 * T_degC)) * Ion_str)

#total sulfate in seawater
Sulf8_T <- (0.1400/96.062) * (S_psu/1.80655)  

# Acid dissociation constant of HSO4-
K_sulf8 <- (1-(0.001005 * S_psu)) * exp(((-4276.1/T_k) + 141.328 - (23.093*log(T_k))) +
                                          (((-13856/T_k) +324.57-(47.986*log(T_k)))*sqrt(Ion_str)) +
                                          (((35474/T_k) - 771.54 + (114.723 * log(T_k))) * Ion_str) -
                                          ((2698/T_k) * Ion_str^1.5) + ((1776/T_k) * Ion_str^2))

#pH internal caclulation ################################

# electrode cell voltage junction to pH

df1$pH_int_cell <- ((df1$pH_int_v - k0_int) - (k2_int * T_degC)) / Snernst

df1$pH_int_cell <- round(df1$pH_int_cell, digits = 4)


# pH external caclulation ###########################

# electrode cell voltage junction to pH

df1$pH_ext_cell <- (((df1$pH_ext_v - k0_ext) - (k2_ext * T_degC)) / Snernst) +
  log10(Cl_T) + (2 * log_HCl_ac) - log10(1+(Sulf8_T/K_sulf8)) - log10((1000-(1.005*S_psu)) / 1000)

df1$pH_ext_cell <- round(df1$pH_ext_cell, digits = 4)


#calculate difference between pH electrodes

df1$abs_pH_diff <- abs(df1$pH_int_cell - df1$pH_ext_cell)

df1$abs_v_diff <- abs(df1$pH_ext_v - df1$pH_int_v)


# save mari 2019 PRE deployment dickson std run #################################

#clear workspace and reload tidied data
save(df1, file = "mari.2019.adj.final.RData")

median(df1$pH_int_cell)
median(df1$og_pH_int)
median(df1$pH_ext_cell)
median(df1$og_pH_ext)

rm(list=ls())

load("mari.2019.adj.final.RData")


rm(list=ls())

# MID deployment common bath run 2019 ############################################

#pull in instrument data

#read data tables and match var names
df1 = read.csv("mari field bath 201900521.csv",
               header=T, stringsAsFactors=F, sep=",")


#get var names by printing data
head(df1)
# 

# FrameSync      DateTime..UTC. Sample.Number.... Error.Flags.... Temperature..Celsius.
# 1 SSPHOX01004 03/01/2019 04:25:42                 1              20               16.2112
# 
# External.pH..pH. Internal.pH..pH. External.pH..Volt. Internal.pH..Volt. pH.Temperature..Celsius.
# 1           7.8070           8.0059          -0.996100          -1.031704                  16.5352
# 
# Pressure..Decibar. Salinity..psu. Conductivity..S.m. Oxygen..ml.L. Relative.Humidity....
# 1              0.219        32.7005            4.15122         5.679                     0
#
# Int.Temperature..Celsius.
# 1                      16.9

df1 <- df1 %>%                     
  rename(datetime = 'DateTime..UTC.',
         sample.num = 'Sample.Number....',
         sbe_err = 'Error.Flags....', 
         ctd_temp = 'Temperature..Celsius.', 
         pH_ext = 'External.pH..pH.', 
         pH_int = 'Internal.pH..pH.', 
         pH_ext_v = "External.pH..Volt.",
         pH_int_v = 'Internal.pH..Volt.', 
         pH_temp = 'pH.Temperature..Celsius.', 
         press_dbar = 'Pressure..Decibar.',
         ctd_sal = 'Salinity..psu.',
         cond_Sm = "Conductivity..S.m.",
         ctd_o2_ml_l = 'Oxygen..ml.L.', 
         RH = "Relative.Humidity....",
         pH_int_temp = 'Int.Temperature..Celsius.')

df1 <- select(df1, -FrameSync, -sbe_err, -sample.num, -press_dbar, -cond_Sm, -ctd_o2_ml_l, -RH, -pH_int_temp)


#setting date time to POSIXct and arranging data by datetime

df1$datetime <- as.POSIXct(df1$datetime, format = "%m/%d/%Y %H:%M") 

#have data ascend in time

df1 <- df1 %>%
  arrange(datetime)

#calculate difference between pH electrodes
df1$pH_diff <- df1$pH_int - df1$pH_ext
df1$abs_pH_diff <- abs(df1$pH_int - df1$pH_ext)

df1$volt_diff <- df1$pH_ext_v - df1$pH_int_v
df1$abs_v_diff <- abs(df1$pH_ext_v - df1$pH_int_v)

#saving final test proof data for seafetV2 translator script
save(df1, file = "mari-field-bath-20190521.sphx.RData")

rm(list=ls())


#double check looks good
load("mari-field-bath-20190521.sphx.RData")




rm(list=ls())




# POST deployment common bath run 2019 ############################################

#pull in instrument data

#read data tables and match var names
df1 = read.csv("post-deployment.mari-2019/20191112 pH bath and dickson run/mari-post-deployment.common.bath-20191030-20191106.csv",
               header=T, stringsAsFactors=F, sep=",")

# create variable with oxygen converted to mg per liter

df1$Oxygen..mg.L. <- 1.42903*df1$Oxygen..ml.L.

#get var names by printing data
head(df1)
summary(df1)
# 

# FrameSync      DateTime..UTC. Sample.Number.... Error.Flags.... Temperature..Celsius. External.pH..pH.
# SSPHOX01004 10/30/2019 21:40:01             11679              20               14.1991           7.9407
# 
# Internal.pH..pH. External.pH..Volt. Internal.pH..Volt. pH.Temperature..Celsius. Pressure..Decibar.
# 7.9267          -0.989392          -1.037188                  14.1402              0.268
# 
# Salinity..psu. Conductivity..S.m. Oxygen..ml.L. Relative.Humidity.... Int.Temperature..Celsius.
# 31.6877            3.85404         5.776                   5.2                      14.2

# Oxygen..mg.L.
# 8.254077

#rename variables to codeable names

df1 <- df1 %>%                     
  rename(datetime = 'DateTime..UTC.',
         sample.num = 'Sample.Number....',
         sbe_err = 'Error.Flags....', 
         ctd_temp = 'Temperature..Celsius.', 
         pH_ext = 'External.pH..pH.', 
         pH_int = 'Internal.pH..pH.', 
         pH_ext_v = "External.pH..Volt.",
         pH_int_v = 'Internal.pH..Volt.', 
         pH_temp = 'pH.Temperature..Celsius.', 
         press_dbar = 'Pressure..Decibar.',
         ctd_sal = 'Salinity..psu.',
         cond_Sm = "Conductivity..S.m.",
         ctd_o2_ml_l = 'Oxygen..ml.L.',
         ctd_o2_mg_l = 'Oxygen..mg.L.',
         RH = "Relative.Humidity....",
         pH_int_temp = 'Int.Temperature..Celsius.')


df1 <- select(df1, -FrameSync, -sbe_err, -sample.num, -press_dbar, -cond_Sm, -ctd_o2_ml_l, -RH, -pH_int_temp)


#setting date time to POSIXct and arranging data by datetime

df1$datetime <- as.POSIXct(df1$datetime, format = "%m/%d/%Y %H:%M") 

#have data ascend in time

df1 <- df1 %>%
  arrange(datetime)

#calculate difference between pH electrodes
df1$pH_diff <- df1$pH_int - df1$pH_ext
df1$abs_pH_diff <- abs(df1$pH_int - df1$pH_ext)

df1$volt_diff <- df1$pH_ext_v - df1$pH_int_v
df1$abs_v_diff <- abs(df1$pH_ext_v - df1$pH_int_v)

#saving final test proof data for seafetV2 translator script
save(df1, file = "mari-common-bath-post.deploy.2019.sphx.RData")

rm(list=ls())


#double check looks good
load("mari-common-bath-post.deploy.2019.sphx.RData")




rm(list=ls())



# POST deployment dickson standard run 2019 ############################################

#read data tables and match var names
df1 = read.csv("post-deployment.mari-2019/20191112 pH bath and dickson run/mari-post-deployment.dickson.std.run-20191106-20191113.csv",
               header=T, stringsAsFactors=F, sep=",")

#get var names by printing data
head(df1)
# 

# FrameSync      DateTime..UTC. Sample.Number.... Error.Flags.... Temperature..Celsius. External.pH..pH.
# 1 SSPHOX01004 03/08/2019 04:40:59              2017              20               14.9507           8.2547
# 
# Internal.pH..pH. External.pH..Volt. Internal.pH..Volt. pH.Temperature..Celsius. Pressure..Decibar. Salinity..psu.
# 1           8.2892          -0.967675          -1.016114                  16.4852              0.160        28.0675
# 
# Conductivity..S.m. Oxygen..ml.L. Relative.Humidity.... Int.Temperature..Celsius.
# 1            3.51603         5.970                     0                      15.3


df1 <- df1 %>%                     
  rename(datetime = 'DateTime..UTC.',
         sample.num = 'Sample.Number....',
         sbe_err = 'Error.Flags....', 
         ctd_temp = 'Temperature..Celsius.', 
         og_pH_ext = 'External.pH..pH.', #origninal pH value based on wrong temp
         og_pH_int = 'Internal.pH..pH.', #origninal pH value based on wrong temp
         pH_ext_v = "External.pH..Volt.",
         pH_int_v = 'Internal.pH..Volt.', 
         pH_temp = 'pH.Temperature..Celsius.', 
         press_dbar = 'Pressure..Decibar.',
         ctd_sal = 'Salinity..psu.',
         cond_Sm = "Conductivity..S.m.",
         ctd_o2_ml_l = 'Oxygen..ml.L.', 
         RH = "Relative.Humidity....",
         pH_int_temp = 'Int.Temperature..Celsius.')

df1 <- select(df1, -FrameSync, -sbe_err, -sample.num, -press_dbar, -cond_Sm, -ctd_o2_ml_l, -RH, -pH_int_temp)


#setting date time to POSIXct and arranging data by datetime

df1$datetime <- as.POSIXct(df1$datetime, format = "%m/%d/%Y %H:%M") 

#have data ascend in time

df1 <- df1 %>%
  arrange(datetime)


#saving final test proof data for seafetV2 translator script
save(df1, file = "mari.dickson.run.post.deploy.2019.sphx.RData")

rm(list=ls())



load("mari.dickson.run.post.deploy.2019.sphx.RData")


#for calculations to be accurate, greatest digit count is 22, deafault is 7
options(digits = 15)

# POST deployment set up seafetV2 processing algorithm mari 2019 ###################

# data and calculations pulled from Seabird Application Note 99 
# see: https://www.seabird.com/application-notes

# creating calibration data frame
# can pull data from seafet summary reports or raw .sbsdata for V2
# in summary report I assumed KDF0 and KDF2 were for the DuraFET internal electrode reference 
# will know if orignal and newly added pH values match

mari.2019.cal.df <- data.frame(k0_int.mari.2019.sbe = -1.47344,
                               k2_int.mari.2019.sbe = -0.00110563, 
                               k0_ext.mari.2019.sbe = -1.463136,
                               k2_ext.mari.2019.sbe = -0.001000847,
                               stringsAsFactors = FALSE)


# defining constants for seafetV2 ################

# calibration constants specific to each instrument's calibration file
# usually in xml format before raw data or in a separate xml data file


k0_int <- mari.2019.cal.df$k0_int.mari.2019.sbe 
k2_int <- mari.2019.cal.df$k2_int.mari.2019.sbe

k0_ext <- mari.2019.cal.df$k0_ext.mari.2019.sbe
k2_ext <- mari.2019.cal.df$k2_ext.mari.2019.sbe

# data value calls

T_degC <- df1$pH_temp

T_k <- T_degC + 273.15 # TdegC converted to Kelvin

# for dickson standard specify 35
S_psu <- 35  

#final algortihm test with mari.data
#S_psu <- df1$ctd_sal

#theoretical constants

Rgas <- 8.3144621 # R is the universal gas constant # units J /(K mol))

Frdy_c <- 96485.365 # Faraday constant # units Coulombs/mole C/mol


# defining oceanographic constants

# expected nernstian slope response
Snernst = Rgas * T_k * log(10) / Frdy_c 

# Total chloride in seawater
Cl_T <- (0.99889/35.453) * (S_psu/1.80655) * ((1000) / (1000-(1.005*S_psu))) 


#sample ionic strength
Ion_str <- (19.924*S_psu) / (1000-(1.005*S_psu))

#Debye-Huckel constant for activity of HCl

A_DH <- (0.0000034286 * T_degC^2) + (0.00067524 * T_degC) + 0.49172143

#logarithm of HCl activity coefficient
log_HCl_ac <- ((-A_DH * sqrt(Ion_str))/(1 + (1.394 * sqrt(Ion_str)))) +((0.08885 - (0.000111 * T_degC)) * Ion_str)

#total sulfate in seawater
Sulf8_T <- (0.1400/96.062) * (S_psu/1.80655)  

# Acid dissociation constant of HSO4-
K_sulf8 <- (1-(0.001005 * S_psu)) * exp(((-4276.1/T_k) + 141.328 - (23.093*log(T_k))) +
                                          (((-13856/T_k) +324.57-(47.986*log(T_k)))*sqrt(Ion_str)) +
                                          (((35474/T_k) - 771.54 + (114.723 * log(T_k))) * Ion_str) -
                                          ((2698/T_k) * Ion_str^1.5) + ((1776/T_k) * Ion_str^2))

#pH internal caclulation ################################

# electrode cell voltage junction to pH

df1$pH_int_cell <- ((df1$pH_int_v - k0_int) - (k2_int * T_degC)) / Snernst

df1$pH_int_cell <- round(df1$pH_int_cell, digits = 4)


# pH external caclulation ###########################

# electrode cell voltage junction to pH

df1$pH_ext_cell <- (((df1$pH_ext_v - k0_ext) - (k2_ext * T_degC)) / Snernst) +
  log10(Cl_T) + (2 * log_HCl_ac) - log10(1+(Sulf8_T/K_sulf8)) - log10((1000-(1.005*S_psu)) / 1000)

df1$pH_ext_cell <- round(df1$pH_ext_cell, digits = 4)


#calculate difference between pH electrodes

#calculate difference between pH electrodes
df1$pH_diff <- df1$pH_int_cell - df1$pH_ext_cell
df1$abs_pH_diff <- abs(df1$pH_int_cell - df1$pH_ext_cell)

df1$volt_diff <- df1$pH_ext_v - df1$pH_int_v
df1$abs_v_diff <- abs(df1$pH_ext_v - df1$pH_int_v)

# save mari 2019 POST deployment dickson std run #################################

#clear workspace and reload tidied data
save(df1, file = "mari-post-deploy-dickson-run-prcsd-2019.RData")

rm(list=ls())

load("mari-post-deploy-dickson-run-prcsd-2019.RData")

# subsetting post-deployment  dickson standard run to get median pH value from
# last ___number____ samples, or _____number______ hours of run


t3 <- '2019-03-08 20:00:00'
df1 <- filter(df1, datetime > t3)

median.dickson.value.int <- median(df1$pH_int_cell)
print(median.dickson.value.int) 
#8.3305

median.dickson.value.ext <- median(df1$pH_ext_cell)
print(median.dickson.value.ext) 
#8.3564

median.temp.value <- median(df1$pH_temp) 
print(median.temp.value) 
#16.0377

rm(list=ls())



#### calibrating 2019 MARI data with pre-deploy  Dickson Run ##############################################

rm(list = ls())

load("mari-pre-deploy-dickson-run-prcsd-calibration-08mar2019.RData")

# data called variables

T_degC <- df1$pH_temp

T_k <- T_degC + 273.15 # TdegC converted to Kelvin

S_psu <- 35

#theoretical constants

# for Bresnahan calibration constant calculation 

Rgas <- 8.3145 # R is the universal gas constant # units J /(K mol))

Frdy_c <- 96487 # Faraday constant # units Coulombs/mole C/mol



# for SeaFETV2
# Rgas <- 8.3144621 # R is the universal gas constant # units J /(K mol))
# 
# Frdy_c <- 96485.365 # Faraday constant # units Coulombs/mole C/mol


# Temperature dependence of standard potentials, Martz et al. 2010
dE0int = -0.001101
dE0ext = -0.001048


# defining oceanographic constants

# expected nernstian slope response
Snernst = Rgas * T_k * log(10) / Frdy_c 

# for SeaFET v1 and Bresnahan calculation
# Total chloride in seawater
Cl_T <- (0.99889/35.453) * (S_psu/1.80655) * ((1000))

# For SeaFET V2
# # Total chloride in seawater
# Cl_T <- (0.99889/35.453) * (S_psu/1.80655) * ((1000) / (1000-(1.005*S_psu))) 

# chloried in mol/kg-H2O
m_Cl = Cl_T*1000/(1000 - ((S_psu*35.165)/35)) 

#sample ionic strength
Ion_str <- (19.924*S_psu) / (1000-(1.005*S_psu))

#Debye-Huckel constant for activity of HCl

A_DH <- (0.0000034286 * T_degC^2) + (0.00067524 * T_degC) + 0.49172143

DHconst = 0.00000343*T_degC^2 + 0.00067524*T_degC + 0.49172143 # Debye-Huckel, Khoo et al. 1977 for SeaFET V1

#logarithm of HCl activity coefficient

# for SeaFET V2

# log_HCl_ac <- ((-A_DH * sqrt(Ion_str))/(1 + (1.394 * sqrt(Ion_str)))) +
#   ((0.08885 - (0.000111 * T_degC)) * Ion_str)

# same formula as above but doubled now instead of later

log10gamma_HCl = 2 * (-DHconst*sqrt(Ion_str) / (1 + 1.394*sqrt(Ion_str)) + (0.08885-0.000111*T_degC)*Ion_str)

#total sulfate in seawater
Sulf8_T <- (0.1400/96.062) * (S_psu/1.80655)  

# Acid dissociation constant of HSO4-
K_sulf8 <- (1-(0.001005 * S_psu)) * exp(((-4276.1/T_k) + 141.328 - (23.093*log(T_k))) +
                                          (((-13856/T_k) +324.57-(47.986*log(T_k)))*sqrt(Ion_str)) +
                                          (((35474/T_k) - 771.54 + (114.723 * log(T_k))) * Ion_str) -
                                          ((2698/T_k) * Ion_str^1.5) + ((1776/T_k) * Ion_str^2))

# Bresnahan 2014 calibration constants matlab script calculations

pHint_free <- df1$og_pH_int + log10(1+Sulf8_T/K_sulf8)

cHfree <- 10^(-pHint_free) # mol/kg-sw

pHint_free <-  pHint_free+log10((1000-S_psu*35.165/35)/1000) # mol/kg-H2O

mHfree = 10^(-pHint_free) # mol/kg-H2O

aHfree_aCl = mHfree*m_Cl*10^(log10gamma_HCl)


#Calc E0int from Nernst & pH @ calibration point

df1$E0int <-  df1$pH_int_v - Snernst*df1$og_pH_int
df1$E0int25 <-  df1$E0int + dE0int*(25-T_degC)

median(df1$E0int25)

#with and without extra chloride term in chloride total
#-1.50105017881742

df1$E0ext <-  df1$pH_ext_v + Snernst*log10(aHfree_aCl)
df1$E0ext25 <-  df1$E0ext + dE0ext*(25-T_degC)

median(df1$E0ext25)
#with extra chloride term in chloride total
#-1.48621134712642


#without extra chloride term in chloride total
#-1.31496268637657



