
#combining data from text files

library(dplyr)
library(lubridate)
library(chron)
library(here)

# 
# "20200430-20200710_processed.csv" 
# "20200710-20200815_processed.csv"
# "20200815-20201025_processed.csv"

# use at your peril...
# rm(list=ls())

getwd()

setwd(here("data", "bob", "seafet"))

getwd()



#read in field check sample data for BOB 2020 ########################################

# #read data tables and match var names
# 
# 
# 
# # df1 = read.csv("bob-buoy_field_samples_metadata_2020.csv",
# #                header=T, stringsAsFactors=F, sep=",")
# # 
# # df1$datetime <- as.POSIXct(paste(df1$Date,df1$UTC), format = "%m/%d/%Y %H:%M") 
# # 
# # df1$pH.check.median <- as.numeric(df1$pH.check.median)
# 
# 
# # head(df1)
# # 
# # Date   UTC Sample.. Cast_Sal Cast_Temp pH.check.median      pH.check1      pH.check2      pH.check3
# # 1  5/1/2020 20:43   B-0061 14.85028  15.86863        7.959483    7.949450246    7.971714823    7.959483096
# 
# # 
# # datetime
# # 1 2020-05-01 20:43:00
# 
# 
# chk.df <- df1
# rm(df1)
# 
# #clear workspace and reload tidied data
# save(chk.df, file = "bob-check.samples-2020.RData")
# 
# rm(list=ls())
# 
# load("bob-check.samples-2020.RData")
# 
# rm(list=ls())


#read in bay water bath check sample data for BOB 2020 ########################################

#read data tables and match var names
#READ IN MARI DATA TIDYING



# PRE deployment dickson standard run 2020 ############################################

# DATA WAS CAPTURED WITH ONBOARD SALINITY OF 35 SO NEEDS NO MANIPULATION OTHER THAN TIDYING

getwd()

setwd(here("data", "bob", "seafet", "pre-deploy.dickson.run-bob-july-2020"))

getwd()

# read data tables with matching var names
# included skip = 8 option for working with SeaFET individual files
# where first 8 columns contain metadata

# adapted from:
# https://www.r-bloggers.com/merging-multiple-data-files-into-one-data-frame/
# https://stackoverflow.com/questions/43858448/how-to-load-and-merge-multiple-csv-files-in-r

multMerge_skip8 = function(mypath){
  filenames = list.files(path = mypath, full.names = TRUE)
  datalist = lapply(filenames, 
                    function(x){read.csv(file = x,
                                         header = FALSE,
                                         stringsAsFactors = FALSE,
                                         skip = 8)})
  Reduce(function(x,y) {merge(x, y, all = TRUE)}, datalist)
}


df  <- multMerge_skip8(here("data", "bob", "seafet", "pre-deploy.dickson.run-bob-july-2020"))



#get var names by printing data
head(df)

# V1      V2        V3      V4      V5      V6  V7  V8  V9 V10        V11        V12       V13    V14
# 1 SATPHA0341 2020185 0.6672978 8.19789 8.12666 20.2416 NaN NaN NaN NaN -0.9253558 -0.8838120 0.8655131 12.795

# V15  V16   V17    V18   V19   V20 V21 V22 V23 V24 V25
# 1  20 12.5 4.922 12.668 6.127 6.268 290 179   0   0 111

df <- df %>%                     
  rename(Instrument = "V1",
         Date =  "V2",
         Time = "V3",
         PH_INT = "V4",
         PH_EXT = "V5",
         TEMP = "V6",
         CTD_TEMP = "V7",
         CTD_SALINITY = "V8",
         CTD_OXYGEN = "V9",
         CTD_PRESSURE ="V10",
         VOLT_INT = "V11",
         VOLT_EXT = "V12",
         VOLT_THERM = "V13",
         VOLT_SUPPLY = "V14",
         I_SUPPLY = "V15",
         HUMIDITY ="V16",
         VOLT_5 = "V17",
         VOLT_MBATT = "V18",
         VOLT_ISO = "V19",
         VOLT_ISO_BATT ="V20",
         I_B = "V21",
         I_K = "V22",
         V_K = "V23",
         STATUS = "V24",
         CHECK  = "V25")



df <- select(df, -Instrument, -CTD_TEMP, -CTD_SALINITY, -CTD_OXYGEN, -CTD_PRESSURE)

df <- df %>%                     
  rename(pH_int = 'PH_INT', 
         pH_ext = 'PH_EXT',
         pH_temp = 'TEMP',
         pH_int_v = 'VOLT_INT',
         pH_ext_v ='VOLT_EXT')


# convert seabird julian date and decimal time to R datetime object

#pull data set year

df.year <-  as.numeric(substr(df$Date[1], 1,4))

#parse out julian day
df$day <- as.numeric(substr(df$Date, 5,8))

# create origin of year
df.origin.yr <- paste0(df.year-1, "-12-31")

df.origin.yr <- as.Date(df.origin.yr)

print(df.origin.yr)

#add days passed to origin
df$date <-  df.origin.yr +df$day

df$hms <- times((df$Time /(24)))

#combine date and time to string
df$datetime <- paste0(df$date," ", df$hms)

# convert datetime to POSIXcT

df$datetime <- as.POSIXct(df$datetime, tz ="GMT") 

#round to nearest 20th minute to group each sampling event
# the setting on the seafet is to take 10 samples every 20 minutes

df$datetime.tag <- round_date(df$datetime, "20 minutes")

# remove datetime generation vars
df <- select(df, -day, -date, -hms)



#reorder and pull select vars

df <- select(df, datetime, pH_int, pH_ext, pH_temp, pH_int_v, pH_ext_v, datetime.tag)


#have data ascend in time

df <- df %>%
  arrange(datetime)


# calculate absolute pH and voltage differences for assessment
# we are looking for convergence in values

df$abs_pH_diff <- abs(df$pH_int - df$pH_ext)

df$abs_v_diff <- abs(df$pH_int_v - df$pH_ext_v)


df <- select(df, datetime,
              pH_int_v, pH_ext_v, abs_v_diff, pH_temp,
              pH_int, pH_ext, abs_pH_diff, everything())


df <- df %>%
  group_by(datetime.tag) %>%
  summarise(pH_int_v = median(pH_int_v),
            pH_ext_v = median(pH_ext_v),
            abs_v_diff = median(abs_v_diff),
            pH_temp = median(pH_temp), 
            pH_int =  median(pH_int), 
            pH_ext =  median(pH_ext), 
            abs_pH_diff =  median(abs_pH_diff))

df <- df %>%                     
  rename(datetime = "datetime.tag")


#### save BOB 2020 PRE deployment dickson std run #################################

here()

setwd(here("tidied-data", "bob", "seafet"))

# rm(df)
# 
# load("bob.dickson.run.predeploy.2020.sft.RData")

getwd()

save(df, file = "bob.dickson.run.predeploy.2020.sft.RData")

#rm(list=ls())

#load("bob.dickson.run.predeploy.2020.sft.RData")


# need to adjust later
# subsetting predeployment  dickson standard run to get median pH value from
# last 278 samples, or 4.6333 hours of run

# t3 <- '2020-03-08 20:00:00'
#df <- filter(df, datetime > t3)

#save(df, file = "mari-pre-deploy-dickson-run-prcsd-calibration-08mar2020.RData")

median.dickson.value.int <- median(df$pH_int)
print(median.dickson.value.int) 
# [1] 8.2572

median.dickson.value.ext <- median(df$pH_ext)
print(median.dickson.value.ext) 
#[1] 8.2162

median.temp.value <- median(df$pH_temp) 
print(median.temp.value) 
#[1] 18.59635

#rm(list=ls())



# PRE deployment bay water bath run 2020 ############################################

# use at your peril...

# rm(list=ls())

getwd()

setwd(here("data", "bob", "seafet", "pre-deploy.bath-bob-2020"))

getwd()


# get file names

list.files()

# [1] "bob-seafet-upload-20200820.CSV" "bob-seafet-upload-20200924.CSV"


instr.info <- read.csv("bob-seafet-upload-20200820.CSV",
                       header=F, nrows = 8,  stringsAsFactors=F, sep=",")

bob.2020.screen.df <- data.frame(k0_int.bob.2020.sbe = as.numeric(instr.info$V3[3]),
                                 k2_int.bob.2020.sbe = as.numeric(instr.info$V3[4]),
                                 k0_ext.bob.2020.sbe = as.numeric(instr.info$V3[5]),
                                 k2_ext.bob.2020.sbe = as.numeric(instr.info$V3[6]),
                                 int.v.min = -1.065,
                                 int.v.max =  -1.055,
                                 ext.v.min = -1.01,
                                 ext.v.max =  -0.99,
                                 stringsAsFactors = FALSE)

#clear workspace and reload tidied data
save(bob.2020.screen.df, file = "bob.2020.screen.RData")

rm(list=ls())

load("bob.2020.screen.RData")


###### pulling raw files #############################################

getwd()

setwd(here("data", "bob", "seafet", "pre-deploy.bath-bob-2020"))

getwd()


# get file names

list.files()

# [1] "bob-seafet-upload-20200820.CSV" "bob-seafet-upload-20200924.CSV"

#read data tables and match var names

#sft data
df1 = read.csv("bob-seafet-upload-20200820.CSV", 
               header=F, skip = 8, stringsAsFactors=F, sep=",")

df2 = read.csv("bob-seafet-upload-20200924.CSV", 
               header=F, skip = 8, stringsAsFactors=F, sep=",")

# #combine into one data frame

df <- bind_rows(df1,df2, .id= NULL)

rm(df1, df2)


df <- df %>%                     
  rename(Instrument = "V1",
         Date =  "V2",
         Time = "V3",
         PH_INT = "V4",
         PH_EXT = "V5",
         TEMP = "V6",
         CTD_TEMP = "V7",
         CTD_SALINITY = "V8",
         CTD_OXYGEN = "V9",
         CTD_PRESSURE ="V10",
         VOLT_INT = "V11",
         VOLT_EXT = "V12",
         VOLT_THERM = "V13",
         VOLT_SUPPLY = "V14",
         I_SUPPLY = "V15",
         HUMIDITY ="V16",
         VOLT_5 = "V17",
         VOLT_MBATT = "V18",
         VOLT_ISO = "V19",
         VOLT_ISO_BATT ="V20",
         I_B = "V21",
         I_K = "V22",
         V_K = "V23",
         STATUS = "V24",
         CHECK  = "V25")

df <- select(df, -Instrument, -CTD_TEMP, -CTD_SALINITY, -CTD_OXYGEN, -CTD_PRESSURE)

df <- df %>%                     
  rename(og_pH_int = 'PH_INT', 
         og_pH_ext = 'PH_EXT',
         pH_temp = 'TEMP',
         pH_int_v = 'VOLT_INT',
         pH_ext_v ='VOLT_EXT')


# convert seabird date and time to datetime object

#pull data set year

df.year <-  as.numeric(substr(df$Date[1], 1,4))

#parse out julian day
df$day <- as.numeric(substr(df$Date, 5,8))

# create origin of year
df.origin.yr <- paste0(df.year-1, "-12-31")

df.origin.yr <- as.Date(df.origin.yr)

print(df.origin.yr)

#add days passed to origin
df$date <-  df.origin.yr +df$day

df$hms <- times((df$Time /(24)))

#combine date and time to string
df$datetime <- paste0(df$date," ", df$hms)

# convert datetime to POSIXcT

df$datetime <- as.POSIXct(df$datetime, tz = "GMT") 

#round to nearest 20th minute

df$datetime.tag <- round_date(df$datetime, "20 minutes")

# remove datetime generation vars
df <- select(df, -day, -date, -hms)



#reorder and pull select vars

df <- select(df, datetime, og_pH_int, og_pH_ext, pH_temp, pH_int_v, pH_ext_v, datetime.tag)



# read in CTD raw data file ########################################

#rm(df4)

getwd()

setwd(here("data", "bob", "ctd", "pre-deploy.bath-bob-2020"))

getwd()

list.files()

# [1] "20200724-20200930-bob-ctd.txt"

# NEED TO CHECK WHICH COLUMNS ARE WHICH...

df4 = read.csv("20200724-20200930-bob-ctd.txt", header=F, stringsAsFactors=F, sep=",")

# pulled below from page 68 of SBE 16 manual with CTD Settings
# volt0 =1 (chl), volt1 = 1 (turb), SBE63 = Y (usec, temp_voltage), outputsal = y

df4 <- df4 %>% 
  rename(ctd_temp = "V1",
         ctd_cond =  "V2",
         ctd_press_db = "V3",
         ctd_chl_volts = "V4",
         ctd_turb_volts = "V5",
         ctd_o2_phase_usec = "V6",
         ctd_02_temp_volts = "V7",
         ctd_sal = "V8",
         datetime = "V9")


#pull only datetime, ctd_temp, ctd_sal

df4 <- select(df4, datetime, ctd_temp, ctd_sal)

# convert to POSIXcT

# format: "%d %b %Y %H:%M:%S"

df4$datetime <- as.POSIXct(df4$datetime, format = "%d %b %Y %H:%M:%S", tz = "GMT") 

df4$datetime.tag <- round_date(df4$datetime, "20 min")


# combine data files

df1 <- left_join(df, df4, by = "datetime.tag")

rm(df,df4)

#remove NA emtpy cell (NA) values (if needed)
#df1 <- df1 %>%
#  filter(ctd_temp != '')


getwd()

setwd(here("data", "bob", "seafet", "pre-deploy.bath-bob-2020"))

getwd()


save(df1, file = 'bob-pre.deploy.bath-2020-raw.RData')


rm(list=ls())



##### SeaFET v1 algorithm pH_Temp and CTD Salinity #######################################

#clear workspace and pull in tidied data

rm(list=ls())

load('bob-pre.deploy.bath-2020-raw.RData')

instr.info <- read.csv("bob-seafet-upload-20200820.CSV",
                       header=F, nrows = 8,  stringsAsFactors=F, sep=",")


# GOAL
# variables pH_int and pH_ext should be same as pH_ext_cell and as pH_int_cell at end
# WHEN WE FIX THE SALINITY TO BE 35

# data and calculations pulled from Seabird SeaFET Manual 2.0.2

#creating calibration calculation data frame
pH.calc.df <- data.frame(k0_int.bob.2020.sbe = as.numeric(instr.info$V3[3]),
                         k2_int.bob.2020.sbe = as.numeric(instr.info$V3[4]), 
                         k0_ext.bob.2020.sbe = as.numeric(instr.info$V3[5]),
                         k2_ext.bob.2020.sbe = as.numeric(instr.info$V3[6]),
                         stringsAsFactors = FALSE)


#for calculations to be accurate 8 digits are needed.
#using this command greatest digit count is 22, deafault is 7
options(digits = 8)

# set up seafetV1 processing algorithm bob deployment 2020 ###################

# defining constants for seafetV1 ################

# calibration constants specific to each instrument's calibration file
# usually in xml format before raw data or in a separate xml data file


k0_int <- pH.calc.df$k0_int.bob.2020.sbe 
k2_int <- pH.calc.df$k2_int.bob.2020.sbe

k0_ext <- pH.calc.df$k0_ext.bob.2020.sbe
k2_ext <- pH.calc.df$k2_ext.bob.2020.sbe

# data value calls

T_degC <- df1$pH_temp

T_k <- T_degC + 273.15 # TdegC converted to Kelvin

# for SeaFET V1 test or dickson standard run to specify 35
#S_psu <- 35  

#final algortihm test with mari.data
S_psu <- df1$ctd_sal

#theoretical constants

Rgas <- 8.314472 # R is the universal gas constant # units J /(K mol))

Frdy_c <- 96485.3415 # Faraday constant # units Coulombs/mole C/mol


# defining oceanographic constants

# expected nernstian slope response
Snernst = Rgas * T_k * log(10) / Frdy_c 

# Total chloride in seawater
Cl_T <- (0.99889/35.453) * (S_psu/1.80655) 


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

df1$pH_int_cell <- ((df1$pH_int_v - pH.calc.df$k0_int.bob.2020.sbe) - 
                      (pH.calc.df$k2_int.bob.2020.sbe * T_k)) / Snernst

df1$pH_int_cell <- round(df1$pH_int_cell, digits = 6)


# pH external caclulation ###########################

# electrode cell voltage junction to pH

df1$pH_ext_cell <- (((df1$pH_ext_v - pH.calc.df$k0_ext.bob.2020.sbe) - 
                       (pH.calc.df$k2_ext.bob.2020.sbe * T_k)) / Snernst) +
  log10(Cl_T) + (2 * log_HCl_ac) - log10(1+(Sulf8_T/K_sulf8)) 

df1$pH_ext_cell <- round(df1$pH_ext_cell, digits = 6)


# calculate difference between pH electrodes and voltages

df1$abs_og_pH_diff <- abs(df1$og_pH_int - df1$og_pH_ext)

df1$abs_pH_diff <- abs(df1$pH_int_cell - df1$pH_ext_cell)

df1$abs_v_diff <- abs(df1$pH_int_v - df1$pH_ext_v)


#reorder and pull select vars


df1 <- select(df1, datetime.tag,
              pH_int_v, pH_ext_v, abs_v_diff, pH_temp, ctd_sal, ctd_temp,
              pH_int_cell, pH_ext_cell, abs_pH_diff)


df1 <- rename(df1, datetime = datetime.tag,
              pH_int = pH_int_cell,
              pH_ext = pH_ext_cell)


df1 <- df1 %>%
  group_by(datetime) %>%
  summarise(pH_int_v = median(pH_int_v),
            pH_ext_v = median(pH_ext_v),
            abs_v_diff = median(abs_v_diff),
            pH_int =  median(pH_int), 
            pH_ext =  median(pH_ext), 
            abs_pH_diff =  median(abs_pH_diff),
            pH_temp = median(pH_temp),
            ctd_sal = median(ctd_sal),
            ctd_temp = median(ctd_temp))


getwd()

setwd(here("tidied-data", "bob", "seafet"))

getwd()



save(df1, file = 'bob-pre.deploy.bath-2020-prcsd.RData')

rm(list=ls())

load('bob-pre.deploy.bath-2020-prcsd.RData')

write.csv(df1, "bob-pre.deploy.bath-2020-prcsd.csv", row.names = F)


#flag out bad voltages

# data <- filter(data, pH_int_v > mari.2020.screen.df$int.v.min & pH_int_v < mari.2020.screen.df$int.v.max)
# 
# data <- filter(data, pH_ext_v > mari.2020.screen.df$ext.v.min & pH_ext_v < mari.2020.screen.df$ext.v.max)







# DEPLOYMENT instrument data tidying ####################################################

rm(list=ls())

load("bob.2020.screen.RData")

#clear workspace and reload tidied data



###### pulling raw files #############################################

#read data tables and match var names
df1 = read.csv("bob seafet raw/20200430-20200710_raw.csv",
               header=F, skip = 8, stringsAsFactors=F, sep=",")

#read data tables and match var names
df2 = read.csv("bob seafet raw/20200710-20200815_raw.csv",
               header=F, skip = 8, stringsAsFactors=F, sep=",")

#read data tables and match var names
df3 = read.csv("bob seafet raw/20200815-20201025_raw.csv",
               header=F, skip = 8, stringsAsFactors=F, sep=",")

#combine into one data frame

df <- bind_rows(df1,df2, df3, .id= NULL)

rm(df1, df2, df3)


df <- df %>%                     
  rename(Instrument = "V1",
         Date =  "V2",
         Time = "V3",
         PH_INT = "V4",
         PH_EXT = "V5",
         TEMP = "V6",
         CTD_TEMP = "V7",
         CTD_SALINITY = "V8",
         CTD_OXYGEN = "V9",
         CTD_PRESSURE ="V10",
         VOLT_INT = "V11",
         VOLT_EXT = "V12",
         VOLT_THERM = "V13",
         VOLT_SUPPLY = "V14",
         I_SUPPLY = "V15",
         HUMIDITY ="V16",
         VOLT_5 = "V17",
         VOLT_MBATT = "V18",
         VOLT_ISO = "V19",
         VOLT_ISO_BATT ="V20",
         I_B = "V21",
         I_K = "V22",
         V_K = "V23",
         STATUS = "V24",
         CHECK  = "V25")

df <- select(df, -Instrument, -CTD_TEMP, -CTD_SALINITY, -CTD_OXYGEN, -CTD_PRESSURE)

df <- df %>%                     
  rename(og_pH_int = 'PH_INT', 
         og_pH_ext = 'PH_EXT',
         pH_temp = 'TEMP',
         pH_int_v = 'VOLT_INT',
         pH_ext_v ='VOLT_EXT')


# convert seabird date and time to datetime object

#pull data set year

df.year <-  as.numeric(substr(df$Date[1], 1,4))

#parse out julian day
df$day <- as.numeric(substr(df$Date, 5,8))

# create origin of year
df.origin.yr <- paste0(df.year-1, "-12-31")

df.origin.yr <- as.Date(df.origin.yr)

print(df.origin.yr)

#add days passed to origin
df$date <-  df.origin.yr +df$day

df$hms <- times((df$Time /(24)))

#combine date and time to string
df$datetime <- paste0(df$date," ", df$hms)

# convert datetime to POSIXcT

df$datetime <- as.POSIXct(df$datetime) 

#round to nearest 20th minute

df$datetime.tag <- round_date(df$datetime, "20 minutes")

# remove datetime generation vars
df <- select(df, -day, -date, -hms)



#reorder and pull select vars

df <- select(df, datetime, og_pH_int, og_pH_ext, pH_temp, pH_int_v, pH_ext_v, datetime.tag)



# read in CTD raw data file ########################################

df1 = read.csv("bob ctd raw/20200501-20200710 sbe 16 T S data.csv",
               header=F, stringsAsFactors=F, sep=",")

df2 = read.csv("bob ctd raw/20200710-20200815 sbe16 T S data.csv",
               header=F, stringsAsFactors=F, sep=",")

df3 = read.csv("bob ctd raw/20200815-20201025 sbe16 T S data.csv",
               header=F, stringsAsFactors=F, sep=",")


df4 <- bind_rows(df1,df2, df3, .id= NULL)

rm(df1, df2, df3)

df4 <- df4 %>% 
  rename(datetime = "V1",
         ctd_temp =  "V2",
         ctd_sal = "V3")

# convert to POSIXcT

df4$datetime <- as.POSIXct(df4$datetime) 

df4$datetime.tag <- round_date(df4$datetime, "20 min")


# combine data files

df1 <- left_join(df, df4, by = "datetime.tag")

#remove NA emtpy cell (NA) values
df1 <- df1 %>%
  filter(ctd_temp != '')


save(df1, file = 'bob.2020.pH.T.S.combined.raw.RData')

rm(list=ls())



##### SeaFET v1 algorithm pH_Temp and CTD Salinity #######################################

#clear workspace and pull in tidied data

rm(list=ls())

load('bob.2020.pH.T.S.combined.raw.RData')

instr.info <- read.csv("bob seafet raw/20200430-20200710_raw.csv",
                       header=F, nrows = 8,  stringsAsFactors=F, sep=",")


# GOAL
# variables pH_int and pH_ext should be same as pH_ext_cell and as pH_int_cell at end
# WHEN WE FIX THE SALINITY TO BE 35

# data and calculations pulled from Seabird SeaFET Manual 2.0.2

#creating calibration calculation data frame
pH.calc.df <- data.frame(k0_int.bob.2020.sbe = as.numeric(instr.info$V3[3]),
                         k2_int.bob.2020.sbe = as.numeric(instr.info$V3[4]), 
                         k0_ext.bob.2020.sbe = as.numeric(instr.info$V3[5]),
                         k2_ext.bob.2020.sbe = as.numeric(instr.info$V3[6]),
                         stringsAsFactors = FALSE)


#for calculations to be accurate, greatest digit count is 22, deafault is 7
options(digits = 8)

# set up seafetV1 processing algorithm bob deployment 2020 ###################

# defining constants for seafetV1 ################

# calibration constants specific to each instrument's calibration file
# usually in xml format before raw data or in a separate xml data file


k0_int <- pH.calc.df$k0_int.bob.2020.sbe 
k2_int <- pH.calc.df$k2_int.bob.2020.sbe

k0_ext <- pH.calc.df$k0_ext.bob.2020.sbe
k2_ext <- pH.calc.df$k2_ext.bob.2020.sbe

# data value calls

T_degC <- df1$pH_temp

T_k <- T_degC + 273.15 # TdegC converted to Kelvin

# for SeaFET V1 test or dickson standard run to specify 35
#S_psu <- 35  

#final algortihm test with mari.data
S_psu <- df1$ctd_sal

#theoretical constants

Rgas <- 8.314472 # R is the universal gas constant # units J /(K mol))

Frdy_c <- 96485.3415 # Faraday constant # units Coulombs/mole C/mol


# defining oceanographic constants

# expected nernstian slope response
Snernst = Rgas * T_k * log(10) / Frdy_c 

# Total chloride in seawater
Cl_T <- (0.99889/35.453) * (S_psu/1.80655) 


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

df1$pH_int_cell <- ((df1$pH_int_v - pH.calc.df$k0_int.bob.2020.sbe) - 
                      (pH.calc.df$k2_int.bob.2020.sbe * T_k)) / Snernst

df1$pH_int_cell <- round(df1$pH_int_cell, digits = 6)


# pH external caclulation ###########################

# electrode cell voltage junction to pH

df1$pH_ext_cell <- (((df1$pH_ext_v - pH.calc.df$k0_ext.bob.2020.sbe) - 
                       (pH.calc.df$k2_ext.bob.2020.sbe * T_k)) / Snernst) +
  log10(Cl_T) + (2 * log_HCl_ac) - log10(1+(Sulf8_T/K_sulf8)) 

df1$pH_ext_cell <- round(df1$pH_ext_cell, digits = 6)


# calculate difference between pH electrodes and voltages

df1$abs_og_pH_diff <- abs(df1$og_pH_int - df1$og_pH_ext)

df1$abs_pH_diff <- abs(df1$pH_int_cell - df1$pH_ext_cell)

df1$abs_v_diff <- abs(df1$pH_int_v - df1$pH_ext_v)


#reorder and pull select vars

df1 <- select(df1, datetime.x, datetime.y, datetime.tag,
              pH_int_v, pH_ext_v, abs_v_diff, pH_temp, ctd_sal,
              pH_int_cell, pH_ext_cell, abs_pH_diff)


save(df1, file = 'bob.seafet.pro.2020.RData')

rm(list=ls())

load('bob.seafet.pro.2020.RData')

write.csv(df1, "20200502_20201024_bob_seafet_prcsd.csv", row.names = F)


#flag out bad voltages

# data <- filter(data, pH_int_v > mari.2020.screen.df$int.v.min & pH_int_v < mari.2020.screen.df$int.v.max)
# 
# data <- filter(data, pH_ext_v > mari.2020.screen.df$ext.v.min & pH_ext_v < mari.2020.screen.df$ext.v.max)


# ADJUSTED DEPLOYMENT DATA BASED ON CALIBRATION K0 (E*) VALUES #########



# NO MID deployment BOB common bath run 2020 ############################################


# POST deployment common bath run 2020 ############################################

rm(list=ls())


load("bob.2020.screen.RData")


###### pulling raw files #############################################

#read data tables and match var names

#sft data
df = read.csv("bob seafet raw/post-deployment.bob-2020/bob-seafet-post.deploy.bath-20201025-20201107.csv", 
              header=F, skip = 8, stringsAsFactors=F, sep=",")



# #combine into one data frame
# 
# df <- bind_rows(df1,df2, df3, .id= NULL)
# 
# rm(df1, df2, df3)


df <- df %>%                     
  rename(Instrument = "V1",
         Date =  "V2",
         Time = "V3",
         PH_INT = "V4",
         PH_EXT = "V5",
         TEMP = "V6",
         CTD_TEMP = "V7",
         CTD_SALINITY = "V8",
         CTD_OXYGEN = "V9",
         CTD_PRESSURE ="V10",
         VOLT_INT = "V11",
         VOLT_EXT = "V12",
         VOLT_THERM = "V13",
         VOLT_SUPPLY = "V14",
         I_SUPPLY = "V15",
         HUMIDITY ="V16",
         VOLT_5 = "V17",
         VOLT_MBATT = "V18",
         VOLT_ISO = "V19",
         VOLT_ISO_BATT ="V20",
         I_B = "V21",
         I_K = "V22",
         V_K = "V23",
         STATUS = "V24",
         CHECK  = "V25")

df <- select(df, -Instrument, -CTD_TEMP, -CTD_SALINITY, -CTD_OXYGEN, -CTD_PRESSURE)

df <- df %>%                     
  rename(og_pH_int = 'PH_INT', 
         og_pH_ext = 'PH_EXT',
         pH_temp = 'TEMP',
         pH_int_v = 'VOLT_INT',
         pH_ext_v ='VOLT_EXT')


# convert seabird date and time to datetime object

#pull data set year

df.year <-  as.numeric(substr(df$Date[1], 1,4))

#parse out julian day
df$day <- as.numeric(substr(df$Date, 5,8))

# create origin of year
df.origin.yr <- paste0(df.year-1, "-12-31")

df.origin.yr <- as.Date(df.origin.yr)

print(df.origin.yr)

#add days passed to origin
df$date <-  df.origin.yr +df$day

df$hms <- times((df$Time /(24)))

#combine date and time to string
df$datetime <- paste0(df$date," ", df$hms)

# convert datetime to POSIXcT

df$datetime <- as.POSIXct(df$datetime) 

#round to nearest 20th minute

df$datetime.tag <- round_date(df$datetime, "20 minutes")

# remove datetime generation vars
df <- select(df, -day, -date, -hms)



#reorder and pull select vars

df <- select(df, datetime, og_pH_int, og_pH_ext, pH_temp, pH_int_v, pH_ext_v, datetime.tag)



# read in CTD raw data file ######################################################

# ctd data pulled from MARI SeapHOx in same bath. Partial file.
# if you want the full bath file track down the SBE16 data

df4 = read.csv("bob ctd raw/post.deployment.ctd.raw-2020/mari-post-deployment.common.bath-20201030-20201106_T_S_only.csv",
               header=F, stringsAsFactors=F, sep=",")


df4 <- df4 %>% 
  rename(datetime = "V1",
         ctd_temp =  "V2",
         ctd_sal = "V3")

# convert to POSIXcT

df4$datetime <- as.POSIXct(df4$datetime) 

df4$datetime.tag <- round_date(df4$datetime, "20 min")


# combine data files

df1 <- left_join(df, df4, by = "datetime.tag")

rm(df,df4)

#remove NA emtpy cell (NA) values
df1 <- df1 %>%
  filter(ctd_temp != '')


save(df1, file = 'bob-post.deploy.bath-2020-raw.RData')


rm(list=ls())



##### SeaFET v1 algorithm pH_Temp and CTD Salinity #######################################

#clear workspace and pull in tidied data

rm(list=ls())

load('bob-post.deploy.bath-2020-raw.RData')

instr.info <- read.csv("bob seafet raw/20200430-20200710_raw.csv",
                       header=F, nrows = 8,  stringsAsFactors=F, sep=",")


# data and calculations pulled from Seabird SeaFET Manual 2.0.2

#creating calibration calculation data frame
pH.calc.df <- data.frame(k0_int.bob.2020.sbe = as.numeric(instr.info$V3[3]),
                         k2_int.bob.2020.sbe = as.numeric(instr.info$V3[4]), 
                         k0_ext.bob.2020.sbe = as.numeric(instr.info$V3[5]),
                         k2_ext.bob.2020.sbe = as.numeric(instr.info$V3[6]),
                         stringsAsFactors = FALSE)


#for calculations to be accurate, greatest digit count is 22, deafault is 7
options(digits = 8)

# set up seafetV1 processing algorithm bob post deployment bath 2020 ###################

# defining constants for seafetV1 ################

# calibration constants specific to each instrument's calibration file
# usually in xml format before raw data or in a separate xml data file


k0_int <- pH.calc.df$k0_int.bob.2020.sbe 
k2_int <- pH.calc.df$k2_int.bob.2020.sbe

k0_ext <- pH.calc.df$k0_ext.bob.2020.sbe
k2_ext <- pH.calc.df$k2_ext.bob.2020.sbe

# data value calls

T_degC <- df1$pH_temp

T_k <- T_degC + 273.15 # TdegC converted to Kelvin

# for SeaFET V1 test or dickson standard run to specify 35
#S_psu <- 35  

#final algortihm test with mari.data
S_psu <- df1$ctd_sal

#theoretical constants

Rgas <- 8.314472 # R is the universal gas constant # units J /(K mol))

Frdy_c <- 96485.3415 # Faraday constant # units Coulombs/mole C/mol


# defining oceanographic constants

# expected nernstian slope response
Snernst = Rgas * T_k * log(10) / Frdy_c 

# Total chloride in seawater
Cl_T <- (0.99889/35.453) * (S_psu/1.80655) 


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

df1$pH_int_cell <- ((df1$pH_int_v - pH.calc.df$k0_int.bob.2020.sbe) - 
                      (pH.calc.df$k2_int.bob.2020.sbe * T_k)) / Snernst

df1$pH_int_cell <- round(df1$pH_int_cell, digits = 6)


# pH external caclulation ###########################

# electrode cell voltage junction to pH

df1$pH_ext_cell <- (((df1$pH_ext_v - pH.calc.df$k0_ext.bob.2020.sbe) - 
                       (pH.calc.df$k2_ext.bob.2020.sbe * T_k)) / Snernst) +
  log10(Cl_T) + (2 * log_HCl_ac) - log10(1+(Sulf8_T/K_sulf8)) 

df1$pH_ext_cell <- round(df1$pH_ext_cell, digits = 6)


# calculate difference between pH electrodes and voltages

df1$abs_og_pH_diff <- abs(df1$og_pH_int - df1$og_pH_ext)

df1$abs_pH_diff <- abs(df1$pH_int_cell - df1$pH_ext_cell)

df1$abs_v_diff <- abs(df1$pH_int_v - df1$pH_ext_v)


#reorder and pull select vars

df1 <- select(df1, datetime.x, datetime.y, datetime.tag,
              pH_int_v, pH_ext_v, abs_v_diff, pH_temp, ctd_sal,
              pH_int_cell, pH_ext_cell, abs_pH_diff)


save(df1, file = 'bob-post.deploy.bath-2020-prcsd.RData')

rm(list=ls())

load('bob-pre.deploy.bath-2020-prcsd.RData')

write.csv(df1, "bob-post.deploy.bath-2020-prcsd.csv", row.names = F)




# POST deployment dickson standard run 2020 ############################################

# DATA WAS CAPTURED WITH ONBOARD SALINITY OF 35 SO NEEDS NO MANIPULATION OTHER THAN TIDYING

#read data tables and match var names

#sft data
df = read.csv("bob seafet raw/post-deployment.bob-2020/bob-dickson.std.run-20201107-20201112.csv", 
              header=F, skip = 8, stringsAsFactors=F, sep=",")



# #combine into one data frame
# 
# df <- bind_rows(df1,df2, df3, .id= NULL)
# 
# rm(df1, df2, df3)


df <- df %>%                     
  rename(Instrument = "V1",
         Date =  "V2",
         Time = "V3",
         PH_INT = "V4",
         PH_EXT = "V5",
         TEMP = "V6",
         CTD_TEMP = "V7",
         CTD_SALINITY = "V8",
         CTD_OXYGEN = "V9",
         CTD_PRESSURE ="V10",
         VOLT_INT = "V11",
         VOLT_EXT = "V12",
         VOLT_THERM = "V13",
         VOLT_SUPPLY = "V14",
         I_SUPPLY = "V15",
         HUMIDITY ="V16",
         VOLT_5 = "V17",
         VOLT_MBATT = "V18",
         VOLT_ISO = "V19",
         VOLT_ISO_BATT ="V20",
         I_B = "V21",
         I_K = "V22",
         V_K = "V23",
         STATUS = "V24",
         CHECK  = "V25")

df <- select(df, -Instrument, -CTD_TEMP, -CTD_SALINITY, -CTD_OXYGEN, -CTD_PRESSURE)

df <- df %>%                     
  rename(pH_int = 'PH_INT', 
         pH_ext = 'PH_EXT',
         pH_temp = 'TEMP',
         pH_int_v = 'VOLT_INT',
         pH_ext_v ='VOLT_EXT')


# convert seabird date and time to datetime object

#pull data set year

df.year <-  as.numeric(substr(df$Date[1], 1,4))

#parse out julian day
df$day <- as.numeric(substr(df$Date, 5,8))

# create origin of year
df.origin.yr <- paste0(df.year-1, "-12-31")

df.origin.yr <- as.Date(df.origin.yr)

print(df.origin.yr)

#add days passed to origin
df$date <-  df.origin.yr +df$day

df$hms <- times((df$Time /(24)))

#combine date and time to string
df$datetime <- paste0(df$date," ", df$hms)

# convert datetime to POSIXcT

df$datetime <- as.POSIXct(df$datetime) 

#round to nearest 20th minute

df$datetime.tag <- round_date(df$datetime, "20 minutes")

# remove datetime generation vars
df <- select(df, -day, -date, -hms)



#reorder and pull select vars

df1 <- select(df, datetime, pH_int, pH_ext, pH_temp, pH_int_v, pH_ext_v, datetime.tag)

rm(df)

df1$abs_pH_diff <- abs(df1$pH_int - df1$pH_ext)

df1$abs_v_diff <- abs(df1$pH_int_v - df1$pH_ext_v)


df1 <- select(df1, datetime,
              pH_int_v, pH_ext_v, abs_v_diff, pH_temp,
              pH_int, pH_ext, abs_pH_diff, everything())


#saving final test proof data for seafetV2 translator script
save(df1, file = "bob.dickson.run-post-deploy.2020.raw.RData")

rm(list=ls())



load("bob.dickson.run-post-deploy.2020.raw.RData")



# # save BOB 2020 POST deployment dickson std run #################################

#clear workspace and reload tidied data
save(df1, file = "bob-post-deploy-dickson-run-prcsd-2020.RData")

rm(list=ls())

load("bob-post-deploy-dickson-run-prcsd-2020.RData")

# need to adjust later
# subsetting predeployment  dickson standard run to get median pH value from
# last 278 samples, or 4.6333 hours of run

t3 <- '2020-03-08 20:00:00'
#df1 <- filter(df1, datetime > t3)

#save(df1, file = "mari-pre-deploy-dickson-run-prcsd-calibration-08mar2020.RData")

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



#### calibrating 2020 BOB data with _______________________ #################################

rm(list = ls())

load("mari-pre-deploy-dickson-run-prcsd-calibration-08mar2020.RData")

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



