library(tidyverse)
library(lubridate)
library(here)


#combining data from text files

setwd(here())

getwd()

setwd(here("data", "eos-met"))

getwd()


# ====================================================================================================================================
#   Date     Time  Temp SpCond   Cond  Resist   TDS    Sal   Press   Depth    pH      pH   Chl   Chl Turbid+ ODOsat    ODO Battery
# y/m/d hh:mm:ss     C  uS/cm  uS/cm  Ohm*cm   g/L    ppt    psia  meters            mV  ug/L   RFU     NTU      %   mg/L   volts
# ------------------------------------------------------------------------------------------------------------------------------------


list.files(pattern = ".txt")

# [2] "RTC_MET_20200902-20201219.txt"           
# [3] "RTC_MET_20201219-20210226.txt"           
# [4] "RTC_MET_20210226-20210519.txt"   


list.files(path = here("data", "eos-met", "rt"), pattern = ".txt")

# "20210519-20210615-real_time_met_raw.txt"


file1 <- "RTC_MET_20200902-20201219.txt"  
file2 <- "RTC_MET_20201219-20210226.txt"


file3 <- "RTC_MET_20210226-20210519.txt" 
file4 <- here("data", "eos-met", "rt",
              "20210519-20210615-real_time_met_raw.txt")
# file5 <- "RTC_MET_20200813-20200902.txt"  
# file6 <- "RTC_MET_20200902-20201219.txt" 


header <- read.table(file1, as.is = T, skip = 1, 
                     nrows = 1, header = F, sep = ",")

#### read data by ERDDAP -----------------------------
# 
# #read data tables and match var names
# df1 = read.csv("EOS_YSI_20190601-20191201.csv",
#                header=T, stringsAsFactors=F, sep=",")
# 
# 
# #gets rid of the first row of units below the header of each column
# df1 <- df1[-1,] 
# 
# 
# 
# #the following determine what data type each variable is
# class(df1$temperature) 
# class(df1$salinity)
# class(df1$ph)
# class(df1$chl)
# class(df1$odo)
# class(df1$turbid)
# class(df1$odosat)
# 
# 
# # I found downloading and importing data as is from ERDDAP into R meant the environmental 
# # data was all read in as characters instead of numbers
# # the following code changes each parameter back to a number
# 
# df1$temperature <- as.numeric(df1$temperature)
# df1$salinity <- as.numeric(df1$salinity)
# df1$ph <- as.numeric(df1$ph)
# df1$chl <- as.numeric(df1$chl)
# df1$odo <- as.numeric(df1$odo)
# df1$turbid <- as.numeric(df1$turbid)
# df1$odosat <- as.numeric(df1$odosat)
# 
# 
# #creating time periods for subsetting using plyr package
# 
# 
# df1 <- df1 %>% 
#   mutate(date = substr(time, 1, 10) , 
#          mon = substr(time, 6, 7) , 
#          day = substr(time, 9, 10),  
#          year = substr(time, 1, 4)) 
# 
# 
# df1 <- df1 %>% 
#   mutate(datetime = paste0(substr(time, 1, 4),"-", substr(time, 6, 7), "-", 
#                            substr(time, 9, 10), " ", substr(time,12,19))) 
# 
# #converts time to a readable format for R to use in plotting
# 
# df1$datetime <- as.POSIXct(df1$datetime, format = "%Y-%m-%d %H:%M:%S")
# 
# class(df1$datetime) # see that the class of this data is now POSIXct
# 
# df1 <- select(df1, -time, -odosat, -pressure) # gets rid of the original time stamp for neatness
# 
# 
# save(df1, file = "EOS_YSI_20190601-20191201.RData")
# 
# rm(list=ls())
# 
# load(file = "EOS_YSI_20190601-20191201.RData")
# 
# 
# 
# #OR

#### #read text files -----------------------------------

# option sep is separator, as.is = T keeps the strings as strings and not factors
df1 <- read.table(file1, sep = ",", as.is = T, skip = 5, header = F)

# assigns columns names pulled from header "file"
colnames(df1) <- header[1,]

str(df1)


df2 <- read.table(file2, sep = ",", as.is = T, skip = 5, header = F)

# assigns columns names pulled from header "file"
colnames(df2) <- header[1,]

str(df2)



df3 <- read.table(file3, sep = ",", as.is = T, skip = 5, header = F)

# assigns columns names pulled from header "file"
colnames(df3) <- header[1,]

str(df3)


df4 <- read.table(file4, sep = ",", as.is = T, skip = 5, header = F)

# assigns columns names pulled from header "file"
colnames(df4) <- header[1,]

str(df4)

# # comment in as more files are added
#
# df5 <- read.table(file5, sep = ",", as.is = T, skip = 5, header = F)
# 
# # assigns columns names pulled from header "file"
# colnames(df5) <- header[1,]
# 
# str(df5)
# 
# 
# df6 <- read.table(file6, sep = ",", as.is = T, skip = 5, header = F)
# 
# # assigns columns names pulled from header "file"
# colnames(df6) <- header[1,]
# 
# str(df6)
#
## uncomment here



# getting variable names
# head(df1)
# 
# V1         V2       V3       V4       V5   V6          V7             V8         V9
# TIMESTAMP  RECORD   BattV    AirTC    RH   BP_mmHg     WS_ms          WindDir    PAR_Den
# TS         RN       Volts    Deg C    %    mmHg        meters/second  Degrees    umol/s/m^2
# 
# V10       V11     V12     V13
# PAR_Tot   SlrkW   SlrMJ   Rain_mm
# mmol/m^2  kW/m^2  MJ/m^2  mm



#df1 missing pH mV data so binding by name of vars
# df1 <- df1 %>%                     
#   rename(datetime = 'V1', 
#          record ='V2', 
#          batt = 'V3', 
#          airtemp = 'V4', 
#          rh = 'V5',
#          bp = 'V6',
#          ws = 'V7',
#          wdir = 'V8', 
#          par_dens = 'V9', 
#          par_tot = 'V10', 
#          sol_irr_kw = 'V11', 
#          sol_irr_mj = 'V12',
#          rain = 'V13')
# 

# df <- bind_rows(df1, df2, df3, df4, df5, df6, .id= NULL)

df <- bind_rows(df1, df2, df3, df4, .id= NULL)

df$datetime <- ymd_hms(df$TIMESTAMP, tz = "UTC")

str(df)
str(df$datetime)

df$date <- date(df$datetime)

#not needed with above line
#df1$datetime <- as.POSIXct(df1$datetime, format = "%Y/%m/%d %H:%M:%S")

df <- df %>%
  arrange(datetime)

# 
# library(magrittr)
# #change all character columns to numeric
# df1 %<>% mutate_if(is.character,as.numeric)

eos_met_2021 <- select(df, datetime, date, TIMESTAMP, everything())

rm(df)


save(eos_met_2021, file = "EOS_MET_20200902-20210615.RData")

rm(eos_met_2021)

load(file = "EOS_MET_20200902-20210615.RData")


write.csv(eos_met_2021, file = "EOS_MET_20200902-20210615.csv", row.names = F)

rm(eos_met_2021)

