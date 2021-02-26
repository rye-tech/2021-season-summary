
#combining data from text files

#big laptop
setwd("C:/Users/Ryan/Desktop/2019 season summary/data/fort-pt")

#for work computer

#setwd("C:/Users/915712257/Box Sync/Inbox/oceanographic work/2019 season summary/data/fort-pt")

#for Ryan commuter computer

#setwd("C:/Users/rrkad/Box Sync/Inbox/oceanographic work/2019 season summary/data/fort-pt")

rm(list = ls())

library(dplyr)
library(lubridate)

list.files()

# "fort-point_3f18_3878_4f82_SSS.csv"
# "fort-point_5536_d855_a740_SST.csv"


####salinity ####

#read data tables and match var names
df1 = read.csv("fort-point_3f18_3878_4f82_SSS.csv",
               header=T, stringsAsFactors=F, sep=",")

#gets rid of the first row of units below the header of each column
df1 <- df1[-1,] 

#get var names by printing data
head(df1)
tail(df1)

# 
#time sea_water_practical_salinity sea_water_practical_salinity_qc_agg3

#the following code takes the time stamp from ERDDAP and converts it to a string 
# that can be used to plot data later on

df1 <- df1 %>% 
  mutate(datetime = paste0(substr(time, 1, 4),"-", substr(time, 6, 7), "-", 
                           substr(time, 9, 10), " ", substr(time,12,19))) 

#removes the original time stamp
df1 <- df1[,-1]

#removes qc flag
df1 <- df1[,-2]

df1 <- df1 %>%                     
  rename(sss = 'sea_water_practical_salinity')


df1$datetime <- as.POSIXct(df1$datetime, format = "%Y-%m-%d %H:%M:%S")

df1 <- filter(df1, datetime > "2019-03-01 00:03:00")

df1$tag <- as.character(df1$datetime)

#confirm sss was numeric
class(df1$sss)

################### temperature ####

#read data tables and match var names
df2 = read.csv("fort-point_5536_d855_a740_SST.csv",
               header=T, stringsAsFactors=F, sep=",")

#gets rid of the first row of units below the header of each column
df2 <- df2[-1,] 

#get var names by printing data
head(df2)
tail(df2)
# 
#  time sea_water_temperature sea_water_temperature_qc_agg

#the following code takes the time stamp from ERDDAP and converts it to a string 
# that can be used to plot data later on

df2 <- df2 %>% 
  mutate(datetime = paste0(substr(time, 1, 4),"-", substr(time, 6, 7), "-", 
                           substr(time, 9, 10), " ", substr(time,12,19))) 

#removes the original time stamp
df2 <- df2[,-1]

#removes qc flag
df2 <- df2[,-2]

df2 <- df2 %>%                     
  rename(sst = 'sea_water_temperature')

#confirm sst is numeric
class(df2$sst)
#was character so changed to numeric

df2$sst <- as.numeric(df2$sst)


df2$datetime <- as.POSIXct(df2$datetime, format = "%Y-%m-%d %H:%M:%S")

df2 <- filter(df2, datetime > "2019-03-01 00:03:00")

df2$tag <- as.character(df2$datetime)

#### combining data ####

#combine into one data frame

data <- left_join(df1,df2, by = "tag")

tail(data)

#created tag instead

#had to do this time formatting after the join. Formatting before screwed up the join and added a bunch of N/A values to the data set, because join works by matching character vectors, NOT POSIXcT

#data$datetime <- as.POSIXct(data$datetime, format = "%Y-%m-%d %H:%M:%S")

#have data ascend in time

data <- select(data, -datetime.y, 
                   -tag)

data <- data %>%                     
  rename(datetime = 'datetime.x')


data <- data %>%
  arrange(datetime)


head(data)
tail(data)


#clear workspace and reload tidied data
save(data, file = "ftpt.2019.RData")

rm(list=ls())

load("ftpt.2019.RData")









#############SCRAP##########################
# #reading in the text file, as.is = T keeps the strings as strings and not factors
# df1 <- read.table("20190311-20190521 mari flat.txt", as.is = T, header = F)
# 
# #df1 missing pH mV data so binding by name of vars
# df1 <- df1 %>%                     
#   rename(date = 'V1', 
#          time ='V2', 
#          temp_degC = 'V3', 
#          SpCond = 'V4', 
#          Cond = 'V5',
#          Resist = 'V6',
#          TDS = 'V7',
#          Sal = 'V8', 
#          Press = 'V9', 
#          Depth = 'V10', 
#          pH = 'V11', 
#          Turbid = 'V12',
#          Chl_ugL = 'V13',
#          Chl_rfu = 'V14', 
#          ODOsat = 'V15',
#          ODO = 'V16', 
#          Battery = 'V17')
# 
# 
# 
# 
# #reading in the text file, as.is = T keeps the strings as strings and not factors
# df2 <- read.table("YSI_RTC_20190502-20190621-CENCOOS BACKUP SONDE time fixed.txt", as.is = T, header = F)
# 
# 
# 
# #reading in the text file, as.is = T keeps the strings as strings and not factors
# df3 <- read.table("YSI_RTC_20190621-20190808.txt", as.is = T, header = F)
# 
# 
# 
# #reading in the text file, as.is = T keeps the strings as strings and not factors
# df4 <- read.table("YSI_RTC_20190813-20190919.txt", as.is = T, header = F)
# 
# 
# #reading in the text file, as.is = T keeps the strings as strings and not factors
# df5 <- read.table("YSI_RTC_20190920-20191001.txt", as.is = T, header = F)
# 
# 
# #reading in the text file, as.is = T keeps the strings as strings and not factors
# df6 <- read.table("YSI_RTC_20191001-20191114.txt", as.is = T, header = F)
# 
# 
# 
# 
# df <- bind_rows(df2, df3, df4, df5, df6, .id= NULL)
# 
# 
# df <- df %>%                     
#   rename(date = 'V1', 
#          time ='V2', 
#          temp_degC = 'V3', 
#          SpCond = 'V4', 
#          Cond = 'V5',
#          Resist = 'V6',
#          TDS = 'V7',
#          Sal = 'V8', 
#          Press = 'V9', 
#          Depth = 'V10', 
#          pH = 'V11', 
#          pH_mV = 'V12',
#          Chl_ugL = 'V13',
#          Chl_rfu = 'V14', 
#          Turbid = 'V15',
#          ODOsat = 'V16', 
#          ODO = 'V17', 
#          Battery ='V18')
# 
# df <- bind_rows(df1, df, .id= NULL)
# 
# df <- df %>%
#   mutate(datetime =  paste(df$date,df$time))
# 
# df$datetime <- as.POSIXct(df$datetime, format = "%Y/%m/%d %H:%M:%S")
# 
# df <- df %>%
#   arrange(datetime)
# 
# eos_2019 <- select(df, -date, 
#          -time, 
#          -SpCond, 
#          -Cond,
#          -Resist,
#          -TDS, 
#          -Press, 
#          -Depth, 
#          -pH_mV,
#          -Chl_ugL,
#          -Chl_rfu, 
#          -Turbid,
#          -ODOsat, 
#          -Battery)
# 
# 
# save(eos_2019, file = "EOS_YSI_20190322-20191114.RData")
# 
# rm(list=ls())
# 
# load(file = "EOS_YSI_20190322-20191114.RData")
# 
# 
# # 
# # #writes a text file to current directory, with options to exclude row and column numbers and quotations 
# # write.table(df, "CMA_YSI_20190323-20191107.txt", sep = ",", row.names = F, col.names =T, quote =F)
# # 
# # 
# # 
# # #reading in the text file, as.is = T keeps the strings as strings and not factors
# # dfcheck <- read.table("CMA_YSI_20190323-20191107.txt", sep =",", as.is = T, header = F)
# # 
# 
# 
# 
# 
