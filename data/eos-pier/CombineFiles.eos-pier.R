library(dplyr)
library(lubridate)
library(here)


#combining data from text files

setwd(here())

getwd()

setwd(here("data", "eos-pier"))

getwd()


rm(list = ls())



# ====================================================================================================================================
#   Date     Time  Temp SpCond   Cond  Resist   TDS    Sal   Press   Depth    pH      pH   Chl   Chl Turbid+ ODOsat    ODO Battery
# y/m/d hh:mm:ss     C  uS/cm  uS/cm  Ohm*cm   g/L    ppt    psia  meters            mV  ug/L   RFU     NTU      %   mg/L   volts
# ------------------------------------------------------------------------------------------------------------------------------------


list.files()


#assign file names

#June 2021 Files

file1 <- "YSI_RTC_20201124-20201218.csv"
file2 <- "YSI_RTC_20201218-20201219.csv"    
file3 <- "YSI_RTC_20201219-20210130.csv"


# file4 <- "YSI_RTC_20200207-20200317.csv"   
# file5 <- "YSI_RTC_20200317-20200403.csv"   
# file6 <- "YSI_RTC_20200403-20200515.csv"    
# file7 <- "YSI_RTC_20200515.csv" 

#December 2021 Files

# file7.1 <- "YSI_RTC_20200515-20200619.csv"
# file8 <-  "YSI_RTC_20200620-20200723.csv"   
# file9 <-  "YSI_RTC_20200723-20200820.csv"   
# file10 <-  "YSI_RTC_20200820-20200923.csv"   
# file11 <- "YSI_RTC_20200924-20201015.csv"   
# file12<-  "YSI_RTC_20201016-20201118.csv"   
# file13 <-  "YSI_RTC_20201118-20201123.csv"   
# file14 <-  "YSI_RTC_20201124-20201218.csv"   
# file15 <-  "YSI_RTC_20201218-20201219.csv"   
 

# header for a text file from EcoWatch
# header <- read.table(file2, as.is = T, skip = 1, nrows = 1, header = F)
# header$V12 <- "pH.1"
# header$V14 <- "Chl.1"


# header for a .csv file from EcoWatch Lite
# not sure if this is needed anymore
#header <- read.table(file1, as.is = T, sep = ",", nrows = 1, header = F)


#read data files and match var names ######################################


# # assigns columns names pulled from header "file"
# colnames(df1) <- header[1,]

df1 = read.csv(file1,
               header=T, stringsAsFactors=F, sep=",")
df1 <- df1[-1,]



df2 = read.csv(file2,
               header=T, stringsAsFactors=F, sep=",")
df2 <- df2[-1,]


# # use this option if you want to read a text file
# # option as.is = T keeps the strings as strings and not factors
# df2 <- read.table(file2, as.is = T, skip = 5, header = F)
# 
# # assigns columns names pulled from header "file"
# colnames(df2) <- header[1,]


# still need to read in .csv files
# need to replace all cma with rtc
# need to rewrite read.csv below with "file2.... filen"


df3 = read.csv(file3,
               header=T, stringsAsFactors=F, sep=",")
df3 <- df3[-1,]

# add in as files come in
# 
# df4 = read.csv(file4,
#                header=T, stringsAsFactors=F, sep=",")
# df4 <- df4[-1,]
# 
# 
# df5 = read.csv(file5,
#                header=T, stringsAsFactors=F, sep=",")
# df5 <- df5[-1,]
# 
# 
# df6 = read.csv(file6,
#                header=T, stringsAsFactors=F, sep=",")
# df6 <- df6[-1,]
# 
# 
# df7 = read.csv(file7,
#                header=T, stringsAsFactors=F, sep=",")
# df7 <- df7[-1,]
# 
# 
# df7.1 = read.csv(file7.1,
#                header=T, stringsAsFactors=F, sep=",")
# df7.1 <- df7.1[-1,]
# 
# 
# 
# df8 = read.csv(file8,
#                header=T, stringsAsFactors=F, sep=",")
# df8 <- df8[-1,]
# 
# 
# df9 = read.csv(file9,
#                header=T, stringsAsFactors=F, sep=",")
# df9 <- df9[-1,]
# 
# 
# df10 = read.csv(file10,
#                header=T, stringsAsFactors=F, sep=",")
# df10 <- df10[-1,]
# 
# 
# df11 = read.csv(file11,
#                header=T, stringsAsFactors=F, sep=",")
# df11 <- df11[-1,]
# 
# 
# df12 = read.csv(file12,
#                header=T, stringsAsFactors=F, sep=",")
# df12 <- df12[-1,]
# 
# 
# df13 = read.csv(file13,
#                header=T, stringsAsFactors=F, sep=",")
# df13 <- df13[-1,]
# 
# 
# df14 = read.csv(file14,
#                header=T, stringsAsFactors=F, sep=",")
# df14 <- df14[-1,]
# 
# 
# df15 = read.csv(file15,
#                header=T, stringsAsFactors=F, sep=",")
# df15 <- df15[-1,]
# 
# 


# df <- bind_rows(df1, df3, df4, df5, df6, df7,
#                 df7.1, df8, df9, df10, df11, df12,
#                 df13, df14, df15, .id= NULL)


df <- bind_rows(df1, df2, df3, .id= NULL)



df <- df %>%                     
  rename(sst = Temp, 
         sss = Sal,
         o2_mg_l = ODO,
         o2_sat = ODOsat,
         pH_mv = pH.1,
         chl_ugl = Chl,
         chl_rfu = Chl.1,
         Turb = Turbid.)


# df2 <- df2 %>%                     
#   rename(sst = Temp, 
#          sss = Sal,
#          o2_mg_l = ODO,
#          o2_sat = ODOsat,
#          pH_mv = pH.1,
#          chl_ugl = Chl,
#          chl_rfu = Chl.1,
#          Turb = "Turbid+")




df$sst <- as.numeric(df$sst)
df$SpCond <- as.numeric(df$SpCond)
df$Cond <- as.numeric(df$Cond)
df$Resist <- as.numeric(df$Resist)
df$TDS <- as.numeric(df$TDS)
df$sss <- as.numeric(df$sss)
df$Press <- as.numeric(df$Press)
df$Depth <- as.numeric(df$Depth)
df$pH <- as.numeric(df$pH)
df$pH_mv <- as.numeric(df$pH_mv)
df$chl_ugl <- as.numeric(df$chl_ugl)
df$chl_rfu <- as.numeric(df$chl_rfu)
df$Turb <- as.numeric(df$Turb)
df$o2_sat <- as.numeric(df$o2_sat)
df$o2_mg_l <- as.numeric(df$o2_mg_l)
df$Battery <- as.numeric(df$Battery)


# df <- bind_rows(df, df2, .id= NULL)

str(df)


rm(list=setdiff(ls(), c("df")))



df <- df %>%
  mutate(datetime =  paste(df$Date,df$Time))

df$datetime <- as.POSIXct(df$datetime, format = "%Y/%m/%d %H:%M:%S", tz = "GMT")

str(df)

df <- df %>%
  arrange(datetime)


eos_pier_2021 <- select(df, datetime, everything())


#save(eos_pier_2020, file = "EOS_YSI_20191201-20200515.RData")

save(eos_pier_2021, file = "EOS_YSI_20191124-20210130.RData")


rm(list = ls())

load(file = "EOS_YSI_20191124-20210130.RData")

str(eos_pier_2021)

rm(list = ls())



