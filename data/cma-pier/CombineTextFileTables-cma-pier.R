library(dplyr)
library(lubridate)
library(here)


#combining data from text files

setwd(here())

getwd()

setwd(here("data", "cma-pier"))

getwd()


rm(list = ls())

# ====================================================================================================================================
#   Date     Time  Temp SpCond   Cond  Resist   TDS    Sal   Press   Depth    pH      pH   Chl   Chl Turbid+ ODOsat    ODO Battery
# y/m/d hh:mm:ss     C  uS/cm  uS/cm  Ohm*cm   g/L    ppt    psia  meters            mV  ug/L   RFU     NTU      %   mg/L   volts
# ------------------------------------------------------------------------------------------------------------------------------------


list.files()

file1 <- "CMA_YSI_20191108-20191210.csv"   
file2 <- "CMA_YSI_20191210-20200103.csv"   
file3 <- "CMA_YSI_20200103-2020117.csv"    
file4 <- "CMA_YSI_20200117-20200206.csv"   
file5 <- "CMA_YSI_20200117.csv"            
file6 <- "CMA_YSI_20200206-20200221.csv"   
file7 <- "CMA_YSI_20200221-20200402.csv"   
file8 <- "CMA_YSI_20200403-20200514.csv"   
file9 <- "CMA_YSI_20200514-20200710.csv"   
file10 <- "CMA_YSI_20200714-20200820.csv"   
file11 <- "CMA_YSI_20200821-20200923.csv"   
file12 <- "CMA_YSI_20200924-20201014.csv"   
file13 <- "CMA_YSI_20201015-20201102.csv"   
file14 <- "CMA_YSI_20201114-20201215.csv"   
file15 <- "CMA_YSI_20201216-20201219.csv"   

#read data files and match var names ######################################

df1 = read.csv(file1,
               header=T, stringsAsFactors=F, sep=",")
df1 <- df1[-1,]


df2 = read.csv(file2,
               header=T, stringsAsFactors=F, sep=",")
df2 <- df2[-1,]


df3 = read.csv(file3,
               header=T, stringsAsFactors=F, sep=",")
df3 <- df3[-1,]


df4 = read.csv(file4,
               header=T, stringsAsFactors=F, sep=",")
df4 <- df4[-1,]


df5 = read.csv(file5,
               header=T, stringsAsFactors=F, sep=",")
df5 <- df5[-1,]


df6 = read.csv(file6,
               header=T, stringsAsFactors=F, sep=",")
df6 <- df6[-1,]


df7 = read.csv(file7,
               header=T, stringsAsFactors=F, sep=",")
df7 <- df7[-1,]


df8 = read.csv(file8,
               header=T, stringsAsFactors=F, sep=",")
df8 <- df8[-1,]


df9 = read.csv(file9,
               header=T, stringsAsFactors=F, sep=",")
df9 <- df9[-1,]


df10 = read.csv(file10,
               header=T, stringsAsFactors=F, sep=",")
df10 <- df10[-1,]


df11 = read.csv(file11,
               header=T, stringsAsFactors=F, sep=",")
df11 <- df11[-1,]


df12 = read.csv(file12,
               header=T, stringsAsFactors=F, sep=",")
df12 <- df12[-1,]


df13 = read.csv(file13,
               header=T, stringsAsFactors=F, sep=",")
df13 <- df13[-1,]


df14 = read.csv(file14,
               header=T, stringsAsFactors=F, sep=",")
df14 <- df14[-1,]


df15 = read.csv(file15,
               header=T, stringsAsFactors=F, sep=",")
df15 <- df15[-1,]

# doesnt work. may need grep?
# bind_list <- paste0("df", c(1:15))
# 
# df <- bind_rows(bind_list, .id= NULL)

df <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8, df9,
                df10, df11, df12, df15, .id= NULL)

df16 <- bind_rows(df13, df14, .id= NULL)

df16$Date <- mdy(df16$Date)

df16$Date <- as.character(df16$Date)

df16$Date <- gsub("-", "/", df16$Date)

df <- bind_rows(df, df16, .id = NULL)


rm(list=setdiff(ls(), c("df")))


df <- df %>%                     
  rename(sst = Temp, 
         sss = Sal,
         o2_mg_l = ODO,
         o2_sat = ODOsat,
         pH_mv = pH.1,
         chl_ugl = Chl,
         chl_rfu = Chl.1,
         Turb = Turbid.)



df <- df %>%
  mutate(datetime =  paste(df$Date,df$Time))

str(df)

df$datetime <- as.POSIXct(df$datetime, format = "%Y/%m/%d %H:%M:%S", tz = "GMT")



df <- df %>%
  arrange(datetime)


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


str(df)



cma_2020 <- select(df, datetime, everything())

rm(df)

save(cma_2020, file = "CMA_YSI_20191108-20201219.RData")

rm(list=ls())

load(file = "CMA_YSI_20191108-20201219.RData")

str(cma_2020)



# write.csv(cma_2020, file = "CMA_YSI_20191108-20200710.csv", row.names = F)

rm(cma_2020)

