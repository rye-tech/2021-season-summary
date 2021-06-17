library(tidyverse)
library(lubridate)
library(here)
library(janitor)

#combining data from text files

setwd(here())

getwd()

setwd(here("data", "cma-pier"))

getwd()


# ====================================================================================================================================
#   Date     Time  Temp SpCond   Cond  Resist   TDS    Sal   Press   Depth    pH      pH   Chl   Chl Turbid+ ODOsat    ODO Battery
# y/m/d hh:mm:ss     C  uS/cm  uS/cm  Ohm*cm   g/L    ppt    psia  meters            mV  ug/L   RFU     NTU      %   mg/L   volts
# ------------------------------------------------------------------------------------------------------------------------------------


list.files(pattern = ".csv")

# [1] "CMA_YSI_20201114-20201215.csv"
# [2] "CMA_YSI_20201216-20201219.csv"
# [3] "CMA_YSI_20201219-20210113.csv"
# [4] "CMA_YSI_20210113-20210203.csv"
# [5] "CMA_YSI_20210203-20210215.csv"
# [6] "CMA_YSI_20210215-20210322.csv"
# [7] "CMA_YSI_20210322-20210512.csv"



list.files(path = here("data", "cma-pier", "rt"), pattern = ".txt")
# [1] "20210512-20210614-cma_realtime_raw.txt"
# [2] "cma_ysi_rt_header.txt" 


file1 <- "CMA_YSI_20201114-20201215.csv"  
file2 <- "CMA_YSI_20201216-20201219.csv"  
file3 <- "CMA_YSI_20201219-20210113.csv"  
file4 <- "CMA_YSI_20210113-20210203.csv"  


file5 <- "CMA_YSI_20210203-20210215.csv"
file6 <- "CMA_YSI_20210215-20210322.csv"
file7 <- "CMA_YSI_20210322-20210512.csv"
file8 <- here("data", "cma-pier", "rt",
              "20210512-20210614-cma_realtime_raw.txt")
file9 <- here("data", "cma-pier", "rt",
              "cma_ysi_rt_header.txt")

# add in as you add files

# file10 <- "CMA_YSI_20200714-20200820.csv"   
# file11 <- "CMA_YSI_20200821-20200923.csv"   
# file12 <- "CMA_YSI_20200924-20201014.csv"   
# file13 <- "CMA_YSI_20201015-20201102.csv"   
# file14 <- "CMA_YSI_20201114-20201215.csv"   
# file15 <- "CMA_YSI_20201216-20201219.csv"   


#### header troubleshooting if needed ####

# header for a text file from EcoWatch or Realtime on Box (same thing)
# header <- read.table(file2, as.is = T, skip = 1, nrows = 1, header = F)
# header$V12 <- "pH.1"
# header$V14 <- "Chl.1"


# header for a .csv file from EcoWatch Lite
# not sure if this is needed anymore
# header <- read.table(file1, as.is = T, sep = ",", nrows = 1, header = F)


# # assigns columns names pulled from header "file"
# colnames(df1) <- header[1,]


# # reading in the real time header
# var_rt <- read.table(file8, as.is = T, skip = 1, nrows = 1, header = F)
# units_rt <- read.table(file8, as.is = T, skip = 2, nrows = 1, header = F)
# 
# units_rt <- units_rt %>%
#   mutate(V18 = NA, .after = "V10")
# 
# header_rt <- paste0(var_rt, "_", units_rt)




#### archived .csv file header ####

var_arch <- read.table(file1, as.is = T, sep = ",", 
                       nrows = 1, header = F)

units_arch <- read.table(file1, as.is = T, sep = ",",
                         skip = 1, nrows = 1, header = F)

header_arch <- paste0(var_arch, "_", units_arch)

header_arch <- make_clean_names(header_arch)

print(header_arch)



#### real time header #####


# reading in the real time header
var_rt <- read.table(file9, as.is = T, skip = 1, nrows = 1, header = F)

units_rt <- read.table(file9, as.is = T, skip = 2, nrows = 1, header = F)

units_rt <- units_rt %>%
  mutate(V10.1 = NA, .after = "V10")

header_rt <- paste0(var_rt, "_", units_rt)

header_rt <- make_clean_names(header_rt)

print(header_rt)

header_rt[4] <- "sp_cond_u_s"
header_rt[5] <- "cond_u_s"
header_rt[9] <- "press_psir"


print(header_rt)


#### read data files and match var names #################################

# Notes on file formats:

# # use this option if you want to read a text file
# df2 <- read.table(file2, as.is = T, skip = 5, header = F)
# # option as.is = T keeps the strings as strings and not factors

# # if you need to 
# # assigns columns names pulled from header "file"
# colnames(df2) <- header[1,]



df1 = read.csv(file1,
               header=T, stringsAsFactors=F, sep=",")

#drop units row
df1 <- df1[-1,]

# # assigns columns names pulled from header "file"
colnames(df1) <- header_arch

# convert all character to numeric
df1 <- df1 %>% 
  mutate(across(where(is.character) & !c(date_y_m_d, time_hh_mm_ss), as.numeric))


#repeat for remaining data frames

df2 = read.csv(file2,
               header=T, stringsAsFactors=F, sep=",")
df2 <- df2[-1,]
colnames(df2) <- header_arch

# convert all character to numeric
df2 <- df2 %>% 
  mutate(across(where(is.character) & !c(date_y_m_d, time_hh_mm_ss), as.numeric))



df3 = read.csv(file3,
               header=T, stringsAsFactors=F, sep=",")
df3 <- df3[-1,]
colnames(df3) <- header_arch

# convert all character to numeric
df3 <- df3 %>% 
  mutate(across(where(is.character) & !c(date_y_m_d, time_hh_mm_ss), as.numeric))



df4 = read.csv(file4,
               header=T, stringsAsFactors=F, sep=",")
df4 <- df4[-1,]
colnames(df4) <- header_arch

# convert all character to numeric
df4 <- df4 %>% 
  mutate(across(where(is.character) & !c(date_y_m_d, time_hh_mm_ss), as.numeric))



df5 = read.csv(file5,
               header=T, stringsAsFactors=F, sep=",")
df5 <- df5[-1,]
colnames(df5) <- header_arch

# convert all character to numeric
df5 <- df5 %>% 
  mutate(across(where(is.character) & !c(date_y_m_d, time_hh_mm_ss), as.numeric))



df6 = read.csv(file6,
               header=T, stringsAsFactors=F, sep=",")
df6 <- df6[-1,]
colnames(df6) <- header_arch

# convert all character to numeric
df6 <- df6 %>% 
  mutate(across(where(is.character) & !c(date_y_m_d, time_hh_mm_ss), as.numeric))



df7 = read.csv(file7,
               header=T, stringsAsFactors=F, sep=",")
df7 <- df7[-1,]
colnames(df7) <- header_arch

# convert all character to numeric
df7 <- df7 %>% 
  mutate(across(where(is.character) & !c(date_y_m_d, time_hh_mm_ss), as.numeric))



df8 <- read.table(file8, as.is = T, header = F)
# option as.is = T keeps the strings as strings and not factors

# # assigns columns names pulled from header "file"
colnames(df8) <- header_rt

# convert all character to numeric
df8 <- df8 %>% 
  mutate(across(where(is.character) & !c(date_y_m_d, time_hh_mm_ss), as.numeric))





df <- bind_rows(df1, df2, df3, df4, .id= NULL)



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





str(df)



cma_2021 <- select(df, datetime, everything())

rm(df)

save(cma_2021, file = "CMA_YSI_20191114-20210203.RData")

rm(list=ls())

load(file = "CMA_YSI_20191114-20210203.RData")

str(cma_2021)



# write.csv(cma_2020, file = "CMA_YSI_20191108-20200710.csv", row.names = F)

rm(cma_2021)





#### SCRAP ############################################




# # I switched up my strategy so dropping this here. 
# # The date time adjustment I did could still be useful
# 
# df1 = read.csv(file1,
#                header=T, stringsAsFactors=F, sep=",")
# df1 <- df1[-1,]
# 
# 
# df1$Date <- mdy(df1$Date)
# 
# df1$Date <- as.character(df1$Date)
# 
# df1$Date <- gsub("-", "/", df1$Date)
# 
# 
# df2 = read.csv(file2,
#                header=T, stringsAsFactors=F, sep=",")
# df2 <- df2[-1,]
# 
# 
# df3 = read.csv(file3,
#                header=T, stringsAsFactors=F, sep=",")
# df3 <- df3[-1,]
# 
# 
# df4 = read.csv(file4,
#                header=T, stringsAsFactors=F, sep=",")
# df4 <- df4[-1,]
# 
# 
# # add in files as they come
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

# doesnt work. may need grep?
# bind_list <- paste0("df", c(1:15))
# 
# df <- bind_rows(bind_list, .id= NULL)


# df <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8, df9,
#                 df10, df11, df12, df15, .id= NULL)




# # I do this in the data frame calls instead now instead of all
# # at once
# 
# df$sst <- as.numeric(df$sst)
# df$SpCond <- as.numeric(df$SpCond)
# df$Cond <- as.numeric(df$Cond)
# df$Resist <- as.numeric(df$Resist)
# df$TDS <- as.numeric(df$TDS)
# df$sss <- as.numeric(df$sss)
# df$Press <- as.numeric(df$Press)
# df$Depth <- as.numeric(df$Depth)
# df$pH <- as.numeric(df$pH)
# df$pH_mv <- as.numeric(df$pH_mv)
# df$chl_ugl <- as.numeric(df$chl_ugl)
# df$chl_rfu <- as.numeric(df$chl_rfu)
# df$Turb <- as.numeric(df$Turb)
# df$o2_sat <- as.numeric(df$o2_sat)
# df$o2_mg_l <- as.numeric(df$o2_mg_l)
# df$Battery <- as.numeric(df$Battery)





