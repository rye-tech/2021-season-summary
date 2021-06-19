


##### CeNCOOS Data Tidy Helper ####------------
#Cut and past this piece of code with you.
## Search and replace site names or params in code.

#for search and replacing
# colnames(data)
# [1] "date"     "time"     "sst"      "sss"      "dep"      "ph"       "chl"      "trb"      "o2"
# [10] "datetime"

# Save as copy after all looks good and runs up times or whatevs analysis

#### DATA TOOL END ######--------

# Metadata ---------------
site <-  c("bob", "mari", "EOS-pier", "EOS-Met" , "CMA-pier")
intr <-  c("MapCO2Sys + sbe-seafet + sbe-ctd16", "ysi-6600V4-sonde")
param <- c("ph + c,t,d,odo,chl + sea-xco2,atm-xco2", "c,t,d,odo,")

# add tech("int-isfet/ext-mosfet", "etc") ? phys("surface/EOS shore 12m contour?",
# "20m-depth/EOS-20m contour) conditions/location?



#### Metadata end  ######--------






#### load libraries, set working directory, point to files ####

library(tidyverse)
library(lubridate)
library(here)
library(janitor)

#combining data from text files

setwd(here())

getwd()

setwd(here("data", "cma-pier"))

getwd()



list.files(pattern = ".csv")
# [1] "CMA_YSI_20201114-20201215.csv" "CMA_YSI_20201114-20210615.csv"
# [3] "CMA_YSI_20201216-20201219.csv" "CMA_YSI_20201219-20210113.csv"
# [5] "CMA_YSI_20210113-20210203.csv" "CMA_YSI_20210203-20210215.csv"
# [7] "CMA_YSI_20210215-20210322.csv" "CMA_YSI_20210322-20210512.csv"

list.files(path = here("data", "cma-pier", "rt"), pattern = ".txt")
# [1] "20210512-20210614-cma_realtime_raw.txt" "cma_ysi_rt_header.txt"  

#assign file names

#June 2021 Files


hdr_file <- here("data", "cma-pier", "rt",
                 "cma_ysi_rt_header.txt")


file1 <- "CMA_YSI_20201114-20201215.csv"  
file2 <- "CMA_YSI_20201216-20201219.csv"  
file3 <- "CMA_YSI_20201219-20210113.csv"  
file4 <- "CMA_YSI_20210113-20210203.csv"  


file5 <- "CMA_YSI_20210203-20210215.csv"
file6 <- "CMA_YSI_20210215-20210322.csv"
file7 <- "CMA_YSI_20210322-20210512.csv"
file8 <- here("data", "cma-pier", "rt",
              "20210512-20210614-cma_realtime_raw.txt")

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
# 
# var_rt <- var_rt %>%
#   mutate(V12 = "pH.1", V14 = "Chl.1", V15 = "Turbid.")
# 
# header_rt <- paste0(var_rt)
# 
# print(header_rt)




#### real time header #####


# reading in the real time header
var_rt <- read.table(hdr_file, as.is = T, skip = 1, nrows = 1, header = F)

var_rt <- var_rt %>%
  mutate(V12 = "pH.1", V14 = "Chl.1", V15 = "Turbid.")

header_rt <- paste0(var_rt)

print(header_rt)


##### read data files and match var names ##################################

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

#for this data frame only fix the date wtf why

df1$Date <- mdy(df1$Date)

df1$Date <- as.character(df1$Date)

df1$Date <- gsub("-", "/", df1$Date)

# convert all character to numeric
df1 <- df1 %>% 
  mutate(across(where(is.character) & !c(Date, Time), as.numeric))


#repeat for remaining data frames

df2 = read.csv(file2,
               header=T, stringsAsFactors=F, sep=",")
df2 <- df2[-1,]

df2 <- df2 %>% 
  mutate(across(where(is.character) & !c(Date, Time), as.numeric))



df3 = read.csv(file3,
               header=T, stringsAsFactors=F, sep=",")
df3 <- df3[-1,]

df3 <- df3 %>% 
  mutate(across(where(is.character) & !c(Date, Time), as.numeric))



df4 = read.csv(file4,
               header=T, stringsAsFactors=F, sep=",")
df4 <- df4[-1,]

df4 <- df4 %>% 
  mutate(across(where(is.character) & !c(Date, Time), as.numeric))



df5 = read.csv(file5,
               header=T, stringsAsFactors=F, sep=",")

df5 <- df5[-1,]

df5 <- df5 %>% 
  mutate(across(where(is.character) & !c(Date, Time), as.numeric))



df6 = read.csv(file6,
               header=T, stringsAsFactors=F, sep=",")

df6 <- df6[-1,]

df6 <- df6 %>% 
  mutate(across(where(is.character) & !c(Date, Time), as.numeric))



df7 = read.csv(file7,
               header=T, stringsAsFactors=F, sep=",")

df7 <- df7[-1,]

df7 <- df7 %>% 
  mutate(across(where(is.character) & !c(Date, Time), as.numeric))


# real time data has a different strategy 
# since it did not come with headers in the text file

df8 <- read.table(file8, as.is = T, header = F)
# option as.is = T keeps the strings as strings and not factors

# # assigns columns names pulled from header "file"
colnames(df8) <- header_rt

# convert all character to numeric
df8 <- df8 %>% 
  mutate(across(where(is.character) & !c(Date, Time), as.numeric))


# add df in as files come in --------------------------------
# NOTE DOUBLE CHECK FORMAT MATCHES WORKING CODE
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
# end xtr df code ---------------------------


# below helps me to see where mismatches happen
# when combining data frames
# ignore numeric and integer mismatch

compare_df_cols(df1, df2, df3,
                df4, df5, df6, df7, 
                return = "mismatch")

# THIS GOOD
# [1] column_name df1         df2         df3        
# [5] df4         df5         df6         df7        
# <0 rows> (or 0-length row.names)


#bind all df together

df <- bind_rows(df1, df2, df3, df4,
                df5, df6, df7, .id= NULL)


# dub check headrs good
names(df)


#new cleaner names
df <- df %>%
  rename(sst = Temp,
         sss = Sal,
         o2_mg_l = ODO,
         o2_sat = ODOsat,
         pH_mv = pH.1,
         chl_ugl = Chl,
         chl_rfu = Chl.1,
         Turb = "Turbid.")


# dub check headrs good
names(df)

# check data structure
str(df)


# create datetime object time anchor in UTC/GMT

df <- df %>%
  mutate(datetime =  paste(df$Date, df$Time))

df$datetime <- as.POSIXct(df$datetime, format = "%Y/%m/%d %H:%M:%S", tz = "GMT")

#check that datetime is indeed POSIXct
str(df)

#if POSIXct, arrange ascending order by time
df <- df %>%
  arrange(datetime)


#check for anything weird like NA's
summary(df$datetime) 

#good data 
# Min.               1st Qu. 
# "2020-11-14 02:37:47" "2020-12-29 00:49:30" 
# Median                  Mean 
# "2021-02-11 22:55:29" "2021-02-11 23:34:26" 
# 3rd Qu.                  Max. 
# "2021-03-29 00:43:28" "2021-05-12 19:31:28" 

#Example of bad data
# Min.               1st Qu. 
# "2020-12-16 00:31:29" "2021-01-21 23:43:29" 
# Median                  Mean 
# "2021-02-27 23:01:29" "2021-02-27 22:45:36" 
# 3rd Qu.                  Max. 
# "2021-04-05 23:31:28" "2021-05-12 19:31:28" 
# NA's 
#                "7632" 



# remove duplicate rows with dplyr
df <- df %>% 
  # Base the removal on the "datetime" column
  distinct(datetime, .keep_all = TRUE)


cma_2021 <- select(df, datetime, everything())


save(cma_2021, file = "CMA_YSI_20201114-20210615.RData")

rm(cma_2021)

#restart R to confirm if data saved

load(file = "CMA_YSI_20201114-20210615.RData")

str(cma_2021)


write.csv(cma_2021, file = "CMA_YSI_20201114-20210615.csv", row.names = F)

rm(cma_2021)



#### SCRAP ####


# #Reverting Header Issue because my original approach worked better
# #for the files that were processed in Ecowatch
# 
# archived .csv file header 
# 
# var_arch <- read.table(file1, as.is = T, sep = ",", 
#                        nrows = 1, header = F)
# 
# units_arch <- read.table(file1, as.is = T, sep = ",",
#                          skip = 1, nrows = 1, header = F)
# 
# header_arch <- paste0(var_arch, "_", units_arch)
# 
# header_arch <- make_clean_names(header_arch)
# 
# print(header_arch)
# 
# 
# 
# 
# df1 = read.csv(file1,
#                header=T, stringsAsFactors=F, sep=",")
# 
# #drop units row
# df1 <- df1[-1,]
# 
# # # assigns columns names pulled from header "file"
# colnames(df1) <- header_arch
# 
# 


# this worked but adjusting to match the other data in the set.

# 
# # real time header #
# 
# 
# # reading in the real time header
# var_rt <- read.table(file8, as.is = T, skip = 1, nrows = 1, header = F)
# 
# units_rt <- read.table(file8, as.is = T, skip = 2, nrows = 1, header = F)
# 
# units_rt <- units_rt %>%
#   mutate(V10.1 = NA, .after = "V10")
# 
# header_rt <- paste0(var_rt, "_", units_rt)
# 
# header_rt <- make_clean_names(header_rt)
# 
# print(header_rt)
# 
# header_rt[4] <- "sp_cond_u_s"
# header_rt[5] <- "cond_u_s"
# header_rt[9] <- "press_psir"
# 
# 
# print(header_rt)
# 
# 

# # these names worked when I used an overcomplex strategy 
# df <- df %>%                     
#   rename(sst = temp_c, 
#          sss = sal_ppt,
#          o2_mg_l = odo_mg_l,
#          o2_sat = od_osat_percent,
#          pH_mv = p_h_m_v,
#          pH = p_h_na,
#          chl_ugl = chl_ug_l,
#          chl_rfu = chl_rfu,
#          Turb = turbid_ntu)




# df2 <- df2 %>%                     
#   rename(sst = Temp, 
#          sss = Sal,
#          o2_mg_l = ODO,
#          o2_sat = ODOsat,
#          pH_mv = pH.1,
#          chl_ugl = Chl,
#          chl_rfu = Chl.1,
#          Turb = "Turbid+")




# not needed anymore... handle this after calling in each file
# so I could get bind_rows to work

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


# df <- bind_rows(df, df2, .id= NULL)



# went a different route trying to create real time header

# var_rt <- read.table(file8, as.is = T, skip = 1, nrows = 1, header = F)
# units_rt <- read.table(file8, as.is = T, skip = 2, nrows = 1, header = F)
# 
# units_rt <- units_rt %>%
#   mutate(V18 = NA, .after = "V10")
# 
# header_rt <- paste0(var_rt, "_", units_rt)


