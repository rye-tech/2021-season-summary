
#set work environment ------------------------------------------------------

library(dplyr)
library(stringr)
library(here)

setwd(here())

getwd()

setwd(here("check sample work", "TA processing 20210507"))

getwd()

rm(list=ls())


list.files()

#   sample information file
#  "alk run 07apr2021 samples only.csv"  
#  "alk run 27apr2021 samples only.csv" 

#   sample environmental data
# "20210407 sample metadata.csv"
# "20210427 sample metadata.csv"

# alkalinity titration data file
# "alk run 07apr2021 data only.csv" 
# "alk run 27apr2021 data only.csv"


# read in sample summary information ####-----------------------------------------------------

df_sum = read.csv("alk run 27apr2021 samples only.csv",
                  header=T, stringsAsFactors=F, sep=",")

df_sum_rm_rows <- 1:9

df_sum <- df_sum[-df_sum_rm_rows,]


df_sum <- select(df_sum,  X,
                X.5)


df_sum <- df_sum %>% 
  filter(X != '') 


df_sum$X.5 <- str_remove(df_sum$X.5, "g")

df_sum <- df_sum %>%                     
  rename(sample = 'X', 
         weight ='X.5')


df_sum$weight <- as.numeric(df_sum$weight) 

#read in environmental and electrode data ------------------------------------------------


df_env = read.csv("20210427 sample metadata.csv",
                  header=T, stringsAsFactors=F, sep=",")

df_env <- df_env %>%                     
  rename(S = 'salinity', 
         weight ='sample.mass')


# read in sample data ####-------------------------------------------------------------------

# NOTE: needed to manually align these data so they could be processed by this script 

df = read.csv("alk run 27apr2021 data only.csv",
               header=F, skip = 16, stringsAsFactors=F, sep=",")



# below selects variables of interest
# if throws error "unused arguments" restart R session with ctrl + Shift + F10, and reload library(dplyr)

# worked with shifted files

df <- select(df,  V3,
                  V7,
                  V13)

#worked with formatting of csv on my personal computer

# df <- select(df,  V4, 
#              V9, 
#              V14)

#pulled this nifty fix to remove empty cells from each row. 
#removes one value of temperature in first titrations but doesn't matter
# https://markhneedham.com/blog/2015/06/02/r-dplyr-removing-empty-rows/

df <- df %>%
  filter(V3 != '') %>%
  filter(V7 != '') %>%
  filter(V13 != '')


# df <- df %>% 
#   filter(V4 != '') %>%
#   filter(V9 != '') %>%
#   filter(V14 != '')


df <- df %>%
  rename(volume = 'V3',
         E ='V7',
         temperature = 'V13')


# df <- df %>%                     
#   rename(volume = 'V4', 
#          E ='V9',
#          temperature = 'V14')

#point to each titration
titration_list <- df[c(df$volume == "V"),]

#get pointers in list
titration_indices <-  rownames(titration_list)

#get pointers to units of measure from data
drop_units_list <- as.numeric(titration_indices) + 1

#drop units of measure from data
df <- df[-drop_units_list,]

rownames(df) <- NULL

#point to each titration
titration_list <- df[c(df$volume == "V"),]

#get pointers in list
titration_indices <-  rownames(titration_list)


#get last pointer
#last(df$V3)

# pulls the last name of the last row
#rownames(tail(df,1))


#add last pointer to titration indices
titration_indices <- c(titration_indices, rownames(tail(df,1)))


#print(titration_indices)


#get length of sequence for loop
len_loop <- length(titration_indices)

len_alt_loop <- seq(2, length(titration_indices), by = 2)

#get sample names as list and paired

sample_list <- df_sum$sample

sample_list <- c(rep(sample_list, each = 2))

sample_list2 <- sample_list

each_titration <- c("titration1", "titration2")

sample_list <- paste(sample_list,each_titration, sep = "-")


#list1 = ls()


#print(sample_list)
#print(sample_list[1])
#[1] "ALKCRM1"




#test of functionality to pull data
# 
# titration_data <-  slice(df, titration_indices[1]:titration_indices[2])
# 
# titration_data <- slice(titration_data, 1:(n()-1)) #removes last row
# 
# assign(sample_list[1], titration_data)

#env_data <- filter(df_env, sample.id == regex(sample_list2[1], ignore_case = T))
#env_data <- filter(df_env, sample.id == sample_list2[3])

#print(sample_list2[1])
#print(sample_list2)



#loop to pull data --------------------------------------------------------------
# IGNORE WARNING AFTER RUNNING

for (i in 1:len_loop) {
  
  titration_data <-  slice(df, titration_indices[i]:titration_indices[i+1])
  titration_data <- titration_data[-1,] #removes unwanted header row that was needed to point the slice step above
  titration_data <- slice(titration_data, 1:(n()-1)) #removes last unwanted row
  titration_data$E <- as.numeric(titration_data$E)
  titration_data$volume <- as.numeric(titration_data$volume)
  titration_data$temperature <- as.numeric(titration_data$temperature)
  titration_data <- mutate(titration_data, sample.id = sample_list2[i])
  assign(sample_list[i], titration_data)

}


#loop to add volume values from first titration to second titration ----

#point to object in R
# print(sample_list[2])
# titration_data <- get(sample_list[2])


for (i in len_alt_loop) {
  
  titration_data <- get(sample_list[i])
  titration1_volume <- get(sample_list[i-1])
  titration1_volume <- as.numeric(max(titration1_volume$volume))
  titration_data <- mutate(titration_data, volume = volume + titration1_volume)
  assign(sample_list[i], titration_data)
  
}




#loop to add metadata -------------------

for (i in len_alt_loop) {
  
  titration_data <- get(sample_list[i])
  titration_data <- left_join(titration_data, df_env, by = "sample.id")
  assign(sample_list[i], titration_data)
  
}



#loop to sort to gran titration pH bounds ----------------------------

for (i in len_alt_loop) {
  
  titration_data <- get(sample_list[i])
  mV_lwr_bound <- titration_data$EHigh
  mV_upr_bound <- titration_data$ELow
  titration_data <- filter(titration_data, E >= mV_lwr_bound & E <= mV_upr_bound)
  assign(sample_list2[i], titration_data)
  
}


#removes everything except for wanted data
rm(list=setdiff(ls(), sample_list2))


#save all data frames in global environment as csv files
#pulled code from: https://stackoverflow.com/questions/48707198/write-data-frames-in-environment-into-separate-csv-files

files <- mget(ls())

for (i in 1:length(files)){
  write.csv(files[[i]], paste(names(files[i]), ".csv", sep = ""), row.names = F)
}








