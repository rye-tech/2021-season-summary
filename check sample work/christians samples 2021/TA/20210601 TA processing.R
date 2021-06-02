
#alkalinity processing script updated 2020-02-11

#processing data from alkalinity method test on Jan 27th 2019

#CRM info ####

#goal for CRM from batch 162:
# S = 33.312
# TA 2403.72? 0.55 umol?kg-1

#goal for CRM from batch 130 from Stillman:
# S = 33.661
# TA 2238.04? 0.53 umol?kg-1


#goal for CRM from batch 186:
# S = 33.525
# TA 2212 +- 0.53 umol/kg


library(seacarb)
library(dplyr)
library(here)

rm(list = ls())

setwd(here())

getwd()

setwd(here("check sample work", 
           "christians samples 2021",
           "TA",
           "2021-04-20-run"))

getwd()


results_filename <- "20210420-alk.titration.results"



#get character vector of samples ------------------------------------------------------

df_env = read.csv("20210420 sample metadata.csv",
                  header=T, stringsAsFactors=F, sep=",")

sample_list <- df_env$sample.id

len_loop <- length(sample_list)

results_list <- paste(sample_list, "results", sep = "-")

#read titration data into R -----------------------------------------------------------

for (i in 1:len_loop) {
#enter your csv file title below

filename <- sample_list[i]

#results <- paste0(filename, "_processed")
#print(results)

#read data table
#data <- read.csv(paste0(filename,".csv"))

df <- read.csv(paste0(filename,".csv"), header=T, stringsAsFactors=F, sep=",")

assign(sample_list[i], df)

}



#Titrant batch A17 density to molarity/normality calculation ----------------------------------
#parameters provided in batch documentation

#concentration = 0.100362 mol/kg +- 0.000009
#density = 1.02881 - ((1.061* 10^-4) * temperature) - ((4.10 * 10^-6) * temperature^2) grams/cm3
#density = 1.02449 at 22degC 
#HCl = 36.548 grams/mol

#temperature <- mean(data$temperature)

temperature_25_degC <- 25
 
density = 1.02881 - ((1.061* 10^-4) * temperature_25_degC) - ((4.10 * 10^-6) * temperature_25_degC^2) 
density <- signif(density,6)
print(density)
#1.02518

HCl_conc <- 0.100362

HCl_norm <- HCl_conc * (density / 1000) # mol/kg *(grams/cm3 * 1kg/1000grams) ends with units mol/cm3

HCl_norm <- HCl_norm * 1000 # (mol/cm3 * 1000cm3/L) ends with mol/L which is one to one for normality

C <- signif(HCl_norm,6)



#used to check values are what they should be
# S = data$S[1]
# print(S)
# 
# temperature = data$temperature 
# print(temperature)
# 
# pHTris = data$pHTris[1]
# print(pHTris)
# class(pHTris)
# 
# ETris=data$ETris[1]
# print(ETris)
# 
# E=data$E
# print(E)
# 
# weight=data$weight[1]
# print(weight)
# 
# volume=data$volume
# print(volume)

#calculate alkalinity -----------------------------------------------------------------------

# found data appending trick to use in a for loop here: https://stackoverflow.com/questions/29402528/append-data-frames-together-in-a-for-loop/29419402

datalist = list()

#ignore tibble warning in following loop. Yes tibbles are typically better yadda yadda yadda

for (i in 1:len_loop) {
  
data <- get(sample_list[i])


AT.25degC <- at(S=data$S[1], T=temperature_25_degC, pHTris=data$pHTris[1], d = density, C = C,
         ETris=data$ETris[1], E=data$E, weight=data$weight[1], volume=data$volume)

AT.lab.temp <- at(S=data$S[1], T=data$temperature, pHTris=data$pHTris[1], d = density, C = C,
         ETris=data$ETris[1], E=data$E, weight=data$weight[1], volume=data$volume)

AT.insitu.temp <- at(S=data$S[1], T=data$in.situ.temperature[1], pHTris=data$pHTris[1], d = density, C = C,
                ETris=data$ETris[1], E=data$E, weight=data$weight[1], volume=data$volume)

AT.25degC <- AT.25degC * 10^6

AT.lab.temp <- AT.lab.temp * 10^6

AT.insitu.temp <- AT.insitu.temp * 10^6

calculations <- data_frame(sample_list[i], AT.25degC, AT.lab.temp, AT.insitu.temp)

datalist[[i]] <- calculations # data appending trick: add it to your list

}

results_final <- bind_rows(datalist) #final data!!!


#saves a csv file with results

write.csv(results_final, file = paste0(results_filename,".csv"), row.names = FALSE) 






