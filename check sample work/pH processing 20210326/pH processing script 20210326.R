
#### pH data processing for Nielsen Lab ####


#### load libraries ####

library(dplyr)
library(here)


##### set working directory and clear workspace ###############################


setwd(here())

getwd()

setwd(here("check sample work", "pH processing 20210326"))

getwd()

#list all files in directory

list.files()

# "20201117_results.csv"
    

                 

#clear all plots and data

rm(list = ls())

#enter your csv file title below

filename <- "20201117_results"

results <- paste0(filename, "_processed")

print(results)

#read data table
data <- read.csv(paste0(filename,".csv"))

#add desired columns for calculations to work

data$a1a2 <- NA

data$a1a2crct <- NA

data$pH_is <- NA

data$pH_a <- NA

data$pH_is_crct <- NA

data$pH_a_crct <- NA


# The following variables and calculations are worked up as written in section 8.4 of
#   the SOP 6b (version 3.01) in the Guide to Best Practices for Ocean CO2 Measurements: 



#extinction coefficient ratios for m-cresol purple dye
# e1(HI-)/e2(HI-)

e1 <- 0.00691

# e1(I2-)/e2(HI-)

e2 <- 2.2220


# e2(I2-)/e2(HI-)

e3 <- 0.1331


####calculations ####


# calculates length of data table
# then creates sequence by 2 to pick the second row of each sample analyzed
# so the for loop below can find where to pick up the data

len <- seq(2, length(data$X578.nm) - 1, by = 2)



for (i in len){

  
#enter dye volume used in microliters (yes I know the variable is labled mL and not uL, b/c im lame and tired) #
  
dyeV_mL <- data$dyeV[i+1]
    
#volume converted to cubic cm
    
dyeV_cm3  <- dyeV_mL/1000
  
# in situ temperature
  
tsc <- data$temperature[i]
  
tsk <- tsc + 273.15  
  
# enter analysis temperature in degree C
  
tac <-  25
  
tak <- tac + 273.15  
  
# in situ salinity    
  
ss <- data$salinity[i]
  

  

# pK2 of m-cresol dye based on sample temperature
  
pk2_s <- (1245.69/tsk) +3.8275+0.00211*(35 - ss)  
  
# pk2 of on m-cresol dye based on analysis temperature
  
pk2_a <- (1245.69/tak) +3.8275+0.00211*(35 - ss)  
  
    
# a1/a2 difference in absorbances


a1a2 <- (data$X578.nm[i+1] - data$X578.nm[i] -(data$X730.nm[i+1] - data$X730.nm[i])) / 
            (data$X434.nm[i+1] - data$X434.nm[i]-(data$X730.nm[i+1] - data$X730.nm[i]))

a1a2crctd <- a1a2 - (dyeV_cm3 * (0.125 - 0.147*a1a2))

#calculation of pH using known extinction coefficients and calculated activity coefficient

pH_in_situ <- pk2_s + log10((a1a2-e1)/(e2- (a1a2 * e3)))

pH_analysis <- pk2_a + log10((a1a2-e1)/(e2- (a1a2 * e3)))

pH_in_situ_crctd <- pk2_s + log10((a1a2crctd-e1)/(e2- (a1a2crctd * e3)))

pH_analysis_crctd <- pk2_a + log10((a1a2crctd-e1)/(e2- (a1a2crctd * e3)))

#append calculations to data frame

data$a1a2[i] <- a1a2

data$a1a2crct[i] <- a1a2crctd

data$pH_is[i] <- pH_in_situ

data$pH_a[i] <- pH_analysis

data$pH_is_crct[i] <- pH_in_situ_crctd

data$pH_a_crct[i] <- pH_analysis_crctd


}

#saves a csv file appended with results

write.csv(data, file = paste0(results,".csv"), row.names = F) 

