#import the neccessary library
library(xml2) 
library(rvest)
library(stringr)
library(jsonlite)
library(stringi)



climate_data <- read.csv('GlobalLandTemperaturesByCity.csv', header=TRUE, sep = ",")
#climate_data <- read.csv('GlobalLandTemperaturesByMajorCity.csv', header=TRUE, sep = ",")
#climate_data <- read.csv('GlobalLandTemperaturesByCountry.csv', header=TRUE, sep = ",")
#remove null
climate_data <- na.omit(climate_data)

#format the date to y-m-d
climate_data$dt = as.Date(climate_data$dt, "%Y-%m-%d")
mon <- as.numeric(format(climate_data$dt,'%m'))
years <- as.numeric(format(climate_data$dt,'%Y'))


#combine the data
climate_data <- climate_data[,c(2,3,4,5,6,7)]
climate_data_tmp <- cbind(climate_data, mon, years)

#remove null data
climate_data_tmp <- na.omit(climate_data_tmp)

climate_data_tmp <- subset(climate_data_tmp, years > 1980)

#climate_data_tmp$Latitude1 <- str_replace(paste("-",climate_data$Latitude[match(stri_sub(climate_data$Latitude, -1), "S")],sep=" "), "S","")
#climate_data_tmp$Latitude1 <- str_replace(climate_data_tmp$Latitude, "N","")
#climate_data_tmp$Longitude2 <- str_replace(climate_data_tmp$Longitude, "E","")
#climate_data_tmp$Longitude2 <- str_replace(paste("",climate_data_tmp$Longitude,sep=""), "W","")


write.csv(climate_data_tmp, "GlobalLandTemperaturesByCity.csv" ,row.names=FALSE)
#write.csv(climate_data_tmp, "GlobalLandTemperaturesByMajorCity.csv" ,row.names=FALSE)
#write.csv(climate_data_tmp, "GlobalLandTemperaturesByCountry.csv" ,row.names=FALSE)
#saveRDS(A,"C:/Users/User/Documents/modified_file.rds")