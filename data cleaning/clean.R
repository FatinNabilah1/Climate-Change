#import the neccessary library
library(xml2) 
library(rvest)
library(stringr)
library(jsonlite)


climate_data <- read.csv('GlobalLandTemperaturesByMajorCity.csv', header=TRUE, sep = ",")
#remove null
climate_data <- na.omit(climate_data)

#format the date to y-m-d
climate_data$dt = as.Date(climate_data$dt, "%Y-%m-%d")
mon <- as.numeric(format(climate_data$dt,'%m'))
years <- as.numeric(format(climate_data$dt,'%Y'))


#combine the data
climate_data <- climate_data[,c(1,2,3,4,5,6,7)]
climate_data_tmp <- cbind(climate_data, mon, years)

#remove null data
climate_data <- na.omit(climate_data_tmp)
write.csv(climate_data, "modified_file1.csv" ,row.names=FALSE)
#saveRDS(A,"C:/Users/User/Documents/modified_file.rds")