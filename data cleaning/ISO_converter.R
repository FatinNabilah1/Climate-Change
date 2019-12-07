#import library
library(magrittr)
library(rvest)

#retrive the ISO code
url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"
iso_codes <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="CountryCode"]') %>%
  html_table()
iso_codes <- iso_codes[[1]][, -1]
iso_codes <- iso_codes[!apply(iso_codes, 1, function(x){all(x == x[1])}), ]
names(iso_codes) <- c("Country", "ISO2", "ISO3", "UN")


#################################################################################################################

#import library
library(maps)
library(ggplot2)

world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)
head(world_data)

#change the country name in the climate data country to new name
climate_data <- read.csv("GlobalLandTemperaturesByCity.csv", header = TRUE, stringsAsFactors = FALSE)
#climate_data <- read.csv("GlobalLandTemperaturesByMajorCity.csv", header = TRUE, stringsAsFactors = FALSE)
#climate_data <- read.csv("GlobalLandTemperaturesByCountry.csv", header = TRUE, stringsAsFactors = FALSE)

climate_data <- na.omit(climate_data)
old_names <- c("French Southern and Antarctic Lands", "Antigua", "Barbuda", "Saint Barthelemy", "Brunei", "CÃ´te D'Ivoire",
               "Congo (Democratic Republic Of The)", "Republic of Congo", "Falkland Islands", "Micronesia", "UK", 
               "Heard Island", "Cocos Islands", "Iran", "Nevis", "Saint Kitts", "South Korea", "Laos", "Saint Martin",
               "Macedonia", "Pitcairn Islands", "North Korea", "Palestine", "Russia", "South Sandwich Islands",
               "South Georgia", "Syria", "Trinidad", "Tobago", "Taiwan", "Tanzania", "USA", "Vatican", "Grenadines",
               "Saint Vincent", "Venezuela", "Vietnam", "Wallis and Fortuna", "United States", "Burma", "Ã.land", "Virgin Islands",
               "Africa","Antigua And Barbuda","Bosnia And Herzegovina","Congo","Denmark (Europe)","Falkland Islands (Islas Malvinas)"
               ,"Federated States Of Micronesia","France (Europe)","French Southern And Antarctic Lands","Guinea Bissau","Heard Island And Mcdonald Islands",
               "Hong Kong","Isle Of Man","Macau","Netherlands (Europe)","Palestina","Reunion","Saint BarthÃ©lemy","Saint Kitts And Nevis",
               "Saint Pierre And Miquelon","Saint Vincent And The Grenadines","Sao Tome And Principe","South Georgia And The South Sandwich Isla",
               "Svalbard And Jan Mayen","Timor Leste","Trinidad And Tobago","Turks And Caicas Islands","United Kingdom (Europe)")

new_names <- c("French Southern Territories", rep("Antigua and Barbuda", 2), "Saint-Barthélemy",
               "Brunei Darussalam", "Côte d'Ivoire", "Congo, (Kinshasa)", "Congo (Brazzaville)", 
               "Falkland Islands (Malvinas)", "Micronesia, Federated States of", "United Kingdom",
               "Heard and Mcdonald Islands", "Cocos (Keeling) Islands", "Iran, Islamic Republic of",
               rep("Saint Kitts and Nevis", 2), "Korea (South)", "Lao PDR", "Saint-Martin (French part)",
               "Macedonia, Republic of", "Pitcairn", "Korea (North)", "Palestinian Territory", "Russian Federation",
               rep("South Georgia and the South Sandwich Islands", 2), 
               "Syrian Arab Republic (Syria)", rep("Trinidad and Tobago", 2), "Taiwan, Republic of China",
               "Tanzania, United Republic of", "United States of America", "Holy See (Vatican City State)",
               rep("Saint Vincent and Grenadines", 2), "Venezuela (Bolivarian Republic)", "Viet Nam", "Wallis and Futuna Islands",
               "United States of America", "Myanmar","Aland Islands","Virgin Islands, US","Central African Republic","Antigua and Barbuda",
               "Bosnia and Herzegovina","Congo (Brazzaville)","Denmark","Falkland Islands (Malvinas)","Micronesia, Federated States of",
               "France","French Southern Territories","Guinea-Bissau","Heard and Mcdonald Islands","Hong Kong, SAR China","Netherlands","Palestinian Territory",
               "Réunion","Saint-Barthélemy","Saint Kitts and Nevis","Saint Pierre and Miquelon","Saint Vincent and Grenadines","Sao Tome and Principe",
               "South Georgia and the South Sandwich Islands","Svalbard and Jan Mayen Islands","Timor-Leste","Trinidad and Tobago",
               "Turks and Caicos Islands","United Kingdom")

for (i in 1:length(old_names)){
  climate_data$Country[climate_data$Country == old_names[i]] <- new_names[i]
}

for (i in 1:length(old_names)){
  
  world_data$region[world_data$region == old_names[i]] <- new_names[i]
  
}
#create new column in climate data file, and match the ISO code with the new name of country
climate_data['ISO3V10'] <- iso_codes$ISO3[match(climate_data$Country, iso_codes$Country)]
world_data["ISO3"] <- iso_codes$ISO3[match(world_data$region, iso_codes$Country)]

climate_data <- na.omit(climate_data)

write.csv(climate_data, "GlobalLandTemperaturesByCity.csv" ,row.names=FALSE)
#write.csv(climate_data, "GlobalLandTemperaturesByMajorCity.csv" ,row.names=FALSE)
#write.csv(climate_data, "GlobalLandTemperaturesByCountry.csv" ,row.names=FALSE)
