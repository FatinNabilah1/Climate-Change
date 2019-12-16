library(shiny)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(mgcv)
library(DT)
library(rgdal)
library(leaflet)
library(rworldmap)
library(plotly)
library(knitr)

#Load data set
climate_data<-read.csv("GlobalLandTemperaturesByMajorCity.csv", header=TRUE, sep = ",")
climate_data2<-read.csv("GlobalLandTemperaturesByCountry.csv", header=TRUE, sep = ",")
sea_level <- read.csv("epa-sea-level_csv.csv", header = TRUE, sep = ",")
CO2 <- read.table("CO2_level.txt", header = TRUE, "\t")

shinyServer(function(input, output) {
        
        output$markdownICCountry <- renderUI({
                
                HTML(markdown::markdownToHTML("ICCountry-Description.Rmd", fragment.only=TRUE))
                
        })
        
        
        output$markdownmapCity <- renderUI({
                
                HTML(markdown::markdownToHTML("MapCity-Description.Rmd", fragment.only=TRUE))
                
        })
        
        output$markdownmapCountry <- renderUI({
                
                HTML(markdown::markdownToHTML("MapCountry-Description.Rmd", fragment.only=TRUE))
                
        })
        
        #read the 100 cities names (the unique values)
        CityNames<-unique(climate_data$City) 
        
        #Cities names list
        output$CitySelectorIC1<-renderUI({
                selectInput('cities', 'City',
                            CityNames, 
                            multiple=TRUE, 
                            selectize=TRUE, 
                            selected="Jakarta") #default value
        })
        
        #Months abbreviation list
        output$MonthSelectorIC1<-renderUI({
                selectInput('months', 'Month', 
                            set_names(c(1:12),month.abb), 
                            multiple=TRUE, 
                            selectize=TRUE,
                            selected=1) #default January
        })
        
        #get the selected cities in Interactive CHart 1
        SelectedCityIC1<-reactive({
                
                if(is.null(input$cities) || length(input$cities)==0)
                        return()
                as.vector(input$cities)
                
        })
        
        #get the selected month in Interactive Chart 1
        SelectedMonthIC1<-reactive({
                
                if(is.null(input$months) || length(input$months)==0)
                        return()
                as.numeric(as.vector(input$months))
                
        })
        
        #filter the data according to the selected city and month/s
        citiesDF<-reactive({
                climate_data %>%
                        filter(City %in% SelectedCityIC1()) %>%
                        filter(mon %in% SelectedMonthIC1())
        }) 
        
        #get Check group input (type of plot)
        checkedVal <- reactive({
                as.vector(input$checkPlot)
                
        }) 
        
        #get Check group input (type of plot)
        checkedVal1 <- reactive({
                as.vector(input$checkPlot1)
                
        }) 
        
        
        
        ############################ INTERACTIVE CHART 1 #######################################
        
        output$RegPlotCities<-renderPlot({
                #check if city and month are not null
                if ((length(SelectedCityIC1())>0) && (length(SelectedMonthIC1())>0))
                        
                {g<-ggplot(citiesDF(),
                           aes(x=years,y=AverageTemperature,
                               colour=factor(mon)))+
                        labs(x="Year",
                             y="Average Temperature")+
                        facet_wrap(~City)+
                        scale_color_discrete(name="Month",
                                             breaks=c(1:12),
                                             labels=month.abb)
                
                if ("GAM Plot" %in% checkedVal())
                        
                        g<-g+stat_smooth(method="gam", formula=y~s(x),se=FALSE)
                
                if ("Point Plot" %in% checkedVal())
                        
                        g<-g+geom_point(aes(alpha=0.4))+
                                guides(alpha=FALSE)
                
                g
                }
        })
        ############################ INTERACTIVE CHART 2 #######################################
        #get Years from data set Country (the unique values)
        uniqueYearsCountry<-sort(unique(climate_data2$years)) 
        
        #get country (the unique values)
        CountryNames<-sort(unique(climate_data2$Country)) 
        
        #COuntry names list
        output$CountrySelectorIC2<-renderUI({
                selectInput('country', 'Country',
                            CountryNames, 
                            multiple=TRUE, 
                            selectize=TRUE, 
                            selected="Indonesia") #default value
        })
        
        #get the selected Country
        SelectedCountryIC2<-reactive({
                
                if(is.null(input$country) || length(input$country)==0)
                        return()
                as.vector(input$country)
                
        })
        
        #Year list
        output$YearSelectorIC2<-renderUI({
                selectInput('years', 'Year',
                            uniqueYearsCountry, 
                            multiple=FALSE, 
                            selectize=TRUE, 
                            selected="1984") #default value
        })
        
        #get the selected years
        SelectedYearIC2<-reactive({
                
                if(is.null(input$years) || length(input$years)==0)
                        return()
                as.numeric(as.vector(input$years))
                
        })
        
        #filter the data according to the selected country
        countryDF<-reactive({
                climate_data2 %>%
                        filter(years %in% SelectedYearIC2())%>%
                        filter(Country %in% SelectedCountryIC2())
        }) 
        
        output$RegPlotCountry<-renderPlotly({
                #check if city are not null
                if ((length(SelectedCountryIC2())>0))
                        
                {g<-ggplot(countryDF(),
                           aes(x=factor(mon),y=AverageTemperature,
                               color = Country, group = Country,
                               text = paste("Average Temperature: ", AverageTemperature,
                                            "<br>Country: ", Country,
                                            "<br>Month: ", factor(mon),
                                            "<br>Year: ", years)))+
                        ylim(0,40)+
                        geom_line(size = 1, alpha = 0.75) +
                        geom_point(size =1, alpha = 0.75) +
                        
                        ggtitle("Average Temperature per Years by Selected Country") +
                        labs(x="Month",y="Average Temperature")+
                        theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0.5)) +
                        theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))+
                        theme_classic()
                
                ggplotly(g, tooltip = "text")
                }
        })
        
        
        #############################DATA EXPLORER###################################
        #Data explorer for Major City data
        output$climatetableCity = DT::renderDataTable({
                climate_data
        })
        
        #Data explorer for Country data
        output$climatetableCountry = DT::renderDataTable({
                climate_data2
        })
        
        ############################## WORLD MAP Major CIty Data ####################################
        
        #Months abbreviation list
        output$MonthMapSelector<-renderUI({
                sliderInput("monthMap",
                            "Month:",
                            min = 1,
                            max = 12,
                            value = 1,
                            step = 1,
                            width = "100%")
        })
        
        
        
        #Years list
        output$YearMapSelector<-renderUI({
                sliderInput("yearMap",
                            "Year:",
                            min = min(uniqueYearsCountry),
                            max = max(uniqueYearsCountry),
                            value = 1,
                            step = 1,
                            width = "100%")
        })
        
        
        #get the selected Month
        SelectedMonthMap<-reactive({
                
                if(is.null(input$monthMap) || length(input$monthMap)==0)
                        return()
                as.numeric(as.vector(input$monthMap))
                
        })
        
        #get the selected years
        SelectedYearMap<-reactive({
                
                if(is.null(input$yearMap) || length(input$yearMap)==0)
                        return()
                as.numeric(as.vector(input$yearMap))
                
        })
        
        #filter the data according to the selected year and month/s
        ymDF<-reactive({
                climate_data2 %>%
                        filter(years %in% SelectedYearMap())%>%
                        filter(mon %in% SelectedMonthMap())
        }) 
        
        pal <- colorBin(rev(RColorBrewer::brewer.pal(10,'RdYlBu')), domain = climate_data2$AverageTemperature, bins = 9)
        
        output$worldmap <- renderLeaflet({
                leaflet(options = leafletOptions(minZoom = 2))%>%
                        addLegend("bottomright", pal = pal, values = climate_data2$AverageTemperature,
                                  title = "Temperature Range",
                                  opacity = 1)
                
        })
        
        observe({
                if(!is.null(input$yearMap)){
                        map <- joinCountryData2Map(ymDF()
                                                   , joinCode = "ISO3"
                                                   , nameJoinColumn = "ISO3V10")
                        
                        leafletProxy("worldmap", data = map) %>%
                                addTiles() %>% 
                                clearShapes() %>% 
                                addPolygons(fillColor = ~pal(map$AverageTemperature),
                                            weight = 2,
                                            opacity = 1,
                                            color = "white",
                                            dashArray = "3",
                                            fillOpacity = 1,
                                            highlight = highlightOptions(
                                                    weight = 5,
                                                    color = "white",
                                                    dashArray = "3",
                                                    fillOpacity = 100,
                                                    bringToFront = TRUE),
                                            label = ~paste("Country: ",map$Country,
                                                           ", Average Temperature: ", map$AverageTemperature))
                        
                }})
        
        
        
        #####################################WORLD MAP Country Data###############################################
        
        #get Years from data set Major City (the unique values)
        uniqueYearsCity<-sort(unique(climate_data$years)) 
        
        #Months abbreviation list
        output$MonthMap2Selector<-renderUI({
                selectInput('monthMap2', 'Month', 
                            set_names(c(1:12),month.abb), 
                            multiple=FALSE, 
                            selectize=TRUE,
                            selected=1) #default January
        })
        
        #Years list
        output$YearMap2Selector<-renderUI({
                selectInput('yearMap2', 'Year',
                            uniqueYearsCity, 
                            multiple=FALSE, 
                            selectize=TRUE, 
                            selected="1984") #default value
        })
        
        
        #get the selected Month
        SelectedMonthMap2<-reactive({
                
                if(is.null(input$monthMap2) || length(input$monthMap2)==0)
                        return()
                as.numeric(as.vector(input$monthMap2))
                
        })
        
        #get the selected years
        SelectedYearMap2<-reactive({
                
                if(is.null(input$yearMap2) || length(input$yearMap2)==0)
                        return()
                as.numeric(as.vector(input$yearMap2))
                
        })
        
        #filter the data according to the selected year and month/s
        ymDF2<-reactive({
                climate_data %>%
                        filter(years %in% SelectedYearMap2())%>%
                        filter(mon %in% SelectedMonthMap2())
        }) 
        
        # Due to use of leafletProxy below, this should only be called once
        output$worldmap2<-renderLeaflet({
                leaflet(options = leafletOptions(minZoom = 1.8)) %>%
                        addTiles()    
        })
        
        
        
        observe({
                map2<-ymDF2() 
                
                # set text for the clickable popup labels
                borough_popup <- paste0("<strong>City: </strong>", 
                                        map2$City, 
                                        "<br><strong>
                            Average Temp: </strong>", map2$AverageTemperature
                )
                
                # If the data changes, the polygons are cleared and redrawn, however, the map (above) is not redrawn
                leafletProxy("worldmap2",data = map2) %>%
                        clearShapes() %>%
                        #addCircleMarkers(lng = map2$Longitude,
                        #                 lat = map2$Latitude,
                        #                 fillColor = "black",
                        #                 fillOpacity = 0.5, 
                        #                 color = "black", 
                        #                 weight = 1,
                        #                 popup = borough_popup)  
                        addMarkers(~map2$Longitude, ~map2$Latitude, popup = borough_popup)
                
        })
        
        
        #####################################Correlation Data###############################################
        
        
        #read table and preprocess data
        mean_temp <- aggregate(climate_data$AverageTemperature, by = list(climate_data$years), FUN = mean, na.rm = TRUE)
        names(mean_temp) <- c("year", "avg_T")
        sea_level$Year <- as.Date(sea_level$Year)
        sea_level$YYYY <- as.numeric(format(sea_level$Year,'%Y'))
        CO2$Total_CO2 <- CO2$Total*3.667
        
        # merge and subset data between year 1900 and 2013
        df <- merge(mean_temp, sea_level, by.x = "year", by.y = "YYYY")
        df <- merge(df, CO2, by.x = "year", by.y = "Year")
        df <- subset(df, df$year >= 1900 & df$year <= 2013)
        df <- data.frame(df$year, df$avg_T, df$CSIRO.Adjusted.Sea.Level, df$Total_CO2)
        names(df) <- c("year", "global_average_temperature", "sea_level_relative_to_year_1880", "anthropogenic_CO2_emission")
        
        
        output$CorrelationPlot<-renderPlot({
                #melt the data and plot a stacked line graph
                df.m <- melt(df, "year")
                ggplot(df.m, aes(year, value, colour = variable)) + geom_line() +
                        facet_wrap(~variable, ncol = 1, scales = "free_y", 
                                   strip.position = "left", 
                                   labeller = as_labeller(c(global_average_temperature = "deg Celcius",
                                                            sea_level_relative_to_year_1880 = "inch",
                                                            anthropogenic_CO2_emission = "million metric tons")))  +
                        ylab(NULL)
                
        })
        
        output$CorrelationPlot2<-renderPlot({
                
                #To create a pairplot with correlation on the bottom triangle
                # Correlation panel
                panel.cor <- function(x, y){
                        usr <- par("usr"); on.exit(par(usr))
                        par(usr = c(0, 1, 0, 1))
                        r <- round(cor(x, y), digits=4)
                        txt <- paste0("R = ", r)
                        cex.cor <- 0.8/strwidth(txt)
                        text(0.5, 0.5, txt, cex = cex.cor * r)
                }
                # Customize upper panel
                upper.panel<-function(x, y){
                        points(x,y, pch = 19, col = "limegreen")
                        abline(lm(y~x), col = "red", lwd = 3)
                }
                # Create the plots
                pairs(df[,2:4], 
                      lower.panel = panel.cor,
                      upper.panel = upper.panel)
        })
        
})