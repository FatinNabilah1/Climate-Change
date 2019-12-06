#Load Libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(mgcv)
library(DT)
library(rgdal)
library(leaflet)

#remove NA
#Load data
#cmodels_details <- read.csv("C:/Users/User/Documents/modified_file.csv", header=TRUE, sep = ",")
#saveRDS(A,"C:/Users/User/Documents/GlobalLandTemperaturesByMajorCity.rds")
cmodels_details<-read.csv("C:/Users/User/Documents/GlobalLandTemperaturesByMajorCity.csv", header=TRUE, sep = ",")

shinyServer(function(input, output) {
        
        #############################INTERACTIVE 1###################################
        output$overview<-renderText("This Shiny App provides a fast and easy way to explore the \"Earth Surface Temperature Data\" published on Kaggle")
 
        
        #read the 100 cities names (the unique values)
        CityNames<-unique(cmodels_details$City) 
        
        #get Country (the unique values)
        uniqueCountry<-unique(cmodels_details$Country) 
        
        #Cities names list
        output$CitySelector<-renderUI({
                selectInput('cities', 'City',
                            CityNames, 
                            multiple=TRUE, 
                            selectize=TRUE, 
                            selected="Jakarta") #default value
        })
        
        #Months abbreviation list
        output$MonthSelector<-renderUI({
                selectInput('months', 'Month', 
                            set_names(c(1:12),month.abb), 
                            multiple=TRUE, 
                            selectize=TRUE,
                            selected=1) #default January
        })
        
        #get the selected cities
        SelectedCity<-reactive({
                
                if(is.null(input$cities) || length(input$cities)==0)
                        return()
                as.vector(input$cities)
                
        })
        
        #get the selected month
        SelectedMonth<-reactive({
                
                if(is.null(input$months) || length(input$months)==0)
                        return()
                as.numeric(as.vector(input$months))
                
        })
        
        #filter the data according to the selected city and month/s
        citiesDF<-reactive({
                        cmodels_details %>%
                        filter(City %in% SelectedCity()) %>%
                        filter(mon %in% SelectedMonth())
        }) 
        
        output$ff <- renderPrint({
                names(citiesDF())
        })
        
        #get Check group input (type of plot)
        checkedVal <- reactive({
                as.vector(input$checkPlot)
             
        }) 
        
        #get Check group input (type of plot)
        checkedVal1 <- reactive({
                as.vector(input$checkPlot1)
                
        }) 
        
        #############################INTERACTIVE 1 PLOT###################################
        
        output$RegPlotCities<-renderPlot({
                #check if city and month are not null
                if ((length(SelectedCity())>0) && (length(SelectedMonth())>0))
                        
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
        ############################ INTERACTIVE 2 #######################################
        output$overview1<-renderText("This Shiny App provides a fast and easy way to explore the \"Earth Surface Temperature Data\" published on Kaggle")
        
        #get Years (the unique values)
        uniqueYears<-sort(unique(cmodels_details$years)) 
        
        #City names list
        output$CitysSelector<-renderUI({
                selectInput('citys', 'City',
                            CityNames, 
                            multiple=TRUE, 
                            selectize=TRUE, 
                            selected="Jakarta") #default value
        })
        
        #Year list
        output$yearSelector<-renderUI({
                selectInput('years', 'Year',
                            uniqueYears, 
                            multiple=FALSE, 
                            selectize=TRUE, 
                            selected="1984") #default value
        })
        
        #get the selected country
        SelectedCitys<-reactive({
                
                if(is.null(input$citys) || length(input$citys)==0)
                        return()
                as.vector(input$citys)
                
        })
        
        #get the selected Years
        SelectedYear<-reactive({
                
                if(is.null(input$years) || length(input$years)==0)
                        return()
                as.vector(input$years)
                
        })
        
        
        #filter the data according to the selected country
        citysDF<-reactive({
                cmodels_details %>%
                        filter(City %in% SelectedCitys()) %>%
                        filter(years %in% SelectedYear())
        }) 
        
        ############################INTERACTIVE 2 PLOT###################################
        output$RegPlotCountry<-renderPlot({
                #check if country are not null
                if ((length(SelectedCitys())>0))
                        
                {g<-ggplot(citysDF(),
                           aes(x=factor(mon),y=AverageTemperature,
                           color = City, group = City))+
                    geom_line(size = 2, alpha = 0.75) +
                    geom_point(size =3, alpha = 0.75) +
                        
                    ggtitle("Average Temperature per Years by City") +
                    labs(x="month",y="Average Temperature")+
                    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0.5)) +
                    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))+
                    theme_classic()
                
                g
                }
        })
        #############################DATA EXPLORER###################################
        output$climatetable = DT::renderDataTable({
                cmodels_details
        })
        
        ############################## WORLD MAP ####################################
        
        #Cities names list
        output$CitySelector1<-renderUI({
                selectInput('cities1', 'City',
                            CityNames, 
                            multiple=FALSE, 
                            selectize=TRUE, 
                            selected="Jakarta") #default value
        })
        
        #Years list
        output$YearsSelector<-renderUI({
                selectInput('years', 'Years',
                            uniqueYears, 
                            multiple=FALSE, 
                            selectize=TRUE, 
                            selected=1984) #default value
        })
        
        
        #get the selected cities
        SelectedCity1<-reactive({
                
                if(is.null(input$cities1) || length(input$cities1)==0)
                        return()
                as.vector(input$cities1)
                
        })
        
        #get the selected years
        SelectedYears<-reactive({
                
                if(is.null(input$years) || length(input$years)==0)
                        return()
                as.numeric(as.vector(input$years))
                
        })
        
        #filter the data according to the selected city and month/s
        citiesDF1<-reactive({
                cmodels_details %>%
                        filter(City %in% SelectedCity1()) %>%
                        filter(years %in% SelectedYears())
        }) 
        
        
        # Due to use of leafletProxy below, this should only be called once
        output$worldmap<-renderLeaflet({
                
                leaflet() %>%
                        addTiles()    
        })
        
        
        
        observe({
                theData<-citiesDF1() 
                
                # colour palette mapped to data
                pal <- colorQuantile("YlGn", theData$AverageTemperature, n = 9) 
                
                # set text for the clickable popup labels
                borough_popup <- paste0("<strong>City: </strong>", 
                                        theData$City, 
                                        "<br><strong>
                            Average Temp: </strong>", theData$AverageTemperature
                )
                
                # If the data changes, the polygons are cleared and redrawn, however, the map (above) is not redrawn
                leafletProxy("worldmap") %>%
                        clearShapes() %>%
                        addPolygons(lng = theData$Longitud,
                                    lat = theData$Latitude,
                                    fillOpacity = 0.8, 
                                    color = "black", 
                                    weight = 2,
                                    popup = borough_popup)  
                
        })
        
})