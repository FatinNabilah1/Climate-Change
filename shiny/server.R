#Load Libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(mgcv)
library(DT)
library(leaflet)

#remove NA
#Load data
#cmodels_details <- read.csv("C:/Users/User/Documents/modified_file.csv", header=TRUE, sep = ",")
#cmodels_details <- na.omit(cmodels_details)
#saveRDS(A,"C:/Users/User/Documents/GlobalLandTemperaturesByMajorCity.rds")
cmodels_details<-read.csv('./modified_file.csv', header=TRUE, sep = ",")

shinyServer(function(input, output) {
        
        output$overview<-renderText("This Shiny App provides a fast and easy way to explore the \"Earth Surface Temperature Data\" published on Kaggle")
        
        
        #read the 100 cities names (the unique values)
        CityNames<-unique(cmodels_details$City) 
        
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
        
        ############PLOT#########
        output$RegPlot<-renderPlot({
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
        #########################
        output$climatetable = DT::renderDataTable({
                cmodels_details
        })
        
        output$pal <- 
                renderLeaflet({
                        colorQuantile("YlOrRd", NULL, n = 8)
                        leaflet(cmodels_details) %>% addTiles() %>%
                        addCircleMarkers(cmodels_details$Longitude, cmodels_details$Latitude, color = ~pal(tann))
                        
                })
        
})