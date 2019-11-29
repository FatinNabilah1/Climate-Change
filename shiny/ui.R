#Load Libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)

# Define UI for miles per gallon application
shinyUI(fluidPage(
        
        # Application title
        titlePanel("Climate Change in Major Country"),
        navbarPage("", id="nav",
           tabPanel("Interactive map",
                # Sidebar with controls to select city, month and type of plot
                sidebarLayout(
        
                        sidebarPanel(
                                helpText("Select one or more cities:"),
                                
                                uiOutput("CitySelector"),
                                
                                helpText("Select one or more months:"),
                                uiOutput("MonthSelector"),
                                
                                helpText("Select type of plot or histogram:"),
                                
                                checkboxGroupInput("checkPlot", 
                                                   label = ("Plots"), 
                                                   choices=c("GAM Plot","Point Plot"),
                                                   selected = "GAM Plot"
                                ),
                                
                                helpText("Dataset is available below:"),
                                tags$a(href = "https://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data", "Source")
               
                        ),
                        
                        #Main Panel contains the plot/s
                        mainPanel(
        
                                textOutput("overview"),
                                plotOutput("RegPlot")
                                )
                        )
           ),
           tabPanel("Data explorer",
                    basicPage(
                            DT::dataTableOutput("climatetable")
                    )),
           tabPanel("World Map",
                    basicPage(
                            leafletOutput("pal", height=400)
                           
                    ))
        )
))
