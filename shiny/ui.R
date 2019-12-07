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
           tabPanel("Interactive Chart 1",
                # Sidebar with controls to select city, month and type of plot
                sidebarLayout(
        
                        sidebarPanel(
                                helpText("Select one or more cities:"),
                                uiOutput("CitySelectorIC1"),
                                
                                helpText("Select one or more months:"),
                                uiOutput("MonthSelectorIC1"),
                                
                                helpText("Select type of plot:"),
                                
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
                                plotOutput("RegPlotCities")
                                )
                        )
           ),
           tabPanel("Interactive Chart 2",
                    # Sidebar with controls to select City and year
                    sidebarLayout(
                            
                            sidebarPanel(
                                    helpText("Select one or more country:"),
                                    uiOutput("CitySelectorIC2"),
                                    
                                    
                                    helpText("Select years:"),
                                    uiOutput("YearSelectorIC2"),
                            ),
                            
                            #Main Panel contains the plot/s
                            mainPanel(
                                    
                                    textOutput("overview1"),
                                    plotOutput("RegPlotCountry")
                            )
                    )),
           tabPanel("Data explorer",
                    basicPage(
                            DT::dataTableOutput("climatetable")
                    )),
           tabPanel("World Map",
                    sidebarLayout(
                            
                            sidebarPanel(
                                    helpText("Select month:"),
                                    uiOutput("MonthMapSelector"),
                                    helpText("Select years:"),
                                    uiOutput("YearMapSelector")
                                        
                           
                    ),
                    #Main Panel contains the plot/s
                    mainPanel(
                            leafletOutput("worldmap", height=400)
                    ))
        ),
        tabPanel("World Map 2",
                 sidebarLayout(
                         
                         sidebarPanel(
                                 helpText("Select month:"),
                                 uiOutput("MonthMap2Selector"),
                                 helpText("Select years:"),
                                 uiOutput("YearMap2Selector")
                                 
                                 
                         ),
                         #Main Panel contains the plot/s
                         mainPanel(
                                 leafletOutput("worldmap2", height=400)
                         ))
        ))
))
