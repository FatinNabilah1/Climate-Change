library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(shinythemes)
library(plotly)

# Define UI for miles per gallon application
shinyUI(fluidPage(theme = shinytheme("flatly"),
                  
                  # Application title
                  titlePanel("Global Climate Change (Analysis of Earth Surface Temperature)"),
                  navbarPage("", id="nav",
                             navbarMenu("Interactive Chart",
                                     tabPanel("Interactive Chart Major City",
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
                                                              plotOutput("RegPlotCities")
                                                      )
                                              )
                                     ),
                                     tabPanel("Interactive Chart Country",
                                              # Sidebar with controls to select Country and year
                                              sidebarLayout(
                                                      
                                                      sidebarPanel(
                                                              helpText("Select one or more country:"),
                                                              uiOutput("CountrySelectorIC2"),
                                                              
                                                              
                                                              helpText("Select years:"),
                                                              uiOutput("YearSelectorIC2"),
                                                      ),
                                                      
                                                      #Main Panel contains the plot/s
                                                      mainPanel(
                                                          plotlyOutput("RegPlotCountry")
                                                      )
                                              ))),
                             
                             navbarMenu("Data explorer",
                                     tabPanel("Data explorer Major City",
                                              basicPage(
                                                      DT::dataTableOutput("climatetableCity")
                                              )),
                                     tabPanel("Data explorer Country",
                                              basicPage(
                                                      DT::dataTableOutput("climatetableCountry")
                                              ))),
                             navbarMenu("World Map",
                                     tabPanel("World Map Major City",
                                               sidebarLayout(
                                                 
                                               sidebarPanel(
                                               helpText("Select month:"),
                                               uiOutput("MonthMap2Selector"),
                                               helpText("Select years:"),
                                               uiOutput("YearMap2Selector")
                                                     
                                                     
                                               ),
                                              #Main Panel contains the plot/s
                                              mainPanel(
                                              leafletOutput("worldmap2", height=500)
                                              ))
                                     ),
                                     tabPanel("World Map Country",
                                              sidebarLayout(
                                                      
                                                      sidebarPanel(
                                                              helpText("Select month:"),
                                                              uiOutput("MonthMapSelector"),
                                                              helpText("Select years:"),
                                                              uiOutput("YearMapSelector")
                                                              
                                                              
                                                      ),
                                                      #Main Panel contains the plot/s
                                                      mainPanel(
                                                              leafletOutput("worldmap", height=500)
                                                      ))
                                     )))
))