#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)
library(tidyverse)
#https://docs.ropensci.org/PostcodesioR/
#install.packages("PostcodesioR")
library(PostcodesioR)
library(DBI)
library(odbc)
library(readxl)
#install.packages("shinythemes")
library(sf)
#library(tmap)
library(leaflet)
library(tidygeocoder)
library(shinythemes)


# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Smoking Cessation Nearest Pharmacy Finder"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#           textInput("postcode", label = h3("Postcode Input"), value = "E.g. SE1 8UG"),
#           hr()
#           ),
# 
#         mainPanel(
#           textInput("postcode", label = h3("Postcode Input"), value = "E.g. SE1 8UG"),
#           tableOutput('nearestPharmsTable')
#         )
#     )
# )

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinythemes::shinytheme("superhero"),
  
  titlePanel("Nearest Pharmacy Finder"),
  
  fluidRow(
    column(3,
           textInput("postcode", label = h5("Postcode to search:"), value = "SE1 8UG")),
    column(3,
           numericInput("numPharms", label = h5("Number of pharmacies:"), value = 5)
    ),
    column(3,
           radioButtons("serviceType", label = h5("Search across"),
                        choices = list("All pharmacies" = "All", 
                                       "Only pharmacies delivering smoking cessation services" = "smoking",
                                       "Only pharmacies delivering CPCS services" = "cpcs",
                                       "Only pharmacies delivering contraception services" = "contraception",
                                       "Only pharmacies delivering blood pressure checks" = "bp",
                                       "Only pharmacies delivering NMS services" = "nms"), 
                        selected = "All")
    ),
    column(3,
           h5("This application is currently using the"),
           textOutput("pharmListDate"),
           h5("Pharmacutical List")
    )
           
  ),

  hr(),
  leafletOutput("map"),
  hr(),
  tableOutput("nearestPharmsTable")

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  
  source("functions.R")
  

  output$nearestPharmsTable <- renderTable({ get_nearest_pharmacies(search_postcode = input$postcode, 
                                                                    pharm_df = pharmlist, 
                                                                    num_pharms = input$numPharms,
                                                                    serviceType = input$serviceType) })
  
  output$pharmListDate <- renderText({ get_latest_pharm_list_date() })
  
  output$map <- renderLeaflet({
    create_leaflet(search_postcode = input$postcode, 
                   num_pharms = input$numPharms,
                   serviceType = input$serviceType)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
