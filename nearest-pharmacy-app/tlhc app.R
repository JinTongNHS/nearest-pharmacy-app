#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#renv::init()
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

#Notes
#For the Targeted lung health check version - comment out all of the services below
#in the Radiobuttions section except the first one and uncomment: "Only pharmacies registered to deliver targeted lung health check services"

#Do the same for the SCS only version

ui <- fluidPage(
  
  tags$head(tags$title("PhIF Find your pharmacy")),
  
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
                        choices = list("Pharmacies delivering essential services (including NHS Discharge Medicines Service)" = "All", 
                                       "Only pharmacies registered to deliver the targeted long health check service pilot" = "tlhc"), 
                        selected = "All")
    ),
    column(3,
           h5("This application is currently using the"),
           textOutput("pharmListDate"),
           h5("Pharmaceutical List")
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
